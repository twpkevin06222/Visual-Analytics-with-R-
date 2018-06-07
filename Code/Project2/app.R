library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(DT)
library(formattable)
library(plyr)
library(dplyr)
library(stats)
library(tidyr)
library(cluster)

#read data
data <- read_rds("babynames.rds")

#ui------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Clustering Baby Names"),
  dashboardSidebar(
    sliderInput("kInput", "Number of cluster",min = 1, max = 128,
                value = c(10))
  ),
  dashboardBody(
    titlePanel("First Assignment"),

    fluidRow(
      box(
        title = 'K-Clustering', status = "primary", solidHeader = TRUE,
        plotOutput("k_cluster")
      )
    )
  )
)
#server---------------------------------------------------------------------------
server <- function(input,output){

k_c <- reactive({
  m1 <- count(data,forename) #count occurence of forename throughout the dataset
  m2 <- m1[!rowSums(m1[2]<5),]  #delete row less than 5 in m1
  m5 <- merge(m2,data,by.x='forename')
  m6 <- expand.grid(forename=unique(m5$forename), n = unique(m5$n), sex = unique(m5$sex))
  m7 <- merge(m5,m6, all = TRUE)
  m7$rank[is.na(m7$rank)] <- 0  #convert na = 0
  m7$popularity <- ifelse(m7$rank==0, yes=0,no=1-(m7$rank-min(m7$rank))/(max(m7$rank)-min(m7$rank)))
  m11 <- aggregate(popularity~forename+year,m7,sum)
  m12 <- spread(m11,year,popularity)
  m13 <- m12
  m13[is.na(m13)] <- 0 #convert na to 0
  medoids <- m13
  medoids$forename <- NULL #change column name to null
  pam.result <- pam(medoids,input$kInput)
  return(pam.result)
})
  
  #Main Output---------------------------------------------------------------------
  output$k_cluster <- renderPlot({
    clusplot(k_c(), main ='Forename Clustering')
  })
}
#End
#----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
