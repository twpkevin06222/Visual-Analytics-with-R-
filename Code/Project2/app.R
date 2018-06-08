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
library(factoextra)

#read data
data <- read_rds("babynames.rds")

#ui------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Clustering Baby Names"),
  dashboardSidebar(
    disable=TRUE
  ),
  dashboardBody(
    titlePanel("First Assignment"),
    fluidRow(
      box(
        title = 'K-Clustering', status = "primary", solidHeader = TRUE,
        sliderInput("kInput", "Number of cluster",min = 1, max = 40,
                    value = c(10)),
        selectInput("yesnoInput","Toggle labels",choices = c('no','yes')),
        plotOutput("k_cluster")
      )
    ),
    titlePanel("Second Assignment"),
    fluidRow(
      box(
        title="Silhouette", status = "primary", solidHeader = TRUE,
        radioButtons("radio_yes_noInput","Choice of k input:", choices = c('Auto','Manual')),
        sliderInput("k02Input", "Number of cluster (works only if choice of k is manual):",min=1, max=20,
                    value = c(10)),
        plotOutput("s_plot")
      ),
      box(
        title='Table with respect to Cluster', status = "primary", solidHeader = TRUE,
        dataTableOutput('clusterTable')
      )
    )
  )
)
#server---------------------------------------------------------------------------
server <- function(input,output){
#1--------------------------------------------------------------------------------
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
  forename <- m12$forename
  m13[is.na(m13)] <- 0 #convert na to 0
  medoids <- m13
  medoids$forename <- NULL #change column name to null
  rownames(medoids) <- forename
  pam.result <- pam(medoids,input$kInput)
  return(pam.result)
})
#2---------------------------------------------------------------------------------------
dt_c <- reactive({
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
  forename <- m12$forename
  m13[is.na(m13)] <- 0 #convert na to 0
  medoids <- m13
  medoids$forename <- NULL #change column name to null
  rownames(medoids) <- forename
  return(medoids)
})

  #Main Output---------------------------------------------------------------------
  output$k_cluster <- renderPlot({
  if(input$yesnoInput=='no'){
    fviz_cluster(k_c(), geom='point',show.clust.cent = TRUE,mean.point.size=8, label.rectangle=TRUE)+ ggtitle(paste('Baby Forename Clustering with k =',input$kInput))
  }else{
    fviz_cluster(k_c(), check_overlap=TRUE,show.clust.cent = TRUE,mean.point.size=8, label.rectangle=TRUE)+ ggtitle(paste('Baby Forename Clustering with k =',input$kInput))
  }
  })
  #2-------------------
  output$s_plot <- renderPlot({
    if(input$radio_yes_noInput=='Auto'){
      fviz_nbclust(medoids,pam,k.max=20,method='silhouette')
    }
  })
  #2 Table-----------------------------------------------------
  output$clusterTable <- renderDataTable({
    if(input$radio_yes_noInput=='Auto'){
      pam.result <- pam(dt_c(),k=2)
    }else{
      pam.result <- pam(dt_c(),input$k02Input)  
    }
    df <- data.frame(pam.result$clustering)
    datatable(df, colnames=c('forename','clustering'))
  })
  
}
#End
#----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
