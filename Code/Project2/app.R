library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(data.table)
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
      )
    ),
    fluidRow(
      box(
        title='Table with respect to Cluster', status = "primary", solidHeader = TRUE,
        dataTableOutput('clusterTable')
      ),
      box(
        title='Plot with respect to Cluster', status = "primary", solidHeader = TRUE,
        plotOutput('dt_plot_c')
      )
    ),
    titlePanel("Third Assignment"),
    fluidRow(
      box(
        title = "Characteristic of Clustering", status = "primary", solidHeader = TRUE,
        sliderInput("k03Input","Number of Cluster:", min=1,max=10, value = c(5)),
        plotOutput("char01_plot")
      )
    ),
    fluidRow(
      box(
        title="Alternative Clustering Algorithm", status= "primary", solidHeader =TRUE, width=12, 
        radioButtons("kmean_hclusInput","Choice of clustering algorithm:", choices = c('k-means','Hierarchical')),
        conditionalPanel(
          condition = "input.kmean_hclusInput=='k-means'",
          sliderInput("k_meanInput", "Number of cluster:",min=1, max=10,
                      value = c(5)),
          selectInput("k_yesnoInput","Toggle labels",choices = c('no','yes'))
        ),
        conditionalPanel(
          condition = "input.kmean_hclusInput=='Hierarchical'",
          selectInput("hclusInput","Agglomeration Method:",choices = c("single","complete","average","ward.D"))
        ),
        plotOutput("alt_plot",height="600px")
      )
    )
  )
)
#server---------------------------------------------------------------------------
server <- function(input,output){
#general------------------------------------------------------------------------
#refer to dt_c at #2--
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

#3---------------------------------------------------------------------------------
char01 <- reactive({
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
  pam.result <- pam(medoids,input$k03Input)
  datalist=list()
  for(i in 1:input$k03Input){
    t1 <- data.frame(pam.result$clustering)
    t2 <- setDT(t1,keep.rownames=TRUE)[]
    colnames(t2)[1] <- "forename"
    t3 <- merge(t2,m11,by='forename', all=TRUE)
    t4 <- t3 %>% filter(pam.result.clustering==i)
    t1111 <- expand.grid(forename=unique(t4$forename),year=unique(t4$year))
    t1112 <- merge(t4,t1111,all=TRUE)
    t1112$popularity[is.na(t1112$popularity)] <- 0 
    t1113 <- count(t1112,year,popularity)
    t1114 <- aggregate(popularity~year,t1113,sum)
    t1115 <- count(t1112,year)
    t1116 <- merge(t1114,t1115,by='year')
    t1116$avg_popularity <- t1116[,2]/t1116[,3]
    t1116$cluster <- i
    datalist[[i]] <- t1116
  }
  big_data = do.call(rbind,datalist)
  return(big_data)
})
#3.2 Alt clustering-------------------------------------------------------------
alt_k_clus <- reactive({
  km01 <- kmeans(dt_c(),input$k_meanInput)
  return(km01)
})

alt_h_clus <- reactive({
  hc01 <- hclust(dist(dt_c()),method= input$hclusInput)
  hcd01 <- as.dendrogram(hc01)
  return(hcd01)
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
      fviz_nbclust(dt_c(),pam,k.max=20,method='silhouette')
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
  output$dt_plot_c <- renderPlot({
    s <- input$clusterTable_rows_selected
    if(input$radio_yes_noInput=='Auto'){
      pam.result <- pam(dt_c(),k=2)
    }else{
      pam.result <- pam(dt_c(),input$k02Input)  
    }
    clusplot(pam.result, main='Scatterplot for Clustering')
    pca.res <- prcomp(pam.result$data)
    df <- data.frame(pca.res$x)
    if (length(s)) points(df[s, , drop = FALSE], pch = 19, cex = 2)
  })
  
  #3 Plot---------------------------------------------------------
  output$char01_plot <- renderPlot({
    ggplot(char01(),aes(x=year,y=avg_popularity,group=cluster, colour= cluster))+geom_line()+ggtitle(paste('Avg. Popularity w.r.t Year for',input$k03Input,'Cluster'))
  })
  
  output$alt_plot <- renderPlot({
    if(input$kmean_hclusInput=='k-means'){
      if(input$k_yesnoInput=='no'){
        fviz_cluster(alt_k_clus(),dt_c(), geom='point',label.rectangle =TRUE, show.clust.cent=TRUE, mean.point.size=8)+ggtitle(paste('Plot for k-mean clustering with k=',input$k_meanInput))
      }else{
        fviz_cluster(alt_k_clus(),dt_c(), check_overlap=TRUE,label.rectangle =TRUE, show.clust.cent=TRUE, mean.point.size=8)+ggtitle(paste('Plot for k-mean clustering with k=',input$k_meanInput))
      }
    }else{
      plot(alt_h_clus(),type='rectangle', ylab ='Height', ylim=c(0,6))
    }
  })
  
}
#End
#----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
