library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(DT)
library(formattable)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

#read data
data <- read_rds("2017_UN_votes.rds")

#ui------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "UN Voting"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary",tabName="Summary", icon=icon('th'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName='Summary',
        fluidRow(
          tabBox( 
            height = '400px', width = '400px',
            title = 'First Assignment',
            tabPanel("Tab1", DT::dataTableOutput('resolution')),
            tabPanel("Tab2", plotOutput("reso_per_year")),
            tabPanel("Tab3", plotOutput("yes_no_abstain")),
            tabPanel("Tab4", plotOutput("animous_per_year")),
            tabPanel("Tab5", DT::dataTableOutput("all"))
          )
        )
      )
    )
  )
)

#server---------------------------------------------------------------------------
server <- function(input,output){
# 1.1-------------------------------  
  res <- reactive({
    m1 <- count(data,rcid,vote) #count vote type according to rcid
    m2 <- aggregate(n~rcid,m1,sum) #total vote according to rcid
    m3 <- m1 %>% filter(vote=='yes') #get only yes 
    m4 <- merge(m2,m3,by='rcid')
    m4$vote <- NULL #remove vote rows
    m4$p_yes <- m4[,3]/m4[,2] #getting proportion of yes
    names(m4) <- c('rcid','vote','yes_vote','Proportion_of_yes')
    m5 <- count(data,rcid,short)
    m5[,3] <- NULL
    year <- count(data,rcid,year)
    year$n <- NULL
    m6 <- merge(m4,m5,by='rcid')
    m7 <- merge(m6,year,by='rcid')
    m8 <- select(m7,'rcid','year','vote','yes_vote','Proportion_of_yes','short')
    return(m8)
  })
#1.2--------------------------------------
  rpy <- reactive({
    m1 <- count(data,rcid,vote) #count vote type according to rcid
    m2 <- aggregate(n~rcid,m1,sum) #total vote according to rcid
    year <- count(data,rcid,year)
    year$n <- NULL
    t1 <- merge(m2,year,by='rcid')
    t2 <- aggregate(n~year,t1,sum)
    return(t2)
  })
  
#1.3--------------------------------------
  yna <- reactive({
    yes <- count(data %>% filter(vote=='yes'),rcid,vote)
    no <- count(data %>% filter(vote=='no'),rcid,vote)
    abstain <- count(data %>% filter(vote=='abstain'),rcid,vote)
    yes$vote <- NULL
    no$vote <- NULL
    abstain$vote <- NULL
    yes_no <- merge(yes,no,by='rcid')
    yes_no_abstain <- merge(yes_no,abstain,by='rcid')
    names(yes_no_abstain) <- c('rcid','yes','no','abstain')
    year <- count(data,rcid,year)
    year$n <- NULL
    year_yes_no_abstain <- merge(year,yes_no_abstain,by='rcid')
    year_yes_no_abstain$rcid <- NULL
    g1 <- aggregate(.~year,year_yes_no_abstain,sum)
    g2 <- melt(g1,id.vars="year")
    return(g2)
  })
  
#1.4---------------------------------------
rtpy <- reactive({
  data02 <- data[,-c(2:8)]
  k1 <- aggregate(.~rcid,data02,sum)
  year <- count(data,rcid,year)
  year$n <- NULL
  k2 <- merge(year,k1,by='rcid')
  k3 <- aggregate(.~year,k2,sum)
  k3$rcid <- NULL
  k4 <- melt(k3,id.vars='year')
  return(k4)
})

#1.5----------------------------------------
apy <- reactive({
  yes <- count(data %>% filter(vote=='yes'),rcid,vote)
  no <- count(data %>% filter(vote=='no'),rcid,vote)
  abstain <- count(data %>% filter(vote=='abstain'),rcid,vote)
  yes$vote <- NULL
  no$vote <- NULL
  abstain$vote <- NULL
  yes_no <- merge(yes,no,by='rcid')
  yes_no_abstain <- merge(yes_no,abstain,by='rcid')
  names(yes_no_abstain) <- c('rcid','yes','no','abstain')
  year <- count(data,rcid,year)
  year$n <- NULL
  year_yes_no_abstain <- merge(year,yes_no_abstain,by='rcid')
  year_yes_no_abstain$rcid <- NULL
  g1 <- aggregate(.~year,year_yes_no_abstain,sum)
  a1 <- aggregate(no+abstain~year,g1,sum)
  a2 <- merge(g1,a1,by='year')
  a2 <- a2[,-c(3,4)]
  names(a2) <- c('year','unanimous','non-unanimous')
  a2$Total <- a2[,2]+a2[,3]
  a3 <- a2[!((a2$unanimous/a2$Total)<(2/3)),]
  a3$Total <- NULL
  a4 <- melt(a3,id.vars='year')
  return(a4)
})

#1.6--------------------------------
all <- reactive({
  yes <- count(data %>% filter(vote=='yes'),rcid,vote)
  no <- count(data %>% filter(vote=='no'),rcid,vote)
  abstain <- count(data %>% filter(vote=='abstain'),rcid,vote)
  yes$vote <- NULL
  no$vote <- NULL
  abstain$vote <- NULL
  yes_no <- merge(yes,no,by='rcid')
  yes_no_abstain <- merge(yes_no,abstain,by='rcid')
  names(yes_no_abstain) <- c('rcid','yes','no','abstain')
  year <- count(data,rcid,year)
  year$n <- NULL
  year_yes_no_abstain <- merge(year,yes_no_abstain,by='rcid')
  year_yes_no_abstain$rcid <- NULL
  g1 <- aggregate(.~year,year_yes_no_abstain,sum)
  a1 <- aggregate(no+abstain~year,g1,sum)
  a2 <- merge(g1,a1,by='year')
  a2 <- a2[,-c(3,4)]
  names(a2) <- c('year','unanimous','non-unanimous')
  a2$Total <- a2[,2]+a2[,3]
  data02 <- data[,-c(2:8)]
  k1 <- aggregate(.~rcid,data02,sum)
  year <- count(data,rcid,year)
  year$n <- NULL
  k2 <- merge(year,k1,by='rcid')
  k3 <- aggregate(.~year,k2,sum)
  k3$rcid <- NULL
  b1 <- merge(a2,k3,by='year')
  b2 <- b1[!((b1$unanimous/b1$Total)<(2/3)),]
  b2$Total <- NULL
  return(b2)
})
  
#Output--------------------------------------------------------------------------
  output$resolution <- DT::renderDataTable({
    datatable(res(), 
              colnames = c('rcid','Year','No. of Votes','No. of Yes Votes', 'Porportion of Yes Vote','Short Description'),
              extensions ='Responsive'
              )
  })
  
  output$reso_per_year <- renderPlot({
    ggplot(rpy(),aes(x=year, y=n))+geom_bar(stat='identity',fill='maroon')+theme(axis.text.x= element_text(angle=90,hjust=1))+scale_x_continuous(breaks=scales::pretty_breaks(n=20))+scale_y_continuous(breaks=scales::pretty_breaks(n=10))+xlab('Year')+ylab('Number of Votes')+theme(axis.text=element_text(size=14),axis.title = element_text(size=18))
  })
  
  output$yes_no_abstain <- renderPlot({
    ggplot(yna(),aes(year,value,fill=variable))+geom_bar(stat='identity')+scale_x_continuous(breaks=scales::pretty_breaks(n=20))+theme(axis.text.x= element_text(angle=90,hjust=1))+guides(fill=guide_legend(title='Votes Type'))+theme(axis.text=element_text(size=14),axis.title = element_text(size=18))
  })
  
  output$res_type_per_year <- renderPlot({
    ggplot(rtpy(),aes(year,value,fill=variable))+geom_bar(stat='identity')+scale_x_continuous(breaks=scales::pretty_breaks(n=20))+theme(axis.text.x= element_text(angle=90,hjust=1))+guides(fill=guide_legend(title='Res. Type'))+theme(axis.text=element_text(size=14),axis.title = element_text(size=18))
  })
  
  output$animous_per_year <- renderPlot({
    ggplot(apy(),aes(year,value,fill=variable))+geom_bar(stat='identity')+theme(axis.text.x= element_text(angle=90,hjust=1))+guides(fill=guide_legend(title='Animous Type'))+theme(axis.text=element_text(size=14),axis.title = element_text(size=18))+scale_x_continuous(breaks=scales::pretty_breaks(n=20))  
    })
  
  output$all <- DT::renderDataTable(
    datatable(all())
  )
}
#End
#----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
