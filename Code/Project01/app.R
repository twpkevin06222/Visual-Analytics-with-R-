library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(DT)
library(formattable)
library(plyr)
library(dplyr)

#read data
data <- read_rds("european_soccer.rds")

#ui------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Exploring Football Data"),
  dashboardSidebar(
    selectInput("euleagueInput", "European League",
                               choices = c(unique(data$league_name))),
    sliderInput("yearInput", "Year",min = 2008, max = 2015,
                               value = c(2008))
  ),
  dashboardBody(
    titlePanel("First Section"),
    
  fluidRow(
    box(
    title = 'Summary Satistic for Chosen League', status = "primary", solidHeader = TRUE,
    dataTableOutput("leaguedetails")
    ),
    box( 
      title = 'Average Sum of Goals-per-Stage Plot', status = "primary", solidHeader = TRUE,
      plotOutput("avg_goals_perstage")
    )
  ),
  #2nd section---------------------------------------------------------------------- 
   titlePanel("Second Section"),
  fluidRow(
    box(
      title= 'Summary Table for Selected Year or League', status = "primary", solidHeader = TRUE,
      tableOutput("league_year")
        ),
    
    box(
      title ="Distribution of Results Over the Season", status = "primary", solidHeader = TRUE,
      plotOutput("home_away_draw")
      ),
    box(
      title = "Proportion of Goals", status = "primary", solidHeader = TRUE,
      plotOutput("proportion_goals_asc")
      )
  ),
  #3rd Section-----------------------------------------------------------------------  
  titlePanel("Third Section"),  
  fluidRow(
    box(
  title ="Average Number of Scored Goals-per-Month", status = "primary", solidHeader = TRUE,
  plotOutput("avg_goals_per_month")
  ),
  box(
    title ="Ball Possession vs Number of Scored Goals", status = "primary", solidHeader = TRUE,
    plotOutput("boxplot_home"),
    plotOutput("boxplot_away")
  )
  )
  )
)
#server---------------------------------------------------------------------------
server <- function(input,output){
  #management of data 
  df_linkage <- reactive({
    (data %>% filter(league_name == input$euleagueInput))
  })
  
  #changing date to only year
  y_linkage <- reactive({
    data %>% mutate(date=lubridate::year(date))%>% filter(date==input$yearInput)%>% filter(league_name == input$euleagueInput)
  })
  
  #adding a column for month 
  data$month <- data$date
  m_linkage <- reactive({ 
    data %>% mutate(date=lubridate::year(date)) %>% mutate(month=lubridate::month(month)) %>% filter(date==input$yearInput) %>% filter(league_name==input$euleagueInput)%>% mutate(month=month.name[month])
  })
    #------------------------------------------------------------------------------
  #1.2 Datatable-------------------------------------------------------------------
  m3 <- reactive({
    ag_home <- aggregate(home_team_goal~home_team,data=df_linkage(),FUN=sum)
    ag_away <- aggregate(away_team_goal~away_team,data=df_linkage(),FUN=sum)
    ct_home <- count(df_linkage(),home_team)
    ct_away <- count(df_linkage(),away_team)
    m1 <- merge(ag_home,ag_away, by.x = "home_team", by.y = "away_team")
    m2 <- merge(ct_home,ct_away, by.x = "home_team", by.y = "away_team")
    m5 <- merge(m1,m2,by.x="home_team")
    return(m5)
  })
  
  data_alt <- reactive({
    total_goals <- m3()[,2] + m3()[,3]
    total_match <- m3()[,4] + m3()[,5]
    avg_goals <- formatC((total_goals/total_match),digits = 4, format = 'f')
    m4 <- cbind(m3(),total_match,total_goals,avg_goals)
    return(m4)
  })
  #1.4 stage data for line plot---------------------------------------------------------
  data_stage <- reactive({
    ag_stage <- aggregate((home_team_goal+away_team_goal)~stage,data= df_linkage(), sum)
    ct_stage <- count(df_linkage(), stage)
    ctag_stage <- merge(ag_stage,ct_stage,by.x='stage')
    avg_sum <- (ctag_stage[,2]/ctag_stage[,3])
    bind_02 <- cbind(ctag_stage, avg_sum)
    return(bind_02)
  })
  
  #2.2 summary table--------------------------------------------------------------------- 
  st <- reactive({
    ag_home <- aggregate(home_team_goal~home_team,data=y_linkage(),FUN=sum) #home_goal for each team
    ag_away <- aggregate(away_team_goal~away_team,data=y_linkage(),FUN=sum) #away_goal for each team
    ct_home <- count(y_linkage(),home_team) #home freq
    ct_away <- count(y_linkage(),away_team)
    home_possession <- aggregate(home_team_possession~home_team,data=y_linkage(),FUN=sum,na.action=NULL) #home_p for each team
    away_possession <- aggregate(away_team_possession~away_team,data=y_linkage(),FUN=sum,na.action=NULL) #away_p for each team
    m1 <- merge(ag_home,ag_away, by.x = "home_team", by.y = "away_team")
    m2 <- merge(ct_home,ct_away, by.x = "home_team", by.y = "away_team")
    m3 <- merge(m1,m2,by.x="home_team")
    m4 <- merge(home_possession,away_possession, by.x="home_team", by.y = "away_team")
    m5 <- merge(m3,m4,by.x="home_team")
    total_goals <- m5[,2]+ m5[,3]
    avg_home_possession <- m5[,6]/m5[,4]   #home_possession/home_freq
    avg_away_possession <- m5[,7]/m5[,5]   #away_possession/away_freq
    m6 <- cbind(m5,total_goals,avg_home_possession,avg_away_possession)
    m7 <- m6[,c(1,2,3,8,9,10)]
    colnames(m7) <- c('Team Name','Home Team Goals','Away Team Goals','Total Goals','Average Home Ball Possession(%)', 'Average Away Ball Possession(%)')
    return(m7)
  })
  # 2.3 home_away_draw-----------------------------------------------------------------------
  had <- reactive({
    win_rate <- ifelse(y_linkage()$home_team_goal>y_linkage()$away_team_goal,yes = 1,no = 0)
    away_rate <- ifelse(y_linkage()$away_team_goal>y_linkage()$home_team_goal,yes = 1,no = 0)
    draw_rate <- ifelse(y_linkage()$home_team_goal==y_linkage()$away_team_goal,yes = 1,no = 0)
    df_win <- cbind(y_linkage()[,c(5,7,8)],win_rate,away_rate,draw_rate)
    home_win <- aggregate(win_rate~home_team,data = df_win, FUN=sum)
    away_win <- aggregate(away_rate~away_team,data = df_win, FUN=sum)
    draw <- aggregate(draw_rate~home_team, data=df_win, FUN=sum)
    home_away_draw <-merge(merge(home_win,away_win, by.x="home_team", by.y="away_team"),draw,by.x="home_team")
    colnames(home_away_draw) <- c('home_team','Home Wins','Away Wins', 'Draws')
    re <- reshape(home_away_draw, varying = c("Home Wins","Away Wins","Draws") , v.name="Score", timevar = "Situation" ,times =c("Home Wins","Away Wins","Draws"), new.row.names=1:1000, direction="long")
    re.sort <- re[order(re$id),]
    return(re.sort)
  })
  #2.4 proportion_goals_asc-----------------------------------------------------------------
  pga <- reactive({
    home_p <- aggregate(home_team_goal~home_team,data=y_linkage(),FUN=sum)
    away_p <- aggregate(away_team_goal~away_team,data=y_linkage(),FUN=sum)
    m1 <- merge(home_p,away_p,by.x = 'home_team', by.y = 'away_team')
    proportion <- m1[,2]/m1[,3]
    m2 <- cbind(m1,proportion)
    proportion_asc <- m2[order(proportion),] #proportion with ascending orders
    return(proportion_asc)
  })
  #3.1goals per month------------------------------------------------------------------
  gpm <- reactive({
    ct_h_m <- count(m_linkage(),home_team,month) #freq of home team according to month
    ct_a_m <- count(m_linkage(),away_team,month)
    m1 <- merge(ct_h_m,ct_a_m,by.x="home_team",by.y="away_team")
    ag_x <- aggregate(n.x~home_team+month.x,m1,FUN=sum)      #aggregate freq with month 
    ag_y <- aggregate(n.y~home_team+month.y,m1,FUN=sum) #aggregate freq with month
    names(ag_x) <- c("home_team","month","freq")
    names(ag_y) <- c("home_team","month","freq")
    m2 <- merge(ag_x,ag_y, by=c("home_team", "month"))
    ct_a_g <- count(m_linkage(),away_team,month,away_team_goal)
    ct_h_g <- count(m_linkage(),home_team,month,home_team_goal)
    m3 <- merge(ct_h_g,ct_a_g,by.x="home_team",by.y="away_team")
    ag_x_g <- aggregate(home_team_goal~home_team+month.x,m3,FUN=sum)
    ag_y_g <- aggregate(away_team_goal~home_team+month.y,m3,FUN=sum)
    names(ag_x_g) <- c("home_team","month","home_team_goal")
    names(ag_y_g) <- c("home_team","month","away_team_goal")
    m4 <- merge(ag_x_g,ag_y_g,by=c("home_team","month"))
    m5 <- merge(m2,m4,by=c("home_team","month"))
    avg_goal_h <- m5[,c(5)]/m5[,c(3)]
    avg_goal_a <- m5[,c(6)]/m5[,c(4)]
    total_avg_goal <- avg_goal_h+avg_goal_a
    m6 <- cbind(m5,avg_goal_h,avg_goal_a,total_avg_goal) #colnames="home_team""month""freq.x""freq.y""home_team_goal" "away_team_goal""avg_goal_h""avg_goal_a""total_avg_goal"
    f4 <-aggregate(total_avg_goal~month,m6,sum) #only month and total_avg_goal
    f4$month <- factor(f4$month,levels=month.name)
    return(f4)
  })
  #3.2----------------------------------------------------------------
  home_p <- aggregate(home_team_possession~home_team,data=data,FUN=sum)
  away_p <- aggregate(away_team_possession~away_team,data=data,FUN=sum)
  m_p <- merge(home_p,away_p, by.x='home_team',by.y='away_team')
  home_g <- aggregate(home_team_goal~home_team, data,sum)
  away_g <- aggregate(away_team_goal~away_team, data,sum)
  m_g <- merge(home_g,away_g, by.x='home_team', by.y='away_team')
  home_pg <- merge(m_p,m_g, by.x='home_team') #home_p+home_g+away_p+away_g for each team
  h_p_g <- home_pg[,2]/home_pg[,4]
  a_p_g <- home_pg[,3]/home_pg[,5]
  hpg<-cbind(home_pg,h_p_g,a_p_g) #h_p_g = home_ball_p/home goal, a_p_g = away...
  #Main---------------------------------------------------------------
  #1.3
  output$leaguedetails <- renderDataTable({
    datatable(data_alt()[,c(1,6,7,8)],
              colnames = c('Team Name','Number of Matches','Total Goals', 'Average Goals'))
  })
  #1.4
  output$avg_goals_perstage <- renderPlot({
    ggplot(data_stage(), mapping = aes(x= stage, y = avg_sum))+geom_line()+
      ggtitle(paste(input$euleagueInput))+labs(x='Stage', y='Average Sum of Goals')+
      theme_minimal()+ scale_color_manual(values=c('#999999','E69F00'))
  })
  #2.2
  output$league_year <- renderTable({
    st()
    })
  #2.3
  output$home_away_draw <- renderPlot({
    ggplot(had(),aes(x=home_team,y=Score,fill=Situation))+ geom_bar(stat="identity")+ggtitle(paste("Home Wins, Away Wins and Draws for",input$yearInput,input$euleagueInput))+xlab("Team")+ylab("Frequency")+theme_bw()+theme(axis.text.x= element_text(angle=90,hjust=1))
  })
  #2.4
  output$proportion_goals_asc <- renderPlot({
    ggplot(pga(),aes(x=reorder(home_team,-proportion),y=proportion))+ coord_flip()+geom_bar(stat="identity", fill = 'maroon')+ xlab("Team") +ylab("Home Goals by Away Goals")+ggtitle(paste(input$yearInput,input$euleagueInput))
  })
  #3.1
  output$avg_goals_per_month <- renderPlot({
    ggplot(gpm(),aes(x=month,y=total_avg_goal,group=1))+ geom_line()+geom_point()+xlab("Month")+ylab("Average Goal")+theme_bw()+ggtitle(paste(input$yearInput,input$euleagueInput))
  })
  #3.2
  mynames <- c("Very low", "Low", "Intermediate", "High","Very high")
  output$boxplot_home <- renderPlot({
    boxplot(ifelse(hpg$h_p_g<=25, yes = hpg$h_p_g, no= NA),ifelse((hpg$h_p_g<=40)&(hpg$h_p_g>25), yes = hpg$h_p_g, no= NA), ifelse((hpg$h_p_g<=60)&(hpg$h_p_g>40), yes = hpg$h_p_g, no= NA),ifelse((hpg$h_p_g<=75)&(hpg$h_p_g>60), yes = hpg$h_p_g, no= NA),ifelse((hpg$h_p_g<=100)&(hpg$h_p_g>75), yes = hpg$h_p_g, no= NA), names=mynames, main = "Home Score", ylab="(%)")
  })
  output$boxplot_away <- renderPlot({
      boxplot(ifelse(hpg$a_p_g<=25, yes = hpg$a_p_g, no= NA),ifelse((hpg$a_p_g<=40)&(hpg$a_p_g>25), yes = hpg$a_p_g, no= NA), ifelse((hpg$a_p_g<=60)&(hpg$a_p_g>40), yes = hpg$a_p_g, no= NA),ifelse((hpg$a_p_g<=75)&(hpg$a_p_g>60), yes = hpg$a_p_g, no= NA),ifelse((hpg$a_p_g<=100)&(hpg$a_p_g>75), yes = hpg$a_p_g, no= NA), names=mynames, main = "Away Score", ylab="(%)",border="red")
  })
}
#End
#----------------------------------------------------------------------
shinyApp(ui = ui, server = server)


