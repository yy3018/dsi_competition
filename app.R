library(shiny)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(nlme)
library(shinythemes)
library(plotly)
library(DT)

# Data import
#sampleD <- read_csv("sampleD.csv")[,2:4]
sampleD <- read_csv("final.csv")[,2:4]
stay_home_time <- read_csv("data/stay_home_time.csv")


# Model
LMM <- lme(new ~ days, random = ~ 1+ days | state, data = sampleD)

# Fixed effect output
fix_effect <- data.frame(fixed.effects(LMM))
fix_effect[,'statistics'] <- c('baseline','days')
colnames(fix_effect)[1] <- 'value'
fix_effect <- fix_effect %>% select(statistics,value)
rownames(fix_effect) <- NULL

# Random effect output
rm_effect <- data.frame(random.effects(LMM))
rm_effect[,'state'] <- rownames(rm_effect)
rownames(rm_effect) <- NULL

# Combine random and fixed effect
model_effect <- rm_effect %>% select(state,everything()) %>% rename(bl=X.Intercept.)
model_effect[,'gl_bl'] <- fix_effect$value[1]
model_effect[,'gl_days'] <-  fix_effect$value[2]
model_effect[,'overall_trend'] <-  model_effect[,'days'] + model_effect[,'gl_days'] 



# Other summary

avg_total <- sampleD %>% group_by(state) %>% summarise('AVG' = round(mean(new),2),'Total'=sum(new))

sampleD =sampleD %>% arrange(state) 

ui <- fluidPage(
  
  theme = "united",
  titlePanel("Stay-at-Home Order Evaluation"),
  sidebarPanel(
    checkboxGroupInput("states", label = h3("States"), 
                       choices = c("All",unique(sampleD$state)),
                       selected = 'All',
                       inline = T),
    width = 2
  ),

 mainPanel(
   fluidRow(
  
    splitLayout(cellWidths = c("40%","60%"),
                plotlyOutput(outputId = "distPlot"),
                plotlyOutput(outputId = "distPlot2"))
      
      
    ),
  
  fluidRow(
    
    column(5,DTOutput(outputId = "tb1")),
    column(7,tableOutput(outputId = "tb2"))
    
   
  ),
  width=10
  
))

server <- function(input, output){
  
  df <- reactive({
    if('All' %in% input$states){
      sampleD
    }else{
      sampleD %>% filter(state %in% input$states)
    }
  })
  
  
  stat_df <- reactive({
    if('All' %in% input$states){
      stay_home_time
    }else{
      stay_home_time[stay_home_time$state %in% input$states,]
    }
  })
  
  
  model_df <- reactive({
    if('All' %in% input$states){
      model_effect
    }else{
      model_effect[model_effect$state %in% input$states,] 
    }
  })
  
  
  # renders
  output$distPlot <- renderPlotly({
    
      validate(
        need(input$states != "", "Please select at least one state!")
      )
    
   
      
      ggplotly(ggplot(data = df(),aes(x=days,y=cumsum(new),group=state,color=state)) + 
                 labs(x='Days since stay-at-home order',y='Cumulative Cases')+
                 geom_line()+
                 geom_point(size=1)+ 
                 theme(legend.position = "none"))
      
  })
  
  output$distPlot2 <- renderPlotly({
    
    validate(
      need(input$states != "", "Please select at least one state!")
    )

    
    ggplotly(ggplot(data = df(),aes(x=days,y=new,group=state,color=state)) + 
      labs(x='Days since stay-at-home order',y='New Cases')+
      geom_line()+
      geom_point(size=1))
    
  })
  
  output$tb1 <- renderDataTable(stat_df() %>% rename(`Issue Date`=time), options = list(
    pageLength = 6,
    initComplete = JS('function(setting, json) { alert("Apps Ready"); }')
  ))


  
  output$tb2 <- renderTable({
    
    validate(
      need(input$states != "", "Please select at least one state!")
    )
    

    data.frame(Result=c("Amount of states selected",
                        "Number of states with decreasing trend",
                        "Number of states with increasing trend",
                        "Decreasing mostly over time",
                        "Decreasing slowest over time"
                        ),
               Value=c(length(model_df()$days),
                       length(which((model_df()$days+model_df()$gl_days)<0)),
                       length(which((model_df()$days+model_df()$gl_days)>0)),
                       model_df()[which.min(model_df()$overall_trend),'state'],
                       model_df()[which.max(model_df()$overall_trend),'state']
                       )
               )
    
  })
  


}


shinyApp(ui,server)
