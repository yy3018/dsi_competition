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
  fluidRow(
    checkboxGroupInput("states", label = h3("States"), 
                         choices = c("All",unique(sampleD$state)),
                         selected = 'All',
                         inline = TRUE)
    ),

  fluidRow(

      column(4,plotlyOutput(outputId = "distPlot")),
      column(5,plotlyOutput(outputId = "distPlot2"))
      
      
    ),
  
  fluidRow(
    
    column(4,DTOutput(outputId = "tb1")),
    column(4,tableOutput(outputId = "tb2"))
  )
  
)

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
                 geom_point()+ 
                 theme(legend.position = "none"))
      
  })
  
  output$distPlot2 <- renderPlotly({
    
    validate(
      need(input$states != "", "Please select at least one state!")
    )

    
    ggplotly(ggplot(data = df(),aes(x=days,y=new,group=state,color=state)) + 
      labs(x='Days since stay-at-home order',y='New Cases')+
      geom_line()+
      geom_point())
    
  })
  
  output$tb1 <- renderDataTable(stat_df() %>% rename(`Issue Date`=time), options = list(
    pageLength = 6,
    initComplete = JS('function(setting, json) { alert("Apps Ready"); }')
  ))


  
  output$tb2 <- renderTable({
    
    validate(
      need(input$states != "", "Please select at least one state!")
    )
    

    data.frame(Result=c("Least baseline",
                        "Highest baseline",
                        "Fastest decrasing trend since the order",
                        "Slowest decrasing trend since the order",
                        "Fastest decrasing trend overall",
                        "Slowest decrasing trend overall"
                        ),
               Value=c(model_df()[which.min(model_df()$bl),'state'],
                       model_df()[which.max(model_df()$bl),'state'],
                       model_df()[which.min(model_df()$days),'state'],
                       model_df()[which.max(model_df()$days),'state'],
                       model_df()[which.min(model_df()$overall_trend),'state'],
                       model_df()[which.max(model_df()$overall_trend),'state'])
               )
    
  })
  


}


shinyApp(ui,server)
