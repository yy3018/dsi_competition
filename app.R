library(shiny)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(nlme)

sampleD <- read_csv("sampleD.csv")[,2:4]

LMM <- lme(new ~ days, random = ~ 1+ days | state, data = sampleD)
rm_effect <- data.frame(random.effects(LMM))
rm_effect[,'bl_rank'] <- order(rm_effect$X.Intercept.,rownames(rm_effect))
rm_effect[,'time_rank'] <- order(rm_effect$days,rownames(rm_effect))
rm_effect[,'state'] <- rownames(rm_effect)
rm_effect_long <- gather(rm_effect,statistics,value,X.Intercept.:time_rank)


#fix_effect <- data.frame(fixed.effects(LMM))


order(rm_effect$X.Intercept.,rownames(rm_effect))

u <- fluidPage(
  

  titlePanel("Stay-at-Home Order Evaluation"),

  fluidRow(

      selectInput(inputId = "states",label = "State",choices = unique(sampleD$state),selected = 'New York')
      
    ),

  fluidRow(

      column(4,plotOutput(outputId = "distPlot")),
      column(4,plotOutput(outputId = "distPlot2")),
      column(4,plotOutput(outputId = "distPlot3"))
      
      
    ),
  
  fluidRow(
    
    column(4,tableOutput(outputId = "tb1")),
    column(4,tableOutput(outputId = "tb2")),
    column(4,tableOutput(outputId = "tb3"))
    
    
  )
  
)

s <- function(input, output) {
  
  
  # Graph
  output$distPlot <- renderPlot({
    
    subdata <- sampleD %>% filter(state==input$states)
    
    ggplot(data = subdata,aes(x=days,y=new)) + 
      geom_line(color='red')+
      geom_point()
    
  })
  
  output$distPlot2 <- renderPlot({
    
    subdata <- sampleD %>% filter(state==input$states)
    
    ggplot(data = subdata,aes(x=days,y=new)) + 
      geom_line(color='red')+
      geom_point()
    
  })
  
  
  output$distPlot3 <- renderPlot({
    
    subdata <- sampleD %>% filter(state==input$states)
    
    ggplot(data = subdata,aes(x=days,y=new)) + 
      geom_line(color='red')+
      geom_point()
    
  })
  
  # Display statistical output
  
  output$tb1 <- renderTable({
    rm_effect_long[rm_effect_long$state == input$states,]
  })
  
  output$tb2 <- renderTable({
    rm_effect_long[rm_effect_long$state == input$states,]
  })
  
  
  output$tb3 <- renderTable({
    rm_effect_long[rm_effect_long$state == input$states,]
  })
  

  
  
}
shinyApp(u,s)
