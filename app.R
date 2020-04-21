library(shiny)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(nlme)
library(shinythemes)

sampleD <- read_csv("sampleD.csv")[,2:4]

# Random effect output
LMM <- lme(new ~ days, random = ~ 1+ days | state, data = sampleD)
rm_effect <- data.frame(random.effects(LMM))
rm_effect[,'overall'] <- rm_effect[,'X.Intercept.'] + rm_effect[,'days']
rm_effect[,'bl_rank'] <- order(rm_effect$X.Intercept.,rownames(rm_effect))
rm_effect[,'time_rank'] <- order(rm_effect$days,rownames(rm_effect))
rm_effect[,'overall_rank'] <- order(rm_effect$overall,rownames(rm_effect))
rm_effect[,'state'] <- rownames(rm_effect)
rm_effect_long <- gather(rm_effect,statistics,value,X.Intercept.:overall_rank)
rm_effect_long[rm_effect_long$statistics=='X.Intercept.','statistics'] = 'baseline'


# Fixed effect output
fix_effect <- data.frame(fixed.effects(LMM))
fix_effect[,'statistics'] <- c('baseline','days')
colnames(fix_effect)[1] <- 'value' 

stay_home_time <- read_csv("stay_home_time.csv")

table1 <- stay_home_time
table1[,'baseline'] <- fix_effect$value[1]
table1[,'days'] <- fix_effect$value[2]

# Other summary

avg_total <- sampleD %>% group_by(state) %>% summarise('AVG' = mean(new),'Total'=sum(new))

u <- fluidPage(
  
  theme = "united",
  titlePanel("Stay-at-Home Order Evaluation"),

  fluidRow(

      selectInput(inputId = "states",label = "State",choices = unique(sampleD$state),selected = 'New York')
      
    ),

  fluidRow(

      column(4,plotOutput(outputId = "distPlot")),
      column(4,plotOutput(outputId = "distPlot2"))
      
      
    ),
  
  fluidRow(
    
    column(4,tableOutput(outputId = "tb1")),
    column(4,tableOutput(outputId = "tb2"))
    
    
  ))

s <- function(input, output) {
  
  
  # Graph
  output$distPlot <- renderPlot({
    
    subdata <- sampleD %>% filter(state==input$states)
    
    ggplot(data = subdata,aes(x=days,y=cumsum(new))) + 
      geom_line(color='red')+
      geom_point()
    
  })
  
  output$distPlot2 <- renderPlot({
    
    subdata <- sampleD %>% filter(state==input$states)
    
    ggplot(data = subdata,aes(x=days,y=new)) + 
      geom_line(color='red')+
      geom_point()
    
  })
  
  

  
  # Display statistical output
  
  output$tb1 <- renderTable({
    
    t1 <- table1[table1$state == input$states,]
    gather(left_join(t1,avg_total,by='state'),statistics,value,time:Total) %>% 
      dplyr::select(-state)
  },
  spacing = 'm',  
  width = '75%', 
  align = 'c',
  caption = "Data Summary",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$tb2 <- renderTable({
    rm_effect_long[rm_effect_long$state == input$states,] 
    
  } %>% dplyr::select(-state),
  spacing = 'm',  
  width = '75%', 
  align = 'c',
  caption = "Longitudinal Analysis",
  caption.placement = getOption("xtable.caption.placement", "top")
  )
  


}
shinyApp(u,s)
