library(shiny)
library(tidyverse)
library(EpiDynamics)

# read data
death_rate = read_csv("./data/death_rate.csv") %>% janitor::clean_names() 
state = death_rate$location
# shiny app
ui = fluidPage(
  
  # App title ----
  titlePanel("SEIR Model: When can we be free?"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText("Create a SEIR Model"),
      
      selectInput("state", 
                  label = "Choose a state",
                  choices = state ,
                  selected = "New York"),
      
      # Input: Slider for the number of bins ----
      numericInput("beta", 
                   h6("Beta"), 
                   value = 2.47),
      
      numericInput("sigma", 
                   h6("Sigma"), 
                   value = 0.192),
      
      numericInput("gamma", 
                   h6("Gamma"), 
                   value = 0.434),
      
      numericInput("mu", 
                   h6("Mu"), 
                   value = 0.0005),
      
      numericInput("s", 
                   h6("Susceptible"), 
                   value = 0.9),
      
      numericInput("e", 
                   h6("Exposed"), 
                   value = 0.1),
      
      numericInput("i", 
                   h6("Infectious"), 
                   value = 0),
      
      numericInput("r", 
                   h6("Removed/Recovered"), 
                   value = 0),
      
      sliderInput("time", h6("Time/days"),
                  min = 0, max = 300, value = 60)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("curve")
      
    )
  )
)

server = function(input, output) {
  output$curve = renderPlot({
    parameters = list(beta = input$beta, sigma = input$sigma, gamma = input$gamma, mu=input$mu)
    initials = c(S=input$s, E=input$e, I=input$i, R=input$r)
    seir = SEIR(pars = parameters,init = initials, time = 0:input$time) # time unit is day
    PlotMods(seir)
  })
}

shinyApp(ui, server)