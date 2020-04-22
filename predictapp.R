library(shiny)
library(tidyverse)
library(EpiDynamics)


# shiny app
ui = fluidPage(
  
  # App title ----
  titlePanel("SEIR Model: When can we be free?"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText("Create a SEIR Model"),
      
      sliderInput("time", h6("Time/days"),
                  min = 0, max = 60, value = 200),
      
      numericInput("beta", 
                   h6("Beta"), 
                   value = 2.47),
      
      numericInput("s", 
                   h6("Susceptible"), 
                   value = 0.9),
      
      numericInput("e", 
                   h6("Exposed"), 
                   value = 0.1),
      
      numericInput("i", 
                   h6("Infectious"), 
                   value = 0)
      
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

    parameters = list(beta = input$beta, sigma = 0.192, gamma = 0.434, mu=0.0005)
    S=input$s
    E=input$e
    I=input$i
    R=1-S-E-I
    initials = c(S=S,E=E,I=I,R=R)
    seir = SEIR(pars = parameters,init = initials, time = 0:input$time) # time unit is day
    PlotMods(seir)
  })
}

shinyApp(ui, server)