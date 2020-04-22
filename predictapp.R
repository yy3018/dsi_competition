library(shiny)
library(tidyverse)
library(EpiDynamics)
library(deSolve)
library(plotly)

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
                   value = 0.9999),
      
      numericInput("e", 
                   h6("Exposed"), 
                   value = 0.0001),
      
      numericInput("i", 
                   h6("Infectious"), 
                   value = 0)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotlyOutput("curve")
      
    )
  )
)

server = function(input, output) {
  output$curve = renderPlotly({
    
    function2 <- function(time, init, pars) {
      with(as.list(c(init, pars)), {
        dS = mu - S * (beta * I + mu)
        dE = beta * S * I - E * (sigma + mu)
        dI = sigma * E - I * (gamma + mu)
        dR = gamma * I - mu * R
        list(c(dS, dE, dI, dR))
      })
    }
    
    parameters = list(beta = input$beta, sigma = 0.192, gamma = 0.434, mu=0.00005)
    S=input$s
    E=input$e
    I=input$i
    R=1-S-E-I
    initials = c(S=S,E=E,I=I,R=R)
    
    output = ode(times = 0:input$time, func = function2, y = initials, parms = parameters)
    
    visual_data = as.data.frame(output)
    
    vis_seir = visual_data %>% 
      mutate(
        p_S = S,
        p_E = E,
        p_I = I,
        p_R = R
      ) %>% 
      select(time,p_S, p_E, p_I, p_R) %>% 
      pivot_longer(
        p_S:p_R,
        names_to = "group", 
        names_prefix = "p_",
        values_to = "proportion")
    
    fig2 = vis_seir %>% 
      ggplot(aes(x = time, y = proportion,group = group,color = group)) + 
      geom_line()+
      theme_bw()
    
    ggplotly(fig2)
  })
}

shinyApp(ui, server)
