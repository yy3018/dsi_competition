library(shiny)
library(tidyverse)
library(EpiDynamics)
library(deSolve)
library(plotly)

# shiny app
ui = fluidPage(
  theme = "united",
  # App title ----
  titlePanel("SEIR Model: What does "),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText("Create a SEIR Model"),
      
      sliderInput("time", h6("Time/days"),
                  min = 0, max = 200, value = 100),
      fluidRow(
      column(6,numericInput("beta", 
                   h6("Beta"), 
                   value = 2.47)),
      
      column(6,numericInput("s", 
                   h6("Susceptible"), 
                   value = 0.9999))),
      fluidRow(
      column(6,numericInput("e", 
                   h6("Exposed"), 
                   value = 0.0001)),
      
      column(6,numericInput("i", 
                   h6("Infectious"), 
                   value = 0))),
      
      sliderInput("beta_change", 
                   h6("Social Distance Strength"), 
                   min = 0, max = 500, value = 50)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
      column(7, plotlyOutput("curve1", width = "100%", height = "250px")),
      column(5, textOutput("text1"))
      ),
      
      fluidRow(
        column(7, plotlyOutput("curve2", width = "100%", height = "250px")),
        column(5, textOutput("text2"))
      )
      
    )
  )
)

server = function(input, output) {
  output$curve1 = renderPlotly({
    
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
  
  output$curve2 = renderPlotly({
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
    parameters.2 = list(beta = input$beta-(input$beta_change)/100,
                        sigma = 0.192, gamma = 0.434,
                        mu = 0.00005
    )
    
    output = ode(times = 0:input$time, func = function2, y = initials, parms = parameters)
    output.2 = ode(times = 0:input$time, func = function2, y = initials, parms = parameters.2)
    
    visual_data = as.data.frame(output) %>% 
      mutate(group = "B")
    visula_2 = as.data.frame(output.2) %>% 
      mutate(group = "A")
    vis = full_join(visual_data,visula_2) %>% 
      mutate(group = as.factor(group))
    
    # curve when beta is different
    fig1 = vis %>% 
      group_by(group) %>% 
      ggplot(aes(x = time, y = I,color = group)) + 
      geom_line()+
      theme_bw()+
      labs(y="Infectious proportion")+
      geom_hline(yintercept=0.05, color = "red", linetype="dashed", size=1)+
      annotate("text", x=input$time*2/3, y=0.057, label="Healthcare system capacity", size = 3, color = "red")
    ggplotly(fig1)
  })
  
  output$text1 = renderText("Model explanation")
  
  output$text2 = renderText("social distance")
}

shinyApp(ui, server)
