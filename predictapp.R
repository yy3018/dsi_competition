library(shiny)
library(tidyverse)
library(EpiDynamics)
library(deSolve)
library(plotly)

# shiny app
ui = fluidPage(
  theme = "united",
  # App title ----
  titlePanel(h2("Social Distancing Effect - Create an SEIR Model Yourself!")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput("time", h6("Time/days"),
                  min = 0, max = 300, value = 200),
      fluidRow(
      column(6,numericInput("beta", 
                   h6("Beta"), 
                   value = 0.741)),
      
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
                   h6("Decrease in Beta (i.e. with social distancing)"), 
                   min = 0, max = 1, value = 0.48),
      sliderInput("capacity", 
                  h6("Healthcare System Capacity"), 
                  min = 0, max = 0.06, value = 0.05)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
      column(7, plotlyOutput("curve1", width = "100%", height = "250px")),
      column(5, h4(strong("Parameters explanation:")),
                 div(strong("Beta:"), "the average contact rate"),
                 div(strong("Sigma:"), "the inverse of the incubation period (set as 0.156)", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/32046819", "[1]") ),
                 div(strong("Gamma:"), " the inverse of the mean infectious period (set as 0.153)",tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7139011/", "[2]")),
                 div(strong("Mu:"), " the per capita death rate regardless of decease (set as 0.007)"),
                 div(strong("SEIR:"), "susceptible, exposed, infected and recoverd/removed."),
                div(strong("A/B:"),"after/before social distancing."))
      ),
      
      fluidRow(
        column(7, plotlyOutput("curve2", width = "100%", height = "250px")),
        column(5, h4(strong("Model Results:")),
                  div("When more social distancing takes place through the population (i.e. beta decreases), 
                      we can see the flattening effect. For instance, when beta decreases 0.48 from 0.741, 
                      the peak of A-curve meets the healthcare system capacity which is considered to be capable to cover 5% of population.
                      From this result, we reveal the effect of social distancing."))
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
    
    parameters = list(beta = input$beta, sigma =  0.156, gamma = 0.153, mu=0.007)
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
      theme_bw()+
      labs(x = "Time(days)", y="Population Fraction", title = "Fig.1 COVID-19 SEIR Model")+
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 7),
            legend.title = element_blank())
    
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
    
    parameters = list(beta = input$beta, sigma =  0.156, gamma = 0.153, mu=0.007)
    S=input$s
    E=input$e
    I=input$i
    R=1-S-E-I
    initials = c(S=S,E=E,I=I,R=R)
    parameters.2 = list(beta = input$beta-input$beta_change,
                        sigma = 0.156, gamma = 0.153,
                        mu = 0.007
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
      labs(x = "Time(days)", y="Infected Population Proportion", title = "Fig.2 COVID-19 SEIR Model with Social Distancing")+
      geom_hline(yintercept=input$capacity, color = "red", linetype="dashed", size=1)+
      annotate("text", x=input$time*2/3, y=input$capacity+0.007, label="Healthcare system capacity", size = 3, color = "red")+
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 7),
            legend.title = element_blank())
    
    ggplotly(fig1)
  })
  

}

shinyApp(ui, server)
