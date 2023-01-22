library(shiny)
library(dplyr)
library(ggplot2)
library(truncnorm)


ui <- fluidPage(
  titlePanel("Ohm Simulator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("ohm", "Resistance (Ohms)", value = "15.2"),
      numericInput("voltage", "Voltage (V)", value = 20),
      numericInput("uncertainty", "Uncertainty", value = 0.2),
      numericInput("low_limit", "Low Limit", value = 14.8),
      numericInput("high_limit", "High Limit", value = 15.2),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("hist_plot")),
        tabPanel("Line Plot", plotOutput("line_plot"))
      ),
      dataTableOutput("simulation_data")
    )
  )
)



server <- function(input, output) {
  
  # Step 1 Define the variables based on the user input
  ohms <- reactive({input$ohm})
  voltage <- reactive({input$voltage})
  uncertainty <- reactive({input$uncertainty})
  low_limit <- reactive({input$low_limit})
  high_limit <- reactive({input$high_limit})
  # Set up an empty data frame to store the simulation results
  simulation1 <- data.frame()
  
  # When the "Simulate" button is clicked, run the simulation
  observeEvent(input$simulate, {
    
    # Define the parameters of the normal distribution
    mean <- ohms()
    sd <- uncertainty() / 2
    
    # Define the lower and upper limits
    lower_limit <- mean - sd*qnorm(1-((1-0.95)/2))
    upper_limit <- mean + sd*qnorm(1-((1-0.95)/2))
    
    
    
    # Perform the simulation
    for (cycle in 1:100) {
      resistance_uncertain <- rtruncnorm(1, a = lower_limit, b = upper_limit, mean = mean, sd = sd)
      current <- voltage() / resistance_uncertain
      power <- voltage()^2 / resistance_uncertain
      simulation1 <- rbind(simulation1, data.frame(Cycle = cycle, Resistance = resistance_uncertain, Current = current, Power = power))
    }
    
    # Add the process tolerance to the data frame and update the plot
    simulation1 <- simulation1 %>% mutate(ProcTolLow = low_limit(), ProcTolHigh = high_limit())
    
    output$line_plot <- renderPlot({
      ggplot(data = simulation1, aes(x = Cycle, y = Resistance)) +
        geom_line() + 
        geom_line(aes(y = ProcTolLow), color = "red") +  
        geom_line(aes(y = ProcTolHigh), color = "red") +
        labs(title = "Ohm Simulator", x = "Cycle", y = "Resistance (Ohms)")
    })
    
    output$hist_plot <- renderPlot({
      ggplot(data = simulation1, aes(x = Resistance)) +
        geom_histogram(binwidth = .05) +
        geom_vline(xintercept = c(low_limit(), high_limit()), color = "red", linetype = "dashed") +
        labs(title = "Resistance Distribution", x = "Resistance (Ohms)", y = "Frequency")
    })
    
    # Display the simulation data on the user interface
    output$simulation_data <- renderDataTable(simulation1)
  })
}




shinyApp(ui = ui, server = server)
