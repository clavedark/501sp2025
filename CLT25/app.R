#central limit theorem simulation
library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Central Limit Theorem Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution:",
                  choices = c("Normal", "Binomial", "Poisson")),
      sliderInput("n", "Sample Size:", min = 10, max = 1000, value = 100),
      sliderInput("sims", "Number of Simulations:", min = 10, max = 1000, value = 100),
      sliderInput("delay", "Delay (ms):", min = 100, max = 1000, value = 200),
      actionButton("go", "Simulate"),
      actionButton("reset", "Reset"),
      actionButton("stop", "Stop")
    ),
    
    mainPanel(
      plotOutput("histPlot"),
      tags$br(),
      textOutput("progressText")
    )
  )
)

server <- function(input, output, session) {
  
  v <- reactiveValues(plot = NULL, counter = 0, running = FALSE)
  
  observeEvent(input$go, {
    v$plot <- NULL
    v$counter <- 0
    output$progressText <- renderText("")
    v$running <- TRUE
    
    generate_data <- function(dist, n) {
      switch(dist,
             Normal = rnorm(n),
             Binomial = rbinom(n, 10, 0.5),
             Poisson = rpois(n, 5)
      )
    }
    sim_data <- replicate(input$sims, generate_data(input$dist, input$n))
    means <- apply(sim_data, 2, mean)
    
    obs <- observe({
      if (v$running) {
        invalidateLater(input$delay, session)
        
        isolate({
          if (v$counter < input$sims) {
            v$counter <- v$counter + 1
            data <- data.frame(means = means[1:v$counter])
            v$plot <- ggplot(data, aes(x = means)) +
              geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
              geom_density(color = "red") +
              labs(title = paste("Number of Simulations:", v$counter),
                   x = "Sample Means", y = "Density") +
              theme_bw()
            
            output$progressText <- renderText({
              paste("Simulation:", v$counter, "of", input$sims)
            })
          } else {
            v$running <- FALSE
            output$progressText <- renderText("Simulation complete.")
          }
        })
      } # end if(v$running)
    }) # End of observe (obs)
    
    
    observeEvent(v$running, {
      if (!v$running) { # Check if running has become FALSE
        obs$destroy()
      }
    })
    
    
    output$histPlot <- renderPlot({
      v$plot
    })
    
  })  # End of observeEvent(input$go)
  
  
  
  observeEvent(input$reset, {
    v$plot <- NULL
    v$counter <- 0
    output$progressText <- renderText("")
    v$running <- FALSE
  })
  
  
  observeEvent(input$stop, {
    v$running <- FALSE
    output$progressText <- renderText("Simulation stopped.")
    
  })
  
  
}  # End of server function



shinyApp(ui = ui, server = server)