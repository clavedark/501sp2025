library(shiny)
library(ggplot2)
library(patchwork)

ui <- fluidPage(
  titlePanel("Central Limit Theorem Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      HTML("
            <div style='margin-bottom: 15px; font-size: 0.9em; line-height: 1.3'>
            In this simulation, you can:
            <ul style='margin-top: 5px; padding-left: 20px;'>
                <li>Choose different parent distributions</li>
                <li>Adjust the sample size (n)</li>
                <li>Compare the sampling distribution (green) to the theoretical normal curve (red)</li>
            </ul>
            </div>
            "),
      selectInput("dist", "Parent Distribution:",
                  choices = c("Binomial" = "binom",
                              "Normal" = "norm",
                              "Uniform" = "unif",
                              "Exponential" = "exp",
                              "Chi-squared" = "chisq")),
      
      conditionalPanel(
        condition = "input.dist == 'unif'",
        numericInput("unif_min", "Minimum:", value = 0),
        numericInput("unif_max", "Maximum:", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'norm'",
        numericInput("norm_mean", "Mean:", value = 0),
        numericInput("norm_sd", "Standard Deviation:", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'exp'",
        numericInput("exp_rate", "Rate:", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'chisq'",
        numericInput("chisq_df", "Degrees of Freedom:", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'binom'",
        numericInput("binom_size", "Number of Trials:", value = 10),
        numericInput("binom_prob", "Success Probability:", value = 0.5)
      ),
      
      numericInput("sample_size", "Sample Size (n):", 
                   value = 30, min = 2),
      actionButton("do", "Start!"), 
      br(), br(),
      actionButton("dont", "Stop Simulation"),
      br(), br(),
      checkboxInput("show_normal", "Show Theoretical Normal", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  # Initialize storage for means and simulation state
  means <- c()
  now <- Sys.time()
  is_started <- reactiveVal(FALSE)
  initial_parent <- NULL
  
  # Function to generate random numbers
  generate_random <- function(n) {
    switch(input$dist,
           "norm" = rnorm(n, input$norm_mean, input$norm_sd),
           "unif" = runif(n, input$unif_min, input$unif_max),
           "exp" = rexp(n, input$exp_rate),
           "chisq" = rchisq(n, input$chisq_df),
           "binom" = rbinom(n, input$binom_size, input$binom_prob))
  }
  
  # Function to get distribution name
  get_dist_name <- function() {
    switch(input$dist,
           "norm" = "Normal",
           "unif" = "Uniform",
           "exp" = "Exponential",
           "chisq" = "Chi-squared",
           "binom" = "Binomial")
  }
  
  # Stop button handler - now just stops simulation
  observeEvent(input$dont, {
    is_started(FALSE)  # Just stop the simulation, preserve the means
  })
  
  # Main simulation logic
  observeEvent(input$do, {
    is_started(TRUE)
    means <<- c()  # Reset means when starting new simulation
    now <<- Sys.time()
    
    observe({
      if (is_started()) {  # Only run when simulation is active
        invalidateLater(200, session = getDefaultReactiveDomain())
        
        # Check runtime limit (100 seconds)
        if (Sys.time() > now + 100) {
          is_started(FALSE)
        } else {
          # Generate new sample
          sample_data <- generate_random(input$sample_size)
          new_mean <- mean(sample_data)
          means <<- c(means, new_mean)
        }
      }
    })
  })
  
  # Generate initial parent distribution once
  observe({
    input$dist  # React to distribution changes
    initial_parent <<- data.frame(x = generate_random(1000))
  })
  
  # Reactive data for parent distribution
  parent_data <- reactive({
    if (is_started()) {
      invalidateLater(200, session = getDefaultReactiveDomain())
      data.frame(x = generate_random(1000))
    } else {
      initial_parent
    }
  })
  
  # Combined plot output
  output$plot <- renderPlot({
    # Parent distribution plot
    p1 <- ggplot(parent_data(), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, 
                     fill = "#005A43", color = "black") +
      labs(title = paste("Parent Distribution:", get_dist_name()), 
           x = "Value", y = "Density") +
      theme_minimal()
    
    # Sampling distribution plot
    if(length(means) > 0) {
      # Calculate theoretical parameters
      theo_mean <- switch(input$dist,
                          "norm" = input$norm_mean,
                          "unif" = (input$unif_min + input$unif_max)/2,
                          "exp" = 1/input$exp_rate,
                          "chisq" = input$chisq_df,
                          "binom" = input$binom_size * input$binom_prob)
      
      theo_sd <- switch(input$dist,
                        "norm" = input$norm_sd/sqrt(input$sample_size),
                        "unif" = (input$unif_max - input$unif_min)/(sqrt(12 * input$sample_size)),
                        "exp" = 1/(input$exp_rate * sqrt(input$sample_size)),
                        "chisq" = sqrt(2 * input$chisq_df)/sqrt(input$sample_size),
                        "binom" = sqrt(input$binom_size * input$binom_prob * (1-input$binom_prob))/sqrt(input$sample_size))
      
      p2 <- ggplot(data.frame(x = means), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, 
                       fill = "#6CC24A", color = "black") +
        labs(title = paste("Sampling Distribution of Means\n",
                           "n =", input$sample_size,
                           ", samples =", length(means)),
             x = "Sample Mean", y = "Density") +
        theme_minimal()
      
      if (input$show_normal) {  # Only add normal curve if checkbox is checked
        p2 <- p2 + stat_function(
          fun = dnorm,
          args = list(mean = theo_mean, sd = theo_sd),
          color = "red", size = 1
        )
      }
      
      # Use patchwork to combine plots
      p1 + p2
    } else {
      # If no means yet, just show parent distribution
      p1
    }
  })
}

shinyApp(ui, server)