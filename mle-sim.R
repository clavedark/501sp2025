# mle-shiny-app.R
# Maximum Likelihood Estimation Interactive Exploration
# Supports both normal and binary (Bernoulli) distributions

library(shiny)
library(ggplot2)
library(dplyr)

# Define color palette
bucolors <- c("#005A43", "#6CC24A", "#A7DA92", "#BDBEBD", "#000000")

#----------------------------------------
# Data Generation Functions
#----------------------------------------

# Function to generate normal data
generate_normal_data <- function(n = 20, mean = 10, sd = 2, seed = 123) {
  set.seed(seed)
  rnorm(n, mean = mean, sd = sd)
}

# Function to generate binary data
generate_binary_data <- function(n = 50, prob = 0.7, seed = 456) {
  set.seed(seed)
  rbinom(n, size = 1, prob = prob)
}

#----------------------------------------
# Log-Likelihood Functions
#----------------------------------------

# Log-likelihood function for normal distribution
normal_log_likelihood <- function(mu, data, sigma = 2) {
  n <- length(data)
  log_lik <- -n/2 * log(2*pi) - n * log(sigma) - 
    sum((data - mu)^2) / (2 * sigma^2)
  return(log_lik)
}

# Log-likelihood function for binary data
binary_log_likelihood <- function(p, data) {
  s <- sum(data)  # number of successes
  n <- length(data)  # total number of trials
  
  # Calculate log-likelihood
  log_lik <- s * log(p) + (n - s) * log(1 - p)
  
  return(log_lik)
}

#----------------------------------------
# History Generation Functions
#----------------------------------------

# Generate iteration history for normal data
generate_normal_history <- function(data) {
  # Initial values
  iterations <- list(
    c(8.0),    # far below mean
    c(9.0),    # moving toward mean
    c(9.5),    # getting closer
    c(10.0),   # close to true mean
    c(10.5),   # slightly beyond
    c(10.2),   # fine-tuning
    c(10.25),  # fine-tuning
    c(mean(data))  # true MLE
  )
  
  # Calculate log-likelihoods for each iteration
  history <- data.frame(
    iteration = 1:length(iterations),
    param = unlist(iterations),
    log_likelihood = sapply(unlist(iterations), normal_log_likelihood, data = data)
  )
  
  return(history)
}

# Generate iteration history for binary data
generate_binary_history <- function(data) {
  # Calculate the sample proportion (true MLE)
  p_mle <- mean(data)
  
  # Initial values - distributed around the MLE
  iterations <- list(
    c(0.3),    # far below true value
    c(0.5),    # moving toward true value
    c(0.6),    # getting closer
    c(0.68),   # very close
    c(p_mle)   # true MLE (sample proportion)
  )
  
  # Calculate log-likelihoods for each iteration
  history <- data.frame(
    iteration = 1:length(iterations),
    param = unlist(iterations),
    log_likelihood = sapply(unlist(iterations), binary_log_likelihood, data = data)
  )
  
  return(history)
}

#----------------------------------------
# Shiny App
#----------------------------------------

# Define the Shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("Maximum Likelihood Estimation - Interactive Exploration"),
    
    sidebarLayout(
      sidebarPanel(
        # Distribution selection
        radioButtons("distribution", "Distribution Type:",
                     choices = c("Normal" = "normal",
                                 "Binary (Bernoulli)" = "binary"),
                     selected = "normal"),
        
        hr(),
        
        # Data generation settings - Normal
        conditionalPanel(
          condition = "input.distribution == 'normal'",
          selectInput("normal_dataset", "Normal Dataset:", 
                      choices = c("Default (n=20, mean=10, sd=2)" = "default",
                                  "Custom Parameters" = "custom")),
          
          conditionalPanel(
            condition = "input.normal_dataset == 'custom'",
            numericInput("normal_mean", "True Mean:", 10, min = 0, max = 20),
            numericInput("normal_sd", "True Standard Deviation:", 2, min = 0.1, max = 5),
            numericInput("normal_n", "Sample Size:", 20, min = 5, max = 100),
            actionButton("generate_normal", "Generate New Data")
          )
        ),
        
        # Data generation settings - Binary
        conditionalPanel(
          condition = "input.distribution == 'binary'",
          selectInput("binary_dataset", "Binary Dataset:", 
                      choices = c("Default (n=50, p=0.7)" = "default",
                                  "Custom Parameters" = "custom")),
          
          conditionalPanel(
            condition = "input.binary_dataset == 'custom'",
            numericInput("binary_prob", "True Probability:", 0.7, min = 0.1, max = 0.9, step = 0.05),
            numericInput("binary_n", "Sample Size:", 50, min = 10, max = 200),
            actionButton("generate_binary", "Generate New Data")
          )
        ),
        
        hr(),
        
        # Iteration controls - different max values depending on distribution
        conditionalPanel(
          condition = "input.distribution == 'normal'",
          sliderInput("normal_iteration", "Iteration:", 
                      min = 1, max = 8, 
                      value = 1, step = 1, 
                      animate = animationOptions(interval = 1200, loop = TRUE))
        ),
        
        conditionalPanel(
          condition = "input.distribution == 'binary'",
          sliderInput("binary_iteration", "Iteration:", 
                      min = 1, max = 5, 
                      value = 1, step = 1, 
                      animate = animationOptions(interval = 1200, loop = TRUE))
        ),
        
        hr(),
        
        # Display options
        h4("Display Options:"),
        checkboxInput("show_all_points", "Show All Previous Points", value = TRUE),
        checkboxInput("show_mle", "Show True MLE", value = TRUE),
        checkboxInput("show_individual", "Show Individual Contributions", value = FALSE),
        
        hr(),
        
        # Current values display
        h4("Current Values:"),
        verbatimTextOutput("current_values")
      ),
      
      mainPanel(
        # Main plot (moved to the top)
        plotOutput("mle_plot", height = "400px"),
        
        hr(),
        
        # Distribution visualization (moved below the likelihood curve)
        conditionalPanel(
          condition = "input.distribution == 'normal'",
          plotOutput("normal_data_viz", height = "250px")
        ),
        
        conditionalPanel(
          condition = "input.distribution == 'binary'",
          plotOutput("binary_data_viz", height = "250px")
        ),
        
        conditionalPanel(
          condition = "input.show_individual == true",
          hr(),
          h4("Individual Log-Likelihood Contributions"),
          plotOutput("individual_plot", height = "250px")
        ),
        
        hr(),
        
        # Iteration history table
        h4("Iteration History"),
        tableOutput("iteration_table")
      )
    )
  ),
  
  server = function(input, output, session) {
    # Reactive values to store data and history
    rv <- reactiveValues(
      normal_data = generate_normal_data(),
      normal_history = NULL,
      binary_data = generate_binary_data(),
      binary_history = NULL
    )
    
    # Generate histories on startup
    observe({
      rv$normal_history <- generate_normal_history(rv$normal_data)
      rv$binary_history <- generate_binary_history(rv$binary_data)
      
      # Update slider max values based on history lengths
      updateSliderInput(session, "normal_iteration", max = nrow(rv$normal_history))
      updateSliderInput(session, "binary_iteration", max = nrow(rv$binary_history))
    })
    
    # Regenerate normal data when requested
    observeEvent(input$generate_normal, {
      if (input$normal_dataset == "custom") {
        rv$normal_data <- generate_normal_data(
          n = input$normal_n,
          mean = input$normal_mean,
          sd = input$normal_sd,
          seed = as.numeric(Sys.time())  # Use current time as seed
        )
        rv$normal_history <- generate_normal_history(rv$normal_data)
        updateSliderInput(session, "normal_iteration", max = nrow(rv$normal_history))
      }
    })
    
    # Regenerate binary data when requested
    observeEvent(input$generate_binary, {
      if (input$binary_dataset == "custom") {
        rv$binary_data <- generate_binary_data(
          n = input$binary_n,
          prob = input$binary_prob,
          seed = as.numeric(Sys.time())  # Use current time as seed
        )
        rv$binary_history <- generate_binary_history(rv$binary_data)
        updateSliderInput(session, "binary_iteration", max = nrow(rv$binary_history))
      }
    })
    
    # Helper function to get current data based on selected distribution
    get_current_data <- reactive({
      if (input$distribution == "normal") {
        return(rv$normal_data)
      } else {
        return(rv$binary_data)
      }
    })
    
    # Helper function to get current history based on selected distribution
    get_current_history <- reactive({
      if (input$distribution == "normal") {
        return(rv$normal_history)
      } else {
        return(rv$binary_history)
      }
    })
    
    # Helper function to get current iteration based on selected distribution
    get_current_iteration <- reactive({
      if (input$distribution == "normal") {
        return(input$normal_iteration)
      } else {
        return(input$binary_iteration)
      }
    })
    
    # Helper function to get correct log-likelihood function
    get_ll_function <- reactive({
      if (input$distribution == "normal") {
        return(normal_log_likelihood)
      } else {
        return(binary_log_likelihood)
      }
    })
    
    # Calculate the log-likelihood curve for normal data
    normal_ll_curve <- reactive({
      mu_values <- seq(min(rv$normal_history$param) - 2, max(rv$normal_history$param) + 2, by = 0.1)
      ll_values <- sapply(mu_values, normal_log_likelihood, data = rv$normal_data)
      data.frame(param = mu_values, log_likelihood = ll_values)
    })
    
    # Calculate the log-likelihood curve for binary data
    binary_ll_curve <- reactive({
      p_values <- seq(0.1, 0.9, by = 0.01)
      ll_values <- sapply(p_values, binary_log_likelihood, data = rv$binary_data)
      data.frame(param = p_values, log_likelihood = ll_values)
    })
    
    # Get the appropriate curve based on distribution
    get_current_curve <- reactive({
      if (input$distribution == "normal") {
        return(normal_ll_curve())
      } else {
        return(binary_ll_curve())
      }
    })
    
    # Render normal data visualization
    output$normal_data_viz <- renderPlot({
      # Create data frame for plotting
      df <- data.frame(x = rv$normal_data)
      
      # Plot histogram with ggplot
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = 10, fill = bucolors[2], color = "white", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(x)), color = bucolors[1], linewidth = 1.2, linetype = "dashed") +
        labs(title = "Normal Data Distribution",
             subtitle = paste("Sample mean =", round(mean(rv$normal_data), 2),
                              "Sample SD =", round(sd(rv$normal_data), 2),
                              "n =", length(rv$normal_data)),
             x = "Value",
             y = "Count") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        )
    })
    
    # Render binary data visualization
    output$binary_data_viz <- renderPlot({
      # Create a data frame for plotting
      binary_df <- data.frame(
        value = factor(rv$binary_data, levels = c(0, 1), labels = c("Failure (0)", "Success (1)")),
        count = 1
      )
      
      # Create a bar plot
      ggplot(binary_df, aes(x = value, fill = value)) +
        geom_bar() +
        scale_fill_manual(values = c(bucolors[4], bucolors[2])) +
        labs(title = "Binary Data Distribution",
             subtitle = paste("Sample proportion of successes =", round(mean(rv$binary_data), 3),
                              "n =", length(rv$binary_data)),
             x = "Outcome",
             y = "Count") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "none"
        )
    })
    
    # Render the main MLE plot
    output$mle_plot <- renderPlot({
      # Get current data, history, iteration based on selected distribution
      current_data <- get_current_data()
      current_history <- get_current_history()
      current_iteration <- get_current_iteration()
      current_curve <- get_current_curve()
      
      # Subset history up to current iteration
      history_subset <- current_history[1:current_iteration, ]
      current_point <- history_subset[current_iteration, ]
      
      # Set x-axis label based on distribution
      x_label <- ifelse(input$distribution == "normal", 
                        "μ (Mean Parameter)", 
                        "p (Probability Parameter)")
      
      # Get the true MLE value
      true_mle <- mean(current_data)
      
      # Get MLE label based on distribution
      mle_label <- ifelse(input$distribution == "normal", 
                          "True MLE (Sample Mean)", 
                          "True MLE (Sample Proportion)")
      
      # Create the base plot
      p <- ggplot() +
        # Log-likelihood curve
        geom_line(data = current_curve, aes(x = param, y = log_likelihood), 
                  color = bucolors[1], linewidth = 1.2) +
        # Labels and styling
        labs(
          title = paste("Maximum Likelihood Estimation - Iteration", current_iteration),
          subtitle = paste("Current estimate:", 
                           ifelse(input$distribution == "normal", "μ =", "p ="), 
                           round(current_point$param, 4)),
          x = x_label,
          y = "Log-Likelihood"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        )
      
      # Add previous points if requested
      if (input$show_all_points && current_iteration > 1) {
        p <- p + geom_point(
          data = history_subset[-current_iteration, ], 
          aes(x = param, y = log_likelihood), 
          color = bucolors[3], size = 6, alpha = 0.8,
          shape = 16  # Filled circle
        ) +
          geom_text(
            data = history_subset[-current_iteration, ],
            aes(x = param, y = log_likelihood, label = iteration),
            color = "white", fontface = "bold", size = 3,
            vjust = 0.4, hjust = 0.5
          )
      }
      
      # Add the current point
      p <- p + geom_point(
        data = current_point, 
        aes(x = param, y = log_likelihood), 
        color = bucolors[2], size = 8, shape = 16  # Larger filled circle
      ) +
        geom_text(
          data = current_point,
          aes(x = param, y = log_likelihood, label = iteration),
          color = "white", fontface = "bold", size = 3.5,
          vjust = 0.4, hjust = 0.5
        )
      
      # Add the true MLE line if requested
      if (input$show_mle) {
        p <- p + geom_vline(
          xintercept = true_mle, 
          color = bucolors[5], 
          linetype = "dashed", 
          linewidth = 0.8
        ) +
          annotate("text", 
                   x = true_mle + ifelse(input$distribution == "normal", 0.3, 0.05), 
                   y = min(current_curve$log_likelihood) + 2, 
                   label = mle_label, 
                   color = bucolors[5], hjust = 0, size = 3.5)
      }
      
      return(p)
    })
    
    # Render the individual log-likelihoods plot
    output$individual_plot <- renderPlot({
      req(input$show_individual)
      
      # Get current iteration and data based on selected distribution
      if (input$distribution == "normal") {
        current_iteration <- input$normal_iteration
        current_point <- rv$normal_history[current_iteration, ]
        current_data <- rv$normal_data
        
        # Calculate individual log-likelihood terms for each data point
        individual_loglik <- data.frame(
          observation = current_data,
          term = -0.5 * log(2*pi) - log(2) - (current_data - current_point$param)^2 / (2 * 2^2)
        )
        
        # Add observation index
        individual_loglik$index <- 1:nrow(individual_loglik)
        
        # Create the plot
        p <- ggplot(individual_loglik, aes(x = index, y = term)) +
          geom_col(fill = bucolors[2]) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(
            title = paste("Individual Log-Likelihood Terms (μ =", round(current_point$param, 4), ")"),
            subtitle = paste("Sum =", round(sum(individual_loglik$term), 4)),
            x = "Observation",
            y = "Log-Likelihood Contribution"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 12),
            axis.title = element_text(face = "bold")
          )
        
      } else {
        # For binary data
        current_iteration <- input$binary_iteration
        current_point <- rv$binary_history[current_iteration, ]
        current_data <- rv$binary_data
        
        # Calculate individual log-likelihood terms for binary data
        individual_loglik <- data.frame(
          observation = current_data,
          term = ifelse(current_data == 1, 
                        log(current_point$param),     # log(p) for successes
                        log(1 - current_point$param)) # log(1-p) for failures
        )
        
        # Add observation index and observation type
        individual_loglik$index <- 1:nrow(individual_loglik)
        individual_loglik$type <- ifelse(individual_loglik$observation == 1, "Success", "Failure")
        
        # Create the plot
        p <- ggplot(individual_loglik, aes(x = index, y = term, fill = type)) +
          geom_col() +
          scale_fill_manual(values = c("Failure" = bucolors[4], "Success" = bucolors[2])) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(
            title = paste("Individual Log-Likelihood Terms (p =", round(current_point$param, 4), ")"),
            subtitle = paste("Sum =", round(sum(individual_loglik$term), 4),
                             "= Successes × log(p) + Failures × log(1-p)"),
            x = "Observation",
            y = "Log-Likelihood Contribution"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 12),
            axis.title = element_text(face = "bold")
          )
      }
      
      return(p)
    })
    
    # Display current values
    output$current_values <- renderText({
      if (input$distribution == "normal") {
        current_iteration <- input$normal_iteration
        current_point <- rv$normal_history[current_iteration, ]
        current_data <- rv$normal_data
        true_mle <- mean(current_data)
        
        return(paste(
          "μ =", round(current_point$param, 4), "\n",
          "Log-likelihood =", round(current_point$log_likelihood, 4), "\n",
          "Distance from MLE =", round(abs(current_point$param - true_mle), 4), "\n",
          "Sample mean =", round(true_mle, 4), "\n",
          "Sample size =", length(current_data)
        ))
      } else {
        current_iteration <- input$binary_iteration
        current_point <- rv$binary_history[current_iteration, ]
        current_data <- rv$binary_data
        true_mle <- mean(current_data)
        
        return(paste(
          "p =", round(current_point$param, 4), "\n",
          "Log-likelihood =", round(current_point$log_likelihood, 4), "\n",
          "Distance from MLE =", round(abs(current_point$param - true_mle), 4), "\n",
          "Successes =", sum(current_data), "\n",
          "Failures =", length(current_data) - sum(current_data), "\n",
          "Sample proportion =", round(true_mle, 4), "\n",
          "Sample size =", length(current_data)
        ))
      }
    })
    
    # Display the iteration history table
    output$iteration_table <- renderTable({
      if (input$distribution == "normal") {
        current_iteration <- input$normal_iteration
        current_history <- rv$normal_history
        true_mle <- mean(rv$normal_data)
        param_name <- "mu"
      } else {
        current_iteration <- input$binary_iteration
        current_history <- rv$binary_history
        true_mle <- mean(rv$binary_data)
        param_name <- "p"
      }
      
      # Subset and format history
      history_subset <- current_history[1:current_iteration, ]
      names(history_subset)[2] <- param_name  # Rename param column to mu or p
      
      # Add improvement and distance columns
      history_subset$improvement <- c(NA, diff(history_subset$log_likelihood))
      history_subset$distance_from_mle <- abs(history_subset[[param_name]] - true_mle)
      
      # Format columns
      history_subset[[param_name]] <- round(history_subset[[param_name]], 4)
      history_subset$log_likelihood <- round(history_subset$log_likelihood, 4)
      history_subset$improvement <- round(history_subset$improvement, 4)
      history_subset$distance_from_mle <- round(history_subset$distance_from_mle, 4)
      
      return(history_subset)
    })
  }
)