# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

# Simulation function
simulate_tea_test <- function(n_cups = 8, sensitivity = 0.9) {
  true_order <- rep(c("milk_first", "tea_first"), each = n_cups/2)
  cups <- sample(true_order)
  
  lady_guesses <- if(sensitivity > 0.5) {
    sapply(cups, function(x) {
      if(runif(1) < sensitivity) x else sample(c("milk_first", "tea_first"), 1)
    })
  } else {
    sample(c("milk_first", "tea_first"), n_cups, replace = TRUE)
  }
  
  correct_guesses <- sum(lady_guesses == cups)
  return(correct_guesses)
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Fisher's Lady Tasting Tea: A Journey Through Hypothesis Testing"),
  
  navbarPage(
    "Experiment Navigation",
    
    # Tab 1: Theory (moved to first position)
    tabPanel("Theory",
      fluidRow(
        column(12,
          wellPanel(
            HTML("
              <h3>The Lady Tasting Tea: A Foundational Example in Statistics</h3>
              <p>In the 1920s, a colleague of R.A. Fisher claimed she could tell whether milk was added 
              to tea before or after pouring. Instead of dismissing this claim, Fisher designed an 
              elegant experiment that would become a cornerstone example in statistical testing.</p>
              
              <h4>The Experimental Design</h4>
              <ul>
                <li><strong>Setup:</strong> Eight cups of tea are prepared, four with milk added first, 
                four with tea added first</li>
                <li><strong>Randomization:</strong> The cups are presented in random order</li>
                <li><strong>Control:</strong> All other variables (temperature, amount of milk, etc.) 
                are kept constant</li>
                <li><strong>Blind Testing:</strong> The lady doesn't know which cups are which</li>
              </ul>

              <h3>Core Concepts in Hypothesis Testing</h3>
              
              <h4>1. Hypotheses</h4>
              <ul>
                <li><strong>Null Hypothesis (H₀):</strong> The lady cannot discriminate between 
                preparation methods
                <ul>
                  <li>Under H₀, she is guessing randomly</li>
                  <li>Probability of correct guess = 0.5 for each cup</li>
                  <li>This is our skeptical starting position</li>
                </ul></li>
                
                <li><strong>Alternative Hypothesis (H₁):</strong> The lady can discriminate
                <ul>
                  <li>Under H₁, she has some true sensitivity > 0.5</li>
                  <li>This is what we're looking for evidence to support</li>
                </ul></li>
              </ul>

              <h4>2. Test Statistic</h4>
              <ul>
                <li><strong>What we measure:</strong> Number of correct guesses out of 8</li>
                <li><strong>Under H₀:</strong> Follows a Binomial(n=8, p=0.5) distribution</li>
                <li><strong>Key property:</strong> Larger values are more evidence against H₀</li>
              </ul>

              <h4>3. Calculating Critical Values and p-values</h4>
              <div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #00BFC4;'>
                <h5>Critical Value Calculation</h5>
                <p>The critical value is the minimum number of correct guesses needed to reject the null hypothesis at a given significance level (α).</p>
                
                <p><strong>Steps to calculate:</strong></p>
                <ol>
                  <li>Specify the significance level α (e.g., 0.05)</li>
                  <li>Under H₀ (random guessing), the number of correct guesses follows a binomial distribution with parameters n (number of cups) and p=0.5</li>
                  <li>Find the smallest value k such that P(X ≥ k) ≤ α, where X is the number of correct guesses</li>
                </ol>
                
                <p><strong>R code:</strong></p>
                <pre>critical_value <- qbinom(1 - α, size = n, prob = 0.5)</pre>
                
                <p><strong>Example:</strong> With 8 cups and α = 0.05</p>
                <pre>critical_value <- qbinom(1 - 0.05, size = 8, prob = 0.5)
# equals 7</pre>
                
                <p>So we need at least 7 correct guesses (out of 8) to reject H₀ at the 0.05 level.</p>
                
                <h5>p-value Calculation</h5>
                <p>The p-value is the probability of observing a result at least as extreme as the one obtained, assuming the null hypothesis is true.</p>
                
                <p><strong>Steps to calculate:</strong></p>
                <ol>
                  <li>Observe the number of correct guesses k</li>
                  <li>Calculate P(X ≥ k) under H₀</li>
                </ol>
                
                <p><strong>R code:</strong></p>
                <pre>p_value <- pbinom(k - 1, size = n, prob = 0.5, lower.tail = FALSE)
# or equivalently:
p_value <- 1 - pbinom(k - 1, size = n, prob = 0.5)</pre>
                
                <p><strong>Example:</strong> If the lady gets 7 correct guesses out of 8</p>
                <pre>p_value <- 1 - pbinom(7 - 1, size = 8, prob = 0.5)
# equals 0.03516</pre>
                
                <p>This p-value (0.03516) is less than α = 0.05, so we reject H₀.</p>
              </div>

              <h4>4. Significance Level (α)</h4>
              <ul>
                <li><strong>Definition:</strong> Probability of rejecting H₀ when it's actually true</li>
                <li><strong>Traditional choices:</strong> 0.05 or 0.01</li>
                <li><strong>Trade-off:</strong> Smaller α means fewer false positives but less power</li>
              </ul>

              <h4>5. Types of Errors</h4>
              <table class='table'>
                <tr>
                  <th></th>
                  <th>H₀ True (No Ability)</th>
                  <th>H₁ True (Has Ability)</th>
                </tr>
                <tr>
                  <td><strong>Reject H₀</strong></td>
                  <td>Type I Error (α)<br>False claim of ability</td>
                  <td>Correct Decision (Power)<br>True detection of ability</td>
                </tr>
                <tr>
                  <td><strong>Fail to Reject H₀</strong></td>
                  <td>Correct Decision<br>True negative</td>
                  <td>Type II Error (β)<br>Missed detection of ability</td>
                </tr>
              </table>
            ")
          )
        )
      )
    ),
    
    # Tab 2: Single Trial
    tabPanel("Single Trial",
      fluidRow(
        column(12,
          wellPanel(
            HTML("
              <h4>Understanding a Single Experiment</h4>
              <p>Each trial represents one complete experiment where:</p>
              <ul>
                <li>8 cups are prepared in random order</li>
                <li>The lady makes a guess for each cup</li>
                <li>We record her accuracy</li>
              </ul>
              <p>Key points to consider:</p>
              <ul>
                <li>Even if the lady has no ability, she'll get some right by chance</li>
                <li>Even if she has ability, she won't be perfect</li>
                <li>One trial alone cannot prove ability - we need statistical analysis</li>
              </ul>
            ")
          ),
          actionButton("run_single", "Run Single Trial", class = "btn-info"),
          hr(),
          DTOutput("single_trial")
        )
      )
    ),
    
    # Tab 3: Run Experiments
    tabPanel("Run Experiments",
      sidebarLayout(
        sidebarPanel(
          numericInput("n_simulations", "Number of Simulations:", 1000, min = 100, max = 10000),
          numericInput("n_cups", "Number of Cups (even):", 8, min = 4, max = 20, step = 2),
          sliderInput("sensitivity", "Taster's True Sensitivity:", min = 0.5, max = 1.0, value = 0.9, step = 0.05),
          sliderInput("alpha", "Significance Level (α):", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
          actionButton("run_sim", "Run Simulation", class = "btn-primary"),
          
          hr(),
          
          wellPanel(
            h4("Current Settings:"),
            verbatimTextOutput("settings_summary")
          )
        ),
        
        mainPanel(
          plotOutput("distPlot"),
          
          wellPanel(
            h4("Statistical Summary"),
            verbatimTextOutput("summary"),
            
            h4("Explanation of Results"),
            uiOutput("stats_explanation"),
            
            hr(),
            
            h4("Interpretation of the Visualization"),
            HTML("
              <p><strong>The red dashed line</strong> represents the critical value - the minimum number of correct 
              guesses needed to reject the null hypothesis at the chosen significance level (α).</p>
              <ul>
                <li>Any result to the right of this line is considered statistically significant</li>
                <li>This threshold is calculated using the binomial distribution under H₀ (random guessing)</li>
                <li>The calculation finds the minimum value where the probability of that many or more 
                correct guesses by chance is ≤ α</li>
              </ul>
              <p>The distribution shown in the plot represents the outcome of many simulated experiments. 
              The proportion of results that fall to the right of the red line gives us the power 
              of the test - our ability to detect the lady's true sensitivity when it exists.</p>
            ")
          )
        )
      )
    ),
    
    # Tab 4: P-values and Critical Values
    tabPanel("P-values & Critical Values",
      sidebarLayout(
        sidebarPanel(
          numericInput("obs_correct", "Observed Correct Guesses:", 7, min = 0, max = 20),
          numericInput("pv_cups", "Number of Cups:", 8, min = 4, max = 20, step = 2),
          
          hr(),
          
          h4("Calculate p-value"),
          verbatimTextOutput("p_value_calc"),
          
          hr(),
          
          sliderInput("pv_alpha", "Significance Level (α):", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
          h4("Calculate critical value"),
          verbatimTextOutput("crit_value_calc")
        ),
        
        mainPanel(
          wellPanel(
            HTML("
              <h3>Understanding p-values and Critical Values</h3>
              
              <h4>p-value</h4>
              <p>The p-value is the probability of observing a result at least as extreme as the one obtained, 
              assuming the null hypothesis is true.</p>
              
              <p>In the Lady Tasting Tea experiment:</p>
              <ul>
                <li>If the lady gets 8 out of 8 correct: p-value = 0.00391</li>
                <li>If the lady gets 7 out of 8 correct: p-value = 0.03516</li>
                <li>If the lady gets 6 out of 8 correct: p-value = 0.14453</li>
              </ul>
              
              <p>The p-value answers: <em>\"If the lady were just guessing randomly, what's the probability 
              she would get this many or more correct answers?\"</em></p>
              
              <h4>Critical Value</h4>
              <p>The critical value is the minimum number of correct guesses needed to reject H₀ at a given 
              significance level.</p>
              
              <p>For a two-sided test with α = 0.05:</p>
              <ul>
                <li>With 8 cups: need ≥ 7 correct to reject H₀</li>
                <li>With 10 cups: need ≥ 8 correct to reject H₀</li>
                <li>With 16 cups: need ≥ 12 correct to reject H₀</li>
              </ul>
              
              <h4>How They're Related</h4>
              <p>When testing a hypothesis:</p>
              <ul>
                <li>If p-value ≤ α: Reject H₀</li>
                <li>If p-value > α: Fail to reject H₀</li>
              </ul>
              <p>Equivalently:</p>
              <ul>
                <li>If observed value ≥ critical value: Reject H₀</li>
                <li>If observed value < critical value: Fail to reject H₀</li>
              </ul>
            ")
          ),
          
          plotOutput("p_value_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store simulation results
  sim_results <- eventReactive(input$run_sim, {
    replicate(input$n_simulations, 
              simulate_tea_test(input$n_cups, input$sensitivity))
  })
  
  # Settings summary - using the consistent function
  output$settings_summary <- renderText({
    n_cups <- input$n_cups
    alpha <- input$alpha
    
    # Calculate critical value consistently
    critical_value <- get_critical_value(n_cups, alpha)
    
    # Calculate p-value for critical value - 1 (which should be > alpha for n=8, alpha=0.05)
    one_below <- critical_value - 1
    p_value_below <- sum(dbinom(one_below:n_cups, size = n_cups, prob = 0.5))
    
    # Calculate p-value for critical value (which should be <= alpha)
    p_value_at <- sum(dbinom(critical_value:n_cups, size = n_cups, prob = 0.5))
    
    paste0(
      "Critical value: ≥", critical_value, " correct needed\n",
      "p-value if ", one_below, " correct: ", format(p_value_below, digits=5), 
      " (", ifelse(p_value_below <= alpha, "significant", "not significant"), ")\n",
      "p-value if ", critical_value, " correct: ", format(p_value_at, digits=5), 
      " (", ifelse(p_value_at <= alpha, "significant", "not significant"), ")\n",
      "Null hypothesis: 50% accuracy\n",
      "Alternative: ", round(100 * input$sensitivity, 1), "% accuracy"
    )
  })
  
  # Critical value calculation - update ALL usages to be consistent
  get_critical_value <- function(n, alpha) {
    # Calculate p-values for each possible value
    p_values <- sapply(0:n, function(k) {
      sum(dbinom(k:n, size = n, prob = 0.5))  # P(X ≥ k)
    })
    
    # Find minimum k where P(X ≥ k) ≤ alpha
    critical_value <- min(which(p_values <= alpha)) - 1
    return(critical_value)
  }
  
  # Update all critical value calculations to use this function
  output$distPlot <- renderPlot({
    req(sim_results())
    
    # Get current parameters
    n_cups <- input$n_cups
    alpha <- input$alpha
    
    # Calculate critical value consistently
    critical_value <- get_critical_value(n_cups, alpha)
    
    # Count frequencies for each number of correct guesses
    result_counts <- table(factor(sim_results(), levels = 0:n_cups))
    
    # Convert to data frame for ggplot
    plot_data <- data.frame(
      correct_guesses = as.integer(names(result_counts)),
      count = as.integer(result_counts),
      proportion = as.integer(result_counts) / length(sim_results())
    )
    
    # Add significance column
    plot_data$significance <- ifelse(plot_data$correct_guesses >= critical_value, 
                                    "Significant", "Not Significant")
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = correct_guesses, y = proportion)) +
      geom_col(aes(fill = significance), width = 0.7) +
      scale_fill_manual(values = c("Not Significant" = "#00BFC4", 
                                  "Significant" = "#FF8C00")) +
      scale_x_continuous(breaks = 0:n_cups) +
      labs(title = "Distribution of Correct Guesses",
           subtitle = paste0("Critical value = ", critical_value, 
                           " (α = ", alpha, ")"),
           x = "Number of Correct Guesses",
           y = "Proportion",
           fill = "Result") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            legend.position = "bottom")
    
    # Add vertical line at the critical value position
    p + geom_vline(xintercept = critical_value - 0.5, 
                   linetype = "dashed",
                   color = "red", 
                   size = 1.2)
  })
  
  # Statistical summary
  output$summary <- renderText({
    req(sim_results())
    
    critical_value <- qbinom(1 - input$alpha, size = input$n_cups, prob = 0.5)
    n_sig <- sum(sim_results() >= critical_value)
    
    paste0(
      "Total Simulations: ", length(sim_results()), "\n",
      "Mean Correct Guesses: ", round(mean(sim_results()), 2), "\n",
      "Significant Results: ", n_sig, " (",
      round(100 * n_sig/length(sim_results()), 1), "%)"
    )
  })
  
  # Stats explanation
  output$stats_explanation <- renderUI({
    req(sim_results())
    
    critical_value <- qbinom(1 - input$alpha, size = input$n_cups, prob = 0.5)
    power <- mean(sim_results() >= critical_value)
    
    HTML(paste0("
      <p><strong>Mean Correct Guesses:</strong> ", round(mean(sim_results()), 2), " out of ", input$n_cups, "</p>
      <p>This is calculated as: <code>mean(sim_results())</code> - the average number of correct guesses across all simulations.</p>
      
      <p><strong>Significant Results:</strong> ", sum(sim_results() >= critical_value), " out of ", length(sim_results()),
      " (", round(100 * mean(sim_results() >= critical_value), 1), "%)</p>
      <p>This is calculated as: <code>sum(sim_results() >= ", critical_value, ")</code> - counting how many simulation 
      results meet or exceed our critical value of ", critical_value, ".</p>
      
      <p><strong>Power Analysis:</strong> With the lady's true sensitivity at ", input$sensitivity,
      ", we would detect her ability in approximately ", round(100*power),
      "% of experiments using this design.</p>
      <p>This is our <em>statistical power</em> - the probability of correctly rejecting H₀ when H₁ is true.</p>
      <p>Power is calculated as: <code>mean(sim_results() >= ", critical_value, ")</code> - the proportion of 
      simulation results that reach statistical significance.</p>
    "))
  })
  
  # Single trial simulation - fixed with reactiveVal
  trial_data <- reactiveVal(NULL)
  
  observeEvent(input$run_single, {
    true_order <- rep(c("milk_first", "tea_first"), each = input$n_cups/2)
    cups <- sample(true_order)
    
    guesses <- if(input$sensitivity > 0.5) {
      sapply(cups, function(x) {
        if(runif(1) < input$sensitivity) x else sample(c("milk_first", "tea_first"), 1)
      })
    } else {
      sample(c("milk_first", "tea_first"), input$n_cups, replace = TRUE)
    }
    
    trial_df <- data.frame(
      Cup = 1:input$n_cups,
      True_Order = cups,
      Lady_Guess = guesses,
      Correct = cups == guesses
    )
    
    trial_data(trial_df)
  })
  
  output$single_trial <- renderDT({
    req(trial_data())
    
    datatable(trial_data(), 
              options = list(pageLength = 10), 
              rownames = FALSE) %>%
      formatStyle('Correct',
                 backgroundColor = styleEqual(c(FALSE, TRUE),
                                           c('#ffcccc', '#ccffcc')))
  })
  
  # New outputs for the p-value and critical value tab
  
  # p-value calculation
  output$p_value_calc <- renderText({
    k <- input$obs_correct
    n <- input$pv_cups
    
    # p-value
    p_value <- 1 - pbinom(k - 1, size = n, prob = 0.5)
    
    paste0(
      "Observed: ", k, " correct out of ", n, "\n",
      "p-value = P(X ≥ ", k, " | H₀) = ", format(p_value, digits=5), "\n",
      "Calculation: 1 - pbinom(", k-1, ", size=", n, ", prob=0.5)\n",
      "\nInterpretation: If the null hypothesis is true (random guessing),\n",
      "the probability of observing ", k, " or more correct guesses is ", format(p_value, digits=5), ".\n",
      "\nConclusion: ", ifelse(p_value <= input$pv_alpha, 
                            paste0("Reject H₀ (p ≤ ", input$pv_alpha, ")"), 
                            paste0("Fail to reject H₀ (p > ", input$pv_alpha, ")"))
    )
  })
  
  # Critical value calculation
  output$crit_value_calc <- renderText({
    n <- input$pv_cups
    alpha <- input$pv_alpha
    
    critical_value <- qbinom(1 - alpha, size = n, prob = 0.5)
    
    paste0(
      "For n = ", n, " cups and α = ", alpha, ":\n",
      "Critical value = ", critical_value, "\n",
      "Calculation: qbinom(1 - ", alpha, ", size=", n, ", prob=0.5)\n",
      "\nInterpretation: Need ", critical_value, " or more correct guesses\n",
      "to reject H₀ at the ", alpha, " significance level."
    )
  })
  
  # p-value visualization
  output$p_value_plot <- renderPlot({
    k <- input$obs_correct
    n <- input$pv_cups
    
    # Create data for all possible values
    x_values <- 0:n
    probs <- dbinom(x_values, size = n, prob = 0.5)
    plot_data <- data.frame(
      x = x_values,
      prob = probs,
      significance = ifelse(x_values >= k, "Observed or More Extreme", "Less Extreme")
    )
    
    # Calculate p-value
    p_value <- 1 - pbinom(k - 1, size = n, prob = 0.5)
    
    ggplot(plot_data, aes(x = x, y = prob, fill = significance)) +
      geom_col() +
      scale_fill_manual(values = c("Less Extreme" = "#00BFC4", 
                                  "Observed or More Extreme" = "#FF8C00")) +
      scale_x_continuous(breaks = 0:n) +
      labs(title = paste0("Binomial Distribution (n=", n, ", p=0.5) Under H₀"),
           subtitle = paste0("p-value = P(X ≥ ", k, ") = ", format(p_value, digits=5)),
           x = "Number of Correct Guesses",
           y = "Probability",
           fill = "") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            legend.position = "bottom") +
      annotate("text", x = n*0.7, y = max(probs)*0.8, 
               label = paste0("p-value = ", format(p_value, digits=5)),
               color = "#FF8C00", size = 5, fontface = "bold")
  })
}

shinyApp(ui = ui, server = server)
