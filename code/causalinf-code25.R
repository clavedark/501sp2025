# Load required packages
library(haven)
library(stargazer)
library(AER)
library(dplyr)

# Read the data
crime_data <- read_dta("~/downloads/zip/data/crime2.dta")

# Create variables for the level models
crime_data <- crime_data %>%
  mutate(
    # Create violent crime sum and logs
    violent = as.numeric(murder + rape + robbery + assault),
    lviolent = log(pmax(violent, 1)),  # Use pmax to avoid log(0)
    lsworn = log(pmax(sworn, 1)),      # Use pmax to avoid log(0)
    
    # Create the instrument
    elecy = as.numeric(mayor) * as.numeric(governor)
  )

# Create analysis dataset with only complete rows
analysis_data <- crime_data %>%
  dplyr::select(lviolent, lsworn, unemp, sta_welf, sta_educ, 
         citybla, cityfemh, elecy) %>%
  na.omit()

# Model 1: OLS (ignoring endogeneity)
ols_model <- lm(lviolent ~ lsworn + unemp + sta_welf + sta_educ + 
                  citybla + cityfemh, 
                data = analysis_data)

# Model 2: Manual 2SLS
# First stage
first_stage <- lm(lsworn ~ elecy + unemp + sta_welf + sta_educ + 
                    citybla + cityfemh, 
                  data = analysis_data)

# Get predicted values
analysis_data$lsworn_hat <- fitted(first_stage)

# Second stage
second_stage <- lm(lviolent ~ lsworn_hat + unemp + sta_welf + sta_educ + 
                     citybla + cityfemh, 
                   data = analysis_data)

# Model 3: Simultaneous 2SLS using ivreg
iv_model <- ivreg(lviolent ~ lsworn + unemp + sta_welf + sta_educ + 
                    citybla + cityfemh | 
                    elecy + unemp + sta_welf + sta_educ + 
                    citybla + cityfemh,
                  data = analysis_data)

# Display results in stargazer
stargazer(ols_model, second_stage, iv_model, first_stage,
          title = "The Effect of Police on Crime: 2SLS Results (Levels)",
          dep.var.labels = c("log(Violent Crime)", "log(Sworn Officers)"),
          column.labels = c("OLS", "Manual 2SLS", "2SLS", "First Stage"),
          covariate.labels = c("log(Sworn Officers)", 
                               "log(Sworn Officers) [predicted]",
                               "Unemployment Rate",
                               "Welfare Spending", 
                               "Education Spending",
                               "% Black Population",
                               "% Female-Headed HH",
                               "Electoral Cycle Instrument"),
          type = "text",
          digits = 3,
          no.space = TRUE)

# Print additional diagnostics
cat("\nFirst Stage F-statistic for Instrument:\n")
fs_summary <- summary(lm(lsworn ~ elecy, data = analysis_data))
print(fs_summary$fstatistic[1])

cat("\nWu-Hausman test for endogeneity:\n")
print(summary(iv_model, diagnostics = TRUE)$diagnostics[2,])