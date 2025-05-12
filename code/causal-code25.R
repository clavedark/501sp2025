# Load required packages
library(haven)
library(stargazer)
library(AER)
library(plm)
library(dplyr)
rm(list = ls())
# Read the data
crime_data <- read_dta("~/downloads/zip/data/crime2.dta")

# Create logged variables and first differences
crime_data <- crime_data %>%
  arrange(city, year) %>%
  group_by(city) %>%
  mutate(
    # Create logged variables first
    lviolent = log(murder + rape + robbery + assault + 1),
    lsworn = log(sworn + 1),
    
    # Create first differences
    d_lviolent = lviolent - lag(lviolent),
    d_lsworn = lsworn - lag(lsworn),
    d_unemp = unemp - lag(unemp),
    d_sta_welf = sta_welf - lag(sta_welf),
    d_sta_educ = sta_educ - lag(sta_educ),
    d_citybla = citybla - lag(citybla),
    d_cityfemh = cityfemh - lag(cityfemh),
    
    # Create instrument interaction
    elecy = mayor * governor  # interaction of mayoral and gubernatorial elections
  ) %>%
  ungroup()

# Create a complete cases dataset for all variables we'll use
analysis_data <- crime_data %>%
  dplyr::select(d_lviolent, d_lsworn, d_unemp, d_sta_welf, d_sta_educ, 
         d_citybla, d_cityfemh, elecy) %>%
  na.omit()

# Model 1: OLS (ignoring endogeneity)
ols_model <- lm(d_lviolent ~ d_lsworn + d_unemp + d_sta_welf + d_sta_educ + 
                  d_citybla + d_cityfemh, 
                data = analysis_data)

# Model 2: Manual 2SLS
# First stage
first_stage <- lm(d_lsworn ~ elecy + d_unemp + d_sta_welf + d_sta_educ + 
                    d_citybla + d_cityfemh, 
                  data = analysis_data)

# Get predicted values - now the dimensions will match
analysis_data$d_lsworn_hat <- fitted(first_stage)

# Second stage
second_stage <- lm(d_lviolent ~ d_lsworn_hat + d_unemp + d_sta_welf + d_sta_educ + 
                     d_citybla + d_cityfemh, 
                   data = analysis_data)

# Model 3: Simultaneous 2SLS using ivreg
iv_model <- ivreg(d_lviolent ~ d_lsworn + d_unemp + d_sta_welf + d_sta_educ + 
                    d_citybla + d_cityfemh | 
                    elecy + d_unemp + d_sta_welf + d_sta_educ + 
                    d_citybla + d_cityfemh,
                  data = analysis_data)

# Display results in stargazer
stargazer(ols_model, second_stage, iv_model, first_stage,
          title = "The Effect of Police on Crime: 2SLS Results",
          dep.var.labels = c("Δ log(Violent Crime)", "Δ log(Sworn Officers)"),
          column.labels = c("OLS", "Manual 2SLS", "2SLS", "First Stage"),
          covariate.labels = c("Δ log(Sworn Officers)", 
                               "Δ log(Sworn Officers) [predicted]",
                               "Δ Unemployment Rate",
                               "Δ Welfare Spending", 
                               "Δ Education Spending",
                               "Δ % Black Population",
                               "Δ % Female-Headed HH",
                               "Electoral Cycle Instrument"),
          type = "text",
          digits = 3,
          no.space = TRUE)

# Additional diagnostic information
# First stage F-statistic for the instrument
first_stage_summary <- summary(first_stage)
cat("\nFirst stage F-statistic for instrument:\n")
# Extract F-stat for just the instrument
instrument_f <- summary(lm(d_lsworn ~ elecy, data = analysis_data))$fstatistic[1]
print(instrument_f)

# Wu-Hausman test for endogeneity
library(lmtest)
cat("\nWu-Hausman test:\n")
summary(iv_model, diagnostics = TRUE)







rm(list = ls())

# Models using change in log(sworn) as endogenous variable
# Create dataset for models with d_lsworn
analysis_data_dlsworn <- crime_data %>%
  dplyr::select(lviolent, d_lsworn, unemp, sta_welf, sta_educ, 
         citybla, cityfemh, elecy, city, year) %>%
  na.omit() 

#rescale welfare and education spending

analysis_data_dlsworn <- analysis_data_dlsworn %>%
  mutate(sta_welf = sta_welf / 1000,
         sta_educ = sta_educ / 1000)



# Model 1: OLS
ols_dlsworn <- lm(lviolent ~ d_lsworn + unemp + sta_welf + sta_educ + 
                    citybla + cityfemh, 
                  data = analysis_data_dlsworn)

# Model 2: Manual 2SLS
# First stage
first_stage_dlsworn <- lm(d_lsworn ~ elecy + unemp + sta_welf + sta_educ + 
                            citybla + cityfemh, 
                          data = analysis_data_dlsworn)

# Get predicted values
analysis_data_dlsworn$d_lsworn_hat <- fitted(first_stage_dlsworn)

# Second stage
second_stage_dlsworn <- lm(lviolent ~ d_lsworn_hat + unemp + sta_welf + sta_educ + 
                             citybla + cityfemh, 
                           data = analysis_data_dlsworn)

# Model 3: Simultaneous 2SLS
iv_dlsworn <- ivreg(lviolent ~ d_lsworn + unemp + sta_welf + sta_educ + 
                      citybla + cityfemh | 
                      elecy + unemp + sta_welf + sta_educ + 
                      citybla + cityfemh,
                    data = analysis_data_dlsworn)

# Display results
stargazer(ols_dlsworn, second_stage_dlsworn, iv_dlsworn, first_stage_dlsworn,
          title = "The Effect of Change in Police on Crime Levels: 2SLS Results",
          dep.var.labels = c("log(Violent Crime)", "Δ log(Sworn Officers)"),
          column.labels = c("OLS", "Manual 2SLS", "2SLS", "First Stage"),
          covariate.labels = c("Δ log(Sworn Officers)", 
                               "Δ log(Sworn Officers) [predicted]",
                               "Electoral Cycle Instrument",
                               "Unemployment Rate",
                               "Welfare Spending", 
                               "Education Spending",
                               "% Black Population",
                               "% Female-Headed HH"),
          type = "text",
          digits = 3,
          no.space = TRUE)






