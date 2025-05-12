
ticket<-speed
ticket$interaction <- ticket$Age*ticket$Black


m2 <- lm(Amount ~ MPHover+ Female + OutState + Age + Black + interaction, data = ticket)
summary(m2)



#filtering the data for onlythe cases used in the model
ticket$used <- TRUE
ticket$used [na.action(m2)] <- FALSE

ticketsample <- ticket|>
  filter(used=="TRUE")

#loop

pred_ticket <- ticketsample
Age <-0
medxbr0 <-0
ubxbr0 <-0
lbxbr0 <-0
medxbr1 <-0
ubxbr1 <-0
lbxbr1 <-0

pred_ticket$Black <- 0
for(p in seq(1,86,1)){
  pred_ticket$Age <- p+12
  pred_ticket$interaction <- 0
  Age[p] <- p +12
  allpreds <- data.frame(predict(m2, interval="confidence", se.fit=TRUE, newdata = pred_ticket))  
  medxbr0[p] <- median(allpreds$fit.fit, na.rm=TRUE)
  ubxbr0[p] <- median(allpreds$fit.fit, na.rm=TRUE)+1.96*(median(allpreds$se.fit, na.rm=TRUE))
  lbxbr0[p] <- median(allpreds$fit.fit, na.rm=TRUE)-1.96*(median(allpreds$se.fit, na.rm=TRUE))
}

pred_ticket$Black <- 1
for(p in seq(1,86,1)){
  pred_ticket$Age <- p+12
  pred_ticket$interaction <- p +12
  allpreds <- data.frame(predict(m2, interval="confidence", se.fit=TRUE, newdata = pred_ticket))  
  medxbr1[p] <- median(allpreds$fit.fit, na.rm=TRUE)
  ubxbr1[p] <- median(allpreds$fit.fit, na.rm=TRUE)+1.96*(median(allpreds$se.fit, na.rm=TRUE))
  lbxbr1[p] <- median(allpreds$fit.fit, na.rm=TRUE)-1.96*(median(allpreds$se.fit, na.rm=TRUE))
}


df <- data.frame(medxbr0, ubxbr0,lbxbr0,medxbr1, ubxbr1, lbxbr1, Age)

#plotting

ggplot() +
  geom_ribbon(data=df, aes(x=Age, ymin=lbxbr0, ymax=ubxbr0),fill = "grey70", alpha = .4) +
  geom_ribbon(data=df, aes(x=Age, ymin=lbxbr1, ymax=ubxbr1), fill= "grey60",  alpha = .4) +
  geom_line(data= df, aes(x=Age, y=medxbr0))+
  geom_line(data= df, aes(x=Age, y=medxbr1))+
  labs (x = "Age", y =  "Expected Amount of Tickets" ) 


#plot density mphover

ggplot(ticket, aes(x=MPHover)) +
  geom_density(aes(fill = factor(Black)), alpha = 0.5) +
  labs (x = "MPHover", y =  "Density" ) +
  scale_fill_manual(values = c("grey70", "grey60"), name = "Black") +
  theme(legend.position = "top")



















# Read data
states <- read_dta("/Users/dave/Documents/teaching/501/2023/slides/L8_panel/code/USstatedata.dta")
ms <- lm(murder ~ south + unemp + hsdip + citid + prcapinc, data=states)

# Set state and year as panel identifiers
states_panel <- pdata.frame(states, index = c("id", "year"))

# 1. Original OLS model for comparison
ms_ols <- lm(murder ~ south + unemp + hsdip + citid + prcapinc, data = states)
ols_summary <- summary(ms_ols)

# 2. Prais-Winsten correction for panel data with panel-specific AR(1) parameters
# Using the prais package with panel option
ms_pw <- prais_winsten(murder ~ south + unemp + hsdip + citid + prcapinc, 
                       data = states,
                       index = c("id", "year"),  # Specify panel structure
                       panelwise = TRUE,         # Panel-specific rho
                       rhoweight = "T")          # Weight rho by panel size
pw_summary <- summary(ms_pw)


ti <- data.frame(
  term = c("(Intercept)", "south", "unemp", "hsdip", "citid", "prcapinc"),
  estimate = coef(ms_pw), 
  std.error = coef(summary(ms_pw))[, "Std. Error"]
)

gl <- data.frame(
  stat1 =  c("R-squared", sprintf("%.4f", summary(ms_pw)$adj.r.squared)),
  stat2 = c("N", as.character(length(residuals(ms_pw)))),
  stat3 = c("rho (AR1)", sprintf("%.4f", ms_pw$rho[7]))
)

mod_pw <- list(
  tidy = ti,
  glance = gl)
class(mod_pw) <- "modelsummary_list"

modelsummary(mod_pw)

pw <- broom::tidy(mod_pw)















#| echo: true
#| code-fold: true
#| code-summary: "code"
# Load necessary libraries
library(haven)
library(plm)
library(prais)
library(modelsummary)
library(tidyverse)

# Read data
states <- read_dta("/Users/dave/Documents/teaching/501/2023/slides/L8_panel/code/USstatedata.dta")

# Set state and year as panel identifiers
states_panel <- pdata.frame(states, index = c("id", "year"))

# 1. Original OLS model for comparison
ms_ols <- lm(murder ~ south + unemp + hsdip + citid + prcapinc, data = states)

# 2. Prais-Winsten correction for panel data with panel-specific AR(1) parameters
ms_pw <- prais_winsten(murder ~ south + unemp + hsdip + citid + prcapinc, 
                       data = states,
                       index = c("id", "year"),
                       panelwise = TRUE,
                       rhoweight = "T")

# Alternative implementation using plm and ar1 correction
ms_plm <- plm(murder ~ south + unemp + hsdip + citid + prcapinc,
              data = states,
              model = "within",
              effect = "individual",
              index = c("id", "year"))
ms_plm_ar1 <- update(ms_plm, . ~ ., method = "ar1")

# Create tidy dataframe for Prais-Winsten
ti <- data.frame(
  term = c("(Intercept)", "south", "unemp", "hsdip", "citid", "prcapinc"),
  estimate = coef(ms_pw), 
  std.error = coef(summary(ms_pw))[, "Std. Error"],
  statistic = coef(ms_pw) / coef(summary(ms_pw))[, "Std. Error"],
  p.value = 2 * pnorm(abs(coef(ms_pw) / coef(summary(ms_pw))[, "Std. Error"]), lower.tail = FALSE)
)

# Create glance dataframe for Prais-Winsten
gl <- data.frame(
  r.squared = summary(ms_pw)$r.squared,
  adj.r.squared = summary(ms_pw)$adj.r.squared,
  nobs = length(residuals(ms_pw)),
  rho = ms_pw$rho[1]
)

# Create custom model object
mod_pw <- list(
  tidy = ti,
  glance = gl)
class(mod_pw) <- "modelsummary_list"

# Custom footnote with AR(1) coefficient for PLM
plm_rho <- NA
if ("phi" %in% names(ms_plm_ar1)) {
  plm_rho <- ms_plm_ar1$phi
} else if ("rho" %in% names(ms_plm_ar1)) {
  plm_rho <- ms_plm_ar1$rho
} else if (is.list(summary(ms_plm_ar1)) && "rho" %in% names(summary(ms_plm_ar1))) {
  plm_rho <- summary(ms_plm_ar1)$rho
}

# Create a data frame for the footnote
footnote <- paste("AR(1) coefficient for PLM model:", 
                  ifelse(is.na(plm_rho), "Not available", round(plm_rho, 4)))

# Create the GOF map
gof_map <- tribble(
  ~raw,              ~clean,              ~fmt,
  "nobs",            "N",                 0,
  "r.squared",       "R²",                3,
  "adj.r.squared",   "Adjusted R²",       3,
  "rho",             "AR(1) coefficient", 3
)

# Create the model summary with explicit column names
modelsummary(
  list("OLS" = ms_ols, "Prais-Winsten" = mod_pw, "PLM AR(1)" = ms_plm_ar1),
  title = "Comparison of Panel Data Models for Murder Rate",
  coef_map = c(
    "(Intercept)" = "Intercept",
    "south" = "South",
    "unemp" = "Unemployment",
    "hsdip" = "High School Diploma", 
    "citid" = "Citizen Ideology",
    "prcapinc" = "Per Capita Income"
  ),
  gof_map = gof_map,
  estimate = "{estimate} ({std.error})",
  statistic = NULL,
  fmt = "%.4f",
  notes = footnote
)