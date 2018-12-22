## Original code by Rockenschaub, Patrick with additions and modifications by Meg Cupp

library(tidyverse)
library(openxlsx)
library(lmtest)
library(sandwich)

# Clean work space
remove(list = ls())

# Load the ICU dataset created in step 1
load("icu.Rdata")


#############################################################################################
#
#    Prepare the data for modelling
#

# Center the variable around 0 and scale the variance
normalise <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Create direct variables for the log-transformation of bill and length of stay
#APACHE not log-transformed
icu <- icu %>%
  mutate(
    log.bill = Gross.Amount %>% log() %>% normalise(),
    log.LoS = LoS %>% log() %>% normalise(), 
    log.ICU.LoS = ICU.LoS %>% log() %>% normalise(),
    APACHE = APACHE %>% normalise()
  )

# Examine log gross bill, log LoS and APACHE scores for approximate normality
icu %>%
  select(starts_with("log."), APACHE) %>% 
  gather(key = variable, value = value, log.bill, log.LoS, log.ICU.LoS, APACHE) %>% 
  group_by(variable) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) + 
  stat_function(fun = dnorm, colour = "red") + 
  facet_wrap(~ variable, scales = "free")

#############################################################################################
#
#    Build the individual models
#

# Create a subset with only data from 2012-2015 for all models with CCI in them
icu_12 <- icu %>% 
  filter(Year.As.Num > 1)

# Get the models without length of stay, CCI and APACHE II
base.mod <- lm(log.bill ~ Age.Cat + Gender + Race + Year.As.Num + Income.Cat + Non.Comm.Disease,
               data = icu)

base.mod_12 <- lm(log.bill ~ Age.Cat + Gender + Race + Year.As.Num + Income.Cat + Non.Comm.Disease,
                  data = icu_12)

# Get a model without CCI or APACHE II for the whole time period
los.mod <- lm(log.bill ~ Age.Cat + Gender + Race + Year.As.Num + Income.Cat + Non.Comm.Disease + log.LoS,
              data = icu)

los.mod_12 <- lm(log.bill ~ Age.Cat + Gender + Race + Year.As.Num + Income.Cat + Non.Comm.Disease + log.LoS,
                 data = icu_12)

# The next models are only valid for data from 2012
# Include CCI into the model
cci.mod <- lm(log.bill ~ Age.Cat + Gender + Race + Year.As.Num + Income.Cat + Non.Comm.Disease + 
                log.LoS + CCI.Cat,
              data = icu_12)

# Include both CCI and APACHE II into the model
apache.mod <- lm(log.bill ~ Age.Cat + Gender + Race + Year.As.Num + Income.Cat + Non.Comm.Disease + 
                   CCI.Cat + APACHE + log.LoS,
                 data = icu_12)


# Combine all models into a list for further analysis
models <- list(M1_base = base.mod, 
               M2_base_12 = base.mod_12,
               M3_los = los.mod, 
               M4_los_12 = los.mod_12, 
               M5_cci = cci.mod, 
               M6_apache = apache.mod)



#############################################################################################
#
#    Check the residuals
#

# Combine the residuals of the individual models
res <- lapply(models, 
              function(model) { 
                tibble( 
                  row = row.names(model$model),
                  x = residuals(model), 
                  x.norm = (x - mean(x)) / sd(x),
                  fit = fitted.values(model)
                )}) %>% 
  bind_rows(.id = "model")

# Normality
res %>% 
  ggplot(aes(x = x.norm)) + 
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, colour = "red") + 
  facet_wrap(~ model, scales = "free")

# Residuals vs. fitted.values
res %>% 
  ggplot(aes(x = fit, y = x.norm)) + 
  geom_point(alpha = 0.1) + 
  stat_smooth(formula = y ~ 1, method = "lm", colour = "red") + 
  scale_y_continuous(limits = c(-5.8, 5.8), breaks = ((-3:3) * 2)) +
  facet_wrap(~ model, scales = "free_x")


#############################################################################################
#
#    Transfer to Excel
#

# Get the template for the output
path_template <- file.path("model_template.xlsx")
wb <- loadWorkbook(path_template)
template <- readWorkbook(wb, sheet = "Log bill", rows = 4:38, cols = 1)

# Create the coefficient and 95%-confidence interval columns using a 
# a function and applying it to all models estimated above
get_beta_ci <- function(model){
  model %>% 
    # From the model summary select the coefficients as data frames
    coeftest(vcov. = vcovHC) %>% 
    unclass() %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "Variables") %>% 
    # and use the values to compute the estimate as well as lower
    # and upper confidence limits by exponentiation
    mutate(
      beta = round(exp(Estimate), 2),
      lower = round(exp(Estimate - 2 * `Std. Error`), 2),
      upper = round(exp(Estimate + 2 * `Std. Error`), 2),
      p = `Pr(>|t|)`, 
      p.adj = p.adjust(p, method = "BH"),
      sig = if_else(p.adj < 0.001, "***", 
                    if_else(p.adj < 0.01, "**", 
                            if_else(p.adj < 0.05, "*", ""))), 
      ci = paste0("[", lower, " - ", upper, "]", sig)
    ) %>% 
    select(Variables, beta, ci)
}

# Apply the above function to all models to get the coefficient
# and confidence intervals
output <- lapply(models, get_beta_ci) %>% 
  # Then join them pairwise together
  reduce(.f = right_join, by = "Variables") %>% 
  # and bring them into the template format
  right_join(template, by = "Variables") %>% 
  select(-Variables)

# Output the result to the Excel sheet
writeData(wb, output, sheet = "Log bill", startRow = 5, startCol = 3, colNames = FALSE)

# Additionally get the sample sizes and adjuste R-squared for each model 
output <- models %>% 
  lapply(summary) %>% 
  lapply(function(x) cbind(c(length(x$residuals), x$adj.r.squared), c(NA, NA))) %>% 
  # and bind them together
  reduce(function(x, y) cbind(x, y))

# Output the result to the excel sheet
writeData(wb, output, sheet = "Log bill", startRow = 41, startCol = 3, colNames = FALSE)

# Save the resulting workbook
path_output = file.path("models.xlsx")
saveWorkbook(wb, file = path_output, overwrite = TRUE)