## Original code by Rockenschaub, Patrick with additions and modifications by Meg Cupp

library(tidyverse)
library(openxlsx)
library(forcats)
library(broom)
library(stringr)

# Clean work space
remove(list = ls())

# Load the ICU dataset created in step 1
load("icu.Rdata")
load("secondary.Rdata")

# Summarise any measure supplied by year
measure_by_year <- function(data, group, using, padding = "both"){
  # Calculate the total number of cases in each year for 
  # summary measures that need this value
  year_total <- icu %>% 
    group_by(Year) %>% 
    summarise(n_total = n()) %>% 
    select(Year, n_total)
  
  # Compute the count for each group for each year
  # Do so by first joining the total for the year to each row
  x <- data %>% 
    left_join(year_total, by = "Year")
  
  # If a group was supplied, group by it first
  if(is.character(group)){
    x <- x %>% group_by_(group)
  } 
  
  # In any case, group by year and calculate the desired summary
  # Make sure to bring it into long format and remove the grouping
  x <- x %>% 
    group_by_("Year", add = TRUE) %>% 
    summarise_(value = using) %>% 
    mutate(name = "-") %>% 
    spread_(key = "Year", value = "value", fill = "-", sep = "_") %>%
    ungroup() 
  
  # If a group was supplied, additionally rename the grouping variable
  # to the common name "name"
  if(is.character(group)){
    x <- x %>% 
      select(-name) %>% 
      rename_(name = group) %>% 
      mutate(name = as.character(name))
  } 
  
  # Add a header row and a blank row if requested
  blank_row <- tibble(name = "", Year_2010 = NA, 
                      Year_2011 = NA, Year_2012 = NA, Year_2013 = NA, 
                      Year_2014 = NA, Year_2015 = NA)
  
  if(padding %in% c("both", "before")){
    x <- bind_rows(mutate(blank_row, name = "_b"), x)
  }
  if(padding %in% c("both", "after")){
    x <- bind_rows(x, mutate(blank_row, name = "_a"))
  } 
  
  x
}


# Combine two measurements (usually a summary and a variability measure)
measure2 <- function(data, group, using, padding = "both"){
  if(length(using) != 2){
    stop("Must be used with exactly 2 measures. Make sure using is a list of length 2.", call. = FALSE)
  }
  
  x <- using %>%
    map(.f = ~ measure_by_year(using = ., data = data, group = group, padding = padding)) %>% 
    reduce(left_join, by = "name", suffix = c(".x", ".y"))
  
  x[, order(names(x))]
}


#############################################################################################
#
#    Table 1: demography
#

#######################################################################
# Define the measures used in this table
#######################################################################

# Define functions that create lists with formulas for centrality and spread
measure_n <- function(){
  list(
    N = ~ n(),
    percent = ~ round(n() / max(n_total) * 100, 1)
  )
}

measure_median <- function(variable){
  list(
    median = as.formula(paste0("~ median(", variable, ", na.rm = TRUE)")),
    iqr = as.formula(
      paste0("~ paste0(",
             "quantile(", variable, ", 0.25, na.rm = TRUE, type = 2)",
             ", \" - \", ",
             "quantile(", variable, ", 0.75, na.rm = TRUE, type = 2)",
             ")"))
  )
}

measure_mean <- function(variable){
  list(
    mean = as.formula(paste0("~ round(mean(", variable, ", na.rm = TRUE), 1)")), 
    sd = as.formula(paste0("~ round(sd(", variable, ", na.rm = TRUE), 1)"))
  )
}

#######################################################################
# Create the table by combining the individual groups
#######################################################################
table1 <- bind_rows(
  
  # Mortality
  measure2(icu, group = "Deceased.Ind", using = measure_n()) %>% 
    filter(name %in% c("Yes", "_a")), 
  
  # Age
  measure2(icu, group = "Age.Cat", using = measure_n()), 
  
  # Gender
  measure2(icu, group = "Gender", using = measure_n(), padding = "after") %>% 
    filter(name %in% c("Male", "_a")),
  
  # Race
  measure2(icu, group = "Race", using = measure_n()),
  
  # Citizens
  measure2(icu, group = "Residency", using = measure_n()) %>% 
    filter(name %in% c("Singaporean", "_a")),
  
  # Household Monthly Income
  measure2(icu, group = "Income.Cat", using = measure_n()),
  
  # CCI
  measure2(icu, group = "CCI.Cat", using = measure_n()),
  
  # APACHE
  measure2(icu, group = FALSE, using = measure_mean("APACHE"), padding = "none"),
  measure2(icu, group = FALSE, using = measure_median("APACHE"), padding = "after"),
  
  # Hospital LOS
  measure2(icu, group = FALSE, using = measure_mean("LoS"), padding = "none"),
  measure2(icu, group = FALSE, using = measure_median("LoS"), padding = "after"),
  
  # ICU LOS
  measure2(icu, group = FALSE, using = measure_mean("ICU.LoS"), padding = "none"),
  measure2(icu, group = FALSE, using = measure_median("ICU.LoS"), padding = "after")
) %>% 
  # Change "NA - NA"s to "-"
  mutate_all(.funs = function(x){ if_else(x == "NA  -  NA", "-", x)}) %>% 
  filter(!is.na(name))





#############################################################################################
#
#    Table 2: diagnoses
#

# Define the levels used in this table
NCD <- tibble(name = toupper(c("Acute myocardial infarction",
                               "Intracranial Haemorrhage",
                               "Neoplasia",
                               "Congestive Heart Failure",
                               "Traumatic head Injury",
                               "Stroke",
                               "Trauma (Non-Head)",
                               "Cardiac Arrythmias and Arrest",
                               "Diabetes Mellitus",
                               "Chronic Obstructive Pulmonary Disease",
                               "Asthma",
                               "Status Epilepticus",
                               "Renal Failure",
                               "Cirrhosis of liver",
                               "Aortic Dissection",
                               "Peripheral Vacular Disease")))

table2 <- bind_rows(
  # Non-communicable disease overall
  measure2(icu, group = "Non.Comm.Disease", using = measure_n()),
  
  # Communicable disease detail
  rbind(
    c("CD", rep(NA, 16)),
    measure2(icu, group = "Study.Cat", using = measure_n()) %>% 
      anti_join(NCD, by = "name") %>% 
      filter(!(name %in% c("_a", "_b", "CD"))) %>% 
      arrange(desc(as.integer(Year_2015.x))),
    c("", rep(NA, 16))
  ),
  
  # Non-communicable disease detail
  rbind(
    c("NCD", rep(NA, 16)),
    measure2(icu, group = "Study.Cat", using = measure_n()) %>% 
      semi_join(NCD) %>% 
      filter(!(name %in% c("_a", "_b", "NCD"))) %>% 
      arrange(desc(as.integer(Year_2015.x))),
    c("", rep(NA, 16))
  )
)



#############################################################################################
#
#    Table 3a: Gross bill
#

# Define the measures used in this table
total <- ~ round(sum(Gross.Amount, na.rm = TRUE) / 1000, 0)

# Simply sum up the Gross.Amount per year and category
table3a <- bind_rows(
  rbind(
    measure_by_year(icu, group = FALSE, using = total, padding = FALSE),
    c("", rep(NA, 8))
  ),
  measure_by_year(icu, group = "Age.Cat", using = total),
  measure_by_year(icu, group = "Gender", using = total),
  measure_by_year(icu, group = "Income.Cat", using = total),
  measure_by_year(icu, group = "Non.Comm.Disease", using = total),
  # Use more logic to get the study category into the right order
  rbind(
    # Add the header separately
    c("CD", rep(NA, 8)),
    # Then get the values for all study categories
    measure_by_year(icu, group = "Study.Cat", using = total) %>% 
      # remove the non-communicable diseases and the header, so only communicable disease are left
      anti_join(NCD, by = "name") %>%
      filter(!(name %in% c("_a", "_b",  "CD"))) %>% 
      # and order everything by the patient number in 2015
      arrange(desc(as.integer(Year_2015))),
    c("", rep(NA, 8))
  ),
  # Same for the non-communicable diseases
  rbind(
    c("NCD", rep(NA, 8)),
    measure_by_year(icu, group = "Study.Cat", using = total) %>% 
      semi_join(NCD, by = "name") %>%
      filter(!(name %in% c("_a", "_b", "NCD"))) %>% 
      arrange(desc(as.integer(Year_2015))),
    c("", rep(NA, 8))
  )
) %>% 
  # remove the missing category in household income
  filter(!is.na(name)) %>% 
  # Change 0s to "-"          
  mutate(increase = (as.numeric(Year_2015) / as.numeric(Year_2010)) ** (1/5) - 1) %>% 
  mutate_at(vars(Year_2010:Year_2015), .funs = function(x) paste0("$",round(as.numeric(x) / 10, digits = 0)/100)) %>% 
  mutate_at(vars(Year_2010:Year_2015), .funs = function(x){ if_else(x == "$NA", as.character(NA), x)})





#############################################################################################
#
#    Table 3b: Mean bill
#

# Define the measures used in this table (still old structure, see Table 1 for new structure)
med.bill <- ~ round(median(Gross.Amount, na.rm = TRUE) / 1000, 0)
iqr.bill <- ~ paste(round(quantile(Gross.Amount, 0.25, na.rm = TRUE) / 1000, 0), 
                    " - ", 
                    round(quantile(Gross.Amount, 0.75, na.rm = TRUE) / 1000,0))

table3b <- bind_rows(
  # Age
  inner_join(
    measure_by_year(icu, group = "Age.Cat", using = med.bill),
    measure_by_year(icu, group = "Age.Cat", using = iqr.bill),
    by = "name", suffix = c("_1median", "_2IQR")
  ), 
  
  # Gender
  inner_join(
    measure_by_year(icu, group = "Gender", using = med.bill),
    measure_by_year(icu, group = "Gender", using = iqr.bill),
    by = "name", suffix = c("_1median", "_2IQR")
  ), 
  
  # Income
  inner_join(
    measure_by_year(icu, group = "Income.Cat", using = med.bill),
    measure_by_year(icu, group = "Income.Cat", using = iqr.bill),
    by = "name", suffix = c("_1median", "_2IQR")
  ), 
  
  # Non-communicable disease
  inner_join(
    measure_by_year(icu, group = "Non.Comm.Disease", using = med.bill),
    measure_by_year(icu, group = "Non.Comm.Disease", using = iqr.bill),
    by = "name", suffix = c("_1median", "_2IQR")
  ), 
  
  # Communicable disease detail (see table 3a for sorting logic)
  rbind(
    c("CD", rep(NA, 16)),
    inner_join(
      measure_by_year(icu, group = "Study.Cat", using = med.bill), 
      measure_by_year(icu, group = "Study.Cat", using = iqr.bill),
      by = "name", suffix = c("_1median", "_2IQR") 
    ) %>% 
      anti_join(NCD, by = "name") %>% 
      filter(!(name %in% c("_a", "_b", "CD"))) %>% 
      arrange(desc(as.integer(Year_2015_1median))),
    c("", rep(NA, 16))
  ),
  
  # Non-communicable disease detail
  rbind(
    c("NCD", rep(NA, 16)),
    inner_join(
      measure_by_year(icu, group = "Study.Cat", using = med.bill), 
      measure_by_year(icu, group = "Study.Cat", using = iqr.bill),
      by = "name", suffix = c("_1median", "_2IQR") 
    ) %>% 
      semi_join(NCD) %>% 
      filter(!(name %in% c("_a", "_b", "NCD"))) %>% 
      arrange(desc(as.integer(Year_2015_1median))),
    c("", rep(NA, 16))
  )
) %>% 
  # remove the missing category in household income
  filter(!is.na(name))

table3b <- table3b[, order(names(table3b))]


#############################################################################################
#
#    Tables for Online Supplement
#

# Supplement Table 1

# Sum up each column except for the Year* columns
suppl_1 <- sec_diags_present %>% 
  select(-starts_with("Year")) %>% 
  names() %>% 
  set_names() %>% 
  map_df(., measure2, data = sec_diags_present, using = measure_n(), padding = "after", .id = "from") %>% 
  # Combine into output but only keep the "Yes" row for each condition
  bind_rows() %>% 
  filter(name == "TRUE") %>% 
  select(-name) %>% 
  arrange(desc(as.numeric(Year_2015.x)))


# Supplement Table 2

# Table of length of stay vs. income group
suppl_2 <- bind_rows(
  # Hospital LOS
  measure2(icu, group = "Income.Cat", using = measure_mean("LoS"), padding = "none") %>% 
    mutate(var = "Hospital", order = row_number()),
  measure2(icu, group = "Income.Cat", using = measure_median("LoS"), padding = "none") %>% 
    mutate(var = "Hospital", order = row_number()),
  
  # ICU LOS
  measure2(icu, group = "Income.Cat", using = measure_mean("ICU.LoS"), padding = "before") %>% 
    mutate(var = "ICU", order = row_number()),
  measure2(icu, group = "Income.Cat", using = measure_median("ICU.LoS"), padding = "before") %>% 
    mutate(var = "ICU", order = row_number())
) %>% 
  filter(!is.na(name)) %>% 
  arrange(var, order) %>% 
  select(-var, -order)



#############################################################################################
#
#    Write to Excel sheet
#

# Load the predefined template for the output
library(openxlsx)
path_template <- file.path("desc_template.xlsx")
wb <- loadWorkbook(path_template)

# Insert table 1
output <- table1[, -c(1)]
writeData(wb, output, sheet = "Table 1", startRow = 9, startCol = 2, colNames = FALSE)

# Insert table 2
output <- table2[c(1:8, 10:nrow(table2), 9), -c(1)]
writeData(wb, output, sheet = "Table 2", startRow = 7, startCol = 2, colNames = FALSE)

# Insert table 3a
output <- table3a[c(1:27, 29:nrow(table3a), 28), -c(1)]
writeData(wb, output, sheet = "Table 3a", startRow = 5, startCol = 2, colNames = FALSE)

# Insert table 3b
output <- table3b[c(1:29, 31:nrow(table3b), 30), -c(1)]
writeData(wb, output, sheet = "Table 3b", startRow = 8, startCol = 2, colNames = FALSE)

# Insert table Suppl 1
output <- suppl_1[, -c(1)]
writeData(wb, output, sheet = "Suppl 1", startRow = 5, startCol = 2, colNames = FALSE)

# Insert table Suppl 2
output <- suppl_2[, -c(1)]
writeData(wb, output, sheet = "Suppl 2", startRow = 5, startCol = 2, colNames = FALSE)

path_output <- file.path("descriptive.xlsx")
saveWorkbook(wb, file = path_output, overwrite = TRUE)
