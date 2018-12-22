library(tidyverse)
library(readr)
library(readxl)
library(stringr)
library(lubridate)

#Get Working Directory - working directory set to Desktop/Singapore/Singapore Update
getwd()

#############################################################################################
#
#    1. Load the raw data
#

# Clean work space
remove(list = ls())

# Specify which columns of the source file to import and what datatypes we expect
incl_cols <- cols_only(
  `Patient No` =  col_character(),
  `Patient DOB` = col_character(),
  `Race` = col_factor(levels = c("Chinese", "Malay", "Indian", "Others", "Eurasian", "Sikh", "Caucasian")),
  `Gender` = col_factor(levels = c("Female", "Male")),
  `Deceased Ind` = col_factor(levels = c("N", "Y")),
  `Adm Class Group` = col_factor(levels = c("Non Sub", "Sub")),
  `Adm Class Category` = col_factor(levels = c("SINGAPOREAN", "PR", "RESIDENT_FOREIGNER", "NON_RESIDENT")),
  `Adm Class Group FPR Code` = col_factor(levels = c("Class C", "Class B2", "Class B1", "Class A"), ordered = TRUE),
  `Adm DateTime` = col_character(),
  `Adm Year` = col_integer(),
  `Dis/Visit Class Group` = col_factor(levels = c("Non Sub", "Sub")),
  `Dis/Visit Class Group FPR Code` = col_factor(levels = c("Class C", "Class B2", "Class B1", "Class A"), ordered = TRUE),
  `LOS (Days)` = col_double(),
  `ICU LOS (Days)` = col_double(),
  `Primary Diagnosis Code (ICD 10)` = col_character(),
  `Secondary Diagnosis Code (ICD 10)` = col_character(),
  `Study Category` = col_character(),
  `APACHE` = col_integer(),
  `Re-Adm Ind` = col_factor(levels = c("N", "Y")),
  `Planned Adm Ind` = col_factor(levels = c("N", "Y")),
  `Payable Amount` = col_double(),
  `Subsidy Amount` = col_double(),
  `Gross Amount` = col_double(),
  `ICU Payable Amount` = col_double(),
  `ICU Subsidy Amount` = col_double(),
  `ICU Gross Amount` = col_double(),
  `Hospital` = col_factor(levels = c("KTPH", "NUH", "TTSH")),
  `Payer Code` = col_character(),
  `Payer Type` = col_character()
)

# Load the data using the above definitions
icu.source <- read_delim("singaporeICU.txt", col_types = incl_cols, delim = "\t", trim_ws = TRUE)

     ## Missing column names filled in: 'X58' [58] 

# Rename the columns to R-usable names and drop admissions before 2010 
icu <- icu.source %>% 
  rename(Patient.No = `Patient No`,
         Birth.Date = `Patient DOB`,
         Deceased.Ind = `Deceased Ind`,
         Sub = `Adm Class Group`,
         Residency = `Adm Class Category`,
         Class = `Adm Class Group FPR Code`,
         Adm.Date = `Adm DateTime`,
         Year.As.Num = `Adm Year`,
         Dis.Sub = `Dis/Visit Class Group`,
         Dis.Class = `Dis/Visit Class Group FPR Code`,
         LoS = `LOS (Days)`,
         ICU.LoS = `ICU LOS (Days)`,
         Primary.Diag = `Primary Diagnosis Code (ICD 10)`,
         Secondary.Diag = `Secondary Diagnosis Code (ICD 10)`,
         Study.Cat = `Study Category`,
         Re.Adm.Ind = `Re-Adm Ind`,
         Planned.Adm.Ind = `Planned Adm Ind`,
         Payable.Amount = `Payable Amount`,
         Subsidy.Amount = `Subsidy Amount`,
         Gross.Amount = `Gross Amount`,
         ICU.Payable.Amount = `ICU Payable Amount`,
         ICU.Subsidy.Amount = `ICU Subsidy Amount`,
         ICU.Gross.Amount = `ICU Gross Amount`,
         Payer = `Payer Code`,
         Payer.Type = `Payer Type`) %>% 
  filter(Year.As.Num >= 2010)

# Check for patients with more than 1 entry
duplicated(icu$Patient.No)

anyDuplicated(icu$Patient.No)

        ## 164

# Summarise data
summary(icu)


#############################################################################################
#
#    2. Change data types and factor labels of existing variables
#

icu <- icu %>%
  mutate(
    # Convert date of birth to date format, insert 20th century (i.e. 19) before year  
    # to do so correctly
    Birth.Date = dmy(sub(pattern = "(/)([0-9]{2})$", replacement = "\\119\\2", Birth.Date)),
    
    # Do the same for admission datetime, remove the time part to do so (time is
    # not available for all hospitals, and not necessary here)
    Adm.Date = dmy(sub(pattern = " [0-9]{2}:[0-9]{2}$", "", Adm.Date)),
    
    # Factorise year
    Year = factor(Year.As.Num),
    
    # Upper case the diagnoses
    Primary.Diag = toupper(Primary.Diag),
    Secondary.Diag = toupper(Secondary.Diag),
    Study.Cat = factor(toupper(Study.Cat))
  )

# Re-label some of the factor variables for more informative labels
icu <- within(icu, {
  levels(Deceased.Ind) <- c("No", "Yes")
  levels(Re.Adm.Ind) <- c("No", "Yes")
  levels(Planned.Adm.Ind) <- c("No", "Yes")
  levels(Class) <- c("C", "B2", "B1", "A")
  levels(Dis.Class) <- c("C", "B2", "B1", "A")
  levels(Residency) <- c("Singaporean", "Permanent resident", "Resident foreigner", "Non resident")
  levels(Race) <- c("Chinese", "Malay", "Indian", rep("Others", 4))
})


#############################################################################################
#
#    3. Create new variables, clean values that should be missing and recode variables
#

# Alter existing variables
icu <- within(icu, {
  # Let Year.As.Num start at zero
  Year.As.Num <- as.integer(Year.As.Num - 2010)
  
  # Set billing data to missing where gross amount less than 1 dollar or where there 
  # are negative subsidies
  Subsidy.Amount[Subsidy.Amount < 0] <- 0
  Payable.Amount[Payable.Amount < 0] <- 0
  ICU.Subsidy.Amount[ICU.Subsidy.Amount < 0] <- 0
  ICU.Payable.Amount[ICU.Payable.Amount < 0] <- 0
  Gross.Amount[Gross.Amount < 1] <- NA
  ICU.Gross.Amount[ICU.Gross.Amount < 1] <- NA
  
  # Set APACHE score to missing where it is 0 (can't happen)
  APACHE[APACHE == 0] <- NA
}
)

# Define which diseases are non-communicable
non.comm.disease.list <- toupper(c("Acute myocardial infarction",
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
                                   "Peripheral Vacular Disease"))

# Create new variables
icu <- icu %>% 
  mutate(
    # New time variables derived from admission date
    Period = factor(as.numeric(Year.As.Num > 2), levels = c(0,1), 
                    labels = c("2010-12", "2013-15")),
    Day = factor(weekdays(Adm.Date)),
    
    # Calculate age from date of birth and admission date
    Age = as.numeric(Adm.Date - Birth.Date) / 365.25,
    Age.Cat = cut(Age, c(19, 40, 60, 80, Inf), right=FALSE, 
                  include.lowest=TRUE, labels = c("<40", "40-59", "60-79", "80+")),
    
    # Create a category for the percentage of subsidy received
    Subsidy.Cat = cut(round(Subsidy.Amount / Gross.Amount, 1), seq(from=0, to=1, by=0.1), 
                      include.lowest=TRUE, labels = c("0-10%", "11-20%", "21-30%", "31-40%", 
                                                      "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%")),
    
    # Create alternative classifications of disease
    # Is the disease communicable (based on above list)
    Non.Comm.Disease = factor(as.numeric(Study.Cat %in% non.comm.disease.list), 
                              levels = c(0,1), labels = c("No", "Yes")),
    # Which ICD-10 chapter is it in
    ICD.Chapter = factor(toupper(substr(Primary.Diag, 1, 1)))
  )

# Relevel factors
icu <- within(icu, {
  # Regroup the above create ICD-10 chapters to simplify classification
  levels(ICD.Chapter) = list(`Infectious and parasitic disease (A&B)` = c("A", "B"), 
                             `Cancer (C)` = "C", 
                             `Endocrine (E)` = "E", 
                             `Nervous system (G)` = "G",
                             `Circulatory system (I)` = "I", 
                             `Respiratory system (J)` = "J", 
                             `Digestive system (K)` = "K", 
                             `Musculoskeletal (M)` = "M",
                             `Genitourinary (N)` = "N", 
                             `Injury and other external causes (S&T)` = c("S", "T"), 
                             `Other (D,F,H,L,O,P,Q,R,Z)` = c("D", "F", "H", "L", "O", "P", "Q", "R", "Z"))
  
  # Regroup the study categories "PULMONARY EMBOLUS" and "MYOSITIS" into the group "OTHERS"
  # because of small numbers
  levels(Study.Cat) <- sub("(PULMONARY EMBOLUS)|(MYOSITIS)", "OTHERS", levels(Study.Cat))
})


#############################################################################################
#
#    4. Calculate the income bounds of each patient
#

# Create a dataset with the subsidy in percent for Singaporean citizens, based on income
subsidy.regulation <- data.frame(
  income = c(rep(3200, 70), seq(from = 3200, to = 5000, by = 150), 5100, 5200, rep(Inf, 116)),
  subsidy = seq(from = 150, to = -50, by = -1))     # See MT pamphlet for more information

# Function that returns the income class based on admission class, residency and percent 
# of subsidy 
est.income <- function(class, residency, subsidy){
  if(is.na(class) | is.na(subsidy)){      
    return(NA)            # Ignore cases with missing information
  }else if(class == "A" | class == "B1"){ 
    return(4)             # Classes A and B1 always assumed high income
  }else if(class == "B2"){  
    add <- 15             # Class B2 get 15% points less subsidy 
  }else{
    add <- 0              # Class C is the reference class
  }
  
  # Permanent residents get 10% less subsidy
  if(is.na(residency) || residency != "Singaporean"){
    add <- add + 10
  }
  
  # Get the upper income boundary according to the pamphlet (2008 prices)
  # Note: use a tolerance for rounding, i.e. round values with > .95 up to mitigate rounding errors
  income <- subsidy.regulation[subsidy.regulation$subsidy == 
                                 if_else((subsidy * 100) %% 1 > .95, ceiling(subsidy*100), floor(subsidy*100)) + add, 
                               "income"]
  
  # Return the according income level
  if(income <= 3200){
    return(1)
  }else if(income <= 4250){
    return(2)
  }else if(income <= 5200){
    return(3)
  }else{
    return(4)
  }
}

icu <- icu %>%
  mutate(
    # Estimate the income category for each patient (use US-Dollar as labels)
    Income.Cat = factor(
      mapply(FUN = est.income, Class, Residency, Subsidy.Amount / Gross.Amount), 
      levels = 1:4, 
      labels = c("<2328", "2329-3091", "3092-3782", ">3782")
    ),
    
    # For sensitivity analysis, create a copy where income is NA for patients that
    # switched wards during their stay (i.e. received different amounts of subsidy)
    Income.Noswitch = if_else(Class == Dis.Class, true = Income.Cat, false = factor(NA)),
    
    # Further, create a copy where only citizens and permanent residents are estimated
    Income.Citizen = if_else(Residency %in% c("Singaporean", "Permanent resident"), 
                             true = Income.Noswitch, false = factor(NA))
  )


#############################################################################################
#
#    5. Calculate the co-morbidity indices 
#

# Load the list of comorbidities taken from Quan et al.
comorbs <- read_delim("CCI_paper.txt", delim = "\t", trim_ws = TRUE, skip = 2)

# Calculate the Charlson comorbitiy index for each admission
calc.CCI <- function(diag){
  # Split the diagnosis string into individual diagnoses
  s <- str_trim(str_split(diag, pattern = ",", simplify = TRUE))
  
  # Match CCI conditions with the individual diagnoses
  ind <- charmatch(comorbs$Code, s)
  
  # Sum up the scores of all conditions selected 
  # (don't count multiple selection)
  if(any(!is.na(ind))){
    comorbs %>% 
      filter(!is.na(ind)) %>% 
      select(Condition, Score) %>% 
      distinct() %>% 
      select(Score) %>% 
      sum() %>% 
      as.numeric()
  } else {
    return(0)
  }
}

icu <- icu %>%
  mutate(
    # Apply to all secondary diagnoses not from hospital KTPH
    CCI = ifelse(Hospital != "KTPH", 
                 yes = mapply(calc.CCI, Secondary.Diag),
                 no = NA),
    
    # Also group as categorical variable
    CCI.Cat = cut(CCI, breaks = c(0, 1,  3, Inf), 
                  labels = c("0", "1-2", "2+"), 
                  right = FALSE)
  )


#############################################################################################
#
#    6. Adjust the billing data for inflation (if it was not already done)
#

# Load the inflation data based on World Bank figures
inflation <- read_delim("inflation.txt", delim = "\t", 
                        col_types = cols(Year = col_factor(NULL), Inflation = col_double()))

# Adjust all bills to 2015 using the inflation table and the conversion factor for US Dollars
icu <- icu %>%
  left_join(inflation, by = "Year") %>% 
  mutate_at(vars(ends_with(".Amount")), funs((. * Inflation / 1.3748))) %>%
  select(-Inflation)

#Warning message: Column `Year` joining factors with different levels, coercing to character vector 

#############################################################################################
#
#    7. Additionally load and clean the secondary diagnoses information (needs to go into a
#       separate table to not completely inflate the number of columns in the main dataset)
#

# Load the definitions of the secondary diagnoses to classify them in the population
sec_diags <- read_excel(file.path("secondary_diagnoses.xlsx"), sheet = 1, n_max = 21) %>% 
  # Bring them into long format to be able to further process them
  gather(key = seq, value = diag, -category, na.rm = TRUE) %>% 
  select(-seq) %>% 
  arrange(category) %>% 
  # Get rid of the annotationn used by the clinicians to be able to search for them
  mutate_all(
    funs(
      str_to_title(.) %>% 
        str_replace_all("\\*|\\.\\.\\.| ", "")
    )) %>% 
  group_by(category) %>% 
  # Create regex search strings (separated by pipes)
  summarise(pattern = str_c(diag, collapse = "|"))

# Search for each condition in the secondary diagnosis of each patient to determine whether
# whether it was present
#(exposition pipe operator)

sec_diags_present <- sec_diags %$% 
  set_names(as.list(pattern), nm = category) %>% 
  map(~ str_detect(icu$Secondary.Diag, .)) %>% 
  # Bring into tibble format to be able to use "measure2" in the next step
  bind_cols(icu[, c("Year", "Year.As.Num")]) %>% 
  # Because the data is unreliable, remove data from before 2012
  filter(Year.As.Num > 1) %>% 
  replace_na(., rep(FALSE, nrow(sec_diags)) %>% as.list() %>% set_names(nm = sec_diags[["category"]]))


#############################################################################################
#
#    8. Clean up the workspace/dataset and save information 
#

icu <- icu %>%
  select(Patient.No, Birth.Date, Age, Age.Cat, Gender, Race, Residency,
         Income.Cat, Income.Noswitch, Income.Citizen,
         Adm.Date, Year, Year.As.Num, Period, Day, Class, Sub, Re.Adm.Ind, Planned.Adm.Ind,
         Primary.Diag, Study.Cat, ICD.Chapter, Non.Comm.Disease, Secondary.Diag,  
         CCI, CCI.Cat, APACHE, LoS, ICU.LoS, Deceased.Ind,
         Gross.Amount, Subsidy.Amount, ICU.Gross.Amount, ICU.Subsidy.Amount
  )

save(icu, file="icu.Rdata")
save(sec_diags_present, file = "secondary.Rdata")


# Clean work space
remove(list = ls())
