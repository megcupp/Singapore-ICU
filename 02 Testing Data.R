## Original code by Rockenschaub, Patrick with additions and modifications by Meg Cupp

library(VGAM)
library(tidyverse)
library(foreign)
library(readxl)

#packahes for zero-truncated poisson regression
require(foreign)
require(ggplot2)
require(VGAM)
require(boot)

tidyverse::tidyverse_update()

# Clean work space
remove(list = ls())

# Load the ICU dataset created in step 1
load("icu.Rdata")
load("secondary.Rdata")

# Read the table layouts
layouts <- map(2:5, ~ read_excel(file.path("desc_template.xlsx"), 
                                 sheet = ., range = "A5:A57", col_names = "Variable"))

#############################################################################################
#
#    Table 1: demography
#

#######################################################################
# Calculate the p-values for incidence rate
#######################################################################

# Sum up the cases per Year
cases <- icu %>% count(Year)

# Chi-squared for cases over time
chisq.test(cases$n)

# Read the denominator for the rate from the template (Source: singstat.gov.sg )
denom <- read_excel(file.path("desc_template.xlsx"), sheet = "Change in admissions", 
                    range = "C4:H4", col_names = as.character(2010:2015)) %>% 
  gather(key = Year, value = Adult.Pop) %>% 
  # Change denominator from "per 10,000" to "per 100,000"
  mutate(Adult.Pop = Adult.Pop / 10)

# Calculate the p-value via likelihood ratio
glm(n ~ as.numeric(Year), data = cases, family = poisson(link = "log"), offset = log(denom$Adult.Pop)) %>% 
  drop1(test = "LRT")


#######################################################################
# Calculate the p-values categorical variables (Chi-squared)
#######################################################################

# Run the omnibus tests
icu %>% 
  mutate(Residency.Bin = fct_recode(Residency, Other = "Permanent resident",
                                    Other = "Resident foreigner",
                                    Other = "Non resident")) %>% 
  select(Deceased.Ind, Age.Cat, Gender, Race, Residency.Bin, Income.Cat) %>% 
  map(chisq.test, y = icu$Year) %>% 
  map_dbl(1, "p.value" %>% round(digits = 5))

icu %>% filter(Year.As.Num > 1) %>% summarise(p.value = chisq.test(Year, CCI.Cat)$p.value)

# Run the one-against-all tests for variables with more than 2 levels
# Apply multi-testing corrections (http://www.biostathandbook.com/chiind.html)

list("Age.Cat", "Race", "Income.Cat") %>% 
  map(~expand.grid(., levels(icu[[.]]))) %>% 
  bind_rows() %>% 
  rename(factor = Var1, level = Var2) %>% 
  mutate(p.value = map2_dbl(factor, level, 
                            ~ chisq.test(icu[[.x]] == .y, icu$Year)$p.value) %>% 
           round(digits = 5)) %>% 
  group_by(factor) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH") %>% round(digits = 5))

#p-values for Income.Cat not printing

tibble(level = levels(icu$CCI.Cat)) %>% 
  mutate(p.value = map_dbl(level, ~ with(icu %>% filter(Year.As.Num > 1), 
                                         chisq.test(CCI.Cat == ., Year)$p.value))) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH") %>% round(digits = 5))



#######################################################################
# Calculate the p-values for normal variables (ANOVA)
#######################################################################

# APACHE score seems to be the only normally distributed variable. 
# Look only at values from 2010 onwards (before that they are not judged reliable)
icu.apache <- icu

# Look at the histogram with overlaid normal curve
icu.apache %>% 
  ggplot(aes(x = APACHE)) + 
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, color = "red", 
                args = list(mean = mean(icu$APACHE, na.rm = TRUE), sd = sd(icu$APACHE, na.rm = TRUE)))

# Q-Q-plot
qqnorm(icu.apache$APACHE)
qqline(icu.apache$APACHE)

# Test for equal variance to decide whether to use ANOVA or Welch correction
car::leveneTest(APACHE ~ Year, data = icu.apache)
icu.apache %>% 
  group_by(Year) %>%
  summarise(var = var(APACHE, na.rm = TRUE)) %>% 
  mutate(ratio.max = max(var) / var)

# Although variances are hardly different (ratio of min-max = 1.15), use Welch correction
# Overall
oneway.test(APACHE ~ Year, data = icu.apache, na.action = "na.omit", var.equal = FALSE)

icu$Year <- as.numeric(icu$Year)
##Set as.factor to correct error:
#Error in mutate_impl(.data, dots) : 
#Evaluation error: all group levels must be finite.

# Amongst age
icu %>%
  select(Age.Cat, Year, APACHE) %>% 
  remove_missing(na.rm = TRUE) %>% 
  nest(-Age.Cat) %>%
  mutate(kruskal = map_dbl(data, ~ kruskal.test(APACHE ~ Year, data = .)$p.value), 
         anova = map_dbl(data, ~ anova(aov(APACHE ~ Year, data = .))$`Pr(>F)`[1]), 
         welch = map_dbl(data, ~ oneway.test(APACHE ~ Year, data = ., na.action = na.omit, var.equal = FALSE)$p.value)) %>% 
  mutate_at(vars(kruskal:welch), funs(round(p.adjust(., method = "BH")))) %>% round()

rm(icu.apache)


#######################################################################
# Calculate the p-values count variables (LRT)
#######################################################################

# significance for change in hospital length of stay over 5 year period
# run a zero-truncated poisson for count data - ie. length of stay cannot be 0
LoS.trun <- vglm(LoS ~ Year, family = pospoisson(), data = icu)
summary(LoS.trun)
# estimation of p-value from Z value requires an assumption that data follows a normal dist...

icu %>% 
  ggplot(aes(x = LoS)) + 
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, color = "red", 
                args = list(mean = mean(icu$LoS, na.rm = TRUE), sd = sd(icu$LoS, na.rm = TRUE)))
# Q-Q-plot
qqnorm(icu$LoS)
qqline(icu$LoS)

# ... this is not the case

icu$ICU.LoS <- as.integer(icu$ICU.LoS)
#significance for change in ICU length of stay over 5 year period
#run a zero-truncated poisson for count data - ie. length of stay cannot be 0
icu.trun <- vglm(ICU.LoS ~ Year, family = pospoisson(), data = icu)
summary(icu.trun)
#estimation of p-value from Z value requires an assumption that data follows a normal dist...

# are these estimates reliable?

#############################################################################################
#
#    Table 2: diagnoses
#

# Calculate the p-values
# Chi-squared for comm against non-comm
chisq.test(icu$Non.Comm.Disease, icu$Year)

# Calculate Chi-squared tests for each disease against all others
ind_dis <- tibble(disease = levels(icu$Study.Cat)) %>% 
  mutate(p.value = map_dbl(disease, ~ chisq.test(icu$Study.Cat == ., icu$Year)$p.value),
         p.adj = round(p.adjust(p.value, method = "BH"), 5))



#############################################################################################
#
#    Table 3b: average costs
#

# Look at the histogram with overlaid normal curve
icu %>% 
  ggplot(aes(x = log(Gross.Amount))) + 
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, color = "red", 
                args = list(mean = mean(log(icu$Gross.Amount), na.rm = TRUE), sd = sd(log(icu$Gross.Amount), na.rm = TRUE)))

# Q-Q-plot
qqnorm(log(icu$Gross.Amount))
qqline(log(icu$Gross.Amount))

# Test for equal variance to decide whether to use ANOVA or Welch correction
car::leveneTest(log(Gross.Amount) ~ Year, data = icu)
icu %>% 
  group_by(Year) %>%
  summarise(var = var(log(Gross.Amount), na.rm = TRUE)) %>% 
  mutate(ratio.max = max(var) / var)

# Although variances are hardly different (ratio of min-max = 1.16), use Welch correction
# Overall
oneway.test(log(Gross.Amount) ~ Year, data = icu, na.action = "na.omit", var.equal = FALSE)

icu.gross <- icu %>%
  select(Age.Cat, Gender, Income.Cat, Non.Comm.Disease, Study.Cat, Year, Gross.Amount) %>% 
  gather(variable, value, -Year, -Gross.Amount)

# For each variable
icu.gross %>% 
  nest(-variable) %>% 
  mutate(welch = map_dbl(data, ~ oneway.test(log(Gross.Amount) ~ Year + value, data = .,
                                             na.action = na.omit, var.equal = FALSE)$p.value))

icu.gross$Year <- is.character(icu.gross$Year)
##Set year as factor to resolve:
#Error in UseMethod("tbl_vars") : 
#no applicable method for 'tbl_vars' applied to an object of class "character"

# For the individual variable levels
gross <- icu.gross %>% 
  filter(!is.na(value)) %>% 
  nest(-variable, -value) %>%
  mutate(welch = map_dbl(data, ~ oneway.test(log(Gross.Amount) ~ Year, data = ., 
                                             na.action = na.omit, var.equal = FALSE)$p.value)) %>% 
  group_by(variable) %>% 
  mutate(welch = round(p.adjust(welch, method = "BH"), 5)) 
