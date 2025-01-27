'Analysis'

# Clean up 
rm(list = ls())

# Settings
set.seed(42)
options(scipen=2, digits=4)
options(max.print = 9999)

setwd("~/Documents/my documents/Agripath RA/Gender Study/Nepal 2014")

# Main data
load("2014_clean_data.RData")

# Libraries
libraries <- c("dplyr", "haven", "labelled", "readxl", "survey", "readr", "psych", 
               "tidyr", "broom", "tableone", "gtsummary", "gt", "stargazer", 
               "webshot2", "stringr", "purrr", "kableExtra", "forcats")
lapply(libraries, library, character.only=T)

### Summary Statistics -----
# Missing values
vars <- c('HH1', 'HH2', 'HH6', 'HH7', 'stratum', 'HC1A', 'HC11', 'HHSEX', 'helevel', 
          'hhweight', 'windex5r', 'PSU', 'UN13AA', 'UN13AB', 'UN13AC', 'UN13AD', 
          'UN13AE', 'UN13AF', 'UN13AG', 'WAGE', 'WM7', 'MSTATUS', 'welevel',
          'wmweight', 'CM5B', 'Ethnicity', 'SL1_group')
for (i in vars) {
  missing_count <- sum(is.na(merged_data_2014[[i]]))
  print(paste(i, "missings:", missing_count, sep = " "))
}

### Bivariate Regressions ----

### Live in different House 'UN13AA'----
#1. Make Mid Western Mountain the desired reference category
merged_data_2014$HH7 <- relevel(merged_data_2014$HH7, ref = "Mid Western Mountain")
# Verify the releveling
levels(merged_data_2014$HH7)
# Create the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, 
                         nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitA_2014 <- svyglm(UN13AA ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitA_2014)
tbl_regression(weighted_logitA_2014, exponentiate = TRUE)

#2. Make Hindu the desired reference category
merged_data_2014$HC1A <- relevel(merged_data_2014$HC1A, ref = "Hindu")
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, 
                         nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
weighted_logit2A_2014 <- svyglm(UN13AA ~ HC1A, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit2A_2014, exponentiate = TRUE)

#3.
weighted_logit3A_2014 <- svyglm(UN13AA ~ Ethnicity, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit3A_2014, exponentiate = TRUE)

#4.
weighted_logit4A_2014 <- svyglm(UN13AA ~ windex5r, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit4A_2014, exponentiate = TRUE)

#5.
weighted_logit5A_2014 <- svyglm(UN13AA ~ HHSEX, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit5A_2014, exponentiate = TRUE)

#6. Make 'Yes' the desired reference category
weighted_logit6A_2014 <- svyglm(UN13AA ~ HC11, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit6A_2014, exponentiate = TRUE)

#7. 
weighted_logit7A_2014 <- svyglm(UN13AA ~ helevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit7A_2014, exponentiate = TRUE)

#8. 
weighted_logit8A_2014 <- svyglm(UN13AA ~ welevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit8A_2014, exponentiate = TRUE)

#9. 
weighted_logit9A_2014 <- svyglm(UN13AA ~ WAGE, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit9A_2014, exponentiate = TRUE)

#10. 
weighted_logit10A_2014 <- svyglm(UN13AA ~ MSTATUS, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit10A_2014, exponentiate = TRUE)

#11. 
weighted_logit11A_2014 <- svyglm(UN13AA ~ SL1_group, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit11A_2014, exponentiate = TRUE)

# Putting all the regressions in one table
formulas <- list(
  UN13AA ~ HH7,                  
  UN13AA ~ HC1A,                 
  UN13AA ~ Ethnicity,            
  UN13AA ~ windex5r,             
  UN13AA ~ HHSEX,                
  UN13AA ~ HC11,                 
  UN13AA ~ helevel,              
  UN13AA ~ welevel,              
  UN13AA ~ WAGE,                 
  UN13AA ~ MSTATUS,              
  UN13AA ~ SL1_group)

labels <- list(
  HH7 = "Region",
  HC1A = "Religion",
  Ethnicity = "Ethnicity of Household Head",
  windex5r = "Rural Wealth Index",
  HHSEX = "Sex of Household Head",
  HC11 = "Owns Agricultural Land",
  helevel = "Education of Household Head",
  welevel = "Education of Women",
  WAGE = "Age of Women",
  MSTATUS = "Marital Status of Women",
  SL1_group = "Number of Children Aged 1-17")

tables <- lapply(seq_along(formulas), function(i) {
  formula <- formulas[[i]] 
  tbl_regression(
    svyglm(formula, design = design_2014, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table1A <- tbl_stack(tables)
stacked_table %>%
  as_gt() %>%
  gt::tab_header(title = "Living in a different place") %>%
  gtsave("bivariate_living_in_different_place.png")








### #Live in a different room of the same house ----



