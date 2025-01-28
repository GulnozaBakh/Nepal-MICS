'Analysis'

# Clean up 
rm(list = ls())

# Settings
set.seed(42)
options(scipen=2, digits=4)
options(max.print = 9999)

setwd("~/Documents/my documents/Agripath RA/Gender Study/Nepal 2019")

# Main data
load("2019_clean_data.RData")

# Libraries
libraries <- c("dplyr", "haven", "labelled", "readxl", "survey", "readr", "psych", 
               "tidyr", "broom", "tableone", "gtsummary", "gt", "stargazer", 
               "webshot2", "stringr", "purrr", "kableExtra", "forcats", "DescTools")
lapply(libraries, library, character.only=T)

### Summary Statistics -----
# Missing values
vars <- c('HH1', 'HH2', 'HH6', 'HH7', 'stratum', 'HC1A', 'HC15', 'HHSEX', 'helevel1', 
          'hhweight', 'windex5r', 'PSU', 'UN16AA', 'UN16AB', 'UN16AC', 'UN16AD', 
          'UN16AE', 'UN16AF', 'UN16AG', 'UN16AH', 'WAGE', 'WM7', 'MSTATUS', 'welevel1',
          'wmweight', 'CM4', 'Ethnicity', 'HH51_grouped', 'HH52_grouped', 'HHAGEx')
for (i in vars) {
  missing_count <- sum(is.na(merged_data_2019[[i]]))
  print(paste(i, "missings:", missing_count, sep = " "))
}

### Bivariate Regressions ----

### Staying in chaupadi/chhapro 'UN16AA' ----
#1. 
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, 
                         survey.lonely.psu = "adjust", data = merged_data_2019)
weighted_logitA_2019 <- svyglm(UN16AA ~ HH7, design = design_2019, family = quasibinomial)
summary(weighted_logitA_2019)
tbl_regression(weighted_logitA_2019, exponentiate = TRUE)

#2. 
weighted_logit2A_2019 <- svyglm(UN16AA ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2A_2019, exponentiate = TRUE)

#3. 
weighted_logit3A_2019 <- svyglm(UN16AA ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3A_2019, exponentiate = TRUE)

#4.
weighted_logit4A_2019 <- svyglm(UN16AA ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4A_2019, exponentiate = TRUE)

#5. 
weighted_logit5A_2019 <- svyglm(UN16AA ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5A_2019, exponentiate = TRUE)

#6. 
weighted_logit6A_2019 <- svyglm(UN16AA ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6A_2019, exponentiate = TRUE)

#7. 
weighted_logit7A_2019 <- svyglm(UN16AA ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7A_2019, exponentiate = TRUE)

#8. 
weighted_logit8A_2019 <- svyglm(UN16AA ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8A_2019, exponentiate = TRUE)

#9. 
weighted_logit9A_2019 <- svyglm(UN16AA ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9A_2019, exponentiate = TRUE)

#10. 
weighted_logit10A_2019 <- svyglm(UN16AA ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10A_2019, exponentiate = TRUE)

#11. 
weighted_logit11A_2019 <- svyglm(UN16AA ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11A_2019, exponentiate = TRUE)

#12. 
weighted_logit12A_2019 <- svyglm(UN16AA ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12A_2019, exponentiate = TRUE)

#13. 
weighted_logit13A_2019 <- svyglm(UN16AA ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13A_2019, exponentiate = TRUE)

# First regression table
formulas1 <- list(
  UN16AA ~ HH7,                  
  UN16AA ~ HC1A,                 
  UN16AA ~ Ethnicity,            
  UN16AA ~ windex5r,             
  UN16AA ~ HHSEX,                
  UN16AA ~ HC15,                 
  UN16AA ~ helevel1,              
  UN16AA ~ welevel1,              
  UN16AA ~ WAGE,   
  UN16AA ~ HHAGEx,
  UN16AA ~ MSTATUS, 
  UN16AA ~ HH51_grouped,
  UN16AA ~ HH52_grouped)

labels <- list(
  HH7 = "Region",
  HC1A = "Religion",
  Ethnicity = "Ethnicity of Household Head", 
  HC15 = "Owns Agricultural Land", 
  HHSEX = "Sex of Household Head", 
  helevel1 = "Education of Household Head",
  windex5r = "Rural Wealth Index",
  WAGE = "Age Range of Women",
  HHAGEx = "Age Range of Household Head",
  welevel1 = "Education of Women",
  MSTATUS = "Marital Status",
  HH51_grouped = "Number of children under age 5",
  HH52_grouped = "Number of children aged 5-17")

tables1A <- lapply(seq_along(formulas1), function(i) {
  formula1 <- formulas1[[i]] 
  tbl_regression(
    svyglm(formula1, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table1A <- tbl_stack(tables1A)
stacked_table1A %>%
  as_gt() %>%
  gt::tab_header(title = "Living in chaupadi") %>%
  gtsave("bivariate_living_in_chaupadi.png")

### Staying in a separate room 'UN16AB' ----
#1. 
weighted_logitB_2019 <- svyglm(UN16AB ~ HH7, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logitB_2019, exponentiate = TRUE)

#2. 
weighted_logit2B_2019 <- svyglm(UN16AB ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2B_2019, exponentiate = TRUE)

#3. 
weighted_logit3B_2019 <- svyglm(UN16AB ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3B_2019, exponentiate = TRUE)

#4. 
weighted_logit4B_2019 <- svyglm(UN16AB ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4B_2019, exponentiate = TRUE)

#5. 
weighted_logit5B_2019 <- svyglm(UN16AB ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5B_2019, exponentiate = TRUE)

#6. 
weighted_logit6B_2019 <- svyglm(UN16AB ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6B_2019, exponentiate = TRUE)

#7. 
weighted_logit7B_2019 <- svyglm(UN16AB ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7B_2019, exponentiate = TRUE)

#8. 
weighted_logit8B_2019 <- svyglm(UN16AB ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8B_2019, exponentiate = TRUE)

#9. 
weighted_logit9B_2019 <- svyglm(UN16AB ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9B_2019, exponentiate = TRUE)

#10.
weighted_logit10B_2019 <- svyglm(UN16AB ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10B_2019, exponentiate = TRUE)

#11. 
weighted_logit11B_2019 <- svyglm(UN16AB ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11B_2019, exponentiate = TRUE)

#12. 
weighted_logit12B_2019 <- svyglm(UN16AB ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12B_2019, exponentiate = TRUE)

#13. 
weighted_logit13B_2019 <- svyglm(UN16AB ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13B_2019, exponentiate = TRUE)

# 2nd regression table
formulas2 <- list(
  UN16AB ~ HH7,                  
  UN16AB ~ HC1A,                 
  UN16AB ~ Ethnicity,            
  UN16AB ~ windex5r,             
  UN16AB ~ HHSEX,                
  UN16AB ~ HC15,                 
  UN16AB ~ helevel1,              
  UN16AB ~ welevel1,              
  UN16AB ~ WAGE,   
  UN16AB ~ HHAGEx,
  UN16AB ~ MSTATUS, 
  UN16AB ~ HH51_grouped,
  UN16AB ~ HH52_grouped)

tables2 <- lapply(seq_along(formulas2), function(i) {
  formula2 <- formulas2[[i]] 
  tbl_regression(
    svyglm(formula2, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table2 <- tbl_stack(tables2)
stacked_table2 %>%
  as_gt() %>%
  gt::tab_header(title = "Living in separate room") %>%
  gtsave("bivariate_living_in_separate_room.png")

### Staying in the cowshed 'UN16AC' ----


