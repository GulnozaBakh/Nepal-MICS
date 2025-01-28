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
               "webshot2", "stringr", "purrr", "kableExtra", "forcats", "DescTools")
lapply(libraries, library, character.only=T)

# Subset the data for only Terai regions
terai_2014 <- subset(merged_data_2014, grepl("Western Terai|Eastern Terai|Central Terai|Mid Western Terai|Far Western Terai", HH7))

### Summary Statistics -----
# Missing values
vars <- c('HH1', 'HH2', 'HH6', 'HH7', 'stratum', 'HC1A', 'HC11', 'HHSEX', 'helevel', 
          'hhweight', 'windex5r', 'PSU', 'UN13AA', 'UN13AB', 'UN13AC', 'UN13AD', 
          'UN13AE', 'UN13AF', 'UN13AG', 'WAGE', 'WM7', 'MSTATUS', 'welevel',
          'wmweight', 'CM5B', 'Ethnicity', 'SL1_group')
for (i in vars) {
  missing_count <- sum(is.na(terai_2014[[i]]))
  print(paste(i, "missings:", missing_count, sep = " "))
}

# Reorder the levels
terai_2014 <- within(terai_2014, {
  HH7 <- factor(HH7,
                levels = c("Far Western Terai", "Central Terai", "Eastern Terai", "Mid Western Terai", "Western Terai"))
  
  HC1A <- factor(HC1A,
                 levels = c("Hindu", "Buddhist", "Christian", "Muslim", "Other"))
})


# Create survey design object for Terai regions
design_terai_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = terai_2014)

### Bivariate Regressions ----
### UN13AA ----
#1.
terai_logitA_2014 <- svyglm(UN13AA ~ HH7, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logitA_2014, exponentiate = TRUE)

#2. 
terai_logit2A_2014 <- svyglm(UN13AA ~ HC1A, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit2A_2014, exponentiate = TRUE)

#3.
terai_logit3A_2014 <- svyglm(UN13AA ~ Ethnicity, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit3A_2014, exponentiate = TRUE)

#4. 
terai_logit4A_2014 <- svyglm(UN13AA ~ windex5r, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit4A_2014, exponentiate = TRUE)

#5. 
terai_logit5A_2014 <- svyglm(UN13AA ~ HHSEX, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit5A_2014, exponentiate = TRUE)

#6. 
terai_logit6A_2014 <- svyglm(UN13AA ~ HC11, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit6A_2014, exponentiate = TRUE)

#7 
terai_logit7A_2014 <- svyglm(UN13AA ~ helevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit7A_2014, exponentiate = TRUE)

#8 
terai_logit8A_2014 <- svyglm(UN13AA ~ welevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit8A_2014, exponentiate = TRUE)

#9 
terai_logit9A_2014 <- svyglm(UN13AA ~ WAGE, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit9A_2014, exponentiate = TRUE)

#10 
terai_logit10A_2014 <- svyglm(UN13AA ~ MSTATUS, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit10A_2014, exponentiate = TRUE)

#11 
terai_logit11A_2014 <- svyglm(UN13AA ~ SL1_group, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit11A_2014, exponentiate = TRUE)

# First regression table
formulas1 <- list(
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

tables1A <- lapply(seq_along(formulas1), function(i) {
  formula1 <- formulas1[[i]] 
  tbl_regression(
    svyglm(formula1, design = design_terai_2014, family = quasibinomial),
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
  gt::tab_header(title = "Living in a different place") %>%
  gtsave("bivariate_living_in_different_place_terai.png")

### UN13AB ----
#1.
terai_logitB_2014 <- svyglm(UN13AB ~ HH7, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logitB_2014, exponentiate = TRUE)

##2. 
terai_logit2B_2014 <- svyglm(UN13AB ~ HC1A, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit2B_2014, exponentiate = TRUE)

#3.
terai_logit3B_2014 <- svyglm(UN13AB ~ Ethnicity, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit3B_2014, exponentiate = TRUE)

#4. 
terai_logit4B_2014 <- svyglm(UN13AB ~ windex5r, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit4B_2014, exponentiate = TRUE)

#5. 
terai_logit5B_2014 <- svyglm(UN13AB ~ HHSEX, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit5B_2014, exponentiate = TRUE)

#6. 
terai_logit6B_2014 <- svyglm(UN13AB ~ HC11, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit6B_2014, exponentiate = TRUE)

#7 
terai_logit7B_2014 <- svyglm(UN13AB ~ helevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit7B_2014, exponentiate = TRUE)

#8 
terai_logit8B_2014 <- svyglm(UN13AB ~ welevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit8B_2014, exponentiate = TRUE)

#9 
terai_logit9B_2014 <- svyglm(UN13AB ~ WAGE, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit9B_2014, exponentiate = TRUE)

#10 
terai_logit10B_2014 <- svyglm(UN13AB ~ MSTATUS, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit10B_2014, exponentiate = TRUE)

#11 
terai_logit11B_2014 <- svyglm(UN13AB ~ SL1_group, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit11B_2014, exponentiate = TRUE)

# 2nd regression table
formulas2 <- list(
  UN13AB ~ HH7,                  
  UN13AB ~ HC1A,                 
  UN13AB ~ Ethnicity,            
  UN13AB ~ windex5r,             
  UN13AB ~ HHSEX,                
  UN13AB ~ HC11,                 
  UN13AB ~ helevel,              
  UN13AB ~ welevel,              
  UN13AB ~ WAGE,                 
  UN13AB ~ MSTATUS,              
  UN13AB ~ SL1_group)

tablesB <- lapply(seq_along(formulas2), function(i) {
  formula2 <- formulas2[[i]] 
  tbl_regression(
    svyglm(formula2, design = design_terai_2014, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_tableB <- tbl_stack(tablesB)
stacked_tableB %>%
  as_gt() %>%
  gt::tab_header(title = "Living in a different room") %>%
  gtsave("bivariate_living_in_different_room_terai.png")

### UN13AC ----






