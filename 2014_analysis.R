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
    svyglm(formula1, design = design_2014, family = quasibinomial),
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
  gtsave("bivariate_living_in_different_place.png")


### #Live in a different room of the same house 'UN13AB'----
#1.
weighted_logitB_2014 <- svyglm(UN13AB ~ HH7, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logitB_2014, exponentiate = TRUE)

#2. 
weighted_logit2B_2014 <- svyglm(UN13AB ~ HC1A, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit2B_2014, exponentiate = TRUE)

#3.
weighted_logit3B_2014 <- svyglm(UN13AB ~ Ethnicity, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit3B_2014, exponentiate = TRUE)

#4. 
weighted_logit4B_2014 <- svyglm(UN13AB ~ windex5r, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit4B_2014, exponentiate = TRUE)

#5. 
weighted_logit5B_2014 <- svyglm(UN13AB ~ HHSEX, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit5B_2014, exponentiate = TRUE)

#6. 
weighted_logit6B_2014 <- svyglm(UN13AB ~ HC11, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit6B_2014, exponentiate = TRUE)

#7. 
weighted_logit7B_2014 <- svyglm(UN13AB ~ helevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit7B_2014, exponentiate = TRUE)

#8. 
weighted_logit8B_2014 <- svyglm(UN13AB ~ welevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit8B_2014, exponentiate = TRUE)

#9. 
weighted_logit9B_2014 <- svyglm(UN13AB ~ WAGE, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit9B_2014, exponentiate = TRUE)

#10. 
weighted_logit10B_2014 <- svyglm(UN13AB ~ MSTATUS, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit10B_2014, exponentiate = TRUE)

#11. 
weighted_logit11B_2014 <- svyglm(UN13AB ~ SL1_group, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit11B_2014, exponentiate = TRUE)

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
    svyglm(formula2, design = design_2014, family = quasibinomial),
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
  gtsave("bivariate_living_in_different_room.png")

### Live in animal shed 'UN13AC' ----
#1. 
weighted_logitC_2014 <- svyglm(UN13AC ~ HH7, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logitC_2014, exponentiate = TRUE)

#2. 
weighted_logit2C_2014 <- svyglm(UN13AC ~ HC1A, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit2C_2014, exponentiate = TRUE)

#3. 
weighted_logit3C_2014 <- svyglm(UN13AC ~ Ethnicity, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit3C_2014, exponentiate = TRUE)

#4. 
weighted_logit4C_2014 <- svyglm(UN13AC ~ windex5r, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit4C_2014, exponentiate = TRUE)

#5. 
weighted_logit5C_2014 <- svyglm(UN13AC ~ HHSEX, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit5C_2014, exponentiate = TRUE)

#6. 
weighted_logit6C_2014 <- svyglm(UN13AC ~ HC11, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit6C_2014, exponentiate = TRUE)

#7. 
weighted_logit7C_2014 <- svyglm(UN13AC ~ helevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit7C_2014, exponentiate = TRUE)

#8. 
weighted_logit8C_2014 <- svyglm(UN13AC ~ welevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit8C_2014, exponentiate = TRUE)

#9. 
weighted_logit9C_2014 <- svyglm(UN13AC ~ WAGE, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit9C_2014, exponentiate = TRUE)

#10. 
weighted_logit10C_2014 <- svyglm(UN13AC ~ MSTATUS, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit10C_2014, exponentiate = TRUE)

#11. 
weighted_logit11C_2014 <- svyglm(UN13AC ~ SL1_group, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit11C_2014, exponentiate = TRUE)

# 3rd regression table
formulas3 <- list(
  UN13AC ~ HH7,                  
  UN13AC ~ HC1A,                 
  UN13AC ~ Ethnicity,            
  UN13AC ~ windex5r,             
  UN13AC ~ HHSEX,                
  UN13AC ~ HC11,                 
  UN13AC ~ helevel,              
  UN13AC ~ welevel,              
  UN13AC ~ WAGE,                 
  UN13AC ~ MSTATUS,              
  UN13AC ~ SL1_group)

tablesC <- lapply(seq_along(formulas3), function(i) {
  formula3 <- formulas3[[i]] 
  tbl_regression(
    svyglm(formula3, design = design_2014, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_tableC <- tbl_stack(tablesC)
stacked_tableC %>%
  as_gt() %>%
  gt::tab_header(title = "Living in animal shed") %>%
  gtsave("bivariate_living_in_animal_shed.png")

### Eating different type of food 'UN13AD' ----

#1.
weighted_logitD_2014 <- svyglm(UN13AD ~ HH7, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logitD_2014, exponentiate = TRUE)

#2. 
weighted_logit2D_2014 <- svyglm(UN13AD ~ HC1A, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit2D_2014, exponentiate = TRUE)

#3. 
weighted_logit3D_2014 <- svyglm(UN13AD ~ Ethnicity, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit3D_2014, exponentiate = TRUE)

#4. 
weighted_logit4D_2014 <- svyglm(UN13AD ~ windex5r, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit4D_2014, exponentiate = TRUE)

#5. 
weighted_logit5D_2014 <- svyglm(UN13AD ~ HHSEX, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit5D_2014, exponentiate = TRUE)

#6. 
weighted_logit6D_2014 <- svyglm(UN13AD ~ HC11, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit6D_2014, exponentiate = TRUE)

#7. 
weighted_logit7D_2014 <- svyglm(UN13AD ~ helevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit7D_2014, exponentiate = TRUE)

#8. 
weighted_logit8D_2014 <- svyglm(UN13AD ~ welevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit8D_2014, exponentiate = TRUE)

#9. 
weighted_logit9D_2014 <- svyglm(UN13AD ~ WAGE, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit9D_2014, exponentiate = TRUE)

#10. 
weighted_logit10D_2014 <- svyglm(UN13AD ~ MSTATUS, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit10D_2014, exponentiate = TRUE)

#11. 
weighted_logit11D_2014 <- svyglm(UN13AD ~ SL1_group, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit11D_2014, exponentiate = TRUE)

# 4th regression table
formulas4 <- list(
  UN13AD ~ HH7,                  
  UN13AD ~ HC1A,                 
  UN13AD ~ Ethnicity,            
  UN13AD ~ windex5r,             
  UN13AD ~ HHSEX,                
  UN13AD ~ HC11,                 
  UN13AD ~ helevel,              
  UN13AD ~ welevel,              
  UN13AD ~ WAGE,                 
  UN13AD ~ MSTATUS,              
  UN13AD ~ SL1_group)

tablesD <- lapply(seq_along(formulas4), function(i) {
  formula4 <- formulas4[[i]] 
  tbl_regression(
    svyglm(formula4, design = design_2014, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_tableD <- tbl_stack(tablesD)
stacked_tableD %>%
  as_gt() %>%
  gt::tab_header(title = "Eating Different type of food") %>%
  gtsave("bivariate_eating_different_food.png")

### Bathing in a separate place 'UN13AE' ----
#1. 
weighted_logitE_2014 <- svyglm(UN13AE ~ HH7, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logitE_2014, exponentiate = TRUE)

#2. 
weighted_logit2E_2014 <- svyglm(UN13AE ~ HC1A, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit2E_2014, exponentiate = TRUE)

#3. 
weighted_logit3E_2014 <- svyglm(UN13AE ~ Ethnicity, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit3E_2014, exponentiate = TRUE)

#4. 
weighted_logit4E_2014 <- svyglm(UN13AE ~ windex5r, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit4E_2014, exponentiate = TRUE)

#5. 
weighted_logit5E_2014 <- svyglm(UN13AE ~ HHSEX, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit5E_2014, exponentiate = TRUE)

#6. 
weighted_logit6E_2014 <- svyglm(UN13AE ~ HC11, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit6E_2014, exponentiate = TRUE)

#7. 
weighted_logit7E_2014 <- svyglm(UN13AE ~ helevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit7E_2014, exponentiate = TRUE)

#8. 
weighted_logit8E_2014 <- svyglm(UN13AE ~ welevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit8E_2014, exponentiate = TRUE)

#9. 
weighted_logit9E_2014 <- svyglm(UN13AE ~ WAGE, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit9E_2014, exponentiate = TRUE)

#10. 
weighted_logit10E_2014 <- svyglm(UN13AE ~ MSTATUS, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit10E_2014, exponentiate = TRUE)

#11. 
weighted_logit11E_2014 <- svyglm(UN13AE ~ SL1_group, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit11E_2014, exponentiate = TRUE)

# 5th regression table
formulas5 <- list(
  UN13AE ~ HH7,                  
  UN13AE ~ HC1A,                 
  UN13AE ~ Ethnicity,            
  UN13AE ~ windex5r,             
  UN13AE ~ HHSEX,                
  UN13AE ~ HC11,                 
  UN13AE ~ helevel,              
  UN13AE ~ welevel,              
  UN13AE ~ WAGE,                 
  UN13AE ~ MSTATUS,              
  UN13AE ~ SL1_group)

tablesE <- lapply(seq_along(formulas5), function(i) {
  formula5 <- formulas5[[i]] 
  tbl_regression(
    svyglm(formula5, design = design_2014, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_tableE <- tbl_stack(tablesE)
stacked_tableE %>%
  as_gt() %>%
  gt::tab_header(title = "Bathing in a separate place") %>%
  gtsave("bivariate_bathing_in_separate_place.png")

### Absent from school/work 'UN13AF' ----
#1. 
weighted_logitF_2014 <- svyglm(UN13AF ~ HH7, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logitF_2014, exponentiate = TRUE)

#2. 
weighted_logit2F_2014 <- svyglm(UN13AF ~ HC1A, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit2F_2014, exponentiate = TRUE)

#3. 
weighted_logit3F_2014 <- svyglm(UN13AF ~ Ethnicity, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit3F_2014, exponentiate = TRUE)

#4. 
weighted_logit4F_2014 <- svyglm(UN13AF ~ windex5r, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit4F_2014, exponentiate = TRUE)

#5. 
weighted_logit5F_2014 <- svyglm(UN13AF ~ HHSEX, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit5F_2014, exponentiate = TRUE)

#6. 
weighted_logit6F_2014 <- svyglm(UN13AF ~ HC11, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit6F_2014, exponentiate = TRUE)

#7. 
weighted_logit7F_2014 <- svyglm(UN13AF ~ helevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit7F_2014, exponentiate = TRUE)

#8. 
weighted_logit8F_2014 <- svyglm(UN13AF ~ welevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit8F_2014, exponentiate = TRUE)

#9. 
weighted_logit9F_2014 <- svyglm(UN13AF ~ WAGE, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit9F_2014, exponentiate = TRUE)

#10. 
weighted_logit10F_2014 <- svyglm(UN13AF ~ MSTATUS, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit10F_2014, exponentiate = TRUE)

#11. 
weighted_logit11F_2014 <- svyglm(UN13AF ~ SL1_group, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit11F_2014, exponentiate = TRUE)

# 6th regression table
formulas6 <- list(
  UN13AF ~ HH7,                  
  UN13AF ~ HC1A,                 
  UN13AF ~ Ethnicity,            
  UN13AF ~ windex5r,             
  UN13AF ~ HHSEX,                
  UN13AF ~ HC11,                 
  UN13AF ~ helevel,              
  UN13AF ~ welevel,              
  UN13AF ~ WAGE,                 
  UN13AF ~ MSTATUS,              
  UN13AF ~ SL1_group)

tablesF <- lapply(seq_along(formulas6), function(i) {
  formula6 <- formulas6[[i]] 
  tbl_regression(
    svyglm(formula6, design = design_2014, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_tableF <- tbl_stack(tablesF)
stacked_tableF %>%
  as_gt() %>%
  gt::tab_header(title = "Absent from School/Work") %>%
  gtsave("bivariate_absent_from_school_work.png")

### Avoid social gatherings 'UN13AG' ----
#1. 
weighted_logitG_2014 <- svyglm(UN13AG ~ HH7, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logitG_2014, exponentiate = TRUE)

#2. 
weighted_logit2G_2014 <- svyglm(UN13AG ~ HC1A, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit2G_2014, exponentiate = TRUE)

#3. 
weighted_logit3G_2014 <- svyglm(UN13AG ~ Ethnicity, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit3G_2014, exponentiate = TRUE)

#4. 
weighted_logit4G_2014 <- svyglm(UN13AG ~ windex5r, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit4G_2014, exponentiate = TRUE)

#5. 
weighted_logit5G_2014 <- svyglm(UN13AG ~ HHSEX, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit5G_2014, exponentiate = TRUE)

#6. 
weighted_logit6G_2014 <- svyglm(UN13AG ~ HC11, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit6G_2014, exponentiate = TRUE)

#7. 
weighted_logit7G_2014 <- svyglm(UN13AG ~ helevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit7G_2014, exponentiate = TRUE)

#8. 
weighted_logit8G_2014 <- svyglm(UN13AG ~ welevel, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit8G_2014, exponentiate = TRUE)

#9. 
weighted_logit9G_2014 <- svyglm(UN13AG ~ WAGE, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit9G_2014, exponentiate = TRUE)

#10. 
weighted_logit10G_2014 <- svyglm(UN13AG ~ MSTATUS, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit10G_2014, exponentiate = TRUE)

#11. 
weighted_logit11G_2014 <- svyglm(UN13AG ~ SL1_group, design = design_2014, family = quasibinomial)
tbl_regression(weighted_logit11G_2014, exponentiate = TRUE)

# 7th regression table
formulas7 <- list(
  UN13AG ~ HH7,                  
  UN13AG ~ HC1A,                 
  UN13AG ~ Ethnicity,            
  UN13AG ~ windex5r,             
  UN13AG ~ HHSEX,                
  UN13AG ~ HC11,                 
  UN13AG ~ helevel,              
  UN13AG ~ welevel,              
  UN13AG ~ WAGE,                 
  UN13AG ~ MSTATUS,              
  UN13AG ~ SL1_group)

tablesG <- lapply(seq_along(formulas7), function(i) {
  formula7 <- formulas7[[i]] 
  tbl_regression(
    svyglm(formula7, design = design_2014, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_tableG <- tbl_stack(tablesG)
stacked_tableG %>%
  as_gt() %>%
  gt::tab_header(title = "Avoid Social Gatherings") %>%
  gtsave("bivariate_avoid_social_gatherings.png")

### Multivariate Regressions ----





