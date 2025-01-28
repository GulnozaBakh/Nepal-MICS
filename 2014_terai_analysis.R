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

### Correlation Between religion and Enthinicity ----
cramer_v2 <- CramerV(terai_2014$HC1A, terai_2014$Ethnicity)
cat("Cramer's V between HC1A and Ethnicity:", cramer_v2)
# Create a data frame for Cramer's V result
cramer_v_df2 <- data.frame(
  Variable1 = "HC1A",
  Variable2 = "Ethnicity",
  Cramers_V = round(cramer_v2, 3)
)
# Create a LaTeX table for Cramer's V result
cramer_v_latex2 <- kable(cramer_v_df2, format = "latex", booktabs = TRUE, caption = "Cramer's V between Religion and Ethnicity") %>%
  kable_styling(latex_options = c("hold_position"))
# Display the table
cramer_v_latex2

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
#1
terai_logitC_2014 <- svyglm(UN13AC ~ HH7, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logitC_2014, exponentiate = TRUE)

#2. 
terai_logit2C_2014 <- svyglm(UN13AC ~ HC1A, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit2C_2014, exponentiate = TRUE)

#3.
terai_logit3C_2014 <- svyglm(UN13AC ~ Ethnicity, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit3C_2014, exponentiate = TRUE)

#4. 
terai_logit4C_2014 <- svyglm(UN13AC ~ windex5r, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit4C_2014, exponentiate = TRUE)

#5. 
terai_logit5C_2014 <- svyglm(UN13AC ~ HHSEX, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit5C_2014, exponentiate = TRUE)

#6. 
terai_logit6C_2014 <- svyglm(UN13AC ~ HC11, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit6C_2014, exponentiate = TRUE)

#7 
terai_logit7C_2014 <- svyglm(UN13AC ~ helevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit7C_2014, exponentiate = TRUE)

#8 
terai_logit8C_2014 <- svyglm(UN13AC ~ welevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit8C_2014, exponentiate = TRUE)

#9 
terai_logit9C_2014 <- svyglm(UN13AC ~ WAGE, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit9C_2014, exponentiate = TRUE)

#10 
terai_logit10C_2014 <- svyglm(UN13AC ~ MSTATUS, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit10C_2014, exponentiate = TRUE)

#11 
terai_logit11C_2014 <- svyglm(UN13AC ~ SL1_group, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit11C_2014, exponentiate = TRUE)

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
    svyglm(formula3, design = design_terai_2014, family = quasibinomial),
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
  gtsave("bivariate_living_in_animal_shed_terai.png")

### UN13AD ----
#1.
terai_logitD_2014 <- svyglm(UN13AD ~ HH7, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logitD_2014, exponentiate = TRUE)

#2. 
terai_logit2D_2014 <- svyglm(UN13AD ~ HC1A, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit2D_2014, exponentiate = TRUE)

#3.
terai_logit3D_2014 <- svyglm(UN13AD ~ Ethnicity, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit3D_2014, exponentiate = TRUE)

#4. 
terai_logit4D_2014 <- svyglm(UN13AD ~ windex5r, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit4D_2014, exponentiate = TRUE)

#5. 
terai_logit5D_2014 <- svyglm(UN13AD ~ HHSEX, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit5D_2014, exponentiate = TRUE)

#6. 
terai_logit6D_2014 <- svyglm(UN13AD ~ HC11, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit6D_2014, exponentiate = TRUE)

#7 
terai_logit7D_2014 <- svyglm(UN13AD ~ helevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit7D_2014, exponentiate = TRUE)

#8 
terai_logit8D_2014 <- svyglm(UN13AD ~ welevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit8D_2014, exponentiate = TRUE)

#9 
terai_logit9D_2014 <- svyglm(UN13AD ~ WAGE, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit9D_2014, exponentiate = TRUE)

#10 
terai_logit10D_2014 <- svyglm(UN13AD ~ MSTATUS, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit10D_2014, exponentiate = TRUE)

#11 
terai_logit11D_2014 <- svyglm(UN13AD ~ SL1_group, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit11D_2014, exponentiate = TRUE)

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
    svyglm(formula4, design = design_terai_2014, family = quasibinomial),
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
  gtsave("bivariate_eating_different_food_terai.png")

### UN13AE ----
#1. 
terai_logitE_2014 <- svyglm(UN13AE ~ HH7, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logitE_2014, exponentiate = TRUE)

#2. 
terai_logit2E_2014 <- svyglm(UN13AE ~ HC1A, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit2E_2014, exponentiate = TRUE)

#3.
terai_logit3E_2014 <- svyglm(UN13AE ~ Ethnicity, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit3E_2014, exponentiate = TRUE)

#4. 
terai_logit4E_2014 <- svyglm(UN13AE ~ windex5r, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit4E_2014, exponentiate = TRUE)

#5. 
terai_logit5E_2014 <- svyglm(UN13AE ~ HHSEX, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit5E_2014, exponentiate = TRUE)

#6. 
terai_logit6E_2014 <- svyglm(UN13AE ~ HC11, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit6E_2014, exponentiate = TRUE)

#7 
terai_logit7E_2014 <- svyglm(UN13AE ~ helevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit7E_2014, exponentiate = TRUE)

#8 
terai_logit8E_2014 <- svyglm(UN13AE ~ welevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit8E_2014, exponentiate = TRUE)

#9 
terai_logit9E_2014 <- svyglm(UN13AE ~ WAGE, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit9E_2014, exponentiate = TRUE)

#10 
terai_logit10E_2014 <- svyglm(UN13AE ~ MSTATUS, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit10E_2014, exponentiate = TRUE)

#11 
terai_logit11E_2014 <- svyglm(UN13AE ~ SL1_group, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit11E_2014, exponentiate = TRUE)

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
    svyglm(formula5, design = design_terai_2014, family = quasibinomial),
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
  gtsave("bivariate_bathing_separate_terai.png")

### UN13AF ----
#1.
terai_logitF_2014 <- svyglm(UN13AF ~ HH7, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logitF_2014, exponentiate = TRUE)

##2. 
terai_logit2F_2014 <- svyglm(UN13AF ~ HC1A, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit2F_2014, exponentiate = TRUE)

#3.
terai_logit3F_2014 <- svyglm(UN13AF ~ Ethnicity, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit3F_2014, exponentiate = TRUE)

#4. 
terai_logit4F_2014 <- svyglm(UN13AF ~ windex5r, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit4F_2014, exponentiate = TRUE)

#5. 
terai_logit5F_2014 <- svyglm(UN13AF ~ HHSEX, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit5F_2014, exponentiate = TRUE)

#6. 
terai_logit6F_2014 <- svyglm(UN13AF ~ HC11, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit6F_2014, exponentiate = TRUE)

#7 
terai_logit7F_2014 <- svyglm(UN13AF ~ helevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit7F_2014, exponentiate = TRUE)

#8 
terai_logit8F_2014 <- svyglm(UN13AF ~ welevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit8F_2014, exponentiate = TRUE)

#9 
terai_logit9F_2014 <- svyglm(UN13AF ~ WAGE, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit9F_2014, exponentiate = TRUE)

#10 
terai_logit10F_2014 <- svyglm(UN13AF ~ MSTATUS, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit10F_2014, exponentiate = TRUE)

#11 
terai_logit11F_2014 <- svyglm(UN13AF ~ SL1_group, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit11F_2014, exponentiate = TRUE)

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
    svyglm(formula6, design = design_terai_2014, family = quasibinomial),
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
  gtsave("bivariate_absent_from_school_work_terai.png")

### UN13AG ----
#1.
terai_logitG_2014 <- svyglm(UN13AG ~ HH7, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logitG_2014, exponentiate = TRUE)

##2. 
terai_logit2G_2014 <- svyglm(UN13AG ~ HC1A, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit2G_2014, exponentiate = TRUE)

#3.
terai_logit3G_2014 <- svyglm(UN13AG ~ Ethnicity, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit3G_2014, exponentiate = TRUE)

#4. 
terai_logit4G_2014 <- svyglm(UN13AG ~ windex5r, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit4G_2014, exponentiate = TRUE)

#5. 
terai_logit5G_2014 <- svyglm(UN13AG ~ HHSEX, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit5G_2014, exponentiate = TRUE)

#6. 
terai_logit6G_2014 <- svyglm(UN13AG ~ HC11, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit6G_2014, exponentiate = TRUE)

#7 
terai_logit7G_2014 <- svyglm(UN13AG ~ helevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit7G_2014, exponentiate = TRUE)

#8 
terai_logit8G_2014 <- svyglm(UN13AG ~ welevel, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit8G_2014, exponentiate = TRUE)

#9 
terai_logit9G_2014 <- svyglm(UN13AG ~ WAGE, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit9G_2014, exponentiate = TRUE)

#10 
terai_logit10G_2014 <- svyglm(UN13AG ~ MSTATUS, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit10G_2014, exponentiate = TRUE)

#11 
terai_logit11G_2014 <- svyglm(UN13AG ~ SL1_group, design = design_terai_2014, family = quasibinomial)
tbl_regression(terai_logit11G_2014, exponentiate = TRUE)

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
    svyglm(formula7, design = design_terai_2014, family = quasibinomial),
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
  gtsave("bivariate_avoid social_gatherings_terai.png")

### Multivariate Regressions ----
different_house_terai <- svyglm(UN13AA ~ HH7 + HC1A + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")
different_room_terai <- svyglm(UN13AB ~ HH7 + HC1A + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")
animal_shed_terai <- svyglm(UN13AC ~ HH7 + HC1A + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")
different_food_terai <- svyglm(UN13AD ~ HH7 + HC1A + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")

# Function to create a regression table with common formatting
create_regression_table_terai1 <- function(model, labels) {
  tbl <- tbl_regression(model, 
                        label = labels,
                        exponentiate = TRUE,
                        pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels() %>%
    add_significance_stars(hide_ci = TRUE, hide_p = TRUE, pattern = "{estimate} ({conf.low} - {conf.high}){stars}")
  
  # Modify the headers to show OR and CI combined, and remove SE and p-value columns
  tbl <- tbl %>%
    modify_header(
      label = "**Characteristic**",
      estimate = "**OR, 95% CI**"
    )
  
  if ("std.error" %in% names(tbl$table_body)) {
    tbl <- tbl %>% modify_table_body(~ .x %>% select(-std.error))
  }
  
  return(tbl)
}

# Create individual regression tables
terai1 <- create_regression_table_terai1(different_house_terai, list(HH7 = "Region", HC1A = "Religion", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land", HHSEX = "Sex of Household Head", helevel = "Education of Household Head", welevel = "Education of Women", WAGE = "Age of Women", SL1_group = "Number of Children Aged 1-17"))
terai2 <- create_regression_table_terai1(different_room_terai, list(HH7 = "Region", HC1A = "Religion", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land", HHSEX = "Sex of Household Head", helevel = "Education of Household Head", welevel = "Education of Women", WAGE = "Age of Women", SL1_group = "Number of Children Aged 1-17"))
terai3 <- create_regression_table_terai1(animal_shed_terai, list(HH7 = "Region", HC1A = "Religion", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land", HHSEX = "Sex of Household Head", helevel = "Education of Household Head", welevel = "Education of Women", WAGE = "Age of Women", SL1_group = "Number of Children Aged 1-17"))
terai4 <- create_regression_table_terai1(different_food_terai, list(HH7 = "Region", HC1A = "Religion", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land", HHSEX = "Sex of Household Head", helevel = "Education of Household Head", welevel = "Education of Women", WAGE = "Age of Women", SL1_group = "Number of Children Aged 1-17"))
# Combine the tables into one summary table
summary_table1_terai <- tbl_merge(
  tbls = list(terai1, terai2, terai3, terai4),
  tab_spanner = c("**Living in a different house**", "**Living in a different room**", "**Staying in animal shed**", "**Eating different food**")
)

# Hide the p.value columns after merging
summary_table1_terai <- summary_table1_terai %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt1_terai <- as_gt(summary_table1_terai)
# Save the gt table as an image
gtsave(summary_gt1_terai, "multivariate_table1_terai.png")

# Define the regression models for multivariate_table2
bath_different_house_terai <- svyglm(UN13AE ~ HH7 + HC1A + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")
absent_school_work_terai <- svyglm(UN13AF ~ HH7 + HC1A + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")
avoid_social_gatherings_terai <- svyglm(UN13AG ~ HH7 + HC1A + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")

# Function to create a regression table with common formatting
create_regression_table2_terai <- function(model, labels) {
  tbl <- tbl_regression(model, 
                        label = labels,
                        exponentiate = TRUE,
                        pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels() %>%
    add_significance_stars(hide_ci = TRUE, hide_p = TRUE, pattern = "{estimate} ({conf.low} - {conf.high}){stars}")
  
  # Modify the headers to show OR and CI combined, and remove SE and p-value columns
  tbl <- tbl %>%
    modify_header(
      label = "**Characteristic**",
      estimate = "**OR, 95% CI**"
    )
  
  if ("std.error" %in% names(tbl$table_body)) {
    tbl <- tbl %>% modify_table_body(~ .x %>% select(-std.error))
  }
  
  return(tbl)
}

# Create individual regression tables
terai5 <- create_regression_table2_terai(bath_different_house_terai, list(HH7 = "Region", HC1A = "Religion", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land", HHSEX = "Sex of Household Head", helevel = "Education of Household Head", welevel = "Education of Women", WAGE = "Age of Women", SL1_group = "Number of Children Aged 1-17"))
terai6 <- create_regression_table2_terai(absent_school_work_terai, list(HH7 = "Region", HC1A = "Religion", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land", HHSEX = "Sex of Household Head", helevel = "Education of Household Head", welevel = "Education of Women", WAGE = "Age of Women", SL1_group = "Number of Children Aged 1-17"))
terai7 <- create_regression_table2_terai(avoid_social_gatherings_terai, list(HH7 = "Region", HC1A = "Religion", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land", HHSEX = "Sex of Household Head", helevel = "Education of Household Head", welevel = "Education of Women", WAGE = "Age of Women", SL1_group = "Number of Children Aged 1-17"))

# Combine the tables into one summary table
summary_table2_terai <- tbl_merge(
  tbls = list(terai5, terai6, terai7),
  tab_spanner = c("**Bathing in a separate place**", "**Absent from school or work**", "**Avoid social gatherings**")
)

# Hide the p.value columns after merging
summary_table2_terai <- summary_table2_terai %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt2_terai <- as_gt(summary_table2_terai)

# Save the gt table as an image
gtsave(summary_gt2_terai, "multivariate_table2_terai.png")

### multivariate models for latex ----
create_regression_table <- function(model, labels) {
  tbl <- tbl_regression(
    model,
    label = labels,
    exponentiate = TRUE,
    add_estimate_to_reference_rows = TRUE,  # Add 1 to the coef row
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    bold_p(t = 0.05) %>%  # Bold p-values less than 0.05
    bold_labels() %>%
    italicize_levels() 
  
  tbl <- tbl %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(
          stars = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01 ~ "**",
            p.value < 0.05 ~ "*",
            TRUE ~ ""
          ),
          display_estimate = ifelse(
            !is.na(conf.low) & !is.na(conf.high),
            paste0(
              formatC(estimate, format = "f", digits = 2), " (",
              formatC(conf.low, format = "f", digits = 2), " to ",
              formatC(conf.high, format = "f", digits = 2), ")",
              stars
            ),
            formatC(estimate, format = "f", digits = 2)
          )
        ) %>%
        dplyr::mutate(
          display_estimate = ifelse(is.na(estimate), "", display_estimate)  
        ) %>%
        dplyr::select(-estimate, -stars, -conf.low, -conf.high)  # Remove unnecessary columns
    )
  
  tbl <- tbl %>%
    modify_header(
      label = "**Characteristic**",
      display_estimate = "**OR (95% CI)**"
    ) %>%
    modify_table_styling(
      columns = "display_estimate",
      rows = reference_row %in% TRUE,
      missing_symbol = "1.00"
    )
  
  if ("std.error" %in% names(tbl$table_body)) {
    tbl <- tbl %>% modify_table_body(~ .x %>% dplyr::select(-std.error))
  }
  
  tbl <- tbl %>%
    modify_table_body(~ .x %>% dplyr::filter(!variable %in% c("welevel", "SL1_group")))
  
  return(tbl)
}

standard_labels <- list(HH7 = "Region", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land")

# Create individual regression tables
terai1 <- create_regression_table(different_house_terai, standard_labels)
terai2 <- create_regression_table(different_room_terai, standard_labels)
terai3 <- create_regression_table(animal_shed_terai, standard_labels)
terai4 <- create_regression_table(different_food_terai, standard_labels)

# Combine the tables into one summary table
summary_table1_terai <- tbl_merge(
  tbls = list(terai1, terai2, terai3, terai4),
  tab_spanner = c("**Living in a different place (n=2,908)**", "**Living in a different room (n=2,908)**", "**Staying in animal shed (n=2,908)**", "**Eating different food (n=2,908)**")
)

# Hide the p.value columns after merging
summary_table1_terai <- summary_table1_terai %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt1_terai <- as_gt(summary_table1_terai)

# Add the title and the note to the table
summary_gt1_terai <- summary_gt1_terai %>%
  tab_header(
    title = md("**Table 2. Predictors of menstrual restrictions among women and girls in Nepal for Terai Regions, 2014.**")
  ) %>%
  tab_source_note(
    source_note = md("1.00 = Reference category. \nNote: Each model controlled for education and age of women, education and sex of household head, number of children, owning agricultural land. Marital status and rural wealth were not significant for any of the outcome variables at the bivariate level and thus were not included in the models.")
  ) %>%
  tab_options(
    heading.align = "left"
  )

# Display the final table
summary_gt1_terai

# Export gt table to LaTeX code
latex_code_terai1 <- as_latex(summary_gt1_terai)

# Display the LaTeX code in the R console
cat(as.character(latex_code_terai1))
# Save to a .tex file
writeLines(latex_code_tera1, "my_table1.tex")

# doing the remaining variables for latex output
bath_different_terai <- svyglm(UN13AE ~ HH7 + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")
absent_school_work_terai <- svyglm(UN13AF ~ HH7 + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")
avoid_social_gatherings_terai <- svyglm(UN13AG ~ HH7 + Ethnicity + HC11 + HHSEX + helevel + welevel + WAGE + SL1_group, design = design_terai_2014, family = "quasibinomial")

standard_labels <- list(HH7 = "Region", Ethnicity = "Ethnicity", HC11 = "Owns Agricultural Land")

# Create individual regression tables
terai5 <- create_regression_table(bath_different_terai, standard_labels)
terai6 <- create_regression_table(absent_school_work_terai, standard_labels)
terai7 <- create_regression_table(avoid_social_gatherings_terai, standard_labels)

# Combine the tables into one summary table
summary_table2_terai <- tbl_merge(
  tbls = list(terai5, terai6, terai7),
  tab_spanner = c("**Bathing in a different place (n=2,908)**", "**Absent from school/work (n=2,908)**", "**Avoid social gatherings (n=2,908)**")
)

# Hide the p.value columns after merging
summary_table2_terai <- summary_table2_terai %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt2_terai <- as_gt(summary_table2_terai)

# Add the title and the note to the table
summary_gt2_terai <- summary_gt2_terai %>%
  tab_header(
    title = md("**Table 2. Predictors of menstrual restrictions among women and girls in Nepal for terai regions, 2014. (Continued)**")
  ) %>%
  tab_source_note(
    source_note = md("1.00 = Reference category. \nNote: Each model controlled for education and age of women, education and sex of household head, number of children. Marital status and rural wealth were not significant for any of the outcome variables at the bivariate level and thus were not included in the models.")
  ) %>%
  tab_options(
    heading.align = "left"
  )

# Display the final table
summary_gt2_terai

# Export gt table to LaTeX code
latex_code2_terai <- as_latex(summary_gt2_terai)

# Display the LaTeX code in the R console
cat(as.character(latex_code2_terai))
# Save to a .tex file
writeLines(latex_code2_terai, "my_table2.tex")












