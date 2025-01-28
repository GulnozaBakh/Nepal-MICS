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

### Correlation between Religion and Ethnicity ----
# Cramer's V:
cramer_v <- CramerV(merged_data_2019$HC1A, merged_data_2019$Ethnicity)
cat("Cramer's V between HC1A and Ethnicity:", cramer_v)
cramer_v_df <- data.frame(
  Variable1 = "HC1A",
  Variable2 = "Ethnicity",
  Cramers_V = round(cramer_v, 3)
)

cramer_v_latex <- kable(cramer_v_df, format = "latex", booktabs = TRUE, caption = "Cramer's V between HC1A and Ethnicity") %>%
  kable_styling(latex_options = c("hold_position"))

cramer_v_latex

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
#1.
weighted_logitC_2019 <- svyglm(UN16AC ~ HH7, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logitC_2019, exponentiate = TRUE)

#2. 
weighted_logit2C_2019 <- svyglm(UN16AC ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2C_2019, exponentiate = TRUE)

#3.
weighted_logit3C_2019 <- svyglm(UN16AC ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3C_2019, exponentiate = TRUE)

#4. 
weighted_logit4C_2019 <- svyglm(UN16AC ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4C_2019, exponentiate = TRUE)

#5. 
weighted_logit5C_2019 <- svyglm(UN16AC ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5C_2019, exponentiate = TRUE)

#6. 
weighted_logit6C_2019 <- svyglm(UN16AC ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6C_2019, exponentiate = TRUE)

#7. 
weighted_logit7C_2019 <- svyglm(UN16AC ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7C_2019, exponentiate = TRUE)

#8. 
weighted_logit8C_2019 <- svyglm(UN16AC ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8C_2019, exponentiate = TRUE)

#9. 
weighted_logit9C_2019 <- svyglm(UN16AC ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9C_2019, exponentiate = TRUE)

#10. 
weighted_logit10C_2019 <- svyglm(UN16AC ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10C_2019, exponentiate = TRUE)

#11.
weighted_logit11C_2019 <- svyglm(UN16AC ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11C_2019, exponentiate = TRUE)

#12. 
weighted_logit12C_2019 <- svyglm(UN16AC ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12C_2019, exponentiate = TRUE)

#13.
weighted_logit13C_2019 <- svyglm(UN16AC ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13C_2019, exponentiate = TRUE)

# 3rd regression table
formulas3 <- list(
  UN16AC ~ HH7,                  
  UN16AC ~ HC1A,                 
  UN16AC ~ Ethnicity,            
  UN16AC ~ windex5r,             
  UN16AC ~ HHSEX,                
  UN16AC ~ HC15,                 
  UN16AC ~ helevel1,              
  UN16AC ~ welevel1,              
  UN16AC ~ WAGE,   
  UN16AC ~ HHAGEx,
  UN16AC ~ MSTATUS, 
  UN16AC ~ HH51_grouped,
  UN16AC ~ HH52_grouped)

tables3 <- lapply(seq_along(formulas3), function(i) {
  formula3 <- formulas3[[i]] 
  tbl_regression(
    svyglm(formula3, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table3 <- tbl_stack(tables3)
stacked_table3 %>%
  as_gt() %>%
  gt::tab_header(title = "Living in cowshed") %>%
  gtsave("bivariate_living_in_cowshed.png")

### Eating in a separate place 'UN16AD' ----
#1. 
weighted_logitD_2019 <- svyglm(UN16AD ~ HH7, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logitD_2019, exponentiate = TRUE)

#2. 
weighted_logit2D_2019 <- svyglm(UN16AD ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2D_2019, exponentiate = TRUE)

#3.
weighted_logit3D_2019 <- svyglm(UN16AD ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3D_2019, exponentiate = TRUE)

#4. 
weighted_logit4D_2019 <- svyglm(UN16AD ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4D_2019, exponentiate = TRUE)

#5. 
weighted_logit5D_2019 <- svyglm(UN16AD ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5D_2019, exponentiate = TRUE)

#6. 
weighted_logit6D_2019 <- svyglm(UN16AD ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6D_2019, exponentiate = TRUE)

#7. 
weighted_logit7D_2019 <- svyglm(UN16AD ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7D_2019, exponentiate = TRUE)

#8. 
weighted_logit8D_2019 <- svyglm(UN16AD ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8D_2019, exponentiate = TRUE)

#9. 
weighted_logit9D_2019 <- svyglm(UN16AD ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9D_2019, exponentiate = TRUE)

#10. 
weighted_logit10D_2019 <- svyglm(UN16AD ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10D_2019, exponentiate = TRUE)

#11.
weighted_logit11D_2019 <- svyglm(UN16AD ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11D_2019, exponentiate = TRUE)

#12.
weighted_logit12D_2019 <- svyglm(UN16AD ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12D_2019, exponentiate = TRUE)

#13.
weighted_logit13D_2019 <- svyglm(UN16AD ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13D_2019, exponentiate = TRUE)

# 4th regression table
formulas4 <- list(
  UN16AD ~ HH7,                  
  UN16AD ~ HC1A,                 
  UN16AD ~ Ethnicity,            
  UN16AD ~ windex5r,             
  UN16AD ~ HHSEX,                
  UN16AD ~ HC15,                 
  UN16AD ~ helevel1,              
  UN16AD ~ welevel1,              
  UN16AD ~ WAGE,   
  UN16AD ~ HHAGEx,
  UN16AD ~ MSTATUS, 
  UN16AD ~ HH51_grouped,
  UN16AD ~ HH52_grouped)

tables4 <- lapply(seq_along(formulas4), function(i) {
  formula4 <- formulas4[[i]] 
  tbl_regression(
    svyglm(formula4, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table4 <- tbl_stack(tables4)
stacked_table4 %>%
  as_gt() %>%
  gt::tab_header(title = "Eating in a separate place") %>%
  gtsave("bivariate_eating_in_separate_place.png")

### Bathing in a separate place 'UN16AE' ----
#1.
weighted_logitE_2019 <- svyglm(UN16AE ~ HH7, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logitE_2019, exponentiate = TRUE)

#2. 
weighted_logit2E_2019 <- svyglm(UN16AE ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2E_2019, exponentiate = TRUE)

#3.
weighted_logit3E_2019 <- svyglm(UN16AE ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3E_2019, exponentiate = TRUE)

#4.
weighted_logit4E_2019 <- svyglm(UN16AE ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4E_2019, exponentiate = TRUE)

#5. 
weighted_logit5E_2019 <- svyglm(UN16AE ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5E_2019, exponentiate = TRUE)

#6. 
weighted_logit6E_2019 <- svyglm(UN16AE ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6E_2019, exponentiate = TRUE)

#7. 
weighted_logit7E_2019 <- svyglm(UN16AE ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7E_2019, exponentiate = TRUE)

#8.
weighted_logit8E_2019 <- svyglm(UN16AE ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8E_2019, exponentiate = TRUE)

#9.
weighted_logit9E_2019 <- svyglm(UN16AE ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9E_2019, exponentiate = TRUE)

#10.
weighted_logit10E_2019 <- svyglm(UN16AE ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10E_2019, exponentiate = TRUE)

#11. 
weighted_logit11E_2019 <- svyglm(UN16AE ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11E_2019, exponentiate = TRUE)

#12.
weighted_logit12E_2019 <- svyglm(UN16AE ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12E_2019, exponentiate = TRUE)

#13.
weighted_logit13E_2019 <- svyglm(UN16AE ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13E_2019, exponentiate = TRUE)

# 5th regression table
formulas5 <- list(
  UN16AE ~ HH7,                  
  UN16AE ~ HC1A,                 
  UN16AE ~ Ethnicity,            
  UN16AE ~ windex5r,             
  UN16AE ~ HHSEX,                
  UN16AE ~ HC15,                 
  UN16AE ~ helevel1,              
  UN16AE ~ welevel1,              
  UN16AE ~ WAGE,   
  UN16AE ~ HHAGEx,
  UN16AE ~ MSTATUS, 
  UN16AE ~ HH51_grouped,
  UN16AE ~ HH52_grouped)

tables5 <- lapply(seq_along(formulas5), function(i) {
  formula5 <- formulas5[[i]] 
  tbl_regression(
    svyglm(formula5, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table5 <- tbl_stack(tables5)
stacked_table5 %>%
  as_gt() %>%
  gt::tab_header(title = "Bathing in a separate place") %>%
  gtsave("bivariate_bathing_separately.png")

### Staying away from school or work 'UN16AF' ----
#1.
weighted_logitF_2019 <- svyglm(UN16AF ~ HH7, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logitF_2019, exponentiate = TRUE)

#2.
weighted_logit2F_2019 <- svyglm(UN16AF ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2F_2019, exponentiate = TRUE)

#3.
weighted_logit3F_2019 <- svyglm(UN16AF ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3F_2019, exponentiate = TRUE)

#4.
weighted_logit4F_2019 <- svyglm(UN16AF ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4F_2019, exponentiate = TRUE)

#5.
weighted_logit5F_2019 <- svyglm(UN16AF ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5F_2019, exponentiate = TRUE)

#6.
weighted_logit6F_2019 <- svyglm(UN16AF ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6F_2019, exponentiate = TRUE)

#7.
weighted_logit7F_2019 <- svyglm(UN16AF ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7F_2019, exponentiate = TRUE)

#8.
weighted_logit8F_2019 <- svyglm(UN16AF ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8F_2019, exponentiate = TRUE)

#9.
weighted_logit9F_2019 <- svyglm(UN16AF ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9F_2019, exponentiate = TRUE)

#10.
weighted_logit10F_2019 <- svyglm(UN16AF ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10F_2019, exponentiate = TRUE)

#11.
weighted_logit11F_2019 <- svyglm(UN16AF ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11F_2019, exponentiate = TRUE)

#12.
weighted_logit12F_2019 <- svyglm(UN16AF ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12F_2019, exponentiate = TRUE)

#13.
weighted_logit13F_2019 <- svyglm(UN16AF ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13F_2019, exponentiate = TRUE)

# 6th regression table
formulas6 <- list(
  UN16AF ~ HH7,                  
  UN16AF ~ HC1A,                 
  UN16AF ~ Ethnicity,            
  UN16AF ~ windex5r,             
  UN16AF ~ HHSEX,                
  UN16AF ~ HC15,                 
  UN16AF ~ helevel1,              
  UN16AF ~ welevel1,              
  UN16AF ~ WAGE,   
  UN16AF ~ HHAGEx,
  UN16AF ~ MSTATUS, 
  UN16AF ~ HH51_grouped,
  UN16AF ~ HH52_grouped)

tables6 <- lapply(seq_along(formulas6), function(i) {
  formula6 <- formulas6[[i]] 
  tbl_regression(
    svyglm(formula6, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table6 <- tbl_stack(tables6)
stacked_table6 %>%
  as_gt() %>%
  gt::tab_header(title = "Staying Away from School/Work") %>%
  gtsave("bivariate_staying_away_from_school_work.png")

### Staying away from social gatherings 'UN16AG' ----
#1.
weighted_logitG_2019 <- svyglm(UN16AG ~ HH7, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logitG_2019, exponentiate = TRUE)

#2.
weighted_logit2G_2019 <- svyglm(UN16AG ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2G_2019, exponentiate = TRUE)

#3.
weighted_logit3G_2019 <- svyglm(UN16AG ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3G_2019, exponentiate = TRUE)

#4.
weighted_logit4G_2019 <- svyglm(UN16AG ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4G_2019, exponentiate = TRUE)

#5. 
weighted_logit5G_2019 <- svyglm(UN16AG ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5G_2019, exponentiate = TRUE)

#6.
weighted_logit6G_2019 <- svyglm(UN16AG ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6G_2019, exponentiate = TRUE)

#7.
weighted_logit7G_2019 <- svyglm(UN16AG ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7G_2019, exponentiate = TRUE)

#8.
weighted_logit8G_2019 <- svyglm(UN16AG ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8G_2019, exponentiate = TRUE)

#9.
weighted_logit9G_2019 <- svyglm(UN16AG ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9G_2019, exponentiate = TRUE)

#10.
weighted_logit10G_2019 <- svyglm(UN16AG ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10G_2019, exponentiate = TRUE)

#11.
weighted_logit11G_2019 <- svyglm(UN16AG ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11G_2019, exponentiate = TRUE)

#12.
weighted_logit12G_2019 <- svyglm(UN16AG ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12G_2019, exponentiate = TRUE)

#13.
weighted_logit13G_2019 <- svyglm(UN16AG ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13G_2019, exponentiate = TRUE)

# 7th regression table
formulas7 <- list(
  UN16AG ~ HH7,                  
  UN16AG ~ HC1A,                 
  UN16AG ~ Ethnicity,            
  UN16AG ~ windex5r,             
  UN16AG ~ HHSEX,                
  UN16AG ~ HC15,                 
  UN16AG ~ helevel1,              
  UN16AG ~ welevel1,              
  UN16AG ~ WAGE,   
  UN16AG ~ HHAGEx,
  UN16AG ~ MSTATUS, 
  UN16AG ~ HH51_grouped,
  UN16AG ~ HH52_grouped)

tables7 <- lapply(seq_along(formulas7), function(i) {
  formula7 <- formulas7[[i]] 
  tbl_regression(
    svyglm(formula7, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table7 <- tbl_stack(tables7)
stacked_table7 %>%
  as_gt() %>%
  gt::tab_header(title = "Staying Away from Social Gatherings") %>%
  gtsave("bivariate_staying_away_from_social_gatherings.png")

### Staying away from religious work 'UN16AH' ----
#1.
weighted_logitH_2019 <- svyglm(UN16AH ~ HH7, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logitH_2019, exponentiate = TRUE)

#2.
weighted_logit2H_2019 <- svyglm(UN16AH ~ HC1A, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit2H_2019, exponentiate = TRUE)

#3.
weighted_logit3H_2019 <- svyglm(UN16AH ~ Ethnicity, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit3H_2019, exponentiate = TRUE)

#4.
weighted_logit4H_2019 <- svyglm(UN16AH ~ windex5r, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit4H_2019, exponentiate = TRUE)

#5.
weighted_logit5H_2019 <- svyglm(UN16AH ~ HHSEX, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit5H_2019, exponentiate = TRUE)

#6.
weighted_logit6H_2019 <- svyglm(UN16AH ~ HC15, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit6H_2019, exponentiate = TRUE)

#7.
weighted_logit7H_2019 <- svyglm(UN16AH ~ helevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit7H_2019, exponentiate = TRUE)

#8.
weighted_logit8H_2019 <- svyglm(UN16AH ~ welevel1, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit8H_2019, exponentiate = TRUE)

#9.
weighted_logit9H_2019 <- svyglm(UN16AH ~ WAGE, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit9H_2019, exponentiate = TRUE)

#10.
weighted_logit10H_2019 <- svyglm(UN16AH ~ HHAGEx, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit10H_2019, exponentiate = TRUE)

#11.
weighted_logit11H_2019 <- svyglm(UN16AH ~ MSTATUS, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit11H_2019, exponentiate = TRUE)

#12.
weighted_logit12H_2019 <- svyglm(UN16AH ~ HH51_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit12H_2019, exponentiate = TRUE)

#13.
weighted_logit13H_2019 <- svyglm(UN16AH ~ HH52_grouped, design = design_2019, family = quasibinomial)
tbl_regression(weighted_logit13H_2019, exponentiate = TRUE)

# 8th regression table
formulas8 <- list(
  UN16AH ~ HH7,                  
  UN16AH ~ HC1A,                 
  UN16AH ~ Ethnicity,            
  UN16AH ~ windex5r,             
  UN16AH ~ HHSEX,                
  UN16AH ~ HC15,                 
  UN16AH ~ helevel1,              
  UN16AH ~ welevel1,              
  UN16AH ~ WAGE,   
  UN16AH ~ HHAGEx,
  UN16AH ~ MSTATUS, 
  UN16AH ~ HH51_grouped,
  UN16AH ~ HH52_grouped)

tables8 <- lapply(seq_along(formulas8), function(i) {
  formula8 <- formulas8[[i]] 
  tbl_regression(
    svyglm(formula8, design = design_2019, family = quasibinomial),
    label = labels[i],  
    exponentiate = TRUE,  
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
    add_global_p() %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels()
})
stacked_table8 <- tbl_stack(tables8)
stacked_table8 %>%
  as_gt() %>%
  gt::tab_header(title = "Staying Away from Religious Work") %>%
  gtsave("bivariate_staying_away_from_religious_work.png")

### Multivariate Regressions ----
chaupadi <- svyglm(UN16AA ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
separate_room <- svyglm(UN16AB ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
cowshed <- svyglm(UN16AC ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
eating_separate <- svyglm(UN16AD ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")

# Function to create a regression table with common formatting
create_regression_table <- function(model, labels) {
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
table1_2019 <- create_regression_table(chaupadi, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))
table2_2019 <- create_regression_table(separate_room, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))
table3_2019 <- create_regression_table(cowshed, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))
table4_2019 <- create_regression_table(eating_separate, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))

# Combine the tables into one summary table
summary_table1_2019 <- tbl_merge(
  tbls = list(table1_2019, table2_2019, table3_2019, table4_2019),
  tab_spanner = c("**Staying in chaupadi/chhapro**", "**Staying in a separate room**", "**Staying in the cowshed**", "**Eating in a separate palce**")
)

# Hide the p.value columns after merging
summary_table1_2019 <- summary_table1_2019 %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt1_2019 <- as_gt(summary_table1_2019)
# Save the gt table as an image
gtsave(summary_gt1_2019, "multivariate_table1.png")

# Define the regression models for multivariate_table2
bathing_separate <- svyglm(UN16AE ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
away_from_school_work <- svyglm(UN16AF ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
away_from_social_gatherings <- svyglm(UN16AG ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
away_from_religious_work <- svyglm(UN16AH ~ HH7 + HC1A + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")

# Function to create a regression table with common formatting
create_regression_table2_2019 <- function(model, labels) {
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
table5_2019 <- create_regression_table2_2019(bathing_separate, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))
table6_2019 <- create_regression_table2_2019(away_from_school_work, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))
table7_2019 <- create_regression_table2_2019(away_from_social_gatherings, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))
table8_2019 <- create_regression_table2_2019(away_from_religious_work, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC15 = "Owns Agricultural Land", helevel1 = "Education of Household head", welevel1 = "Education of Women", HH52_grouped = "Number of Children Aged 5-17"))

# Combine the tables into one summary table
summary_table2_2019 <- tbl_merge(
  tbls = list(table5_2019, table6_2019, table7_2019, table8_2019),
  tab_spanner = c("**Bathing in a separate place**", "**Staying away from school/work**", "**Staying away from social gatherings**", "**Staying away from religious work*")
)

# Hide the p.value columns after merging
summary_table2_2019 <- summary_table2_2019 %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt2_2019 <- as_gt(summary_table2_2019)

# Save the gt table as an image
gtsave(summary_gt2_2019, "multivariate_table2.png")

### multivariate models for latex ----
create_regression_table1_2019 <- function(model, labels) {
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

standard_labels_2019 <- list(HH7 = "Region", Ethnicity = "Ethnicity", windex5r = "Rural Wealth")

# Create individual regression tables
table1_2019 <- create_regression_table1_2019(chaupadi, standard_labels_2019)
table2_2019 <- create_regression_table1_2019(separate_room, standard_labels_2019)
table3_2019 <- create_regression_table1_2019(cowshed, standard_labels_2019)
table4_2019 <- create_regression_table1_2019(eating_separate, standard_labels_2019)

# Combine the tables into one summary table
summary_table1_2019 <- tbl_merge(
  tbls = list(table1_2019, table2_2019, table3_2019, table4_2019),
  tab_spanner = c("**Living in chaupadi (n=5,465)**", "**Living in a different room (n=5,465)**", "**Staying in cowshed (n=5,465)**", "**Eating in a separate place (n=5,465)**")
)

# Hide the p.value columns after merging
summary_table1_2019 <- summary_table1_2019 %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt1_2019 <- as_gt(summary_table1_2019)

# Add the title and the note to the table
summary_gt1_2019 <- summary_gt1_2019 %>%
  tab_header(
    title = md("**Table 3. Predictors of menstrual restrictions among women and girls in Nepal, 2019.**")
  ) %>%
  tab_source_note(
    source_note = md("1.00 = Reference category. \nNote: Each model controlled for education of women and household head, number of children aged 5-17, owning agricultural land. Marital status, age of women and household head, sex of household head and number of children under age 5 were not significant for any of the outcome variables at the bivariate level and thus were not included in the models.")
  ) %>%
  tab_options(
    heading.align = "left"
  )

summary_gt1_2019

# Export gt table to LaTeX code
latex_code_2019 <- as_latex(summary_gt1_2019)

# Display the LaTeX code in the R console
cat(as.character(latex_code_2019))

# Save to a .tex file
writeLines(latex_code_2019, "multivate_table1_2019.tex")

# doing the remaining variables for latex output
bathing_separate <- svyglm(UN16AE ~ HH7 + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
away_from_school_work <- svyglm(UN16AF ~ HH7 + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
away_from_social_gatherings <- svyglm(UN16AG ~ HH7 + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")
away_from_religious_work <- svyglm(UN16AH ~ HH7 + Ethnicity + windex5r + HC15 + helevel1 + welevel1 + HH52_grouped, design = design_2019, family = "quasibinomial")

# Standardize labels for all regression tables
standard_labels_2019 <- list(HH7 = "Region", Ethnicity = "Ethnicity", windex5r = "Rural Wealth")

# Create individual regression tables
table5_2019 <- create_regression_table1_2019(bathing_separate, standard_labels_2019)
table6_2019 <- create_regression_table1_2019(away_from_school_work, standard_labels_2019)
table7_2019 <- create_regression_table1_2019(away_from_social_gatherings, standard_labels_2019)
table8_2019 <- create_regression_table1_2019(away_from_religious_work, standard_labels_2019)

# Combine the tables into one summary table
summary_table2_2019 <- tbl_merge(
  tbls = list(table5_2019, table6_2019, table7_2019, table8_2019),
  tab_spanner = c("**Bathing in a separate place (n=5,465)**", "**Staying away from school/work (n=5,465)**", "**Staying away from social gatherings (n=5,465)**", "**Staying away from religious work (n=5,465)**")
)

# Hide the p.value columns after merging
summary_table2_2019 <- summary_table2_2019 %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt2_2019 <- as_gt(summary_table2_2019)

# Add the title and the note to the table
summary_gt2_2019 <- summary_gt2_2019 %>%
  tab_header(
    title = md("**Table 3. Predictors of menstrual restrictions among women and girls in Nepal, 2019. (Continued)**")
  ) %>%
  tab_source_note(
    source_note = md("1.00 = Reference category. \nNote: Each model controlled for education of women and household head, number of children aged 5-17, owning agricultural land. Marital status, age of women and household head, sex of household head and number of children under age 5 were not significant for any of the outcome variables at the bivariate level and thus were not included in the models.")
  ) %>%
  tab_options(
    heading.align = "left"
  )

# Display the final table
summary_gt2_2019

# Export gt table to LaTeX code
latex_code2_2019 <- as_latex(summary_gt2_2019)
cat(as.character(latex_code2_2019))
writeLines(latex_code2_2019, "multivate_table2_2019.tex")











