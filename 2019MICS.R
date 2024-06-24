library(dplyr)
library(haven)
library(labelled)
library(readxl)
library(survey)
library(readr)
library(psych)
library(tidyr)
library(broom)
library(tableone)
library(gtsummary)
library(gt)
library(stargazer)
library(webshot2) 
library(stringr)
library(purrr)
library(kableExtra)
library(forcats)

setwd("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019")
#Load the data
data_wm_2019 <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/wm.sav")
data_hh_2019 <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/hh.sav")

# Convert labelled vectors to character vectors
data_hh_2019 <- labelled::to_character(data_hh_2019)
data_wm_2019 <- labelled::to_character(data_wm_2019)

# Select relevant variables from the household dataset
selected_hh_2019 <- data_hh_2019 %>%
  select(HH1, HH2, HH6, HH7, HH51, HH52, HC1A, HC2, HC15, HHSEX, HHAGE, helevel1, hhweight, windex5r, PSU, stratum)

# Select relevant variables from the women's dataset
selected_wm_2019 <- data_wm_2019 %>%
  select(HH1, HH2, UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG,UN16AH, WAGE, WM17, MSTATUS, welevel1, wmweight, CM4)

# Merge the datasets on HH1 and HH2
merged_data_2019 <- merge(selected_hh_2019, selected_wm_2019, by = c("HH1", "HH2"))

# Use subset to filter only cases where the interview is completed and only rural areas
merged_data_2019 <- subset(merged_data_2019, WM17 == "COMPLETED" & HH6 == "RURAL")

# Function to clean the data
clean_data1 <- function(df, columns) {
  df %>%
    mutate(across(all_of(columns), 
                  ~ toupper(str_trim(gsub("[^[:print:]]", "", .))))) %>%
    mutate(across(all_of(columns), ~ na_if(., "NO RESPONSE")))
}

# Specify the columns to clean
columns_to_clean1 <- c("UN16AA", "UN16AB", "UN16AC", "UN16AD", "UN16AE", "UN16AF", "UN16AG", "UN16AH")

# Clean the data
merged_data_2019 <- clean_data1(merged_data_2019, columns_to_clean1)

# Filter out rows with NA in any of the selected columns
merged_data_2019 <- merged_data_2019 %>%
  filter(complete.cases(select(., all_of(columns_to_clean1))))
# Verify the filtering
summary(merged_data_2019)

#convert Yes and NO to 1 and 0 in the practices columns
merged_data_2019 <- merged_data_2019 %>%
  mutate(across(c(UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG, UN16AH), ~ ifelse(. == "YES", 1, ifelse(. == "NO", 0, NA))))

# Combine low count categories for demonstration (Religion)
merged_data_2019$HC1A <- with(merged_data_2019, ifelse(HC1A %in% c("BON", "NO RELIGION", "OTHERS", "PRAKRITI", "JAIN", "KIRAT"), "OTHER", HC1A))
merged_data_2019$HC1A <- factor(merged_data_2019$HC1A)

# Define the mapping of each ethnicity to its group
ethnicity_mapping2 <- list(
  "Brahman or Chhetri" = c("Amat", "Brahman - Hill", "Brahman - Tarai", "Chhetree", "Hajam/Thakur", "Rajput", "Sanyasi/Dashnami", "Thakuri"),
  "Tarai or Madhesi Other Castes" = c("Bantar/Sardar", "Bantaba", "Baraee", "Haluwai", "Kahar", "Kalwar", "Kanu", "Kathbaniyan", "Kewat", "Koiri/Kushwaha", "Kumhar", "Kurmi", "Lodh", "Lohar", "Mali", "Mallaha", "Nuniya", "Rajbhar", "Sonar", "Sudhi", "Teli", "Terai Others", "Yadav"),
  "Dalits" = c("Badi", "Chamar/Harijan/Ram", "Dalit Others", "Damai/Dholi", "Dhobi", "Dom", "Dusadh/Pasawan/Pasi", "Gaderi/Bhedhar", "Kami", "Kori", "Musahar", "Sarki", "Tatma/Tatwa"),
  "Newar" = c("Newar"),
  "Janajati" = c("Chhantyal/Chhantel", "Danuwar", "Dhanuk", "Gangai", "Ghale", "Gharti/Bhujel", "Gurung", "Janajati Others", "Jhangad/Dhagar", "Kisan", "Kumal", "Kusunda", "Limbu", "Magar", "Majhi", "Mewahang Bala", "Pahari", "Rai", "Rajbansi", "Raute", "Satar/Santhal", "Sherpa", "Sunuwar", "Tamang", "Thakali", "Tharu" ),
  "Muslim" = c("Musalman"),
  "Other" = c("Others")
)
# Function to map ethnicities to groups, including handling NAs
map_ethnicity2 <- function(ethnicity) {
  if (is.na(ethnicity)) {
    return("Missing")
  }
  for (group in names(ethnicity_mapping2)) {
    if (ethnicity %in% ethnicity_mapping2[[group]]) {
      return(group)
    }
  }
  return("Other")  # Default to "Other" if no match is found
}
# Apply the mapping to create a new variable
merged_data_2019 <-merged_data_2019 %>%
  mutate(Ethnicity = sapply(HC2, map_ethnicity2))
merged_data_2019$Ethnicity <- factor(merged_data_2019$Ethnicity)

# Convert WAGE to a factor 
merged_data_2019$WAGE <- factor(merged_data_2019$WAGE)

# Grouping the number of children For HH51
merged_data_2019$HH51_grouped <- cut(merged_data_2019$HH51, 
                                     breaks = c(-Inf, 0, 1, 2, Inf), 
                                     labels = c("0", "1", "2", "3 and more"))

# For HH52
merged_data_2019$HH52_grouped <- cut(merged_data_2019$HH52, 
                                     breaks = c(-Inf, 0, 1, 2, Inf), 
                                     labels = c("0", "1", "2", "3 and more"))


#Recode HHAGE into HHAGEx (4 groups)
merged_data_2019 <- merged_data_2019 %>%
  mutate(
    HHAGEx = case_when(
      HHAGE >= 15 & HHAGE <= 24 ~ 1,
      HHAGE >= 25 & HHAGE <= 29 ~ 2,
      HHAGE >= 30 & HHAGE <= 34 ~ 3,
      HHAGE >= 35 & HHAGE <= 39 ~ 4,
      HHAGE >= 40 & HHAGE <= 44 ~ 5,
      HHAGE >= 45 & HHAGE <= 49 ~ 6,
      HHAGE >= 50 & HHAGE <= 59 ~ 7,
      HHAGE >= 60 & HHAGE <= 69 ~ 8,
      HHAGE >= 70 ~ 9
    )
  )
# Convert HHAGEx to a factor with appropriate labels
merged_data_2019$HHAGEx <- factor(merged_data_2019$HHAGEx, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("15 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 59", "60 to 69", "70 plus"))

#Group MSTATUS into two groups
merged_data_2019$MSTATUS <- ifelse(merged_data_2019$MSTATUS %in% c("Currently married/in union", "Formerly married/in union"), "Ever Married", "Never Married")
# Convert the new variable to a factor
merged_data_2019$MSTATUS <- factor(merged_data_2019$MSTATUS, levels = c("Ever Married", "Never Married"))

# Convert "NO RESPONSE" to NA for the HC15 column
merged_data_2019 <- merged_data_2019 %>%
  mutate(HC15 = na_if(toupper(str_trim(gsub("[^[:print:]]", "", HC15))), "NO RESPONSE"))
# Remove rows with NA values in HC15
merged_data_2019 <- subset(merged_data_2019, !is.na(HC15))

# Convert multiple columns to factors in one line
merged_data_2019 <- merged_data_2019 %>%
  mutate(across(c(HH7, HHSEX, HC15, helevel1, windex5r, welevel1), as.factor))

####################################Creating a frequency distribution (summary) table on unweighted data
# Reorder the levels of the helevel, welevel, and windex5r variables
merged_data_2019 <- merged_data_2019 %>%
  mutate(
    helevel1 = fct_relevel(helevel1, "None", "Basic (Gr 1-8)", "Secondary (Gr 9-12)", "Higher"),
    welevel1 = fct_relevel(welevel1, "None", "Basic (Gr 1-8)", "Secondary (Gr 9-12)", "Higher"),
    windex5r = fct_relevel(windex5r, "Poorest", "Second", "Middle", "Fourth", "Richest"),
    HC1A = fct_relevel(HC1A, "HINDU", "BUDDHIST", "CHRISTIAN", "ISLAM", "OTHER")
  )

# Select the desired variables including the new summary index
d2 <- merged_data_2019 %>% 
  select(HH7, HC1A, Ethnicity, HC15, HHSEX, helevel1, windex5r, WAGE, HHAGEx, MSTATUS, welevel1, HH51_grouped, HH52_grouped, UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG, UN16AH)

# Example variable labels
variable_labels <- list(
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
  HH52_grouped = "Number of children aged 5-17",
  UN16AA = "Staying in chaupadi/chhapro",
  UN16AB = "Staying in separate room ",
  UN16AC = "Staying in cowshed",
  UN16AD = "Eating in a separate place",
  UN16AE = "Bathing in a separate place",
  UN16AF = "Staying away from school or work",
  UN16AG = "Staying away from social gatherings",
  UN16AH = "Staying away from religious work"
)
# Create the summary table with custom labels and add a caption
summary_table_2019 <- tbl_summary(
  d2,
  label = variable_labels
) %>%
  modify_caption("**Table 1. Sociodemographic characteristics and the menstrual conditions of women, Nepal, 2019.**")

# Display the summary table
summary_table_2019
#Save it 
summary_gt2 <- as_gt(summary_table_2019)
gtsave(summary_gt2, filename = "summary_table_2019.png")

##########################################Bivariate Regressions
#Some factors inside variables have artithmatic opeartors "-", so we need to rename them
# Step 1: Trim whitespace from the levels
merged_data_2019$helevel1 <- trimws(merged_data_2019$helevel1)
merged_data_2019$welevel1 <- trimws(merged_data_2019$welevel1)
merged_data_2019$WAGE <- trimws(merged_data_2019$WAGE)
# Step 2: Convert factor levels with the cleaned data
merged_data_2019$helevel1<- factor(merged_data_2019$helevel1, 
                              levels = c("None", "Basic (Gr 1-8)", "Secondary (Gr 9-12)", "Higher") ,
                              labels = c("None", "Basic", "Secondary", "Higher"))
merged_data_2019$welevel1<- factor(merged_data_2019$welevel1, 
                                   levels = c("None", "Basic (Gr 1-8)", "Secondary (Gr 9-12)", "Higher") ,
                                   labels = c("None", "Basic", "Secondary", "Higher"))
merged_data_2019$WAGE <- factor(merged_data_2019$WAGE, 
                                levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") ,
                                labels = c("15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49"))


#Staying in chaupadi/chhapro UN16AA
#1. Let's make Sudoorpraschim the desired reference category
merged_data_2019$HH7 <- relevel(merged_data_2019$HH7, ref = "SUDOORPASCHIM PROVINCE")
# Verify the releveling
levels(merged_data_2019$HH7)
# Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logitA_2019 <- svyglm(UN16AA ~ HH7, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitA_2019)
tbl_regression(weighted_logitA_2019, exponentiate = TRUE)

#2. Let's make Hindu the desired reference category
merged_data_2019$HC1A <- relevel(merged_data_2019$HC1A, ref = "HINDU")
# Verify the releveling
levels(merged_data_2019$HC1A)
# Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit2A_2019 <- svyglm(UN16AA ~ HC1A, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2A_2019)
tbl_regression(weighted_logit2A_2019, exponentiate = TRUE)

#3. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit3A_2019 <- svyglm(UN16AA ~ Ethnicity, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3A_2019)
tbl_regression(weighted_logit3A_2019, exponentiate = TRUE)

#4. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit4A_2019 <- svyglm(UN16AA ~ windex5r, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4A_2019)
tbl_regression(weighted_logit4A_2019, exponentiate = TRUE)

#5. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit5A_2019 <- svyglm(UN16AA ~ HHSEX, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5A_2019)
tbl_regression(weighted_logit5A_2019, exponentiate = TRUE)

#6. Let's make Yes the desired reference category
merged_data_2019$HC15 <- relevel(merged_data_2019$HC15, ref = "YES")
#Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit6A_2019 <- svyglm(UN16AA ~ HC15, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6A_2019)
tbl_regression(weighted_logit6A_2019, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit7A_2019 <- svyglm(UN16AA ~ helevel1, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7A_2019)
tbl_regression(weighted_logit7A_2019, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit8A_2019 <- svyglm(UN16AA ~ welevel1, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8A_2019)
tbl_regression(weighted_logit8A_2019, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit9A_2019 <- svyglm(UN16AA ~ WAGE, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9A_2019)
tbl_regression(weighted_logit9A_2019, exponentiate = TRUE)

#10 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit10A_2019 <- svyglm(UN16AA ~ HHAGEx, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10A_2019)
tbl_regression(weighted_logit10A_2019, exponentiate = TRUE)

#11 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit11A_2019 <- svyglm(UN16AA ~ MSTATUS, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11A_2019)
tbl_regression(weighted_logit11A_2019, exponentiate = TRUE)

#12 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit12A_2019 <- svyglm(UN16AA ~ HH51_grouped, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit12A_2019)
tbl_regression(weighted_logit12A_2019, exponentiate = TRUE)

#13 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit13A_2019 <- svyglm(UN16AA ~ HH52_grouped, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit13A_2019)
tbl_regression(weighted_logit13A_2019, exponentiate = TRUE)

# First regression table
table1A <- weighted_logitA_2019 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2A <- weighted_logit2A_2019 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3A <- weighted_logit3A_2019 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4A <- weighted_logit4A_2019 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5A <- weighted_logit5A_2019 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6A <- weighted_logit6A_2019 %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7A <- weighted_logit7A_2019 %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8A <- weighted_logit8A_2019 %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9A <- weighted_logit9A_2019 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10A <- weighted_logit10A_2019 %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table11A <- weighted_logit11A_2019 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12A <- weighted_logit12A_2019 %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children under age 5"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13A <- weighted_logit13A_2019 %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children age 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_table1AA <- tbl_stack(
  tbls = list(table1A, table2A, table3A, table4A, table5A, table6A, table7A, table8A, table9A, table10A, table11A, table12A, table13A)
)
# Convert the gtsummary table to a gt table
stacked_gtAA <- as_gt(stacked_table1AA) %>%
  gt::tab_header("Staying in chaupadi")
# Save the gt table as an image
gtsave(stacked_gtAA, "bivariate_staying_in_chaupadi.png")

###########################################################################################
#Staying in a separate room UN16AB
#1. Let's make Sudoorpraschim the desired reference category
merged_data_2019$HH7 <- relevel(merged_data_2019$HH7, ref = "SUDOORPASCHIM PROVINCE")
# Verify the releveling
levels(merged_data_2019$HH7)
# Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logitB_2019 <- svyglm(UN16AB ~ HH7, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitB_2019)
tbl_regression(weighted_logitB_2019, exponentiate = TRUE)

#2. Let's make Hindu the desired reference category
merged_data_2019$HC1A <- relevel(merged_data_2019$HC1A, ref = "HINDU")
# Verify the releveling
levels(merged_data_2019$HC1A)
# Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit2B_2019 <- svyglm(UN16AB ~ HC1A, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2B_2019)
tbl_regression(weighted_logit2B_2019, exponentiate = TRUE)

#3. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit3B_2019 <- svyglm(UN16AB ~ Ethnicity, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3B_2019)
tbl_regression(weighted_logit3B_2019, exponentiate = TRUE)

#4. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit4B_2019 <- svyglm(UN16AB ~ windex5r, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4B_2019)
tbl_regression(weighted_logit4B_2019, exponentiate = TRUE)

#5. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit5B_2019 <- svyglm(UN16AB ~ HHSEX, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5B_2019)
tbl_regression(weighted_logit5B_2019, exponentiate = TRUE)

#6. Let's make Yes the desired reference category
merged_data_2019$HC15 <- relevel(merged_data_2019$HC15, ref = "YES")
#Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit6B_2019 <- svyglm(UN16AB ~ HC15, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6B_2019)
tbl_regression(weighted_logit6B_2019, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit7B_2019 <- svyglm(UN16AB ~ helevel1, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7B_2019)
tbl_regression(weighted_logit7B_2019, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit8B_2019 <- svyglm(UN16AB ~ welevel1, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8B_2019)
tbl_regression(weighted_logit8B_2019, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit9B_2019 <- svyglm(UN16AB ~ WAGE, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9B_2019)
tbl_regression(weighted_logit9B_2019, exponentiate = TRUE)

#10 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit10B_2019 <- svyglm(UN16AB ~ HHAGEx, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10B_2019)
tbl_regression(weighted_logit10B_2019, exponentiate = TRUE)

#11 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit11B_2019 <- svyglm(UN16AB ~ MSTATUS, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11B_2019)
tbl_regression(weighted_logit11B_2019, exponentiate = TRUE)

#12 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit12B_2019 <- svyglm(UN16AB ~ HH51_grouped, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit12B_2019)
tbl_regression(weighted_logit12B_2019, exponentiate = TRUE)

#13 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit13B_2019 <- svyglm(UN16AB ~ HH52_grouped, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit13B_2019)
tbl_regression(weighted_logit13B_2019, exponentiate = TRUE)

# First regression table
table1B <- weighted_logitB_2019 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2B <- weighted_logit2B_2019 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3B <- weighted_logit3B_2019 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4B <- weighted_logit4B_2019 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5B <- weighted_logit5B_2019 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6B <- weighted_logit6B_2019 %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7B <- weighted_logit7B_2019 %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8B <- weighted_logit8B_2019 %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9B <- weighted_logit9B_2019 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10B <- weighted_logit10B_2019 %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table11B <- weighted_logit11B_2019 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12B <- weighted_logit12B_2019 %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children under age 5"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13B <- weighted_logit13B_2019 %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children age 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_table1AB <- tbl_stack(
  tbls = list(table1B, table2B, table3B, table4B, table5B, table6B, table7B, table8B, table9B, table10B, table11B, table12B, table13B)
)
# Convert the gtsummary table to a gt table
stacked_gtAB <- as_gt(stacked_table1AB) %>%
  gt::tab_header("Staying in a separate room")
# Save the gt table as an image
gtsave(stacked_gtAB, "bivariate_staying_in_separate_room.png")

###########################################################################################
#Staying in the cowshed UN16AC
#1. Let's make Sudoorpraschim the desired reference category
merged_data_2019$HH7 <- relevel(merged_data_2019$HH7, ref = "SUDOORPASCHIM PROVINCE")
# Verify the releveling
levels(merged_data_2019$HH7)
# Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logitC_2019 <- svyglm(UN16AC ~ HH7, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitC_2019)
tbl_regression(weighted_logitC_2019, exponentiate = TRUE)

#2. Let's make Hindu the desired reference category
merged_data_2019$HC1A <- relevel(merged_data_2019$HC1A, ref = "HINDU")
# Verify the releveling
levels(merged_data_2019$HC1A)
# Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit2C_2019 <- svyglm(UN16AC ~ HC1A, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2C_2019)
tbl_regression(weighted_logit2C_2019, exponentiate = TRUE)

#3. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit3C_2019 <- svyglm(UN16AC ~ Ethnicity, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3C_2019)
tbl_regression(weighted_logit3C_2019, exponentiate = TRUE)

#4. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit4C_2019 <- svyglm(UN16AC ~ windex5r, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4C_2019)
tbl_regression(weighted_logit4C_2019, exponentiate = TRUE)

#5. Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit5C_2019 <- svyglm(UN16AC ~ HHSEX, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5C_2019)
tbl_regression(weighted_logit5C_2019, exponentiate = TRUE)

#6. Let's make Yes the desired reference category
merged_data_2019$HC15 <- relevel(merged_data_2019$HC15, ref = "YES")
#Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit6C_2019 <- svyglm(UN16AC ~ HC15, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6C_2019)
tbl_regression(weighted_logit6C_2019, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit7C_2019 <- svyglm(UN16AC ~ helevel1, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7C_2019)
tbl_regression(weighted_logit7C_2019, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit8C_2019 <- svyglm(UN16AC ~ welevel1, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8C_2019)
tbl_regression(weighted_logit8C_2019, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit9C_2019 <- svyglm(UN16AC ~ WAGE, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9C_2019)
tbl_regression(weighted_logit9C_2019, exponentiate = TRUE)

#10 #Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit10C_2019 <- svyglm(UN16AC ~ HHAGEx, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10C_2019)
tbl_regression(weighted_logit10C_2019, exponentiate = TRUE)

#11 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit11C_2019 <- svyglm(UN16AC ~ MSTATUS, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11C_2019)
tbl_regression(weighted_logit11C_2019, exponentiate = TRUE)

#12 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit12C_2019 <- svyglm(UN16AC ~ HH51_grouped, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit12C_2019)
tbl_regression(weighted_logit12C_2019, exponentiate = TRUE)

#13 Recreate the survey design object
design_2019 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2019)
# Run the weighted logistic regression
weighted_logit13C_2019 <- svyglm(UN16AC ~ HH52_grouped, design = design_2019, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit13C_2019)
tbl_regression(weighted_logit13C_2019, exponentiate = TRUE)

# First regression table
table1C <- weighted_logitC_2019 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2C <- weighted_logit2C_2019 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3C <- weighted_logit3C_2019 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4C <- weighted_logit4C_2019 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5C <- weighted_logit5C_2019 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6C <- weighted_logit6C_2019 %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7C <- weighted_logit7C_2019 %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8C <- weighted_logit8C_2019 %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9C <- weighted_logit9C_2019 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10C <- weighted_logit10C_2019 %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table11C <- weighted_logit11C_2019 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12C <- weighted_logit12C_2019 %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children under age 5"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13C <- weighted_logit13C_2019 %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children age 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_table1AC <- tbl_stack(
  tbls = list(table1C, table2C, table3C, table4C, table5C, table6C, table7C, table8C, table9C, table10C, table11C, table12C, table13C)
)
# Convert the gtsummary table to a gt table
stacked_gtAC <- as_gt(stacked_table1AC) %>%
  gt::tab_header("Staying in the cowshed")
# Save the gt table as an image
gtsave(stacked_gtAC, "bivariate_staying_in_cowshed.png")





















# save the document, run the following by choosing the directory
write.csv(merged_data_2019, file = "/Users/nasib/Desktop/Nepal MICS/Data/clean_data_2019.csv")

