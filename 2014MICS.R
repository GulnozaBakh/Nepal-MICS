install.packages("dplyr")
install.packages("haven")
install.packages("readr")
install.packages("labelled")
install.packages("survey")
install.packages("kableExtra")

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
data_hh_2014 <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2014/Nepal_MICS5_Datasets/Nepal MICS 2014 SPSS Datasets/hh.sav")
data_wm_2014 <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2014/Nepal_MICS5_Datasets/Nepal MICS 2014 SPSS Datasets/wm.sav")

# Convert labelled vectors to character vectors
data_hh_2014 <- labelled::to_character(data_hh_2014)
data_wm_2014 <- labelled::to_character(data_wm_2014)

#Explore the data
summary(data_hh_2014)
summary(data_wm_2014)
colnames(data_hh_2014)
colnames(data_wm_2014)
str(data_hh_2014)
str(data_wm_2014)

# Select relevant variables from the household dataset
selected_hh_2014 <- data_hh_2014 %>%
  select(HH1, HH2, HH6, HH7, SL1, stratum,HC1A, HC1C, HC11, HHSEX, helevel, hhweight, windex5r, PSU)

# Select relevant variables from the women's dataset
selected_wm_2014 <- data_wm_2014 %>%
  select(HH1, HH2, UN13AA, UN13AB, UN13AC, UN13AD, UN13AE, UN13AF, UN13AG, WAGE, WM7, MSTATUS, welevel, wmweight, CM5B)

# Merge the datasets on HH1 and HH2
merged_data_2014 <- merge(selected_hh_2014, selected_wm_2014, by = c("HH1", "HH2"))

# Use subset to filter only cases where the interview is completed and only rural areas
merged_data_2014 <- subset(merged_data_2014, WM7 == "Completed" & HH6 == "Rural")

# Function to clean the data
clean_data <- function(df, columns) {
  df %>%
    mutate(across(all_of(columns), 
                  ~ toupper(str_trim(gsub("[^[:print:]]", "", .))))) %>%
    mutate(across(all_of(columns), ~ na_if(., "MISSING")))
}

# Specify the columns to clean
columns_to_clean <- c("UN13AA", "UN13AB", "UN13AC", "UN13AD", "UN13AE", "UN13AF", "UN13AG")

# Clean the data
merged_data_2014 <- clean_data(merged_data_2014, columns_to_clean)

# Filter out rows with NA in any of the selected columns
merged_data_2014 <- merged_data_2014 %>%
  filter(complete.cases(select(., all_of(columns_to_clean))))
# Verify the filtering
summary(merged_data_2014)

#convert Yes and NO to 1 and 0 in the practices columns
merged_data_2014 <- merged_data_2014 %>%
  mutate(across(c(UN13AA, UN13AB, UN13AC, UN13AD, UN13AE, UN13AF, UN13AG), ~ ifelse(. == "YES", 1, ifelse(. == "NO", 0, NA))))

 # Combine low count categories for demonstration (Religion)
merged_data_2014$HC1A <- with(merged_data_2014, ifelse(HC1A %in% c("Sikh", "No religion", "Others", "Prakriti", "Bon", "Kirat"), "Other", HC1A))
merged_data_2014$HC1A <- factor(merged_data_2014$HC1A)

#remove don't know and missing (total 5) from ethnicity answers
merged_data_2014 <- merged_data_2014 %>%
  filter(!HC1C %in% c("Don't know", "Missing"))

# Define the mapping of each ethnicity to its group
ethnicity_mapping1 <- list(
  "Brahman or Chhetri" = c("Brahman - Hill", "Brahman - Tarai", "Chame", "Chhetree", "Dev", "Hajam/Thakur", "Kayastha", "Rajput", "Sanyasi/Dashnami", "Thakuri"),
  "Tarai or Madhesi Other Castes" = c("Badhaee", "Bantaba", "Bantar/Sardar", "Baraee ", "Bin", "Dhankar/Kharikar", "Haluwai", "Kahar", "Kalwar", "Kanu", "Kathbaniyan", "Kewat", "Khaling", "Koiri/Kushwaha", "Kumhar", "Kurmi", "Lodh", "Lohar", "Mali", "Mallaha", "Nuniya", "Rajbhar", "Sonar", "Sudhi", "Teli", "Terai Others", "Yadav" ),
  "Dalits" = c("Chamar/Harijan/Ram", "Dalit Others", "Damai/Dholi", "Dhobi", "Dusadh/Pasawan/Pasi", "Gaderi/Bhedhar", "Kami", "Khatwe", "Kori", "Musahar", "Rajdhob", "Sarki", "Tatma/Tatwa"),
  "Newar" = c("Newar"),
  "Janajati" = c("Bhote", "Chamling", "Chepang/Praja ", "Chhantyal/Chhantel", "Danuwar ", "Dhanuk", "Dhimal", "Gangai", "Ghale", "Gharti/Bhujel", "Gurung", "Janajati Others", "Jhangad/Dhagar", "Jirel", "Kumal", "Kulung", "Limbu", "Lhomi", "Magar", "Majhi", "Mewahang Bala", "Nachhiring", "Rai", "Rajbansi", "Samgpang", "Satar/Santhal", "Sherpa", "Sunuwar", "Tajpuriya", "Tamang", "Surel", "Thakali", "Thami", "Tharu", "Yakkha", "Thulung", "Yamphu" ),
  "Muslim" = c("Churaute", "Musalman"),
  "Other" = c("Undefined Others")
  )
# Function to map ethnicities to groups, including handling NAs
map_ethnicity1 <- function(ethnicity) {
  if (is.na(ethnicity)) {
    return("Missing")
  }
  for (group in names(ethnicity_mapping1)) {
    if (ethnicity %in% ethnicity_mapping1[[group]]) {
      return(group)
    }
  }
  return("Other")  # Default to "Other" if no match is found
}
# Apply the mapping to create a new variable
merged_data_2014 <-merged_data_2014 %>%
  mutate(Ethnicity = sapply(HC1C, map_ethnicity1))
merged_data_2014$Ethnicity <- factor(merged_data_2014$Ethnicity)

# Convert WAGE to a factor 
merged_data_2014$WAGE <- factor(merged_data_2014$WAGE)

# Define the breaks and labels for the groups
breaks <- c(-Inf, 0, 1, 2, 3, Inf)  # Group boundaries
labels <- c("0", "1", "2", "3", "4 or more")  # Group labels

# Create the grouped variable
merged_data_2014$SL1_group <- cut(merged_data_2014$SL1, breaks = breaks, labels = labels, right = TRUE)

# Find the mode of the HC11 column (we have 2 missing values for owns hh land variable, so it is better to replace the missing values with the mode)
mode_HC11 <- names(sort(table(merged_data_2014$HC11), decreasing = TRUE))[1]
print(mode_HC11)
# Impute "Missing" values with the mode
merged_data_2014$HC11[merged_data_2014$HC11 == "Missing"] <- mode_HC11
# Convert multiple columns to factors in one line
merged_data_2014 <- merged_data_2014 %>%
  mutate(across(c(HC11, HHSEX, windex5r, welevel, HH7), as.factor))

# Find the mode of the helevel column excluding "Missing/DK" (inside of education of HH head there are 17 missing values, we can impute them into the mode)
mode_helevel <- names(sort(table(merged_data_2014$helevel[merged_data_2014$helevel != "Missing/DK"]), decreasing = TRUE))[1]
print(mode_helevel)  # This should print the most frequent education level
# Impute "Missing/DK" values with the mode
merged_data_2014$helevel[merged_data_2014$helevel == "Missing/DK"] <- mode_helevel
# Convert helevel back to factor
merged_data_2014$helevel <- factor(merged_data_2014$helevel)

#Group MSTATUS into two groups
merged_data_2014$MSTATUS <- ifelse(merged_data_2014$MSTATUS %in% c("Currently married/in union", "Formerly married/in union"), "Ever Married", "Never Married")
# Convert the new variable to a factor
merged_data_2014$MSTATUS <- factor(merged_data_2014$MSTATUS, levels = c("Ever Married", "Never Married"))
# save the document, run the following by choosing the directory

####################################Creating a frequency distribution (summary) table on unweighted data
# Reorder the levels of the helevel, welevel, and windex5r variables
merged_data_2014 <- merged_data_2014 %>%
  mutate(
    helevel = fct_relevel(helevel, "None", "Primary", "Secondary", "Higher"),
    welevel = fct_relevel(welevel, "None", "Primary", "Secondary", "Higher"),
    windex5r = fct_relevel(windex5r, "Poorest", "Second", "Middle", "Fourth", "Richest")
  )

# Select the desired variables including the new summary index
d1 <- merged_data_2014 %>% 
  select(HH7, SL1_group, HC1A, Ethnicity, HC11, HHSEX, helevel, windex5r, WAGE, MSTATUS, welevel, UN13AA, UN13AB, UN13AC, UN13AD, UN13AE, UN13AF, UN13AG)

# Example variable labels
variable_labels <- list(
  HH7 = "Region",
  SL1_group = "Number of children aged 1-17",
  HC1A = "Religion",
  Ethnicity = "Ethnicity of Household Head", 
  HC11 = "Owns Agricultural Land", 
  HHSEX = "Sex of Household Head", 
  helevel = "Education of Household Head",
  windex5r = "Rural Wealth Index",
  WAGE = "Age Range of Women",
  welevel = "Education of Women",
  MSTATUS = "Marital Status",
  UN13AA = "Live in different House",
  UN13AB = "Live in different room of the same house ",
  UN13AC = "Staying in animal shed",
  UN13AD = "Eat different type of food",
  UN13AE = "Bath in a separate place",
  UN13AF = "Absent from school or work",
  UN13AG = "Avoid social gatherings"
)
# Create the summary table with custom labels and add a caption
summary_table_2014 <- tbl_summary(
  d1,
  label = variable_labels
) %>%
  modify_caption("**Table 1. Sociodemographic characteristics and the menstrual conditions of women, Nepal, 2014.**")

# Display the summary table
summary_table_2014
#Save it 
summary_gt1 <- as_gt(summary_table_2014)
gtsave(summary_gt1, filename = "summary_table_2014.png")
##########################################Bivariate Regressions
#Some factors inside variables have artithmatic opeartors "-", so we need to rename them
# Step 1: Trim whitespace from the levels
merged_data_2014$HH7 <- trimws(merged_data_2014$HH7)
merged_data_2014$WAGE <- trimws(merged_data_2014$WAGE)
# Step 2: Convert factor levels with the cleaned data
merged_data_2014$HH7<- factor(merged_data_2014$HH7, 
                                   levels = c("MId-Western Mountain", "Central Hill", "Central Mountain", "Central Terai", "Eastern Hill", "Eastern Mountain", "Eastern Terai", "Far-Western Hill", "Far-Western Mountain", "Far-WesternTerai", "MId-Western Hill", "MId-WesternTerai", "Western  Hill", "Western  Terai", "Western Mountain" ) ,
                                   labels = c("Mid Western Mountain", "Central Hill", "Central Mountain", "Central Terai", "Eastern Hill", "Eastern Mountain", "Eastern Terai", "Far Western Hill", "Far Western Mountain", "Far Western Terai", "Mid Western Hill", "Mid Western Terai", "Western  Hill", "Western  Terai", "Western Mountain"))
merged_data_2014$WAGE <- factor(merged_data_2014$WAGE, 
                                   levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") ,
                                   labels = c("15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49"))


#Live in different House UN13AA
#1. Let's make Mid Western Mountain the desired reference category
merged_data_2014$HH7 <- relevel(merged_data_2014$HH7, ref = "Mid Western Mountain")
# Verify the releveling
levels(merged_data_2014$HH7)
# Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitA_2014 <- svyglm(UN13AA ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitA_2014)
tbl_regression(weighted_logitA_2014, exponentiate = TRUE)

#2. Let's make Hindu the desired reference category
merged_data_2014$HC1A <- relevel(merged_data_2014$HC1A, ref = "Hindu")
# Verify the releveling
levels(merged_data_2014$HC1A)
# Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit2A_2014 <- svyglm(UN13AA ~ HC1A, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2A_2014)
tbl_regression(weighted_logit2A_2014, exponentiate = TRUE)

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit3A_2014 <- svyglm(UN13AA ~ Ethnicity, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3A_2014)
tbl_regression(weighted_logit3A_2014, exponentiate = TRUE)

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit4A_2014 <- svyglm(UN13AA ~ windex5r, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4A_2014)
tbl_regression(weighted_logit4A_2014, exponentiate = TRUE)

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit5A_2014 <- svyglm(UN13AA ~ HHSEX, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5A_2014)
tbl_regression(weighted_logit5A_2014, exponentiate = TRUE)

#6. Let's make Yes the desired reference category
merged_data_2014$HC11 <- relevel(merged_data_2014$HC11, ref = "Yes")
#Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit6A_2014 <- svyglm(UN13AA ~ HC11, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6A_2014)
tbl_regression(weighted_logit6A_2014, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit7A_2014 <- svyglm(UN13AA ~ helevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7A_2014)
tbl_regression(weighted_logit7A_2014, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit8A_2014 <- svyglm(UN13AA ~ welevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8A_2014)
tbl_regression(weighted_logit8A_2014, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit9A_2014 <- svyglm(UN13AA ~ WAGE, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9A_2014)
tbl_regression(weighted_logit9A_2014, exponentiate = TRUE)

#10 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit10A_2014 <- svyglm(UN13AA ~ MSTATUS, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10A_2014)
tbl_regression(weighted_logit10A_2014, exponentiate = TRUE)

#11 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit11A_2014 <- svyglm(UN13AA ~ SL1_group, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11A_2014)
tbl_regression(weighted_logit11A_2014, exponentiate = TRUE)

# First regression table
table1A <- weighted_logitA_2014 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2A <- weighted_logit2A_2014 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
  
table3A <- weighted_logit3A_2014 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4A <- weighted_logit4A_2014 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5A <- weighted_logit5A_2014 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6A <- weighted_logit6A_2014 %>%
  tbl_regression(label = list(HC11 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7A <- weighted_logit7A_2014 %>%
  tbl_regression(label = list(helevel = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8A <- weighted_logit8A_2014 %>%
  tbl_regression(label = list(welevel = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9A <- weighted_logit9A_2014 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10A <- weighted_logit10A_2014 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11A <- weighted_logit11A_2014 %>%
  tbl_regression(label = list(SL1_group = "Number of Children Aged 1-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_table1A <- tbl_stack(
  tbls = list(table1A, table2A, table3A, table4A, table5A, table6A, table7A, table8A, table9A, table10A, table11A)
)
# Convert the gtsummary table to a gt table
stacked_gtA <- as_gt(stacked_table1A) %>%
  gt::tab_header("Living in a different place")
# Save the gt table as an image
gtsave(stacked_gtA, "living_in_different_place.png")

###########################################################################################
#Live in a different room of the same house
#1. # Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitB_2014 <- svyglm(UN13AB ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitB_2014)
tbl_regression(weighted_logitB_2014, exponentiate = TRUE)

#2. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit2B_2014 <- svyglm(UN13AB ~ HC1A, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2B_2014)
tbl_regression(weighted_logit2B_2014, exponentiate = TRUE)

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit3B_2014 <- svyglm(UN13AB ~ Ethnicity, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3B_2014)
tbl_regression(weighted_logit3B_2014, exponentiate = TRUE)

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit4B_2014 <- svyglm(UN13AB ~ windex5r, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4B_2014)
tbl_regression(weighted_logit4B_2014, exponentiate = TRUE)

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit5B_2014 <- svyglm(UN13AB ~ HHSEX, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5B_2014)
tbl_regression(weighted_logit5B_2014, exponentiate = TRUE)

#6. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit6B_2014 <- svyglm(UN13AB ~ HC11, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6B_2014)
tbl_regression(weighted_logit6B_2014, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit7B_2014 <- svyglm(UN13AB ~ helevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7B_2014)
tbl_regression(weighted_logit7B_2014, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit8B_2014 <- svyglm(UN13AB ~ welevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8B_2014)
tbl_regression(weighted_logit8B_2014, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit9B_2014 <- svyglm(UN13AB ~ WAGE, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9B_2014)
tbl_regression(weighted_logit9B_2014, exponentiate = TRUE)

#10 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit10B_2014 <- svyglm(UN13AB ~ MSTATUS, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10B_2014)
tbl_regression(weighted_logit10B_2014, exponentiate = TRUE)

#11 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit11B_2014 <- svyglm(UN13AB ~ SL1_group, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11B_2014)
tbl_regression(weighted_logit11B_2014, exponentiate = TRUE)

# 2nd regression table
table1B <- weighted_logitB_2014 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2B <- weighted_logit2B_2014 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3B <- weighted_logit3B_2014 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4B <- weighted_logit4B_2014 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5B <- weighted_logit5B_2014 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6B <- weighted_logit6B_2014 %>%
  tbl_regression(label = list(HC11 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7B <- weighted_logit7B_2014 %>%
  tbl_regression(label = list(helevel = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8B <- weighted_logit8B_2014 %>%
  tbl_regression(label = list(welevel = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9B <- weighted_logit9B_2014 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10B <- weighted_logit10B_2014 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11B <- weighted_logit11B_2014 %>%
  tbl_regression(label = list(SL1_group = "Number of Children Aged 1-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableB <- tbl_stack(
  tbls = list(table1B, table2B, table3B, table4B, table5B, table6B, table7B, table8B, table9B, table10B, table11B)
)
# Convert the gtsummary table to a gt table
stacked_gtB <- as_gt(stacked_tableB) %>%
  gt::tab_header("Living in a different room of the same house")
# Save the gt table as an image
gtsave(stacked_gtB, "bivariate_living_in_different_room.png")

###########################################################################################
#Live in animal shed UN13AC
#1. # Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitC_2014 <- svyglm(UN13AC ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitC_2014)
tbl_regression(weighted_logitC_2014, exponentiate = TRUE)

#2. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit2C_2014 <- svyglm(UN13AC ~ HC1A, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2C_2014)
tbl_regression(weighted_logit2C_2014, exponentiate = TRUE)

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit3C_2014 <- svyglm(UN13AC ~ Ethnicity, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3C_2014)
tbl_regression(weighted_logit3C_2014, exponentiate = TRUE)

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit4C_2014 <- svyglm(UN13AC ~ windex5r, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4C_2014)
tbl_regression(weighted_logit4C_2014, exponentiate = TRUE)

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit5C_2014 <- svyglm(UN13AC ~ HHSEX, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5C_2014)
tbl_regression(weighted_logit5C_2014, exponentiate = TRUE)

#6. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit6C_2014 <- svyglm(UN13AC ~ HC11, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6C_2014)
tbl_regression(weighted_logit6C_2014, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit7C_2014 <- svyglm(UN13AC ~ helevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7C_2014)
tbl_regression(weighted_logit7C_2014, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit8C_2014 <- svyglm(UN13AC ~ welevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8C_2014)
tbl_regression(weighted_logit8C_2014, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit9C_2014 <- svyglm(UN13AC ~ WAGE, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9C_2014)
tbl_regression(weighted_logit9C_2014, exponentiate = TRUE)

#10 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit10C_2014 <- svyglm(UN13AC ~ MSTATUS, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10C_2014)
tbl_regression(weighted_logit10C_2014, exponentiate = TRUE)

#11 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit11C_2014 <- svyglm(UN13AC ~ SL1_group, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11C_2014)
tbl_regression(weighted_logit11C_2014, exponentiate = TRUE)

# 3rd regression table
table1C <- weighted_logitC_2014 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2C <- weighted_logit2C_2014 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3C <- weighted_logit3C_2014 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4C <- weighted_logit4C_2014 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5C <- weighted_logit5C_2014 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6C <- weighted_logit6C_2014 %>%
  tbl_regression(label = list(HC11 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7C <- weighted_logit7C_2014 %>%
  tbl_regression(label = list(helevel = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8C <- weighted_logit8C_2014 %>%
  tbl_regression(label = list(welevel = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9C <- weighted_logit9C_2014 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10C <- weighted_logit10C_2014 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11C <- weighted_logit11C_2014 %>%
  tbl_regression(label = list(SL1_group = "Number of Children Aged 1-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableC <- tbl_stack(
  tbls = list(table1C, table2C, table3C, table4C, table5C, table6C, table7C, table8C, table9C, table10C, table11C)
)
# Convert the gtsummary table to a gt table
stacked_gtC <- as_gt(stacked_tableC) %>%
  gt::tab_header("Living in animal shed")
# Save the gt table as an image
gtsave(stacked_gtC, "bivariate_living_in_animal_shed.png")

###########################################################################################
#Eating different type of food UN13AD
#1. # Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitD_2014 <- svyglm(UN13AD ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitD_2014)
tbl_regression(weighted_logitD_2014, exponentiate = TRUE)

#2. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit2D_2014 <- svyglm(UN13AD ~ HC1A, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2D_2014)
tbl_regression(weighted_logit2D_2014, exponentiate = TRUE)

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit3D_2014 <- svyglm(UN13AD ~ Ethnicity, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3D_2014)
tbl_regression(weighted_logit3D_2014, exponentiate = TRUE)

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit4D_2014 <- svyglm(UN13AD ~ windex5r, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4D_2014)
tbl_regression(weighted_logit4D_2014, exponentiate = TRUE)

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit5D_2014 <- svyglm(UN13AD ~ HHSEX, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5D_2014)
tbl_regression(weighted_logit5D_2014, exponentiate = TRUE)

#6. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit6D_2014 <- svyglm(UN13AD ~ HC11, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6D_2014)
tbl_regression(weighted_logit6D_2014, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit7D_2014 <- svyglm(UN13AD ~ helevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7D_2014)
tbl_regression(weighted_logit7D_2014, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit8D_2014 <- svyglm(UN13AD ~ welevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8D_2014)
tbl_regression(weighted_logit8D_2014, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit9D_2014 <- svyglm(UN13AD ~ WAGE, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9D_2014)
tbl_regression(weighted_logit9D_2014, exponentiate = TRUE)

#10 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit10D_2014 <- svyglm(UN13AD ~ MSTATUS, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10D_2014)
tbl_regression(weighted_logit10D_2014, exponentiate = TRUE)

#11 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit11D_2014 <- svyglm(UN13AD ~ SL1_group, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11D_2014)
tbl_regression(weighted_logit11D_2014, exponentiate = TRUE)

# 4th regression table
table1D <- weighted_logitD_2014 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2D <- weighted_logit2D_2014 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3D <- weighted_logit3D_2014 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4D <- weighted_logit4D_2014 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5D <- weighted_logit5D_2014 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6D <- weighted_logit6D_2014 %>%
  tbl_regression(label = list(HC11 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7D <- weighted_logit7D_2014 %>%
  tbl_regression(label = list(helevel = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8D <- weighted_logit8D_2014 %>%
  tbl_regression(label = list(welevel = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9D <- weighted_logit9D_2014 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10D <- weighted_logit10D_2014 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11D <- weighted_logit11D_2014 %>%
  tbl_regression(label = list(SL1_group = "Number of Children Aged 1-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableD <- tbl_stack(
  tbls = list(table1D, table2D, table3D, table4D, table5D, table6D, table7D, table8D, table9D, table10D, table11D)
)
# Convert the gtsummary table to a gt table
stacked_gtD <- as_gt(stacked_tableD) %>%
  gt::tab_header("Eating different type of food")
# Save the gt table as an image
gtsave(stacked_gtD, "bivariate_eating_different_food.png")

###########################################################################################
#Bathing in a separate place UN13AE
#1. # Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitE_2014 <- svyglm(UN13AE ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitE_2014)
tbl_regression(weighted_logitE_2014, exponentiate = TRUE)

#2. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit2E_2014 <- svyglm(UN13AE ~ HC1A, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2E_2014)
tbl_regression(weighted_logit2E_2014, exponentiate = TRUE)

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit3E_2014 <- svyglm(UN13AE ~ Ethnicity, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3E_2014)
tbl_regression(weighted_logit3E_2014, exponentiate = TRUE)

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit4E_2014 <- svyglm(UN13AE ~ windex5r, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4E_2014)
tbl_regression(weighted_logit4E_2014, exponentiate = TRUE)

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit5E_2014 <- svyglm(UN13AE ~ HHSEX, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5E_2014)
tbl_regression(weighted_logit5E_2014, exponentiate = TRUE)

#6. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit6E_2014 <- svyglm(UN13AE ~ HC11, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6E_2014)
tbl_regression(weighted_logit6E_2014, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit7E_2014 <- svyglm(UN13AE ~ helevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7E_2014)
tbl_regression(weighted_logit7E_2014, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit8E_2014 <- svyglm(UN13AE ~ welevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8E_2014)
tbl_regression(weighted_logit8E_2014, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit9E_2014 <- svyglm(UN13AE ~ WAGE, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9E_2014)
tbl_regression(weighted_logit9E_2014, exponentiate = TRUE)

#10 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit10E_2014 <- svyglm(UN13AE ~ MSTATUS, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10E_2014)
tbl_regression(weighted_logit10E_2014, exponentiate = TRUE)

#11 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit11E_2014 <- svyglm(UN13AE ~ SL1_group, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11E_2014)
tbl_regression(weighted_logit11E_2014, exponentiate = TRUE)

# 5th regression table
table1E <- weighted_logitE_2014 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2E <- weighted_logit2E_2014 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3E <- weighted_logit3E_2014 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4E <- weighted_logit4E_2014 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5E <- weighted_logit5E_2014 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6E <- weighted_logit6E_2014 %>%
  tbl_regression(label = list(HC11 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7E <- weighted_logit7E_2014 %>%
  tbl_regression(label = list(helevel = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8E <- weighted_logit8E_2014 %>%
  tbl_regression(label = list(welevel = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9E <- weighted_logit9E_2014 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10E <- weighted_logit10E_2014 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11E <- weighted_logit11E_2014 %>%
  tbl_regression(label = list(SL1_group = "Number of Children Aged 1-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableE <- tbl_stack(
  tbls = list(table1E, table2E, table3E, table4E, table5E, table6E, table7E, table8E, table9E, table10E, table11E)
)
# Convert the gtsummary table to a gt table
stacked_gtE <- as_gt(stacked_tableE) %>%
  gt::tab_header("Bathing in a separate place")
# Save the gt table as an image
gtsave(stacked_gtE, "bivariate_bathing_in_separate_place.png")

###########################################################################################
#Absent from school/work UN13AF
#1. # Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitF_2014 <- svyglm(UN13AF ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitF_2014)
tbl_regression(weighted_logitF_2014, exponentiate = TRUE)

#2. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit2F_2014 <- svyglm(UN13AF ~ HC1A, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2F_2014)
tbl_regression(weighted_logit2F_2014, exponentiate = TRUE)

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit3F_2014 <- svyglm(UN13AF ~ Ethnicity, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3F_2014)
tbl_regression(weighted_logit3F_2014, exponentiate = TRUE)

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit4F_2014 <- svyglm(UN13AF ~ windex5r, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4F_2014)
tbl_regression(weighted_logit4F_2014, exponentiate = TRUE)

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit5F_2014 <- svyglm(UN13AF ~ HHSEX, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5F_2014)
tbl_regression(weighted_logit5F_2014, exponentiate = TRUE)

#6. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit6F_2014 <- svyglm(UN13AF ~ HC11, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6F_2014)
tbl_regression(weighted_logit6F_2014, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit7F_2014 <- svyglm(UN13AF ~ helevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7F_2014)
tbl_regression(weighted_logit7F_2014, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit8F_2014 <- svyglm(UN13AF ~ welevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8F_2014)
tbl_regression(weighted_logit8F_2014, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit9F_2014 <- svyglm(UN13AF ~ WAGE, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9F_2014)
tbl_regression(weighted_logit9F_2014, exponentiate = TRUE)

#10 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit10F_2014 <- svyglm(UN13AF ~ MSTATUS, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10F_2014)
tbl_regression(weighted_logit10F_2014, exponentiate = TRUE)

#11 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit11F_2014 <- svyglm(UN13AF ~ SL1_group, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11F_2014)
tbl_regression(weighted_logit11F_2014, exponentiate = TRUE)

# 6th regression table
table1F <- weighted_logitF_2014 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2F <- weighted_logit2F_2014 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3F <- weighted_logit3F_2014 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4F <- weighted_logit4F_2014 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5F <- weighted_logit5F_2014 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6F <- weighted_logit6F_2014 %>%
  tbl_regression(label = list(HC11 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7F <- weighted_logit7F_2014 %>%
  tbl_regression(label = list(helevel = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8F <- weighted_logit8F_2014 %>%
  tbl_regression(label = list(welevel = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9F <- weighted_logit9F_2014 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10F <- weighted_logit10F_2014 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11F <- weighted_logit11F_2014 %>%
  tbl_regression(label = list(SL1_group = "Number of Children Aged 1-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableF <- tbl_stack(
  tbls = list(table1F, table2F, table3F, table4F, table5F, table6F, table7F, table8F, table9F, table10F, table11F)
)
# Convert the gtsummary table to a gt table
stacked_gtF <- as_gt(stacked_tableF) %>%
  gt::tab_header("Absent from school/work")
# Save the gt table as an image
gtsave(stacked_gtF, "bivariate_absent_from_school_work.png")

###########################################################################################
#Avoid social gatherings UN13AG
#1. # Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logitG_2014 <- svyglm(UN13AG ~ HH7, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitG_2014)
tbl_regression(weighted_logitG_2014, exponentiate = TRUE)

#2. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit2G_2014 <- svyglm(UN13AG ~ HC1A, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2G_2014)
tbl_regression(weighted_logit2G_2014, exponentiate = TRUE)

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit3G_2014 <- svyglm(UN13AG ~ Ethnicity, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit3G_2014)
tbl_regression(weighted_logit3G_2014, exponentiate = TRUE)

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit4G_2014 <- svyglm(UN13AG ~ windex5r, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit4G_2014)
tbl_regression(weighted_logit4G_2014, exponentiate = TRUE)

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit5G_2014 <- svyglm(UN13AG ~ HHSEX, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit5G_2014)
tbl_regression(weighted_logit5G_2014, exponentiate = TRUE)

#6. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit6G_2014 <- svyglm(UN13AG ~ HC11, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit6G_2014)
tbl_regression(weighted_logit6G_2014, exponentiate = TRUE)

#7 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit7G_2014 <- svyglm(UN13AG ~ helevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit7G_2014)
tbl_regression(weighted_logit7G_2014, exponentiate = TRUE)

#8 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit8G_2014 <- svyglm(UN13AG ~ welevel, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit8G_2014)
tbl_regression(weighted_logit8G_2014, exponentiate = TRUE)

#9 #Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit9G_2014 <- svyglm(UN13AG ~ WAGE, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit9G_2014)
tbl_regression(weighted_logit9G_2014, exponentiate = TRUE)

#10 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit10G_2014 <- svyglm(UN13AG ~ MSTATUS, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit10G_2014)
tbl_regression(weighted_logit10G_2014, exponentiate = TRUE)

#11 Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, nest = TRUE, survey.lonely.psu = "adjust", data = merged_data_2014)
# Run the weighted logistic regression
weighted_logit11G_2014 <- svyglm(UN13AG ~ SL1_group, design = design_2014, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit11G_2014)
tbl_regression(weighted_logit11G_2014, exponentiate = TRUE)

# 7th regression table
table1G <- weighted_logitG_2014 %>%
  tbl_regression(label = list(HH7 = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2G <- weighted_logit2G_2014 %>%
  tbl_regression(label = list(HC1A = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3G <- weighted_logit3G_2014 %>%
  tbl_regression(label = list(Ethnicity = "Ethnicity of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4G <- weighted_logit4G_2014 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5G <- weighted_logit5G_2014 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6G <- weighted_logit6G_2014 %>%
  tbl_regression(label = list(HC11 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7G <- weighted_logit7G_2014 %>%
  tbl_regression(label = list(helevel = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table8G <- weighted_logit8G_2014 %>%
  tbl_regression(label = list(welevel = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table9G <- weighted_logit9G_2014 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
table10G <- weighted_logit10G_2014 %>%
  tbl_regression(label = list(MSTATUS = "Marital Status of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11G <- weighted_logit11G_2014 %>%
  tbl_regression(label = list(SL1_group = "Number of Children Aged 1-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableG <- tbl_stack(
  tbls = list(table1G, table2G, table3G, table4G, table5G, table6G, table7G, table8G, table9G, table10G, table11G)
)
# Convert the gtsummary table to a gt table
stacked_gtG <- as_gt(stacked_tableG) %>%
  gt::tab_header("Avoid social gatherings")
# Save the gt table as an image
gtsave(stacked_gtG, "bivariate_avoid_social_gatherings.png")

##############################################################################################
#Multivariate analysis
#1. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_2014)
different_house <- svyglm(UN13AA ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
summary(different_house)           # for p-values

# Tidy the model results
tidy_different_house <- tidy(different_house, exponentiate = TRUE, conf.int = TRUE)
# Create the summary table using gtsummary
gt_summary <- tbl_regression(different_house, exponentiate = TRUE)
# Add a table caption
gt_summary <- gt_summary %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")
# Print the table
gt_summary

#2. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_2014)
different_room <- svyglm(UN13AB ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
summary(different_room)           # for p-values

# Tidy the model results
tidy_different_room <- tidy(different_room, exponentiate = TRUE, conf.int = TRUE)
# Create the summary table using gtsummary
gt_summary2 <- tbl_regression(different_room, exponentiate = TRUE)
# Add a table caption
gt_summary2 <- gt_summary2 %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")
# Print the table
gt_summary2

#3. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_2014)
animal_shed <- svyglm(UN13AC ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
summary(animal_shed)           # for p-values

# Tidy the model results
tidy_animal_shed <- tidy(animal_shed, exponentiate = TRUE, conf.int = TRUE)
# Create the summary table using gtsummary
gt_summary3 <- tbl_regression(animal_shed, exponentiate = TRUE)
# Add a table caption
gt_summary3 <- gt_summary3 %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")
# Print the table
gt_summary3

#4. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_2014)
eating_different <- svyglm(UN13AD ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
summary(eating_different)           # for p-values

# Tidy the model results
tidy_eating_different <- tidy(eating_different, exponentiate = TRUE, conf.int = TRUE)
# Create the summary table using gtsummary
gt_summary4 <- tbl_regression(eating_different, exponentiate = TRUE)
# Add a table caption
gt_summary4 <- gt_summary4 %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")
# Print the table
gt_summary4

#5. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_2014)
bathing_separate <- svyglm(UN13AE ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
summary(bathing_separate)           # for p-values

# Tidy the model results
tidy_bathing_separate <- tidy(bathing_separate, exponentiate = TRUE, conf.int = TRUE)
# Create the summary table using gtsummary
gt_summary5 <- tbl_regression(bathing_separate, exponentiate = TRUE)
# Add a table caption
gt_summary5 <- gt_summary5 %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")
# Print the table
gt_summary5

#6. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_2014)
absent_school_work <- svyglm(UN13AF ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
summary(absent_school_work)           # for p-values

# Tidy the model results
tidy_absent_school_work <- tidy(absent_school_work, exponentiate = TRUE, conf.int = TRUE)
# Create the summary table using gtsummary
gt_summary6 <- tbl_regression(absent_school_work, exponentiate = TRUE)
# Add a table caption
gt_summary6 <- gt_summary6 %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")
# Print the table
gt_summary6

#7. Recreate the survey design object
design_2014 <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_2014)
avoid_social_gatherings <- svyglm(UN13AG ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
summary(avoid_social_gatherings)           # for p-values

# Tidy the model results
tidy_avoid_social_gatherings <- tidy(avoid_social_gatherings, exponentiate = TRUE, conf.int = TRUE)
# Create the summary table using gtsummary
gt_summary7 <- tbl_regression(avoid_social_gatherings, exponentiate = TRUE)
# Add a table caption
gt_summary7 <- gt_summary7 %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")
# Print the table
gt_summary7

# Function to create a regression table with common formatting
create_regression_table <- function(model, labels) {
  tbl <- tbl_regression(model, 
                        label = labels,
                        exponentiate = TRUE,
                        pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
    bold_p(t = 0.10) %>%
    bold_labels() %>%
    italicize_levels() %>%
    add_significance_stars(hide_ci = FALSE, hide_p = FALSE, pattern = "{estimate} ({conf.low}, {conf.high}){stars}")
  
  # Modify the headers to show OR, CI, and p-values
  tbl <- tbl %>%
    modify_header(
      label = "**Characteristic**",
      estimate = "**OR**",
      ci = "**95% CI**",
      p.value = "**p-value**"
    ) %>%
    modify_table_body(~ .x %>% select(-std.error))
  
  return(tbl)
}

# Create individual regression tables
table1 <- create_regression_table(different_house, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC11 = "Owns Agricultural Land", welevel = "Education of Women", SL1_group = "Number of Children Aged 1-17"))
table2 <- create_regression_table(different_room, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC11 = "Owns Agricultural Land", welevel = "Education of Women", SL1_group = "Number of Children Aged 1-17"))
table3 <- create_regression_table(animal_shed, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC11 = "Owns Agricultural Land", welevel = "Education of Women", SL1_group = "Number of Children Aged 1-17"))

# Combine the tables into one summary table
summary_table <- tbl_merge(
  tbls = list(table1, table2, table3),
  tab_spanner = c("**Living in a different house**", "**Living in a different room**", "**Staying in animal shed**")
)

# Convert the gtsummary table to a gt table
summary_gt <- as_gt(summary_table)

# Save the gt table as an image
gtsave(summary_gt, "multivariate_table1.png")


# Define the regression models
different_house <- svyglm(UN13AA ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
different_room <- svyglm(UN13AB ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")
animal_shed <- svyglm(UN13AC ~ HH7 + HC1A + Ethnicity + windex5r + HC11 + welevel + SL1_group, design = design_2014, family = "quasibinomial")

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
table1 <- create_regression_table(different_house, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC11 = "Owns Agricultural Land", welevel = "Education of Women", SL1_group = "Number of Children Aged 1-17"))
table2 <- create_regression_table(different_room, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC11 = "Owns Agricultural Land", welevel = "Education of Women", SL1_group = "Number of Children Aged 1-17"))
table3 <- create_regression_table(animal_shed, list(HH7 = "Region", HC1A = "Religion of Household Head", Ethnicity = "Ethnicity of Household Head", windex5r = "Rural Wealth Index", HC11 = "Owns Agricultural Land", welevel = "Education of Women", SL1_group = "Number of Children Aged 1-17"))

# Combine the tables into one summary table
summary_table1 <- tbl_merge(
  tbls = list(table1, table2, table3),
  tab_spanner = c("**Living in a different house**", "**Living in a different room**", "**Staying in animal shed**")
)

# Hide the p.value columns after merging
summary_table1 <- summary_table1 %>%
  modify_table_styling(columns = starts_with("p.value"), hide = TRUE)

# Convert the gtsummary table to a gt table
summary_gt1 <- as_gt(summary_table1)

# Save the gt table as an image
gtsave(summary_gt1, "multivariate_table1.png")

#Total 15 Regions 
unique(data_hh_2014$HH7)
#[1] "Eastern Mountain"     "Eastern Hill"         "Eastern Terai"        "Central Mountain"    
#[5] "Central Hill"         "Central Terai"        "Western Mountain"     "Western  Hill"       
#[9] "Western  Terai"       "MId-Western Mountain" "MId-Western Hill"     "MId-WesternTerai"    
#[13] "Far-Western Mountain" "Far-Western Hill"     "Far-WesternTerai"   







# save the document, run the following by choosing the directory
write.csv(merged_data_2014, file = "/Users/nasib/Desktop/Nepal MICS/Data/clean_data_2014.csv")

