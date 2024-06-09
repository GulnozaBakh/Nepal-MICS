install.packages("dplyr")
install.packages("haven")
install.packages("labelled")
install.packages("tidyverse")
install.packages("survey")
install.packages("readr")
install.packages("psych")
install.packages("tidyr")
install.packages("broom")
install.packages("tableone")
install.packages("gtsummary")

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

setwd("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019")
 
#open dataset
data_wm <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/wm.sav")
data_hh <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/hh.sav")

head(data_wm)
head(data_hh)
str(data_wm)
str(data_hh)

summary(data_wm)
summary(data_hh)

# Convert labelled vectors to character vectors
data_hh <- labelled::to_character(data_hh)
data_wm <- labelled::to_character(data_wm)

glimpse(data_hh)
glimpse(data_wm)

# Use subset to filter only cases where the interview is completed and only rural areas
filtered_data_wm <- subset(data_wm, WM17 == "COMPLETED" & HH6 == "RURAL")

# Define the mapping of each ethnicity to its group
ethnicity_mapping <- list(
  "Brahman/Chhetri" = c("Brahman - Hill", "Chhetree", "Thakuri", "Sanyasi/Dashnami", "Brahman - Tarai", "Rajput", "Kayastha"),
  "Tarai/Madhesi Other Castes" = c("Teli", "Terai Others", "Rajbansi", "Gangai", "Yadav", "Bantaba", "Jhangad/Dhagar", "Bantar/Sardar", 
                                   "Kewat", "Kahar", "Musahar", "Hajam/Thakur", "Koiri/Kushwaha", "Sonar", "Bangali", "Dhimal", "Danuwar", 
                                   "Tajpuriya", "Kurmi", "Dhanuk", "Meche", "Koche", "Khawas", "Mallaha", "Kori", "Marwadi", "Kalwar", 
                                   "Tatma/Tatwa", "Sarbaria", "Mali", "Dhobi", "Amat", "Baraee", "Dom", "Gaderi/Bhedhar", "Sudhi", "Lohar", 
                                   "Kumhar", "Kanu", "Kathbaniyan", "Nuniya", "Dev", "Khatwe", "Kamar", "Badhaee", "Bin", "Rajbhar", 
                                   "Lodh", "Chidimar"),
  "Dalits" = c("Sarki", "Damai/Dholi", "Dalit Others", "Kami", "Chamar/Harijan/Ram", "Dusadh/Pasawan/Pasi", "Badi"),
  "Newar" = c("Newar"),
  "Janajati" = c("Gharti/Bhujel", "Limbu", "Tamang", "Gurung", "Rai", "Kulung", "Sherpa", "Magar", "Majhi", "Lepcha", "Aathpariya", 
                 "Chamling", "Yakkha", "Hayu", "Jirel", "Chepang/Praja", "Brahmu/Baramo", "Dura", "Thakali", "Kisan", "Chhantyal/Chhantel", 
                 "Bote", "Darai", "Byasi/Sanka", "Mewahang Bala", "Kusunda", "Raji", "Sunuwar", "Ghale", "Pahari", "Natuwa", "Thami", 
                 "Raute", "Gurung"),
  "Muslim" = c("Musalman"),
  "Other" = c("Others")
)

# Function to map ethnicities to groups, including handling NAs
map_ethnicity <- function(ethnicity) {
  if (is.na(ethnicity)) {
    return("Missing")
  }
  for (group in names(ethnicity_mapping)) {
    if (ethnicity %in% ethnicity_mapping[[group]]) {
      return(group)
    }
  }
  return("Other")  # Default to "Other" if no match is found
}

# Apply the mapping to create a new variable
data_hh <- data_hh %>%
  mutate(EthnicityGroup = sapply(HC2, map_ethnicity))

#Recode HHAGE into HHAGEx (4 groups)
data_hh <- data_hh %>%
  mutate(
    HHAGEx = case_when(
      HHAGE >= 15 & HHAGE <= 19 ~ 1,
      HHAGE >= 20 & HHAGE <= 24 ~ 2,
      HHAGE >= 25 & HHAGE <= 49 ~ 3,
      HHAGE >= 50 ~ 4
    )
  )
# Convert HHAGEx to a factor with appropriate labels
data_hh$HHAGEx <- factor(data_hh$HHAGEx, levels = c(1, 2, 3, 4), labels = c("15-19", "20-24", "25-49", "50+"))

# Check the summary to ensure the recoding worked
summary(data_hh$HHAGEx)

# Select relevant variables from the household dataset
selected_data_hh <- data_hh %>%
  select(HH1, HH2, HH6, HH7, HH49, stratum, windex5r, HH51, HH52, HC1A, EthnicityGroup, HC15, helevel1, hhweight, HHAGEx, HHSEX, HHAGE)

# Select relevant variables from the women's dataset
selected_data_wm <- filtered_data_wm %>%
  select(HH1, HH2, UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG, UN16AH, WAGE, WM17, CM4, MSTATUS, welevel1, wmweight, WB4)

# Merge the datasets on HH1 and HH2
merged_data_new <- merge(selected_data_hh, selected_data_wm, by = c("HH1", "HH2"))

# Filter out rows with "No response" in any of the specified variables
updated_data <- merged_data_new %>%
  filter(
    UN16AA != "No response",
    UN16AB != "No response",
    UN16AC != "No response",
    UN16AD != "No response",
    UN16AE != "No response",
    UN16AF != "No response",
    UN16AG != "No response",
    UN16AH != "No response"
  )

# Verify the filtering
summary(updated_data)

# Define the columns related to practices
practice_columns_wm <- c("UN16AA", "UN16AB", "UN16AC", "UN16AD", "UN16AE", "UN16AF", "UN16AG", "UN16AH")

# Filter to keep only the defined columns
practices <- merged_data_new %>%
  select(all_of(practice_columns_wm)) 

# Make a copy of the data
data_numeric <- practices

# Replace "NO RESPONSE" with NA
practices[practices == "NO RESPONSE"] <- NA

# Replace "YES" with 1 and "NO" with 0
data_numeric <- practices
data_numeric[data_numeric == "YES"] <- 1
data_numeric[data_numeric == "NO"] <- 0

# Convert to numeric, ensuring NA values are preserved
data_numeric <- as.data.frame(lapply(data_numeric, function(x) as.numeric(as.character(x))))

# Check the summary to ensure conversion worked correctly
summary(data_numeric)
# Sum the columns
column_sums <- colSums(data_numeric, na.rm = TRUE)
print(column_sums)
#Table for the number of women following practices in different regions
xtabs(~UN16AA + HH7, data=merged_data_new)

# Combine low count categories for demonstration
merged_data_new$HC1A_combined <- with(merged_data_new, ifelse(HC1A %in% c("JAIN", "NO RELIGION", "OTHERS", "PRAKRITI", "BON", "KIRAT"), "OTHER", HC1A))
# Create a new grouped variable for CM4
merged_data_new$CM4_grouped <- ifelse(merged_data_new$CM4 >= 3, "More than 2", as.character(merged_data_new$CM4))
# Convert the new variable to a factor
merged_data_new$CM4_grouped <- factor(merged_data_new$CM4_grouped, levels = c("0", "1", "2", "More than 2"))
#Group MSTATUS into two groups
merged_data_new$MSTATUS_grouped <- ifelse(merged_data_new$MSTATUS %in% c("Currently married/in union", "Formerly married/in union"), "Ever Married", "Never Married")
# Convert the new variable to a factor
merged_data_new$MSTATUS_grouped <- factor(merged_data_new$MSTATUS_grouped, levels = c("Ever Married", "Never Married"))

# Count the number of NA values in each column
na_counts <- colSums(is.na(merged_data_new))
# Print the counts
print(na_counts)

# Create a new variable HH51_grouped in the dataset
merged_data_new$HH51_grouped <- ifelse(merged_data_new$HH51 > 2, "3+", as.character(merged_data_new$HH51))
# Convert to a factor with meaningful levels
merged_data_new$HH51_grouped <- factor(merged_data_new$HH51_grouped, levels = c("0", "1", "2", "3+"))
# Check the distribution of the new variable
table(merged_data_new$HH51_grouped)
# Create a new variable HH51_grouped in the dataset
merged_data_new$HH52_grouped <- ifelse(merged_data_new$HH52 > 3, "4+", as.character(merged_data_new$HH52))
# Convert to a factor with meaningful levels
merged_data_new$HH52_grouped <- factor(merged_data_new$HH52_grouped, levels = c("0", "1", "2", "3", "4+"))
# Check the distribution of the new variable
table(merged_data_new$HH52_grouped)

# Create the survey design object
survey_design <- svydesign(
  id = ~HH1,               # Primary sampling unit
  strata = ~stratum,       # Stratification variable
  weights = ~wmweight,     # Women's weight variable
  data = merged_data_new     # Filtered data
)

summary(survey_design)
svymean(~WB4, survey_design) #mean age of women

# Using table to count the number of each factor level including NA
count_un16aa <- table(addNA(updated_data$UN16AH))
print(count_un16aa)
         
# Select the desired variables including the new summary index
d <- merged_data_new %>% 
  select(stratum, windex5r, HH51_grouped, HH52_grouped, EthnicityGroup, HC1A_combined, HC15, helevel1, HHAGEx, HHSEX, menstrual_index, WAGE, welevel1, MSTATUS_grouped, CM4_grouped)

# Create the summary table
tbl_summary(d)

#####################
# Convert "YES" to 1 and "NO" to 0 in column UN16AA
merged_data_new$UN16AA <- ifelse(merged_data_new$UN16AA == "YES", 1, 
                                 ifelse(merged_data_new$UN16AA == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AA)

# Convert "YES" to 1 and "NO" to 0 in column UN16AB
merged_data_new$UN16AB <- ifelse(merged_data_new$UN16AB == "YES", 1, 
                                 ifelse(merged_data_new$UN16AB == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AB)

# Convert "YES" to 1 and "NO" to 0 in column UN16AC
merged_data_new$UN16AC <- ifelse(merged_data_new$UN16AC == "YES", 1, 
                                 ifelse(merged_data_new$UN16AC == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AC)

# Convert "YES" to 1 and "NO" to 0 in column UN16AC
merged_data_new$UN16AD <- ifelse(merged_data_new$UN16AD == "YES", 1, 
                                 ifelse(merged_data_new$UN16AD == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AD)
# Convert "YES" to 1 and "NO" to 0 in column UN16AE
merged_data_new$UN16AE <- ifelse(merged_data_new$UN16AE == "YES", 1, 
                                 ifelse(merged_data_new$UN16AE == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AE)
# Convert "YES" to 1 and "NO" to 0 in column UN16AF
merged_data_new$UN16AF <- ifelse(merged_data_new$UN16AF == "YES", 1, 
                                 ifelse(merged_data_new$UN16AF == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AF)
# Convert "YES" to 1 and "NO" to 0 in column UN16AG
merged_data_new$UN16AG <- ifelse(merged_data_new$UN16AG == "YES", 1, 
                                 ifelse(merged_data_new$UN16AG == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AG)
# Convert "YES" to 1 and "NO" to 0 in column UN16AH
merged_data_new$UN16AH <- ifelse(merged_data_new$UN16AH == "YES", 1, 
                                 ifelse(merged_data_new$UN16AH == "NO", 0, NA))
# Check the transformation
unique(merged_data_new$UN16AH)

# 1. Create a weighted contingency table for chi-squared test
contingency_table <- svytable(~UN16AA + stratum, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table <- round(contingency_table)
# Print the contingency table
print(rounded_contingency_table)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + stratum, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 2. Create a weighted contingency table for chi-squared test
contingency_table2 <- svytable(~UN16AA + windex5r, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table2 <- round(contingency_table2)
# Print the contingency table
print(rounded_contingency_table2)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + windex5r, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 3. Create a weighted contingency table for chi-squared test
contingency_table3 <- svytable(~UN16AA +HH51, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table3 <- round(contingency_table3)
# Print the contingency table
print(rounded_contingency_table3)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + HH51, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 4. Create a weighted contingency table for chi-squared test
contingency_table4 <- svytable(~UN16AA +HH52, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table4 <- round(contingency_table4)
# Print the contingency table
print(rounded_contingency_table4)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + HH52, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 5. Create a weighted contingency table for chi-squared test
contingency_table5 <- svytable(~UN16AA +HC1A_combined, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table5 <- round(contingency_table5)
# Print the contingency table
print(rounded_contingency_table5)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + HC1A_combined, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 6. Create a weighted contingency table for chi-squared test
contingency_table6 <- svytable(~UN16AA + EthnicityGroup, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table6 <- round(contingency_table6)
# Print the contingency table
print(rounded_contingency_table6)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + EthnicityGroup, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 7. Create a weighted contingency table for chi-squared test
contingency_table7 <- svytable(~UN16AA + HC15, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table7 <- round(contingency_table7)
# Print the contingency table
print(rounded_contingency_table7)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + HC15, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 8. Create a weighted contingency table for chi-squared test
contingency_table8 <- svytable(~UN16AA + helevel1, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table8 <- round(contingency_table8)
# Print the contingency table
print(rounded_contingency_table8)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + helevel1, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 9. Create a weighted contingency table for chi-squared test
contingency_table9 <- svytable(~UN16AA + HHAGEx, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table9 <- round(contingency_table9)
# Print the contingency table
print(rounded_contingency_table9)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + HHAGEx, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 10. Create a weighted contingency table for chi-squared test
contingency_table10 <- svytable(~UN16AA + HHSEX, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table10 <- round(contingency_table10)
# Print the contingency table
print(rounded_contingency_table10)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + HHSEX, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 11. Create a weighted contingency table for chi-squared test
contingency_table11 <- svytable(~UN16AA + WAGE, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table11 <- round(contingency_table11)
# Print the contingency table
print(rounded_contingency_table11)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + WAGE, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 12. Create a weighted contingency table for chi-squared test
contingency_table12 <- svytable(~UN16AA + CM4_grouped, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table12 <- round(contingency_table12)
# Print the contingency table
print(rounded_contingency_table12)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + CM4_grouped, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 13. Create a weighted contingency table for chi-squared test
contingency_table13 <- svytable(~UN16AA + MSTATUS_grouped, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table13 <- round(contingency_table13)
# Print the contingency table
print(rounded_contingency_table13)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + MSTATUS_grouped, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)

# 14. Create a weighted contingency table for chi-squared test
contingency_table14 <- svytable(~UN16AA + welevel1, design = survey_design)
# Round the values in the contingency table
rounded_contingency_table14 <- round(contingency_table14)
# Print the contingency table
print(rounded_contingency_table14)
# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + welevel1, design = survey_design)
# Print the Chi-squared test result
print(chi_test_result)




# Run the logistic regression model
logistic1 <- glm(UN16AA ~ stratum, data=merged_data_new, family="binomial")
summary(logistic1)
logistic2 <- glm(UN16AA ~ windex5r, data=merged_data_new, family="binomial")
summary(logistic2)
logistic3 <- glm(UN16AA ~ HH51, data=merged_data_new, family="binomial")
summary(logistic3)
logistic4 <- glm(UN16AA ~ HH52, data=merged_data_new, family="binomial")
summary(logistic4)
logistic5 <- svyglm(formula = UN16AA ~ HC1A_combined, design = survey_design, family = quasibinomial())
summary(logistic5)
logistic6 <- svyglm(formula = UN16AA ~ EthnicityGroup, design = survey_design, family = quasibinomial())
summary(logistic6)
logistic7 <- glm(UN16AA ~ HC15, data=merged_data_new, family="binomial")
summary(logistic7)

logistic_survey1 <- svyglm(UN16AA ~ HC1A, design = survey_design, family = quasibinomial())
summary(logistic_survey1)

logistic7 <- glm(UN16AA ~ HC1A, data=merged_data_new, family="binomial")
summary(logistic7)


exp(coef(logistic1)) #if you need to export odds ratio




#Preparing the data for logistic regression: create factors
attr(merged_data_new$stratum, "label") == "Region"




  
    
#Export data to csv
write.csv(data_wm, file = "/Users/nasib/Desktop/data_wm.csv")
write.csv(data_hh, file = "/Users/nasib/Desktop/data_hh.csv")
write.csv(data_hl, file = "/Users/nasib/Desktop/data_hl.csv")
write.csv(merged_data_hh_wm, file = "/Users/nasib/Desktop/merged_data_updated.csv")

# Inspect the merged data
head(merged_data)
str(merged_data)

# Remove the column HHAGEx column
data_hh$HHAGEx <- NULL 
rm(merged_data_hh_wm)


xtabs(~UN16AA + HH7, data=merged_data_new)
# Count the number of occurrences of each category in the WM17 column
table(merged_data_hh_wm$WM17)
table(merged_data_hh_wm$HH6)

logistic_survey2 <- svyglm(UN16AA ~ windex5r, design = survey_design, family = quasibinomial())
summary(logistic_survey2)