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

# Create the survey design object
survey_design <- svydesign(
  id = ~HH1,               # Primary sampling unit
  strata = ~stratum,       # Stratification variable
  weights = ~wmweight,     # Women's weight variable
  data = merged_data_new     # Filtered data
)

summary(survey_design)
svymean(~WB4, survey_design)

# Convert "YES" to 1 and "NO" to 0 in column UN16AA
merged_data_new$UN16AA <- ifelse(merged_data_new$UN16AA == "YES", 1, 
                                 ifelse(merged_data_new$UN16AA == "NO", 0, NA))

# Check the transformation
unique(merged_data_new$UN16AA)

# Re-run the logistic regression model
logistic <- glm(UN16AA ~ MSTATUS, data=merged_data_new, family="binomial")
summary(logistic)
logistic2 <- glm(UN16AA ~ HH7+MSTATUS, data=merged_data_new, family="binomial")

# Print the table with formatted output
print(table1, formatOptions = list(big.mark = ",", digits = 2))

# Create a weighted contingency table for chi-squared test
contingency_table <- svytable(~UN16AA + WAGE, design = survey_design)

# Print the contingency table
print(contingency_table)

# Perform a Chi-squared test on the weighted contingency table
chi_test_result <- svychisq(~UN16AA + stratum, design = survey_design)

# Print the Chi-squared test result
print(chi_test_result)





  
    
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



# Count the number of occurrences of each category in the WM17 column
table(merged_data_hh_wm$WM17)
table(merged_data_hh_wm$HH6)