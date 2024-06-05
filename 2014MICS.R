install.packages("dplyr")
install.packages("haven")
install.packages("readr")
install.packages("labelled")
install.packages("survey")
install.packages("kableExtra")

library(dplyr)
library(haven)
library(readr)
library(labelled)
library(survey)
library(kableExtra)

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

#Tables to check household and women clusters
# Summarize the data by counting occurrences to check household clusters (characterized)
data_hh_2014_summary <- data_hh_2014 %>%
  group_by(HH1) %>%
  summarize(
    HH6_counts = n(),
    HH6_unique_values = paste(unique(HH6), collapse = ", "),
    HH6_unique_count = n_distinct(HH6),
    HH7_counts = n(),
    HH7_unique_values = paste(unique(HH7), collapse = ", "),
    HH7_unique_count = n_distinct(HH7)
  )
# Display the table
data_hh_2014_summary %>%
  kbl(caption = "Women's region and type of place of residence by cluster") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Summarize the women data by counting occurrences to check household clusters  (characterized)
data_wm_2014_summary <- data_wm_2014 %>%
  group_by(HH1) %>%
  summarize(
    HH6_counts = n(),
    HH6_unique_values = paste(unique(HH6), collapse = ", "),
    HH6_unique_count = n_distinct(HH6),
    HH7_counts = n(),
    HH7_unique_values = paste(unique(HH7), collapse = ", "),
    HH7_unique_count = n_distinct(HH7)
  )
# Display the table
data_wm_2014_summary %>%
  kbl(caption = "Women's region and type of place of residence by cluster") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#Task 1. CREATE AGGREGATE FILE FROM WOMAN DATA FILE
# Create indicator variables (woman= each number of women in the dataset, cwoman= number of women completed or not completed interviews)
data_wm_2014 <- data_wm_2014 %>%
  mutate(
    woman = 1,
    cwoman = ifelse(WM7 == "Completed", 1, 0)
  )
# Aggregate data
aggregate_data <- data_wm_2014 %>%
  group_by(HH1, HH2) %>%
  summarize(
    totwoman = sum(woman, na.rm = TRUE),
    totcwoman = sum(cwoman, na.rm = TRUE)
  )
# Merge the aggregated women's data with the household data
merged_data <- data_hh_2014 %>%
  left_join(aggregate_data, by = c("HH1", "HH2"))

#the variables changed from 210 to 212, so we can check the column names
colnames(merged_data)

# Save the merged data
write.csv(merged_data, file = "/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2014/Nepal_MICS5_Datasets/merged_data.csv")

# Check for discrepancies between aggregated woman data and hh data
# Check if columns exist (replace `HH12`, `HH13` with actual column names)
if (!("HH12" %in% colnames(merged_data) & "HH13" %in% colnames(merged_data))) {
  stop("Columns HH12 (total women) and/or HH13 (completed interviews women) not found in the household data.")
}

# Create inconsistency flags
merged_data <- merged_data %>%
  mutate(
    badwom   = ifelse(totwoman != HH12, 1, 0),  # Check total women
    badcwom  = ifelse(totcwoman != HH13, 1, 0)   # Check completed women interviews
  )
# Filter for discrepancies (either total or completed interviews)
discrepancies <- merged_data %>%
  filter(badwom == 1 | badcwom == 1)

# Report discrepancies
if (nrow(discrepancies) > 0) {
  cat("MICS5 Listing of inconsistencies between cases reported at household level and within the women's file:\n\n")
  print(discrepancies %>% select(HH1, HH2, HH12, totwoman, HH13, totcwoman)) 
} else {
  cat("No inconsistencies found.\n")
}

#Task 2. CHECK WOMEN'S FILE AGAINST THE HOUSEHOLD FILE
# Create check variable in household data
data_hh_2014$check <- 1

# Sort both datasets
data_hh_2014 <- data_hh_2014 %>% arrange(HH1, HH2)
data_wm_2014 <- data_wm_2014 %>% arrange(HH1, HH2, LN)

# Merge data
merged_data2 <- left_join(data_wm_2014, data_hh_2014, by = c("HH1", "HH2"))

# Identify cases present in women's file but not in household file
discrepancies <- merged_data2 %>%
  filter(is.na(check)) %>%
  select(HH1, HH2, LN)

# Display discrepancies
if (nrow(discrepancies) > 0) {
  cat("Case present in WOMEN file but not in HOUSEHOLD file:\n")
  print(discrepancies)
} else {
  cat("No discrepancies found.\n")
}
#save the merged data 2
write.csv(merged_data2, file = "/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2014/Nepal_MICS5_Datasets/merged_data2.csv")

#Task 3. Household weight checking frequencies
# Check the structure of HH9 in HH data to understand the values it contains
str(data_hh_2014$HH9)
unique(data_hh_2014$HH9)

# Filter the data in column HH9, making sure to handle any potential missing values
filtered_hh_data <- data_hh_2014[data_hh_2014$HH9 == "Completed" & !is.na(data_hh_2014$HH9), ]

# Check the unweighted frequencies for household 
if (nrow(filtered_hh_data) > 0) { #if the filtered data is not empty
  cat('!!! UNWEIGHTED FREQUENCIES FOR HOUSEHOLD !!!\n')
  unweighted_hh_freq <- lapply(filtered_hh_data[c("HH6", "HH7")], table) #counts how often each value in HH6 and HH7 occurs in the dataset
  print(unweighted_hh_freq)
} else {
  cat('No rows match the filtering criteria.\n')
}
# Check the weighted frequencies for household
hh_design <- svydesign(ids = ~1, data = data_hh_2014, weights = ~hhweight) #original dataset is used for weighted calculations 
cat('!!! WEIGHTED FREQUENCIES FOR HOUSEHOLD !!!\n')
weighted_hh_freq <- svytable(~HH6 + HH7, hh_design) #shows how each combination of values in HH6 and HH7 occurs, taking into account survey weights
print(weighted_hh_freq)

#Women weight checking frequencies
# Check the structure of WM7 in WM data to understand the values it contains
str(data_wm_2014$WM7)
unique(data_wm_2014$WM7)

# Filter the data in column WM7, making sure to handle any potential missing values
filtered_wm_data <- data_wm_2014[data_wm_2014$WM7 == "Completed" & !is.na(data_wm_2014$WM7),]

# Check the unweighted frequencies for women 
if (nrow(filtered_wm_data) > 0) {
  cat('!!! UNWEIGHTED FREQUENCIES FOR WOMEN !!!\n')
  unweighted_wm_freq <- lapply(filtered_wm_data[c("HH6", "HH7", "welevel", "WAGE")], table)
  print(unweighted_wm_freq)
} else {
  cat('No rows match the filtering criteria.\n')
}

#Check the weighted frequencies for women 
wm_design <- svydesign(ids = ~1, data = filtered_wm_data, weights = ~wmweight)
cat('!!! WEIGHTED FREQUENCIES FOR WOMEN !!!\n')
weighted_wm_freq_HH6_HH7 <- svytable(~HH6 + HH7, wm_design)
weighted_wm_freq_welevel <- svytable(~welevel, wm_design)
weighted_wm_freq_wage <- svytable(~WAGE, wm_design)

print(weighted_wm_freq_HH6_HH7)
print(weighted_wm_freq_welevel)
print(weighted_wm_freq_wage)

#Task 4. Calculating and appending background variables (perhaps needs to be done before aggregating)
#existing ethnicities in the data
unique(data_hh_2014$HC1C)

# Recode ethnicity (code provided in the syntax files, but how to group the ethnicities?)
data_hh_2014 <- data_hh_2014 %>%
  mutate(ethnicity = case_when(
    HC1C == 1 ~ 1,
    HC1C %in% c(2, 3) ~ 2,
    HC1C == 4 ~ 3,
    HC1C %in% c(8, 9) ~ 9,
    TRUE ~ NA_real_
  )) %>%
  mutate(ethnicity = factor(ethnicity, levels = c(1, 2, 3, 9), labels = c("Group 1", "Group 2", "Group 3", "Missing/DK")))





