" Study Data Analysis - Data Preprocessing
Use only this script to transform the raw data from the study, to produce
files that are fit for statistical analysis and external data repositories.
This includes validity checks, recoding of variable types and values,
merging and aggregation.
"

# Preamble ----
# Clean up 
rm(list = ls())

# Settings
set.seed(42)
options(scipen=2, digits=4)
options(max.print = 9999)

# Libraries
libraries <- c("dplyr", "haven", "labelled", "readxl", "survey", "readr", "psych", 
               "tidyr", "broom", "tableone", "webshot2", "stringr", "purrr")
lapply(libraries, library, character.only=T)

setwd("~/Documents/my documents/Agripath RA/Gender Study/Nepal 2014")

#Load the data
data_hh_2014 <- read_sav("Nepal_MICS5_Datasets/Nepal MICS 2014 SPSS Datasets/hh.sav")
data_wm_2014 <- read_sav("Nepal_MICS5_Datasets/Nepal MICS 2014 SPSS Datasets/wm.sav")

# Convert labelled vectors to character vectors
data_hh_2014 <- labelled::to_character(data_hh_2014)
data_wm_2014 <- labelled::to_character(data_wm_2014)

# mappers for correct names 
source("mappers.R")

#Explore the data
summary(data_hh_2014)
summary(data_wm_2014)
colnames(data_hh_2014)
colnames(data_wm_2014)
str(data_hh_2014)
str(data_wm_2014)

# Select relevant variables from the household dataset
selected_hh_2014 <- data_hh_2014 %>%
  select(HH1, HH2, HH6, HH7, SL1, stratum,HC1A, HC1C, HC11, HHSEX, helevel, 
         hhweight, windex5r, PSU)

# Select relevant variables from the women's dataset
selected_wm_2014 <- data_wm_2014 %>%
  select(HH1, HH2, UN13AA, UN13AB, UN13AC, UN13AD, UN13AE, UN13AF, UN13AG, 
         WAGE, WM7, MSTATUS, welevel, wmweight, CM5B)

# Merge the datasets on HH1 and HH2
merged_data_2014 <- merge(selected_hh_2014, selected_wm_2014, by = c("HH1", "HH2")); 
rm(selected_hh_2014); rm(selected_wm_2014)

# Use subset to filter only cases where the interview is completed and only rural areas
merged_data_2014 <- subset(merged_data_2014, WM7 == "Completed" & HH6 == "Rural")

# Function to clean the data (Replacing "MISSING" with NA, trimming leading and 
# trailing whitespace, removing any non-printable characters)
clean_data <- function(df, columns) {
  df %>%
    mutate(across(all_of(columns), 
                  ~ toupper(str_trim(gsub("[^[:print:]]", "", .))))) %>%
    mutate(across(all_of(columns), ~ na_if(., "MISSING")))
}

# Specify the columns to clean
columns_to_clean <- c("UN13AA", "UN13AB", "UN13AC", "UN13AD", "UN13AE", "UN13AF", "UN13AG", "HC11")

# Apply the function (Clean data)
merged_data_2014 <- clean_data(merged_data_2014, columns_to_clean)

# Filter out rows with NA in any of the selected columns
#merged_data_2014 <- merged_data_2014 %>%
 # filter(complete.cases(select(., all_of(columns_to_clean))))
# Verify the filtering
#summary(merged_data_2014)

#convert Yes and NO to 1 and 0 in the practices columns
merged_data_2014 <- merged_data_2014 %>%
  mutate(across(c(UN13AA, UN13AB, UN13AC, UN13AD, UN13AE, UN13AF, UN13AG), 
                ~ ifelse(. == "YES", 1, ifelse(. == "NO", 0, NA))))

# Combine low count categories for demonstration (Religion)
merged_data_2014$HC1A <- with(merged_data_2014, ifelse(HC1A %in% c("Sikh", "No religion", 
                                  "Others", "Prakriti", "Bon", "Kirat"), "Other", HC1A))
#merged_data_2014$HC1A <- factor(merged_data_2014$HC1A)

#remove don't know and missing (total 5) from ethnicity answers
#merged_data_2014 <- merged_data_2014 %>%
  #filter(!HC1C %in% c("Don't know", "Missing"))

# Apply the mapping to create a new variable
merged_data_2014 <-merged_data_2014 %>%
  mutate(Ethnicity = sapply(HC1C, map_ethnicity1))

# Define the breaks and labels for the groups of the number of children SL1
breaks <- c(-Inf, 0, 1, 2, 3, Inf)  # Group boundaries
labels <- c("0", "1", "2", "3", "4 or more")  # Group labels

# Create the grouped variable
merged_data_2014$SL1_group <- cut(merged_data_2014$SL1, breaks = breaks, labels = labels, right = TRUE)

# Find the mode of the HC11 column (we have 2 missing values for owns hh land variable, so it is better to replace the missing values with the mode)
#mode_HC11 <- names(sort(table(merged_data_2014$HC11), decreasing = TRUE))[1]
#print(mode_HC11)
# Impute "Missing" values with the mode
#merged_data_2014$HC11[merged_data_2014$HC11 == "Missing"] <- mode_HC11

# Convert multiple columns to factors in one line
merged_data_2014 <- merged_data_2014 %>%
  mutate(across(c(HC11, HHSEX, windex5r, welevel, HH6, HH7, Ethnicity, HC1A, WAGE), as.factor))

# Find the mode of the helevel column excluding "Missing/DK" (inside of education 
# of HH head there are 17 missing values, we can impute them into the mode)
mode_helevel <- names(sort(table(merged_data_2014$helevel[merged_data_2014$helevel != "Missing/DK"]), decreasing = TRUE))[1]
print(mode_helevel)  # This should print the most frequent education level
# Impute "Missing/DK" values with the mode
merged_data_2014$helevel[merged_data_2014$helevel == "Missing/DK"] <- mode_helevel
# Convert helevel back to factor
merged_data_2014$helevel <- factor(merged_data_2014$helevel)

#Group MSTATUS into two groups
merged_data_2014$MSTATUS <- ifelse(merged_data_2014$MSTATUS %in% c("Currently married/in union", 
                                  "Formerly married/in union"), "Ever Married", "Never Married")
# Convert the new variable to a factor
merged_data_2014$MSTATUS <- factor(merged_data_2014$MSTATUS, levels = c("Ever Married", "Never Married"))

# Trim whitespace from the levels
merged_data_2014$HH7 <- trimws(merged_data_2014$HH7)
merged_data_2014$WAGE <- trimws(merged_data_2014$WAGE)

# Convert factor levels with the cleaned data
merged_data_2014 <- within(merged_data_2014, {
  HH7 <- factor(HH7,
                levels = c("MId-Western Mountain", "Central Hill", "Central Mountain",
                           "Central Terai", "Eastern Hill", "Eastern Mountain", "Eastern Terai",
                           "Far-Western Hill", "Far-Western Mountain", "Far-WesternTerai",
                           "MId-Western Hill", "MId-WesternTerai", "Western  Hill", "Western  Terai",
                           "Western Mountain"),
                labels = c("Mid Western Mountain", "Central Hill", "Central Mountain", "Central Terai",
                           "Eastern Hill", "Eastern Mountain", "Eastern Terai", "Far Western Hill",
                           "Far Western Mountain", "Far Western Terai", "Mid Western Hill", "Mid Western Terai",
                           "Western  Hill", "Western  Terai", "Western Mountain"))
  
  WAGE <- factor(WAGE,
                 levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
                 labels = c("15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49"))
  
  windex5r <- factor(windex5r,
                     levels = c("Poorest", "Second", "Middle", "Fourth", "Richest"),
                     labels = c("Poorest", "Second", "Middle", "Fourth", "Richest"))
  
  HC11 <- factor(HC11,
                 levels = c("YES", "NO"),
                 labels = c("Yes", "No"))
  
  helevel <- factor(helevel,
                    levels = c("None", "Primary", "Secondary", "Higher"),
                    labels = c("None", "Primary", "Secondary", "Higher"))
  
  welevel <- factor(welevel,
                    levels = c("None", "Primary", "Secondary", "Higher"),
                    labels = c("None", "Primary", "Secondary", "Higher"))
})


# Store
save(merged_data_2014, file="2014_clean_data.RData")  # cleaned data 2014 for furthere analysis

