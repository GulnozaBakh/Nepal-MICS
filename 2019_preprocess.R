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

setwd("~/Documents/my documents/Agripath RA/Gender Study/Nepal 2019")

#Load the data
data_wm_2019 <- read_sav("Nepal 2019/Nepal MICS6 SPSS Datasets/wm.sav")
data_hh_2019 <- read_sav("Nepal 2019/Nepal MICS6 SPSS Datasets/hh.sav")

# Convert labelled vectors to character vectors
data_hh_2019 <- labelled::to_character(data_hh_2019)
data_wm_2019 <- labelled::to_character(data_wm_2019)

# Mappers for correct names 
source("mappers.R")

# Select relevant variables from the household dataset
selected_hh_2019 <- data_hh_2019 %>%
  select(HH1, HH2, HH6, HH7, HH51, HH52, HC1A, HC2, HC15, HHSEX, HHAGE, helevel1, 
         hhweight, windex5r, PSU, stratum)

# Select relevant variables from the women's dataset
selected_wm_2019 <- data_wm_2019 %>%
  select(HH1, HH2, UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG,UN16AH, 
         WAGE, WM17, MSTATUS, welevel1, wmweight, CM4)

# Merge the datasets on HH1 and HH2
merged_data_2019 <- merge(selected_hh_2019, selected_wm_2019, by = c("HH1", "HH2")); 
rm(selected_hh_2019); rm(selected_wm_2019)

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
columns_to_clean1 <- c("UN16AA", "UN16AB", "UN16AC", "UN16AD", "UN16AE", "UN16AF", "UN16AG", "UN16AH", "HC15")

# Clean the data
merged_data_2019 <- clean_data1(merged_data_2019, columns_to_clean1)

#convert Yes and NO to 1 and 0 in the practices columns
merged_data_2019 <- merged_data_2019 %>%
  mutate(across(c(UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG, UN16AH),
                ~ ifelse(. == "YES", 1, ifelse(. == "NO", 0, NA))))

# Combine low count categories for demonstration (Religion)
merged_data_2019$HC1A <- with(merged_data_2019, ifelse(HC1A %in% c("BON", "NO RELIGION", 
                                 "OTHERS", "PRAKRITI", "JAIN", "KIRAT"), "OTHER", HC1A))

# Apply the mapping to create a new variable
merged_data_2019 <-merged_data_2019 %>%
  mutate(Ethnicity = sapply(HC2, map_ethnicity2))
merged_data_2019$Ethnicity <- factor(merged_data_2019$Ethnicity)

# Grouping the number of children For HH51
merged_data_2019$HH51_grouped <- cut(merged_data_2019$HH51, 
                                     breaks = c(-Inf, 0, 1, 2, Inf), 
                                     labels = c("0", "1", "2", "3 and more"))

# For HH52
merged_data_2019$HH52_grouped <- cut(merged_data_2019$HH52, 
                                     breaks = c(-Inf, 0, 1, 2, Inf), 
                                     labels = c("0", "1", "2", "3 and more"))

# Recode HHAGE into HHAGEx (4 groups)
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
      HHAGE >= 70 ~ 9))

# Convert HHAGEx to a factor with appropriate labels
merged_data_2019$HHAGEx <- factor(merged_data_2019$HHAGEx, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                            labels = c("15 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                                       "45 to 49", "50 to 59", "60 to 69", "70 plus"))

#Group MSTATUS into two groups
merged_data_2019$MSTATUS <- ifelse(merged_data_2019$MSTATUS %in% c("Currently married/in union", 
                                     "Formerly married/in union"), "Ever Married", "Never Married")
# Convert the new variable to a factor
merged_data_2019$MSTATUS <- factor(merged_data_2019$MSTATUS, levels = c("Ever Married", "Never Married"))

# Convert multiple columns to factors in one line
merged_data_2019 <- merged_data_2019 %>%
  mutate(across(c(HH6, HH7, HHSEX, HC15, helevel1, windex5r, stratum, welevel1, WAGE, HC1A), as.factor))

# Trim whitespace from the levels
merged_data_2019$helevel1 <- trimws(merged_data_2019$helevel1)
merged_data_2019$welevel1 <- trimws(merged_data_2019$welevel1)
merged_data_2019$WAGE <- trimws(merged_data_2019$WAGE)

# Convert factor levels with the cleaned data
merged_data_2019 <- within(merged_data_2019, {
  helevel1 <- factor(helevel1, 
                     levels = c("None", "Basic (Gr 1-8)", "Secondary (Gr 9-12)", "Higher") ,
                     labels = c("None", "Basic", "Secondary", "Higher")) 
  welevel1 <- factor(welevel1, 
                     levels = c("None", "Basic (Gr 1-8)", "Secondary (Gr 9-12)", "Higher") ,
                     labels = c("None", "Basic", "Secondary", "Higher"))
  WAGE <- factor(WAGE, 
                 levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") ,
                 labels = c("15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49"))
  HH7 <- factor(HH7,
                levels = c("SUDOORPASCHIM PROVINCE", "PROVINCE NO. 1", "PROVINCE NO. 2", "PROVINCE NO. 3", 
                           "GANDAKI PROVINCE", "PROVINCE NO. 5", "KARNALI PROVINCE"),
                labels = c("Sudoorpaschim Province", "Province No. 1", "Province No. 2", "Province No. 3", 
                           "Gandaki Province", "Province No. 5", "Karnali Province"))
  HC1A <- factor(HC1A, 
                 levels = c("HINDU", "BUDDHIST", "CHRISTIAN", "ISLAM", "OTHER"),
                 labels = c("Hindu", "Buddhist", "Christian", "Islam", "Other"))
  HC15 <- factor(HC15,
                 levels = c("YES", "NO"),
                 labels = c("Yes", "No"))
  windex5r <- factor(windex5r,
                     levels = c("Poorest", "Second", "Middle", "Fourth", "Richest"),
                     labels = c("Poorest", "Second", "Middle", "Fourth", "Richest"))
  
  })


# Store
save(merged_data_2019, file="2019_clean_data.RData")  # cleaned data 2019 for furthere analysis




















