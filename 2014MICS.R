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

#Total 15 Regions, which ones to include? 
unique(data_hh_2014$HH7)
#[1] "Eastern Mountain"     "Eastern Hill"         "Eastern Terai"        "Central Mountain"    
#[5] "Central Hill"         "Central Terai"        "Western Mountain"     "Western  Hill"       
#[9] "Western  Terai"       "MId-Western Mountain" "MId-Western Hill"     "MId-WesternTerai"    
#[13] "Far-Western Mountain" "Far-Western Hill"     "Far-WesternTerai"   









