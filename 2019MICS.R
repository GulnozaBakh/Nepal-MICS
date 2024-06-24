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
      HHAGE >= 15 & HHAGE <= 19 ~ 1,
      HHAGE >= 20 & HHAGE <= 24 ~ 2,
      HHAGE >= 25 & HHAGE <= 49 ~ 3,
      HHAGE >= 50 ~ 4
    )
  )
# Convert HHAGEx to a factor with appropriate labels
merged_data_2019$HHAGEx <- factor(merged_data_2019$HHAGEx, levels = c(1, 2, 3, 4), labels = c("15 to 19", "20 to 24", "25 to 49", "50 plus"))

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



