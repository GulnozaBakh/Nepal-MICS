install.packages("dplyr")
install.packages("haven")
install.packages("readr")
install.packages("labelled")
install.packages("survey")
install.packages("kableExtra")
install.packages("stringr")

library(dplyr)
library(haven)
library(readr)
library(labelled)
library(survey)
library(kableExtra)
library(stringr)

#open dataset
data_wm <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/wm.sav")
data_hh <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/hh.sav")
data_ch <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/ch.sav")

# Convert labelled vectors to character vectors
data_hh <- labelled::to_character(data_hh)
data_wm <- labelled::to_character(data_wm)
data_ch <- labelled::to_character(data_ch)

#Explore the data
summary(data_hh)
summary(data_wm)
colnames(data_hh)
colnames(data_wm)
str(data_hh)
str(data_wm)

#Task1. CommonVarsHH. Recode HHAGE into HHAGEx (4 groups)
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

# Recode age into four groups
data_wm <- data_wm %>%
  mutate(
    AgeGroup = case_when(
      WB4 >= 15 & WB4 <= 19 ~ "15-19",
      WB4 >= 20 & WB4 <= 29 ~ "20-29",
      WB4 >= 30 & WB4 <= 39 ~ "30-39",
      WB4 >= 40 & WB4 <= 49 ~ "40-49",
      TRUE ~ NA_character_  # Handle unexpected values and missing data
    )
  )
# Convert AgeGroup to a factor with the appropriate order
data_wm$AgeGroup <- factor(data_wm$AgeGroup, levels = c("15-19", "20-29", "30-39", "40-49"))

# Check the summary to ensure the recoding worked
summary(data_wm$AgeGroup)

# Task2. CommonVarsWM. 
#Recode MSTATUS into MSTATUS2 based on character strings
data_wm <- data_wm %>%
  mutate(
    MSTATUS2 = case_when(
      MSTATUS %in% c("Currently married/in union", "Formerly married/in union") ~ 1,
      MSTATUS == "Never married/in union" ~ 2,
      is.na(MSTATUS) ~ 9,
      TRUE ~ NA_real_  # Handle unexpected values
    ),
    MSTATUS2 = factor(MSTATUS2, levels = c(1, 2, 9), labels = c("Ever married/in union", "Never married/in union", "Missing"))
  )

unique(data_wm$WB6B)
#Recode education for women
data_wm <- data_wm %>%
  mutate(
    WB6B = str_trim(WB6B),  # Remove leading and trailing whitespace
    EducationGroup = case_when(
      WB5 == "NO" ~ "Illiterate",
      WB5 == "YES" & WB6B %in% c("ECE", "CLASS 1", "CLASS 2", "CLASS 3", "CLASS 4", "CLASS 5") ~ "Literate or up to class 5",
      WB5 == "YES" & WB6B %in% c("CLASS 6", "CLASS 7", "CLASS 8", "CLASS 9", "CLASS 10") ~ "Class 6-10",
      WB5 == "YES" & WB6B %in% c("CLASS 11", "CLASS 12") ~ "Class 11-12",
      WB5 == "YES" & WB6B %in% c("BACHELORS", "MASTERS OR ABOVE") ~ "Bachelor’s Degree or above",
      TRUE ~ NA_character_  # Handle unexpected values and missing data
    )
  )
# Convert EducationGroup to a factor with the appropriate order
data_wm$EducationGroup <- factor(data_wm$EducationGroup, 
                                 levels = c("Illiterate", "Literate or up to class 5", "Class 6-10", "Class 11-12", "Bachelor’s Degree or above"))

# Check the summary to ensure the recoding worked
summary(data_wm$EducationGroup)

unique(data_hh$HC2)
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









 





data_wm$DaughtersLivingWithYou <- NULL



