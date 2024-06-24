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


























# save the document, run the following by choosing the directory
write.csv(merged_data_2019, file = "/Users/nasib/Desktop/Nepal MICS/Data/clean_data_2019.csv")

