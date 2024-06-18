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
merged_data_2014$HC1A <- with(merged_data_2014, ifelse(HC1A %in% c("Sikh", "No religion", "Others", "Prakriti", "Bon"), "OTHER", HC1A))
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
##########################################
















#Total 15 Regions 
unique(data_hh_2014$HH7)
#[1] "Eastern Mountain"     "Eastern Hill"         "Eastern Terai"        "Central Mountain"    
#[5] "Central Hill"         "Central Terai"        "Western Mountain"     "Western  Hill"       
#[9] "Western  Terai"       "MId-Western Mountain" "MId-Western Hill"     "MId-WesternTerai"    
#[13] "Far-Western Mountain" "Far-Western Hill"     "Far-WesternTerai"   







# save the document, run the following by choosing the directory
write.csv(merged_data_2014, file = "/Users/nasib/Desktop/Nepal MICS/Data/clean_data_2014.csv")

