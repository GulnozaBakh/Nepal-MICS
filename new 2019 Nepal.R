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
install.packages("webshot2")

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

# Function to clean the data
clean_data <- function(df, columns) {
  df %>%
    mutate(across(all_of(columns), 
                  ~ toupper(str_trim(gsub("[^[:print:]]", "", .))))) %>%
    mutate(across(all_of(columns), ~ na_if(., "NO RESPONSE")))
}

# Specify the columns to clean
columns_to_clean <- c("UN16AA", "UN16AB", "UN16AC", "UN16AD", "UN16AE", "UN16AF", "UN16AG", "UN16AH")

# Clean the data
filtered_data_wm <- clean_data(filtered_data_wm, columns_to_clean)

# Create a logical condition to filter out rows with "NO RESPONSE" or NA
condition <- !(
  is.na(filtered_data_wm$UN16AA) | is.na(filtered_data_wm$UN16AB) | 
    is.na(filtered_data_wm$UN16AC) | is.na(filtered_data_wm$UN16AD) | 
    is.na(filtered_data_wm$UN16AE) | is.na(filtered_data_wm$UN16AF) | 
    is.na(filtered_data_wm$UN16AG) | is.na(filtered_data_wm$UN16AH)
)

# Use subset to filter the data
updated_data <- subset(filtered_data_wm, condition)

# Verify the filtering
summary(updated_data)

# Define the mapping of each ethnicity to its group
ethnicity_mapping <- list(
  "Brahman or Chhetri" = c("Brahman - Hill", "Chhetree", "Thakuri", "Sanyasi/Dashnami", "Brahman - Tarai", "Rajput", "Kayastha"),
  "Tarai or Madhesi Other Castes" = c("Teli", "Terai Others", "Rajbansi", "Gangai", "Yadav", "Bantaba", "Jhangad/Dhagar", "Bantar/Sardar", 
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
data_hh$HHAGEx <- factor(data_hh$HHAGEx, levels = c(1, 2, 3, 4), labels = c("15-19", "20-24", "25-49", "50 plus"))

# Check the summary to ensure the recoding worked
summary(data_hh$HHAGEx)

# Convert "NO RESPONSE" to NA for the HC15 column
data_hh <- data_hh %>%
  mutate(HC15 = na_if(toupper(str_trim(gsub("[^[:print:]]", "", HC15))), "NO RESPONSE"))

# Remove rows with NA values in HC15
cleaned_data_hh <- subset(data_hh, !is.na(HC15))

# Verify the unique values in HC15 after cleaning
unique(cleaned_data_hh$HC15)

# Select relevant variables from the household dataset
selected_data_hh <- cleaned_data_hh %>%
  select(HH1, HH2, HH6, HH7, HH49, stratum, windex5r, HH51, HH52, HC1A, EthnicityGroup, HC15, helevel1, hhweight, HHAGEx, HHSEX, HHAGE)

# Select relevant variables from the women's dataset
selected_data_wm <- updated_data %>%
  select(HH1, HH2, UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG, UN16AH, WAGE, WM17, CM4, MSTATUS, welevel1, wmweight, WB4)

# Merge the datasets on HH1 and HH2
merged_data_new <- merge(selected_data_hh, selected_data_wm, by = c("HH1", "HH2"))

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
# Create a new variable HH51_grouped in the dataset
merged_data_new$HH51_grouped <- ifelse(merged_data_new$HH51 > 2, "more than 3", as.character(merged_data_new$HH51))
# Convert to a factor with meaningful levels
merged_data_new$HH51_grouped <- factor(merged_data_new$HH51_grouped, levels = c("0", "1", "2", "more than 3"))
# Check the distribution of the new variable
table(merged_data_new$HH51_grouped)
# Create a new variable HH51_grouped in the dataset
merged_data_new$HH52_grouped <- ifelse(merged_data_new$HH52 > 3, "more than 4", as.character(merged_data_new$HH52))
# Convert to a factor with meaningful levels
merged_data_new$HH52_grouped <- factor(merged_data_new$HH52_grouped, levels = c("0", "1", "2", "3", "more than 4"))
# Check the distribution of the new variable
table(merged_data_new$HH52_grouped)

# Convert character variables to factors
merged_data_new <- merged_data_new %>%
  mutate(
    HH6 = as.factor(HH6),
    HH7 = as.factor(HH7),
    stratum = as.factor(stratum),
    windex5r = as.factor(windex5r),
    HC1A = as.factor(HC1A),
    EthnicityGroup = as.factor(EthnicityGroup),
    HC15 = as.factor(HC15),
    helevel1 = as.factor(helevel1),
    HHSEX = as.factor(HHSEX),
    WAGE = as.factor(WAGE),
    WM17 = as.factor(WM17),
    welevel1 = as.factor(welevel1),
    HC1A_combined = as.factor(HC1A_combined)
  )
# Verify the conversion
str(merged_data_new)
#If you want to save the document, run the following by choosing the directory
write.csv(merged_data_new, file = "/Users/nasib/Desktop/Nepal MICS/merged_data_with_NAs_2019.csv")
####################################Creating a frequency distribution (summary) table on unweighted data
# Select the desired variables including the new summary index
d <- merged_data_new %>% 
  select(stratum, windex5r, HH51_grouped, HH52_grouped, EthnicityGroup, HC1A_combined, HC15, helevel1, HHAGEx, HHSEX, WAGE, welevel1, MSTATUS_grouped, UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG, UN16AH)

# Example variable labels
variable_labels <- list(
  stratum = "Region",
  windex5r = "Rural Wealth Index Quintile",
  HH51_grouped = "Number of Children Under Age 5",
  HH52_grouped = "Number of Children Age 5-17",
  EthnicityGroup = "Ethnicity of Household Head",
  MSTATUS_grouped = "Marital Status",
  HC1A_combined = "Religion of Household Head",
  HC15 = "Owns Agricultural Land",
  helevel1 = "Education Level of Household Head",
  HHAGEx = "Age of Household Head",
  HHSEX = "Sex of Household Head",
  WAGE = "Age Range of Women",
  welevel1 = "Education of Women",
  UN16AA = "Staying in a chaupadi/chhapro",
  UN16AB = "Staying in a separate room ",
  UN16AC = "Staying in the cowshed",
  UN16AD = "Eating in a separate place",
  UN16AE = "Bathing in a separate place",
  UN16AF = "Staying away from school or work",
  UN16AG = "Staying away from social gatherings",
  UN16AH = "Staying away from religious work "
)
# Create the summary table with custom labels
summary_table <- tbl_summary(
  d,
  label = variable_labels
)
# Display the summary table
summary_table
#Save it 
summary_gt <- as_gt(summary_table)
gtsave(summary_gt, filename = "summary_table1.png")
##########################################
#convert Yes and NO to 1 and 0 in the practices columns
merged_data_new <- merged_data_new %>%
  mutate(across(c(UN16AA, UN16AB, UN16AC, UN16AD, UN16AE, UN16AF, UN16AG, UN16AH), ~ ifelse(. == "YES", 1, ifelse(. == "NO", 0, NA))))
# Step 1: Trim whitespace from the levels
merged_data_new$helevel1 <- trimws(merged_data_new$helevel1)
merged_data_new$welevel1 <- trimws(merged_data_new$welevel1)
# Step 2: Convert factor levels with the cleaned data
merged_data_new$helevel1 <- factor(merged_data_new$helevel1, 
                                   levels = c("Basic (Gr 1-8)", "Higher", "None", "Secondary (Gr 9-12)") ,
                                   labels = c("Basic", "Higher", "None", "Secondary"))
merged_data_new$welevel1 <- factor(merged_data_new$welevel1, 
                                   levels = c("Basic (Gr 1-8)", "Higher", "None", "Secondary (Gr 9-12)") ,
                                   labels = c("Basic", "Higher", "None", "Secondary"))
merged_data_new$HHAGEx <- factor(merged_data_new$HHAGEx, 
                                   levels = c("15-19", "20-24","25-49", "50 plus" ),
                                   labels = c("15 to 19", "20 to 24","25 to 49", "50 plus"   ))
merged_data_new$WAGE <- factor(merged_data_new$WAGE, 
                                 levels = c("15-19", "20-24", "25-29", "30-34", "35-39",  "40-44", "45-49" ),
                                 labels = c("15 to 19", "20 to 24", "25 to 29","30 to 34", "35 to 39", "40 to 44",  "45 to 49" ))

# Ensure UN16AA is a factor
merged_data_new$UN16AA <- factor(merged_data_new$UN16AA, levels = c(0, 1))

# Create survey design objects for each level
hh_design <- svydesign(id = ~HH1, weights = ~hhweight,strata = ~stratum, data = merged_data_new)
wm_design <- svydesign(id = ~HH1, weights = ~wmweight,strata = ~stratum, data = merged_data_new)

summary(hh_design)
svymean(~WB4, hh_design) #mean age of women just to check the difference
svymean(~WB4, wm_design)
mean(merged_data_new$WB4) #mean age without applying weights

# Unweighted frequencies for household 
if (nrow(merged_data_new) > 0) { 
  cat('!!! UNWEIGHTED FREQUENCIES FOR HOUSEHOLD !!!\n')
  unweighted_freq <- lapply(merged_data_new[c("stratum")], table)
  print(unweighted_freq)
} else {
  cat('No rows match the filtering criteria.\n')
}
# Weighted frequencies for household
cat('!!! WEIGHTED FREQUENCIES FOR HOUSEHOLD !!!\n')
weighted_freq <- svytable(~stratum, hh_design)
print(weighted_freq)
############################## Change the Variable names (labels) names
attr(merged_data_new[["stratum"]], "label") <- "Region"
attr(merged_data_new[["windex5r"]], "label") <- "Rural Wealth Index Quintile"
attr(merged_data_new[["HH51_grouped"]], "label") <- "Number of Chilren Aged below 5"
attr(merged_data_new[["HH52_grouped"]], "label") <- "Number of Chilren Aged 5-17"
attr(merged_data_new[["HC1A_combined"]], "label") <- "Religion"
attr(merged_data_new[["EthnicityGroup"]], "label") <- "Ethnicity"
attr(merged_data_new[["HC15"]], "label") <- "Owns Agricultural Land"
attr(merged_data_new[["helevel1"]], "label") <- "Education of Household Head"
attr(merged_data_new[["HHAGEx"]], "label") <- "Age of Household Head"
attr(merged_data_new[["HHSEX"]], "label") <- "Sex of Household Head"
attr(merged_data_new[["UN16AA"]], "label") <- "Staying in a chaupadi/chhapro"
attr(merged_data_new[["UN16AB"]], "label") <- "Staying in a separate room "
attr(merged_data_new[["UN16AC"]], "label") <- "Staying in the cowshed"
attr(merged_data_new[["UN16AD"]], "label") <- "Eating in a separate place"
attr(merged_data_new[["UN16AE"]], "label") <- "Bathing in a separate place"
attr(merged_data_new[["UN16AF"]], "label") <- "Staying away from school or work"
attr(merged_data_new[["UN16AG"]], "label") <- "Staying away from social gatherings"
attr(merged_data_new[["UN16AH"]], "label") <- "Staying away from religious work "
attr(merged_data_new[["WAGE"]], "label") <- "Age of Women "
attr(merged_data_new[["CM4"]], "label") <- "Number of daughters living together "
attr(merged_data_new[["MSTATUS_grouped"]], "label") <- "Marital Status "
attr(merged_data_new[["welevel1"]], "label") <- "Education of women"

#1.Unweighted logistic regression
unweighted_logit <- glm(UN16AA ~ stratum, data = merged_data_new, family = binomial)
summary(unweighted_logit)

# Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitA <- svyglm(UN16AA ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitA)
tbl_regression(weighted_logitA, exponentiate = TRUE)

# Predict the probabilities in order to check the regressions accuracy 
probabilities <- predict(weighted_logitA, merged_data_new, type = "response")
# Check if there are any NA values in the predicted probabilities
print(sum(is.na(probabilities)))
# Convert probabilities to predicted classes
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Ensure that the predicted classes are in the same format as the actual classes
actual_classes <- ifelse(merged_data_new$UN16AA == 1, "pos", "neg")
# Check the unique values of actual and predicted classes
print(unique(actual_classes))
print(unique(predicted.classes))
# Calculate the accuracy
accuracy <- mean(predicted.classes == actual_classes)
print(accuracy)

#to check the frequency 
xtabs(~UN16AA + stratum, data=merged_data_new) #for unweighted
svytable(~UN16AA + stratum, hh_design) #for weighted


#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2 <- svyglm(UN16AA ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2)
tbl_regression(weighted_logit2, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3 <- svyglm(UN16AA ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3)
tbl_regression(weighted_logit3, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")
# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4 <- svyglm(UN16AA ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4)
tbl_regression(weighted_logit4, exponentiate = TRUE)

# format results into data frame with global p-values
weighted_logit4 %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5 <- svyglm(UN16AA ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5)
tbl_regression(weighted_logit5, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6 <- svyglm(UN16AA ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6)

#7.
# Let's make 15-49 the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "15 to 19")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7 <- svyglm(UN16AA ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7)
tbl_regression(weighted_logit7, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8 <- svyglm(UN16AA ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8)
tbl_regression(weighted_logit8, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9 <- svyglm(UN16AA ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9)
tbl_regression(weighted_logit9, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10<- svyglm(UN16AA ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10)
tbl_regression(weighted_logit10, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11 <- svyglm(UN16AA ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11)
tbl_regression(weighted_logit11, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1 <- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12 <- svyglm(UN16AA ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12)
tbl_regression(weighted_logit12, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13 <- svyglm(UN16AA ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13)
tbl_regression(weighted_logit13, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14 <- svyglm(UN16AA ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14)
tbl_regression(weighted_logit14, exponentiate = TRUE)

# format results into data frame with global p-values
weighted_logitA %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# First regression table
table1 <- weighted_logitA %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2 <- weighted_logit2 %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3 <- weighted_logit3 %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4 <- weighted_logit4 %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5 <- weighted_logit5 %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6 <- weighted_logit6 %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7 <- weighted_logit7 %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table8 <- weighted_logit8 %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9 <- weighted_logit9 %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10 <- weighted_logit10 %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11 <- weighted_logit11 %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12 <- weighted_logit12 %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13 <- weighted_logit13 %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14 <- weighted_logit14 %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_table <- tbl_stack(
  tbls = list(table1, table2, table3, table4, table5, table6, table7, table8, table9, table10, table11, table12, table13, table14)
)
# Convert the gtsummary table to a gt table
stacked_gt <- as_gt(stacked_table)
# Save the gt table as an image
gtsave(stacked_gt, "staying_in_chaupadi_2019.png")

###############################################################################################
#Staying in a separate room UN16AB

#1. Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitB <- svyglm(UN16AB ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitB)
tbl_regression(weighted_logitB, exponentiate = TRUE)

#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2B <- svyglm(UN16AB ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2B)
tbl_regression(weighted_logit2B, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3B <- svyglm(UN16AB ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3B)
tbl_regression(weighted_logit3B, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")
# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4B <- svyglm(UN16AB ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4B)
tbl_regression(weighted_logit4B, exponentiate = TRUE)

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5B <- svyglm(UN16AB ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5B)
tbl_regression(weighted_logit5B, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6B <- svyglm(UN16AB ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6B)

#7.
# Let's make 25-49 the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "15 to 19")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7B <- svyglm(UN16AB ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7B)
tbl_regression(weighted_logit7B, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8B <- svyglm(UN16AB ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8B)
tbl_regression(weighted_logit8B, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9B <- svyglm(UN16AB ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9B)
tbl_regression(weighted_logit9B, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10B<- svyglm(UN16AB ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10B)
tbl_regression(weighted_logit10B, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11B <- svyglm(UN16AB ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11B)
tbl_regression(weighted_logit11B, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1 <- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12B <- svyglm(UN16AB ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12B)
tbl_regression(weighted_logit12B, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13B <- svyglm(UN16AB ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13B)
tbl_regression(weighted_logit13B, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14B <- svyglm(UN16AB ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14B)
tbl_regression(weighted_logit14B, exponentiate = TRUE)

# Second regression table
table1B <- weighted_logitB %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2B <- weighted_logit2B %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3B <- weighted_logit3B %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4B <- weighted_logit4B %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5B <- weighted_logit5B %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6B <- weighted_logit6B %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7B <- weighted_logit7B %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table8B <- weighted_logit8B %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9B <- weighted_logit9B %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10B <- weighted_logit10B %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11B <- weighted_logit11B %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12B <- weighted_logit12B %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13B <- weighted_logit13B %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14B <- weighted_logit14B %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableB <- tbl_stack(
  tbls = list(table1B, table2B, table3B, table4B, table5B, table6B, table7B, table8B, table9B, table10B, table11B, table12B, table13B, table14B)
)
# Convert the gtsummary table to a gt table
stacked_gtB <- as_gt(stacked_tableB)
# Save the gt table as an image
gtsave(stacked_gtB, "staying_in_separate_room.png")


###############################################################################################
#Staying in the cowshed UN16AC

#1. Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitC <- svyglm(UN16AC ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitC)
tbl_regression(weighted_logitC, exponentiate = TRUE)

#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2C <- svyglm(UN16AC ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2C)
tbl_regression(weighted_logit2C, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3C <- svyglm(UN16AC ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3C)
tbl_regression(weighted_logit3C, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")

# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4C <- svyglm(UN16AC ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4C)
tbl_regression(weighted_logit4C, exponentiate = TRUE)

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5C <- svyglm(UN16AC ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5C)
tbl_regression(weighted_logit5C, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6C <- svyglm(UN16AC ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6C)
tbl_regression(weighted_logit6C, exponentiate = TRUE)

#7.
# Let's make 25-49 the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "15 to 19")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7C <- svyglm(UN16AC ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7C)
tbl_regression(weighted_logit7C, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8C <- svyglm(UN16AC ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8C)
tbl_regression(weighted_logit8C, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9C <- svyglm(UN16AC ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9C)
tbl_regression(weighted_logit9C, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10C<- svyglm(UN16AC ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10C)
tbl_regression(weighted_logit10C, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11C <- svyglm(UN16AC ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11C)
tbl_regression(weighted_logit11C, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1 <- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12C <- svyglm(UN16AC ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12C)
tbl_regression(weighted_logit12C, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13C <- svyglm(UN16AC ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13C)
tbl_regression(weighted_logit13C, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14C <- svyglm(UN16AC ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14C)
tbl_regression(weighted_logit14C, exponentiate = TRUE)

# Third regression table
table1C <- weighted_logitC %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2C <- weighted_logit2C %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3C <- weighted_logit3C %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4C <- weighted_logit4C %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5C <- weighted_logit5C %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6C <- weighted_logit6C %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7C <- weighted_logit7C %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table8C <- weighted_logit8C %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9C <- weighted_logit9C %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10C <- weighted_logit10C %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11C <- weighted_logit11C %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12C <- weighted_logit12C %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13C <- weighted_logit13C %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14C <- weighted_logit14C %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableC <- tbl_stack(
  tbls = list(table1C, table2C, table3C, table4C, table5C, table6C, table7C, table8C, table9C, table10C, table11C, table12C, table13C, table14C)
)
# Convert the gtsummary table to a gt table
stacked_gtC <- as_gt(stacked_tableC)
# Save the gt table as an image
gtsave(stacked_gtC, "staying_in_cowshed.png")

###############################################################################################
#Eating in a separate place UN16AD

#1. Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitD <- svyglm(UN16AD ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitD)
tbl_regression(weighted_logitD, exponentiate = TRUE)

#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2D <- svyglm(UN16AD ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2D)
tbl_regression(weighted_logit2D, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3D <- svyglm(UN16AD ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3D)
tbl_regression(weighted_logit3D, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")
# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4D <- svyglm(UN16AD ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4D)
tbl_regression(weighted_logit4D, exponentiate = TRUE)

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5D <- svyglm(UN16AD ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5D)
tbl_regression(weighted_logit5D, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6D <- svyglm(UN16AD ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6D)
tbl_regression(weighted_logit6D, exponentiate = TRUE)

#7.
# Let's make 25-49 the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "15 to 19")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7D <- svyglm(UN16AD ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7D)
tbl_regression(weighted_logit7D, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8D <- svyglm(UN16AD ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8D)
tbl_regression(weighted_logit8D, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9D <- svyglm(UN16AD ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9D)
tbl_regression(weighted_logit9D, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10D<- svyglm(UN16AD ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10D)
tbl_regression(weighted_logit10D, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11D <- svyglm(UN16AD ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11D)
tbl_regression(weighted_logit11D, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1<- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12D <- svyglm(UN16AD ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12D)
tbl_regression(weighted_logit12D, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13D <- svyglm(UN16AD ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13D)
tbl_regression(weighted_logit13D, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14D <- svyglm(UN16AD ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14D)
tbl_regression(weighted_logit14D, exponentiate = TRUE)

# 4th regression table
table1D <- weighted_logitD %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2D <- weighted_logit2D %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3D <- weighted_logit3D %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4D <- weighted_logit4D %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5D <- weighted_logit5D %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6D <- weighted_logit6D %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7D <- weighted_logit7D %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()


table8D <- weighted_logit8D %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9D <- weighted_logit9D %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10D <- weighted_logit10D %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11D <- weighted_logit11D %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12D <- weighted_logit12D %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13D <- weighted_logit13D %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14D <- weighted_logit14D %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableD <- tbl_stack(
  tbls = list(table1D, table2D, table3D, table4D, table5D, table6D, table7D, table8D, table9D, table10D, table11D, table12D, table13D, table14D)
)
# Convert the gtsummary table to a gt table
stacked_gtD <- as_gt(stacked_tableD)
# Save the gt table as an image
gtsave(stacked_gtD, "eating_in_separate_place.png")

###############################################################################################
#Bathing in a separate place UN16AE

#1. Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitE <- svyglm(UN16AE ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitE)
tbl_regression(weighted_logitE, exponentiate = TRUE)

#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2E <- svyglm(UN16AE ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2E)
tbl_regression(weighted_logit2E, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3E <- svyglm(UN16AE ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3E)
tbl_regression(weighted_logit3E, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")
# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4E <- svyglm(UN16AE ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4E)
tbl_regression(weighted_logit4E, exponentiate = TRUE)

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5E <- svyglm(UN16AE ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5E)
tbl_regression(weighted_logit5E, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6E <- svyglm(UN16AE ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6E)
tbl_regression(weighted_logit6E, exponentiate = TRUE)

#7.
# Let's make 25-49 the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "15 to 19")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7E <- svyglm(UN16AE ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7E)
tbl_regression(weighted_logit7E, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8E <- svyglm(UN16AE ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8E)
tbl_regression(weighted_logit8E, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9E <- svyglm(UN16AE ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9E)
tbl_regression(weighted_logit9E, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10E<- svyglm(UN16AE ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10E)
tbl_regression(weighted_logit10E, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11E <- svyglm(UN16AE ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11E)
tbl_regression(weighted_logit11E, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1<- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12E <- svyglm(UN16AE ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12E)
tbl_regression(weighted_logit12E, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13E <- svyglm(UN16AE ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13E)
tbl_regression(weighted_logit13E, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14E <- svyglm(UN16AE ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14E)
tbl_regression(weighted_logit14E, exponentiate = TRUE)

# 5th regression table
table1E <- weighted_logitE %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2E <- weighted_logit2E %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3E <- weighted_logit3E %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4E <- weighted_logit4E %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5E <- weighted_logit5E %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6E <- weighted_logit6E %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7E <- weighted_logit7E %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table8E <- weighted_logit8E %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9E <- weighted_logit9E %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10E <- weighted_logit10E %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11E <- weighted_logit11E %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12E <- weighted_logit12E %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13E <- weighted_logit13E %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14E <- weighted_logit14E %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableE <- tbl_stack(
  tbls = list(table1E, table2E, table3E, table4E, table5E, table6E, table7E, table8E, table9E, table10E, table11E, table12E, table13E, table14E)
)
# Convert the gtsummary table to a gt table
stacked_gtE <- as_gt(stacked_tableE)
# Save the gt table as an image
gtsave(stacked_gtE, "bathing_in_separate_place.png")

###############################################################################################
#Staying away from school or work UN16AF

#1. Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitF <- svyglm(UN16AF ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitF)
tbl_regression(weighted_logitF, exponentiate = TRUE)

#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2F <- svyglm(UN16AF ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2F)
tbl_regression(weighted_logit2F, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3F <- svyglm(UN16AF ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3F)
tbl_regression(weighted_logit3F, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")
# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4F <- svyglm(UN16AF ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4F)
tbl_regression(weighted_logit4F, exponentiate = TRUE)

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5F <- svyglm(UN16AF ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5F)
tbl_regression(weighted_logit5F, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6F <- svyglm(UN16AF ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6F)
tbl_regression(weighted_logit6F, exponentiate = TRUE)

#7.
# Let's make 25-49 the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "15 to 19")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7F <- svyglm(UN16AF ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7F)
tbl_regression(weighted_logit7F, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8F <- svyglm(UN16AF ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8F)
tbl_regression(weighted_logit8F, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9F <- svyglm(UN16AF ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9F)
tbl_regression(weighted_logit9F, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10F<- svyglm(UN16AF ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10F)
tbl_regression(weighted_logit10F, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11F <- svyglm(UN16AF ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11F)
tbl_regression(weighted_logit11F, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1<- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12F <- svyglm(UN16AF ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12F)
tbl_regression(weighted_logit12F, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13F <- svyglm(UN16AF ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13F)
tbl_regression(weighted_logit13F, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14F <- svyglm(UN16AF ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14F)
tbl_regression(weighted_logit14F, exponentiate = TRUE)

# 6th regression table
table1F <- weighted_logitF %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2F <- weighted_logit2F %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3F <- weighted_logit3F %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4F <- weighted_logit4F %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5F <- weighted_logit5F %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6F <- weighted_logit6F %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7F <- weighted_logit7F %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table8F <- weighted_logit8F %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9F <- weighted_logit9F %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10F <- weighted_logit10F %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11F <- weighted_logit11F %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12F <- weighted_logit12F %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13F <- weighted_logit13F %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14F <- weighted_logit14F %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableF <- tbl_stack(
  tbls = list(table1F, table2F, table3F, table4F, table5F, table6F, table7F, table8F, table9F, table10F, table11F, table12F, table13F, table14F)
)
# Convert the gtsummary table to a gt table
stacked_gtF <- as_gt(stacked_tableF)
# Save the gt table as an image
gtsave(stacked_gtF, "staying_away_from_school_work.png")

#################################################################################
#Staying away from social gatherings UN16AG

#1. Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitG <- svyglm(UN16AG ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitG)
tbl_regression(weighted_logitG, exponentiate = TRUE)

#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2G <- svyglm(UN16AG ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2G)
tbl_regression(weighted_logit2G, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3G <- svyglm(UN16AG ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3G)
tbl_regression(weighted_logit3G, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")
# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4G <- svyglm(UN16AG ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4G)
tbl_regression(weighted_logit4G, exponentiate = TRUE)

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5G <- svyglm(UN16AG ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5G)
tbl_regression(weighted_logit5G, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6G <- svyglm(UN16AG ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6G)
tbl_regression(weighted_logit6G, exponentiate = TRUE)

#7.
# Let's make 15-49 the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "15 to 19")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7G <- svyglm(UN16AG ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7G)
tbl_regression(weighted_logit7G, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8G <- svyglm(UN16AG ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8G)
tbl_regression(weighted_logit8G, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9G <- svyglm(UN16AG ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9G)
tbl_regression(weighted_logit9G, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10G<- svyglm(UN16AG ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10G)
tbl_regression(weighted_logit10G, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11G <- svyglm(UN16AG ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11G)
tbl_regression(weighted_logit11G, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1<- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12G <- svyglm(UN16AG ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12G)
tbl_regression(weighted_logit12G, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13G <- svyglm(UN16AG ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13G)
tbl_regression(weighted_logit13G, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14G <- svyglm(UN16AG ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14G)
tbl_regression(weighted_logit14G, exponentiate = TRUE)

# 7th regression table
table1G <- weighted_logitG %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2G <- weighted_logit2G %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3G <- weighted_logit3G %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4G <- weighted_logit4G %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5G <- weighted_logit5G %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6G <- weighted_logit6G %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7G <- weighted_logit7G %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table8G <- weighted_logit8G %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9G <- weighted_logit9G %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10G <- weighted_logit10G %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11G <- weighted_logit11G %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12G <- weighted_logit12G %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13G <- weighted_logit13G %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14G <- weighted_logit14G %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableG <- tbl_stack(
  tbls = list(table1G, table2G, table3G, table4G, table5G, table6G, table7G, table8G, table9G, table10G, table11G, table12G, table13G, table14G)
)
# Convert the gtsummary table to a gt table
stacked_gtG <- as_gt(stacked_tableG)
# Save the gt table as an image
gtsave(stacked_gtG, "staying_away_from_social_gatherings.png")

#####################################################################################

#Staying away from religious work UN16AH

#1. Let's make Sudoorpaschim province Rural the desired reference category
merged_data_new$stratum <- relevel(merged_data_new$stratum, ref = "Sudoorpaschim province Rural")
# Verify the releveling
levels(merged_data_new$stratum)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logitH <- svyglm(UN16AH ~ stratum, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logitH)
tbl_regression(weighted_logitH, exponentiate = TRUE)

#2. 
# Let's make Poorest the desired reference category
merged_data_new$windex5r <- relevel(merged_data_new$windex5r, ref = "Poorest")
# Verify the releveling
levels(merged_data_new$windex5r)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Run the weighted logistic regression
weighted_logit2H <- svyglm(UN16AH ~ windex5r, design = hh_design, family = quasibinomial)
# Print the summary of the model
summary(weighted_logit2H)
tbl_regression(weighted_logit2H, exponentiate = TRUE)

#3.  
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit3H <- svyglm(UN16AH ~ HH51_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit3H)
tbl_regression(weighted_logit3H, exponentiate = TRUE)

#4 # Let's make 2 the desired reference category
merged_data_new$HH52_grouped <- relevel(merged_data_new$HH52_grouped, ref = "2")
# Verify the releveling
levels(merged_data_new$HH52_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit4H <- svyglm(UN16AH ~ HH52_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit4H)
tbl_regression(weighted_logit4H, exponentiate = TRUE)

#5. 
# Let's make Hindu the desired reference category
merged_data_new$HC1A_combined <- relevel(merged_data_new$HC1A_combined, ref = "HINDU")
# Verify the releveling
levels(merged_data_new$HC1A_combined)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit5H <- svyglm(UN16AH ~ HC1A_combined, design = hh_design, family = quasibinomial)
summary(weighted_logit5H)
tbl_regression(weighted_logit5H, exponentiate = TRUE)

#6.
# Let's make None the desired reference category
merged_data_new$helevel1 <- relevel(merged_data_new$helevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$helevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit6H <- svyglm(UN16AH ~ helevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit6H)
tbl_regression(weighted_logit6H, exponentiate = TRUE)

#7.
# Let's make 50 plus the desired reference category
merged_data_new$HHAGEx <- relevel(merged_data_new$HHAGEx, ref = "50 plus")
# Verify the releveling
levels(merged_data_new$HHAGEx)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit7H <- svyglm(UN16AH ~ HHAGEx, design = hh_design, family = quasibinomial)
summary(weighted_logit7H)
tbl_regression(weighted_logit7H, exponentiate = TRUE)

#8.  
# Weighted logistic regression
weighted_logit8H <- svyglm(UN16AH ~ HHSEX, design = hh_design, family = quasibinomial)
summary(weighted_logit8H)
tbl_regression(weighted_logit8H, exponentiate = TRUE)

#9.  
# Weighted logistic regression
weighted_logit9H <- svyglm(UN16AH ~ WAGE, design = hh_design, family = quasibinomial)
summary(weighted_logit9H)
tbl_regression(weighted_logit9H, exponentiate = TRUE)

#10.
#chnage reference# Let's make 1 the desired reference category
merged_data_new$CM4_grouped <- relevel(merged_data_new$CM4_grouped, ref = "1")
# Verify the releveling
levels(merged_data_new$CM4_grouped)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
weighted_logit10H<- svyglm(UN16AH ~ CM4_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit10H)
tbl_regression(weighted_logit10H, exponentiate = TRUE)

#11.  
# Weighted logistic regression
weighted_logit11H <- svyglm(UN16AH ~ MSTATUS_grouped, design = hh_design, family = quasibinomial)
summary(weighted_logit11H)
tbl_regression(weighted_logit11H, exponentiate = TRUE)

#12.# Let's make None the desired reference category
merged_data_new$welevel1<- relevel(merged_data_new$welevel1, ref = "None")
# Verify the releveling
levels(merged_data_new$welevel1)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit12H <- svyglm(UN16AH ~ welevel1, design = hh_design, family = quasibinomial)
summary(weighted_logit12H)
tbl_regression(weighted_logit12H, exponentiate = TRUE)

#13# Let's make YES the desired reference category
merged_data_new$HC15 <- relevel(merged_data_new$HC15, ref = "YES")
# Verify the releveling
levels(merged_data_new$HC15)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit13H <- svyglm(UN16AH ~ HC15, design = hh_design, family = quasibinomial)
summary(weighted_logit13H)
tbl_regression(weighted_logit13H, exponentiate = TRUE)

#14# Let's make Brahman or Chhetri the desired reference category
merged_data_new$EthnicityGroup <- relevel(merged_data_new$EthnicityGroup, ref = "Brahman or Chhetri")
# Verify the releveling
levels(merged_data_new$EthnicityGroup)
# Recreate the survey design object
hh_design <- svydesign(id = ~HH1, weights = ~hhweight, strata = ~stratum, data = merged_data_new)
# Weighted logistic regression
weighted_logit14H <- svyglm(UN16AH ~ EthnicityGroup, design = hh_design, family = quasibinomial)
summary(weighted_logit14H)
tbl_regression(weighted_logit14H, exponentiate = TRUE)

# 8th regression table
table1H <- weighted_logitH %>%
  tbl_regression(label = list(stratum = "Region"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table2H <- weighted_logit2H %>%
  tbl_regression(label = list(windex5r = "Rural Wealth Index Quintile"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table3H <- weighted_logit3H %>%
  tbl_regression(label = list(HH51_grouped = "Number of Children Aged below five"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table4H <- weighted_logit4H %>%
  tbl_regression(label = list(HH52_grouped = "Number of Children Aged 5-17"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table5H <- weighted_logit5H %>%
  tbl_regression(label = list(HC1A_combined = "Religion"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table6H <- weighted_logit6H %>%
  tbl_regression(label = list(helevel1 = "Education of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table7H <- weighted_logit7H %>%
  tbl_regression(label = list(HHAGEx = "Age of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table8H <- weighted_logit8H %>%
  tbl_regression(label = list(HHSEX = "Sex of Household Head"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table9H <- weighted_logit9H %>%
  tbl_regression(label = list(WAGE = "Age of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table10H <- weighted_logit10H %>%
  tbl_regression(label = list(CM4_grouped = "Number of daughters living together"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table11H <- weighted_logit11H %>%
  tbl_regression(label = list(MSTATUS_grouped = "Marital Status"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table12H <- weighted_logit12H %>%
  tbl_regression(label = list(welevel1 = "Education of Women"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table13H <- weighted_logit13H %>%
  tbl_regression(label = list(HC15 = "Owns Agricultural Land"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

table14H <- weighted_logit14H %>%
  tbl_regression(label = list(EthnicityGroup = "Ethnicity"),
                 exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Stack the tables vertically
stacked_tableH <- tbl_stack(
  tbls = list(table1H, table2H, table3H, table4H, table5H, table6H, table7H, table8H, table9H, table10H, table11H, table12H, table13H, table14H)
)
# Convert the gtsummary table to a gt table
stacked_gtH <- as_gt(stacked_tableH)
# Save the gt table as an image
gtsave(stacked_gtH, "staying_away_from_religious_work.png")

###############################################################################
#Multivariate regression 
chaupadi <- svyglm(UN16AA ~ stratum + windex5r + HC1A_combined + HC15 + EthnicityGroup, design = hh_design, family = "quasibinomial")
summary(chaupadi)           # for p-values

# Tidy the model results
tidy_chaupadi <- tidy(chaupadi, exponentiate = TRUE, conf.int = TRUE)

# Create the summary table using gtsummary
gt_summary <- tbl_regression(chaupadi, exponentiate = TRUE)

# Add a table caption
gt_summary <- gt_summary %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")

# Print the table
gt_summary

#staying in a separate room 
separate_room <- svyglm(UN16AB ~ stratum + windex5r + HC1A_combined + HC15 + EthnicityGroup, design = hh_design, family = "quasibinomial")
summary(separate_room)           # for p-values

# Tidy the model results
tidy_separate_room <- tidy(separate_room, exponentiate = TRUE, conf.int = TRUE)

# Create the summary table using gtsummary
gt_summary <- tbl_regression(separate_room, exponentiate = TRUE)

# Add a table caption
gt_summary <- gt_summary %>%
  modify_header(label = "Variable") %>%
  modify_footnote(all_stat_cols() ~ "OR = Odds Ratio, CI = Confidence Interval, p = P-value") %>%
  modify_caption("Survey-Weighted Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-values")

# Print the table
gt_summary







#Export data to csv
write.csv(data_wm, file = "/Users/nasib/Desktop/data_wm.csv")

# Remove the column HHAGEx column
data_hh$HHAGEx <- NULL 
rm(merged_data_hh_wm)

xtabs(~UN16AA + HH7, data=merged_data_new)
# Count the number of occurrences of each category in the WM17 column
table(merged_data_hh_wm$WM17)
# Using table to count the number of each factor level including NA
count_un16aa <- table(addNA(updated_data$UN16AA))
print(count_un16aa)
# Count the number of NA values in each column
na_counts <- colSums(is.na(merged_data_new))
# Print the counts
print(na_counts)