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











