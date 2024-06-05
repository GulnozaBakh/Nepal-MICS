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
