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

#open dataset
data_wm <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/wm.sav")
data_hh <- read_sav("/Users/nasib/Documents/my documents/Agripath RA/Gender Study/Nepal 2019/Nepal 2019/Nepal MICS6 SPSS Datasets/hh.sav")

# Convert labelled vectors to character vectors
data_hh <- labelled::to_character(data_hh)
data_wm <- labelled::to_character(data_wm)

#Explore the data
summary(data_hh)
summary(data_wm)
colnames(data_hh)
colnames(data_wm)
str(data_hh)
str(data_wm)

#Task1. CommonVarsHH. # Recode HHAGE (age of hh head) into HHAGEy (we can also group into 4 groups)
data_hh <- data_hh %>%
  mutate(
    HHAGEy = case_when(
      HHAGE >= 15 & HHAGE <= 19 ~ 1,
      HHAGE >= 20 & HHAGE <= 24 ~ 2,
      HHAGE >= 25 & HHAGE <= 29 ~ 3,
      HHAGE >= 30 & HHAGE <= 34 ~ 4,
      HHAGE >= 35 & HHAGE <= 39 ~ 5,
      HHAGE >= 40 & HHAGE <= 44 ~ 6,
      HHAGE >= 45 & HHAGE <= 49 ~ 7,
      HHAGE >= 50 & HHAGE <= 59 ~ 8,
      HHAGE >= 60 & HHAGE <= 69 ~ 9,
      HHAGE >= 70 ~ 10
    )
  )
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


