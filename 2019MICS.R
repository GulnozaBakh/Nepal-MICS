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





