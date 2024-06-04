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
