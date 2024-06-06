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
