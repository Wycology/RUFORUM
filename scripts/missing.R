#MISSING DATA
#loading necessary packages
library(tidyverse)
library(learnr) 
library(dplyr)
library(readxl)
library(readr)

#importing missingdata.xlsx in R

#Setting working directory

#Go to File > Import Dataset > From Excel...> Browse > Import

dt<-missingdata # renaming the dataset
is.na(dt) # checking for missing data

dt$Age[dt$Age==25]= 30 # renaming age 25 to 30

dt # viewing the dataset again

#Removing all missing variables
na.omit(dt)
