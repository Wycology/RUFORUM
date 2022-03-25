#Analysis of categorical data

library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(ggpubr)
library(ggplot2)
library(Hmisc)


#Setting working directory
setwd("C:/Users/hnama/Desktop/MAK_TRAINING_IN R_2022/MAK_DAY6.26.3.22/Categorical Data")

salaries<-read.csv("WorkSalaries.csv")
str(salaries)

#categorical variables
salaries$rank = as.factor(salaries$rank)
salaries$discipline = as.factor(salaries$discipline)
salaries$sex =as.factor(salaries$sex)

#----------------------------------------------------------

##ANALYSIS OF CATEGORICAL DATA########

# Recording an existing variable
summary(salaries$salary)

salaries$salarycat<-ifelse(salaries$salary < 113706, c("low"), c("high"))

View(salaries)

mytable<-table(salaries$rank, salaries$salarycat)
mytable
addmargins(mytable, margin=c(1,2))
prop.table(mytable)
chisq.test(salaries$rank, salaries$salarycat)

chisq.test(salaries$sex, salaries$salarycat )


chisq.test(salaries$discipline, salaries$salarycat)



#End
