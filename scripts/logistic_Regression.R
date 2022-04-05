# LOGISTIC REGRESSION

library(tidyverse)
library(learnr) 
library(dplyr)
library(readxl)
library(readr)
library(ggpubr)
library(ggplot2)
library(Hmisc)
library(caret) 

# Logistic regression

graduate <- read.csv("data/mydata.csv")

head(graduate)

str(graduate)

graduate$admit <- as.factor(graduate$admit)

graduate$rank <- as.factor(graduate$rank)

str(graduate)

summary(graduate)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells

table(graduate$admit, graduate$rank)

# or

xtabs(~admit + rank, data = graduate)

str(graduate)

#simple logistic regression model

logit1 <- glm(admit ~ gre, data = graduate, family = "binomial") # with continuous variable

summary(logit1)

#log-odds=-2.901344+ 0.003582*gre

logit2 <- glm(admit ~ rank, data = graduate, family = "binomial") # with categorical variable

summary(logit2)

#log-odds =0.1643 -0.7500*(rank=2)-0.13647*(rank=3)-1.6867*(rank=4)

#Multiple logistic regression model

mylogit <- glm(admit ~ gre + gpa + rank, data = graduate, family = "binomial")

summary(mylogit)


## CIs using profiled log-likelihood

confint(mylogit)

## odds ratios only

exp(coef(mylogit))

## odds ratios and 95% CI

exp(cbind(OR = coef(mylogit), confint(mylogit)))

# Note
# Empty cells or small cells: You should check for empty or small cells by doing
# a crosstab between categorical predictors and the outcome variable. 
# If a cell has very few cases (a small cell), the model may become unstable
# or it might not run at all
