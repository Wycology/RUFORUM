#rm(list=ls())
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(ggpubr)
library(ggplot2)
library(Hmisc)

install.packages("faraway")
library(faraway) #load the gala dataset
data("gala")

write.csv(gala, file = "gala.csv") # exporting data

#set working directory
setwd("C:/Users/hnama/Desktop/MAK_TRAINING_IN R_2022/MAK_DAY6.26.3.22/Poisson")

gala <- read.csv("gala.csv")

print(head(gala))

#or
head(gala)


#to get more insight about the data
?gala

#Species is a response variable.
summary(gala)
#We'll now study a basic summary of the variables.
#Consider only predictor variables


# generate a histogram for Species in order to check if 
#the variable follows the Poisson distribution

hist(gala$Species, breaks = 10, xlab = "Species Count", main = "Distribution of Species", Prob = TRUE)
#The above visualization shows that Species follows a Poisson distribution,
#as the data is right-skewed. 

#We can generate a boxplot too, to get more insight 
#into the distribution pattern
boxplot(gala$Species, main = "Boxplot of Species")

###MORE TO MORE ADDITIONAL NOTES#######

#Create a poisson model
fit <- glm(Species ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent, 
         data = gala, family = poisson())
summary(fit)

#Based on the above analysis, we find that variables Endemics,
#Area, and Nearest are significant and only
#their inclusion is sufficient to build the 
#right Poisson regression model. as there p-values<0.05

#Next
#We'll build a modified Poisson regression model taking 
#into consideration three variables only viz.
#Endemics, Area, and Nearest.

fit.reduced <- glm(Species ~ Endemics + Area + Nearest, data = gala, family = poisson())
summary(fit.reduced)

#The output produces deviances, regression parameters,
#and standard errors. We can see that each of the parameters
#is significant at p < 0.05 level

##Interpreting coefficients#############
coef(fit.reduced)

#In Poisson regression, the dependent variable is modeled as
#the log of the conditional mean loge(l). 
#The regression parameter of 0.0355 for Endemics
#indicates that a one-unit increase in the variable 
#is associated with a 0.04 increase in the log mean number of Species,
#holding other variables constant. 
#The intercept is a log mean number of Species
#when each of the predictors equals zero.

#####Step 10
#However, it is much easier to interpret the regression 
#coefficients in the original scale of the dependent variable
#(number of Species, rather than log number of Species). 
#The exponentiation of the coefficients will allow an easy 
#interpretation. This is done as follows.
exp(coef(fit.reduced))

#From the above findings, we can say that one unit increase
#in Area multiples the expected number of species by 0.9999,
#and a unit increase in the number of endemic species represented
#by Endemics multiplies the number of species by 1.0361.
#The most important aspect of Poisson regression is that
#exponentiated parameters have a multiplicative rather
#than an additive effect on the response variable.

install.packages("qcc")
library(qcc)
qcc.overdispersion.test(gala$Species, type = "poisson")


fit.od <- glm(Species ~ Endemics + Area + Nearest, data = gala, family = quasipoisson())
summary(fit.od)
