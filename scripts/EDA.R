#Install these package if you do not have them on your computer
# install.packages("tidyverse")
# install.packages("learnr")
# install.packages("readxl")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("plotrix")
# install.packages("epiDisplay")
# install.packages("ggplot2")

#load the installed packages
library(tidyverse)#data manipulation
library(learnr) #exports data from r to excel
library(dplyr) # helps to compute summary statistics by groups
library(readxl)#read excel files
library(readr)# read csv files
library(plotrix)#plots 3D graphs
library(epiDisplay)# presents graphs and table in horizotal
library(ggplot2)# data-visualization
library(ggpubr)# used to create graphs

#getwd()
## setting directory
#setwd("~/MACRRI/PRACTICE")

#-----------------------------------------------------------------------------------------------

## dataframe and importing data

employee <- read.csv("data/employee.csv")
head(employee)

#employee is an object I have created

# view the data set employee

head(employee) # gives the first 6 rows

str(employee) # describes the structure of data, numeric (has dec points), integer (no decimal points), character
            # get to know the scale of measurement for each. whether categorized or not

head(employee, 3) # display the first 3 rows of the data.frame

class(employee) # nature of dataset


nrow(employee) # displays the number of rows of the data.frame (no. of observations)

ncol(employee) # no.of variables/no.of columns

dim(employee) # displays the number of rows and columns in a vector of length 2 (#rows, #cols)

colnames(employee) # display the column names (if any)

rownames(employee) # display the row names (if any)

#------------------------------------------------------------------------------------------------

#DESCRIPTIVE STATISTICS 
# central tendency: mean, media, mode
#variability: range, interquartile range, variance, standard deviation
#Frequency tables


summary(employee)

#salary is a continuous variable/ scale so we run a summary statistics
min(employee$salary); max(employee$salary); mean(employee$salary);sd(employee$salary)

range(employee$salary); var(employee$salary); median(employee$salary)
# gender is a categorical variable so we run a frequency table
table(employee$gender) # categories variable

# jobcat is a categorical variable so we run a frequency table
job<-table(employee$jobcat)
job
prop.table(job)
round(prop.table(job)*100,2)


#-------------------------------------------------------------------------------------------------------------------------

# Data visualization in R
#graphics.off()
#since gender is categorical so we run a bar chart or a pie chart 
barplot(table(employee$gender))

dev.off()
# jobcat is categorical so we either run a bar chart or a pie chart
barplot(table(employee$jobcat))


barplot(table (employee$jobcat), horiz = TRUE) # gives a vertical format of the picture


pie( table(employee$jobcat),
     col = c("white", "gray90", "gray60"))

#Constructing a histogram
#hist(table(employee$jocat))
#getting percentages for jobcat and gender

col<-table(employee$jobcat) # gives the frequency for jobcat in object col
col # run the object
prop.table(col) # gives proportions
prop.table(col)*100 # percentages
round(prop.table(col)*100, 2) # round off if necessary
barplot(prop.table(col)*100)

barplot(prop.table(col)*100)# ploting percentages


# Since price is an integer/ scale we run scatterplot or histogram
plot(employee$salary)# this will make your explaination not so clear, so we use a hist


hist(employee$salary)

# Graph salary using blue points overlayed by a line 
plot(employee$salary, type="o", col="blue")

# Create a title with a red, bold/italic font
title(main="Salary", col.main="red", font.main=4)


##########################################################
#Relationships between two variables (either qualitative and quantitative OR quantitative vs quantitative)

#boxplot
boxplot(log(salary,10) ~ jobcat,data = employee,main = "Salary by Job catergory", 
        xlab = "jobcat", ylab = "Salary")

#------------
#Building a plot with ggplot2() command
#constructing a scatter plot
        
ggplot(employee) + geom_point(aes(x = log(salbegin), y = log(salary)))

ggplot(employee, aes(x = salbegin, y = salary)) + geom_point()
###############################################################
#converting to a factor
str(employee)
jobcat <- factor(employee$jobcat)
gender <- factor(employee$gender)
class(gender)
######################################################
#Descriptive by groups

employee %>%
        group_by(jobcat)%>%
        dplyr::summarise(
                count = n(),
                mean = mean(salary, na.rm = TRUE),# na.rm means remove missing values
                sd = sd(salary, na.rm = TRUE))# for salary of employees
 
boxplot(salary~jobcat,data=employee, xlab="jobcat", ylab="salary")

#using ggplot
ggplot(employee,aes(x=jobcat,y=salary))+geom_boxplot()

# For boxplot using the palette function
ggboxplot(employee, x = "jobcat", y = "salary",
          color = "jobcat",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))

?palette
graphics.off()
#---------------------------------------------------------------------------
#Visualisation: Visualize your data using scatter plots
colnames(employee)

ggscatter(employee, x = "salbegin", y = "salary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "salbegin", ylab = "salary")
#--------------------------------------------------------


#Intepretation of Results
#The p-value of the test is 2.2e^{-16}, which is less than the
#significance level alpha = 0.05. We can conclude that 
#carat and price are significantly correlated
#with a correlation coefficient of 0.88 and p-value of 2.2e^{-16} .
#-------------------------------------------------------------------

#Correlation Matrix

# correlation matrix, which is used to investigate
#the dependence between multiple variables at the same time.

## Dropping a column from my dataset
head(employee)
# use command subset()
str(employee)
employee1 = subset(employee, select = -c(gender, jobcat, bdate,id, minority))
head(employee1)

res <- cor(employee1)
res
round(res, 2)

#Correlation matrix with significance levels (p-value)
#The function rcorr() [in Hmisc package] 
#can be used to compute the significance levels 
#for pearson and spearman correlations. 
#It returns both the correlation coefficients and
#the p-value of the correlation for all possible pairs
#of columns in the data table.

install.packages("Hmisc")
library(Hmisc)

res2 <- rcorr(as.matrix(employee1)) # gives both the coefficients and p-values 
res2

#P : the p-values corresponding to the significance levels of correlations.

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

#--------------------------------------------------------------------------------
#constructing multiple graphs on one page
par(mfrow = c(4, 4)) 
plot(employee1)  # Plot

###############################################
#cross tabulation
sa<-table(employee$gender,employee$jobcat)
sa
round(prop.table(sa)*100,2)


 ## cleaning the environment space

#rm will remove all of the objects that are stored in your global environment

 #(which may be what you want) but will not unload any of the packages that you have loaded.

# clear environment just type

rm(list=ls())

# to clear console type
#Ctrl+L  

cat("\014")
#clear plots in r

# Clear all plots
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

#alternatively
dev.off()
#or
graphics.off()


