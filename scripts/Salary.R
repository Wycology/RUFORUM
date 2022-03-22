# install.packages("tidyverse")
# install.packages("learnr")
# install.packages("readxl")
# install.packages("readr")
# install.packages("dplyr")

rm(list = ls()) # clear the environment

library(tidyverse)
library(learnr) 
library(readxl)
library(readr)

## setting directory

# setwd()
# setwd("C:/Users/hnama/Desktop/EDEMA/PREPARATIONS")

## dataframe and importing data

salaries <- read.csv("data/WorkSalaries.csv") # salaries is an object I have created

head(salaries) # gives the first 6 rows

str(salaries) # describes the structure of data, numeric (has dec points), integer (no decimal points), character
# get to know the scale of measurement for each. whether categorized or not

head(salaries, 3) # display the first 3 rows of the data.frame

class(salaries) # nature of dataset


nrow(salaries) # displays the number of rows of the data.frame (no. of observations)

ncol(salaries) # no.of variables/no.of columns

dim(salaries) # displays the number of rows and columns in a vector of length 2 (#rows, #cols)

#structure of dataset

#categorical variables
salaries$rank = as.factor(salaries$rank)
salaries$discipline = as.factor(salaries$discipline)
salaries$sex = as.factor(salaries$sex)

str(salaries)
summary(salaries)
#----------------------------------------------------------------------------------------
#Data manipulation 

# we can change column names by using the rename() function from the R package dplyr
# we could rename the column "carat" to CARAT in the dataset

#library(dplyr)

#DM1: Creating new/adding variables

salaries$halfsalary <- (salaries$salary)/2

view(salaries)

#DM2: Recording an existing variable
summary(salaries$salary)

salaries$salarycat <- ifelse(salaries$salary < 113706, "low", "high")

#DM3:Rename columns of a data frame (df)

salaries = rename(salaries, SEX = sex) # renaming sex to SEX

colnames(salaries) # see the changed name

#DM4: Subset rows / columns 
head(salaries)

salaries[3,2] # in salaries dataset row 3, column 2

str(salaries)

salariesprof <- subset(salaries, rank == "Prof") # df for only profs

view(salariesprof)

salaries$rank # selecting only the rank variable from the main dataset

#DM5: Remove or delete colums from the df

str(salaries)

salaries <- subset(salaries, select = -c(halfsalary)) # removing one variable

salaries <- subset(salaries, select = -c(salary, salarycat)) # removing multiple variables

#DM6:Level of measurements in R

#DM7: Dealing with missing data _basics

#DM8: Merging data in R
library(dplyr)
#Considering WeightA and weightB dataframes (df)

DF1 <- read.csv("weightA.csv")
DF2 <- read.csv("weightB.csv")
str(DF1)
str(DF2)
#left_join()
left_join(DF1, DF2, by = "?..ID")

left <- left_join(DF1, DF2, by = "?..ID") # store in a dataframe

#right_join()
right_join(DF1, DF2, by = "?..ID")

right <- right_join(DF1, DF2, by = "?..ID")
view()

#inner_join()
inner<-inner_join(DF1, DF2, by = "?..ID")


#full_join()
full <- full_join(DF1, DF2, by = "?..ID")
view()
#DM9: DESCRIPTIVE STATISTICS 

salaries <- read.csv("data/WorkSalaries.csv")
# central tendency: mean, media, mode
#variability: range, interquartile range, variance, standard deviation
#Frequency tables


#salary is a continuous variable/ scale so we run a summary statistics

min(salaries$salary); max(salaries$salary); mean(salaries$salary); sd(salaries$salary)

range(salaries$salary); var(salaries$salary); median(salaries$salary)

# rank is a categorical variable so we run a frequency table
table(salaries$rank) # categories variable
table(salaries$discipline)
table(salaries$sex)

#percentages of categorical variables
table(salaries$rank) # categories variable

ranks <- table(salaries$rank) #store in object ranks
ranks
prop.table(ranks) # gives percentages

round(prop.table(ranks),2)*100 # round off at 2 d.p

#Alternatively

round(prop.table(table(salaries$rank)),2)*100

# Data visualization in R
#graphics.off()
#since rank is categorical so we run a bar chart or a pie chart 
barplot(table(salaries$rank))

#library(tidyverse)
dev.off() # removes current figure from the plot window


barplot(table(salaries$rank), horiz = TRUE) # gives a vertical format of the picture


pie(table(salaries$rank),
     col = c("gray90","blue", "black"))


#ploting a continuous variable

# Since salary is an integer/ scale we run scatterplot or histogram
plot(salaries$salary)# this will make your explaination not so clear, so we use a hist


# Graph salary using blue points overlayed by a line 
plot(salaries$salary, type = "o", col = "blue")

# Create a title with a red, bold/italic font
title(main = "Salaries paid to Professors", col.main = "blue", font.main = 4)

#or

hist(salaries$salary)

############################################

#Option 2: use the add on package readxl to read in the excel file using read_excel()

### library(readxl)

?read_excel # see the help file for read_excel()

read_excel("data/WorkSalaries.xlsx")


# the data exporting functions include readr()etc

# Write data to csv files:  
# decimal point = "." and value separators = comma (",")
write.csv(salaries, file = "salaries.csv") # check new diamond.csv
#written in the working folder

#Data Visualisation with ggplot2

library(ggplot2)
str(salaries)
ggplot(salaries) + geom_point(aes(x = yrs.service , y = salary))


#clearing the enviroment window
rm(list = ls()) # clear the environment

# to clear console type
Ctrl+L  


#END OF CLASS ONE
