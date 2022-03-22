# Joining two datasets in R

## Loading the library
library(readr)
library(dplyr)

# Loading the datasets

weightA <- read_csv('data/weightA.csv')
weightB <- read_csv('data/weightB.csv')

## Inner join

AB_inner <- inner_join(weightA, weightB, by = "ID") # all rows in x and y
AB_inner

## Left join

AB_left <- left_join(weightA, weightB, by = "ID") # all rows in x
AB_left

## Right join

AB_right <- right_join(weightA, weightB, by = 'ID') # all rows in y
AB_right

## Full join

AB_full <- full_join(weightA, weightB, by = 'ID')# all rows in x or y
AB_full


## Semi join 
AB_semi <- semi_join(weightA, weightB, by = 'ID') # all rows in x with match in y
AB_semi

## Anti join
AB_anti <- anti_join(weightB, weightA,  by = "ID") # all rows in x without match in y
AB_anti
