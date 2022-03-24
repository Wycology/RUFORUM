#######Calculating sample size#######
rm(list=ls())# clean the environment

#install.packages("pwr")
library(pwr)

#Two techniques 1) without installing any packages, 2) with installing packages

##############Finding sample size (n), given power###################

# for continuous outcome 

#Example One

##Comparing mean between different (independent) groups
?power.t.test

power.t.test(delta = 0.2, sd = 0.5, power = 0.8) # by default, it gives you a two.sample

power.t.test(delta = 0.2, sd = 0.5, power = 0.8, type = "two.sample") 


#Example 

power.t.test(power = 0.9,delta = 0.3,sd = 0.28, type = "one.sample") #one sample

#Aim: to compute the sample size needed to achieve a power of 90% in
#a study which aims to show a difference in means between two independent groups 
#assuming that the magnitude of the difference is 0.3 units and 
#the standard deviation is 0.28 units.

power.t.test(power = 0.9, delta = 0.3, sd = 0.28, type = "two.sample") # two sample
#Possible conclusion sentence:

#To reach a power of 90% the study should include at least 20
#subjects in each group to detect a difference in means of 0.3 units.

### For a binary outcome

# Example one

## Comparing two proportions

power.prop.test(p1 = 0.6, p2 = 0.5, power = 0.8)

#p1 is first proportion with 60%, p2 is second proportion with 50%
#Conclusion: In order to achieve a power of 80% under the assumed event probabilities
#the study should include at least 387 subjects in each of the groups.

# Option 2: Using packages
#use pwr package for esrtimating sample size
library(pwr)

##### Sample size for a given power######

# calculate sample size 
#Test with one mean, that is one sample t.test (t=(xbar-mu0)/(s/sqrt(n))
# you need to specify delta(difference in mean), sigma, power,
#type of sample (one, two or paired)
?pwr.t.test()
delta=10 #difference in mean (sample mean-hypothesized mean)# for one sample
sigma=20 # standard error of the mean
r=delta/sigma # effect size
# d is the effect size, that is ratio of delta/sigma
pwr.t.test(d=r, sig.level = 0.05, power = 0.9, type= "one.sample")

#power depends on your objective, so my objective is to achieve a power of 90%
#since I have specified the power, then it will generate for me the sample size
# since I am using a t.test, then the type is one sample 


# Another condition: test of two means
#Here, we are comparing mean between two independent samples
#Here we only change the type to "two.sample"
delta=10 #difference in mean (sample mean of group 1- sample mean of group 2)# for two sample
sigma=20 # standard error of the mean
r=delta/sigma # effect size
pwr.t.test(d=r, sig.level = 0.1, power = 0.8, type= "two.sample")


# you can play around with the command depending on your objective
#alternative is the two-sided hypothesis


#############calculate the power, given sample size###################

#Aim: to compute the power of a study to show a difference between
#group 1 (n=28) in which the event probability is 30% and 
#group 2 (n=28) in which the event probability is 55%.

power.prop.test(n=28,p1=0.3,p2=0.55) # 
#Possible conclusion sentence:
#The power of a study which includes 28 subjects in each of two experimental groups
#to see a difference between the event probabilities is 48% under
#the assumption that the event probabilities are 30% in group 1 and 55% in group 2.

#For continuous variable/outcome

############Power for a given sample##############

#Aim: to compute the power of a study which aims to show 
#a difference in means between group 1 (n=6) and group 2 (n=6)
#assuming that the magnitude of the difference is 0.3 units and 
#the standard deviation is 0.28 units.

power.t.test(n=6,delta=0.3,sd=0.28,type="two.sample")
#Possible conclusion sentence:
#The power of the study is 39% to detect a difference in means of 0.3 units.


#power calcuations for ANOVA
?pwr.anova.test()
pwr.anova.test(f=0.28,k=4,power=0.80,sig.level=0.05)
pwr.anova.test(f=0.28,k=4,n=20,sig.level=0.05)

