#install.packages("agricolae") #intall package agricolae
library(agricolae)  # call package agricolae
#install.packages ("cwhmisc") #install package cwhmisc
#library(cwhmisc) # call package cwhmisc
library(doBy)
library(lattice)
library(effects)
library(multcomp)
library(ggplot2)
library(car)
#libs(agricolae) #gives you all functions in a package
setwd("~/Desktop/Makerere R Training")
latinsquare<-read.table("LatinSquare.txt", header = T) #reading data from a file
names(latinsquare) #checking the names of the variables
str(latinsquare) #checking the structure of the data
latinsquare$Row<-factor(latinsquare$Row, labels = c("cow1", "cow2", "cow3", "cow4","cow5")) #converting row into factor
latinsquare$Column<-factor(latinsquare$Column, labels =c("milkman1", "milkman2", "milkman3", "milkman4","milkman5") ) #converting column into vector
latinsquare$Treatment<-factor(latinsquare$Treatment) #converting treatment into vector
latinsquare$Plot<-factor(latinsquare$Plot) #converting plot into factor
str(latinsquare)
names(latinsquare)<-c("Datapoint", "Cow","Milkman", "Feed", "Milkyield")
#Boxplot for milkyield by feed types 
ggplot(latinsquare,aes(x=Feed,y= Milkyield))+geom_boxplot() #boxplot for Milkyield by Feed type
ggplot(latinsquare,aes(x=Feed,y= Milkyield))+geom_boxplot() + facet_wrap(~Cow) # per cow
ggplot(latinsquare,aes(x=Feed,y= Milkyield))+geom_boxplot() + facet_wrap(~Milkman) # per milkman
#point plot
ggplot(latinsquare,aes(x=Feed,y= Milkyield, color=Cow))+geom_point() #boxplot for Milkyield by Feed type
ggplot(latinsquare,aes(x=Feed,y= Milkyield, color=Milkman))+geom_point() #boxplot for Milkyield by Feed type

#Boxplot for milkyield by Milkman
ggplot(latinsquare,aes(x=Milkman,y=Milkyield)) +geom_boxplot() 
ggplot(latinsquare,aes(x=Cow,y=Milkyield))+geom_boxplot() 

#analysis option I (Assume experimental units are homogeneous & no milkman effect) - CRD
model0<-aov(Milkyield~1, latinsquare) #null model
anova(model0)
model1<-aov(Milkyield~Feed, latinsquare) #analysis as CRD
anova(model1)

opar<-par( mfrow = c(2,2) )
plot(model1, which=5)
plot(model1, which=1)
plot(model1, which=2)
plot(residuals(model1) ~ Datapoint, main="Residuals vs Plot", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model1) ~ Feed, main="Residuals per treatment", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model1) ~ Cow, main="Residuals per Row", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model1) ~ Milkman, main="Residuals vs Plot", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)


obser_fitted_residual<-cbind(latinsquare$Milkyield, model1$fitted.values, model1$residuals) #displaying observed values, fitted values and residuals
hist(rstudent(model1), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5),breaks=6,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)
#adding row to the model
model2<-aov(Milkyield~Feed + Cow, latinsquare) #analysis as RCBD with cow as block
anova(model2)
plot(model2, which=5)
plot(model2, which=1)
plot(model2, which=2)
plot(residuals(model2) ~ Datapoint, main="Residuals vs Datapoint", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model2) ~ Feed, main="Residuals per Feed", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model2) ~ Cow, main="Residuals per Cow", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model2) ~ Milkman, main="Residuals vs Milkman", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)

fitvalue_model2<-model2$fitted.values
obser_fitted_residual2<-cbind(latinsquare$Milkyield, model2$fitted.values, model2$residuals) #displaying observed values, fitted values and residuals
hist(rstudent(model2), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5),breaks=6,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)

#model with treatment and column effect
model3<-aov(Milkyield~Feed + Milkman, latinsquare) #analysis as RCBD with Column as block
anova(model3)
plot(model3, which=5)
plot(model3, which=1)
plot(model3, which=2)
plot(residuals(model3) ~ Datapoint, main="Residuals vs Plot", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model3) ~ Feed, main="Residuals per Feed", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model3) ~ Cow, main="Residuals per Cow", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model3) ~ Milkman, main="Residuals vs Milkman", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)

obser_fitted_residual3<-cbind(latinsquare$Milkyield, model3$fitted.values, model3$residuals) #displaying observed values, fitted values and residuals
hist(rstudent(model2), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5),breaks=6,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)

#Final model with all factors in the model
model4<-aov(Milkyield~Feed + Cow + Milkman, latinsquare) #analysis as RCBD with Column as block
anova(model4)
plot(model4, which=5)
plot(model4, which=1)
plot(model4, which=2)
plot(residuals(model4) ~ Datapoint, main="Residuals vs Datapoints", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model4) ~ Feed, main="Residuals per Feed", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model4) ~ Cow, main="Residuals per Cow", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)
boxplot(residuals(model4) ~ Milkman, main="Residuals vs Milkman", font.main=1,data=latinsquare)
abline(h = 0, lty = 2)

obser_fitted_residual4<-cbind(latinsquare$Milkield, model4$fitted.values, model4$residuals) #displaying observed values, fitted values and residuals
hist(rstudent(model4), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5),breaks=6,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)

#Posthoc 
LSD.test(model4, "Treatment", console = T)
LSD.test(model4, "Treatment", p.adj = "bonferroni", console = T)


#test for normality using shapiro test
shapiro.test(resid(model1)) #The p-value is compared
shapiro.test(resid(model2))
shapiro.test(resid(model3))
shapiro.test(resid(model4))

#Test for homogeneity of variance among treatments using analysis of variance approached
anova(lm((resid(lm(Milkyield~Feed, latinsquare))^2)~Feed, latinsquare)) #null hypothesis the variance of the different treatments are equal
anova(lm((resid(lm(Milkyield~Feed + Cow, latinsquare))^2)~Feed, latinsquare)) #null hypothesis the variance of the different treatments are equal
anova(lm((resid(lm(Milkyield~Feed + Milkman, latinsquare))^2)~Feed, latinsquare)) #null hypothesis the variance of the different treatments are equal
anova(lm((resid(lm(Milkyield~Feed + Cow + Milkman, latinsquare))^2)~Feed, latinsquare)) #null hypothesis the variance of the different treatments are equal
#Test for homogeniety of variance using Levene Test
leveneTest(latinsquare$Milkyield, latinsquare$Feed)
leveneTest(latinsquare$Milkyield, latinsquare$Cow)
leveneTest(latinsquare$Milkyield, latinsquare$Milkman)
#leveneTest(latinsquare$Yield, latinsquare$Treatment)
#Rule of thumb check ratio of maximum variance to minimum variance should not exceed 5
max(by(latinsquare$Milkyield,latinsquare$Feed,sd))^2/min(by(latinsquare$Milkyield,latinsquare$Feed,sd))^2
max(by(latinsquare$Milkyield,latinsquare$Cow,sd))^2/min(by(latinsquare$Milkyield,latinsquare$Cow,sd))^2
max(by(latinsquare$Milkyield,latinsquare$Milkman,sd))^2/min(by(latinsquare$Milkyield,latinsquare$Milkman,sd))^2

#Factorial Experiment
#creating a dataset - adapted from Discovering statistics using R
gender<-c(rep("Female", 24), rep("Male", 24))
alchol<-c(rep("None",8),rep("2 bottles",8),rep("4 bottles",8), rep("None",8),rep("2 bottles",8),rep("4 bottles",8)  )
attractiveness<-c(65,70, 60, 60, 60, 55, 60, 55, 70, 65, 60 , 70 , 65 , 60 ,60,50,55,65,70,55,55,60,50,50,50,55,80,65,70,75,75,65,45,60,85,65,70,70,80,60,30,30,30,55,35,20,45,40)
attracdata<-data.frame(cbind(gender, alchol, attractiveness))#combining vectors into a dataframe
dim(attracdata)#finding dimensions of the dataframe
str(attracdata)#structure of the data frame
attracdata$gender<-factor(attracdata$gender) #converting to factor
attracdata$alchol<-factor(attracdata$alchol, levels=c("None","2 bottles","4 bottles"))
attracdata$attractiveness<-as.numeric(attracdata$attractiveness)
str(attracdata) #checking the structure again
ggplot(attracdata,aes(x=alchol,y=attractiveness))+geom_boxplot() #boxplot by alcohol
ggplot(attracdata,aes(x=gender,y=attractiveness))+geom_boxplot() #boxplot by gender
ggplot(attracdata,aes(x=alchol,y=attractiveness))+geom_boxplot() +facet_wrap(~gender) #by gender and alcohol

by(attracdata$attractiveness, attracdata$gender, summary) #summary by gender
by(attracdata$attractiveness, attracdata$alchol, summary) #summary by alcohol
by(attracdata$attractiveness, list(attracdata$gender,attracdata$alchol), summary)#by gender & alcohol

#one-way anova
attractmodel1<-aov(attractiveness~gender, data=attracdata)
anova(attractmodel1)
plot(allEffects(mod=attractmodel1))
attractmodel2<-aov(attractiveness~alchol, data=attracdata)
anova(attractmodel2)
plot(allEffects(mod=attractmodel2))
#two-way analysis of variance
attractmodel3<-aov(attractiveness~gender*alchol, data=attracdata)
anova(attractmodel3)
plot(allEffects(mod=attractmodel3)) # plot predicted means and 95% CLs

#contrast
contrasts(attracdata$alchol)<-cbind(c(-2, 1, 1), c(0, -1, 1))#creating contrast for alcohol
contrasts(attracdata$gender)<-c(-1, 1)#creating contrast for gender
attracdata$alchol #checking the two contrast i) no alcohol vs some alcohol ii) 2 bottles vs 4 bottles
attracdata$gender#checking one contrast - male vs female

#re-run our model with modified dataframe
attractmodel4<-aov(attractiveness~gender*alchol, data=attracdata)
anova(attractmodel4)
summary.lm(attractmodel4)

#simple effects - hold level of one factor constant and compare the levels of the other
attracdata$simple_effect<-gl(6,8) #generate a factor with n=6 levels, k=8 replication
attracdata$simple_effect<-factor(attracdata$simple_effect, levels=c(1:6), labels = 
                                   c("Female_None", "Female_2botles", "Female_4botles", 
                                     "Male_None", "Male_2botles", "Male_4botles"))
head(attracdata)
alcEffect1<-c(-2, 1, 1, -2, 1, 1) #Contrast comparing non vs some alcohol
alcEffect2<-c(0, -1, 1, 0, -1, 1) #Contrast comparing 2 bottle vs 4 bottles
gender_none<-c(-1, 0, 0, 1, 0, 0) #Contrast comparing female vs male at No alcohol
gender_2bottles<-c(0, -1, 0, 0, 1, 0) #contrast comparing female vs male at 2 bottles
gender_4bottles<-c(0, 0, -1, 0, 0, 1)#contrast comparing female vs male at 4 bottles
simple<-cbind(alcEffect1, alcEffect2,gender_none,gender_2bottles, gender_4bottles  )
contrasts(attracdata$simple_effect)<-simple
model4<-aov(attractiveness ~ simple_effect, data = attracdata)
summary.lm(model4)
#posthoc
pairwise.t.test(attracdata$attractiveness, attracdata$alchol, p.adjust.method ="bonferroni" )
postHocs<-glht(attractmodel4, linfct = mcp(alchol = "Tukey"))
summary(postHocs)
confint(postHocs)

LSD.test(attractmodel3, "alchol", console = T)
LSD.test(attractmodel3, "alchol", p.adj = "bonferroni", console = T)

TukeyHSD(attractmodel3, console = T)
HSD.test(attractmodel3, "alchol", console = T)
scheffe.test(attractmodel3, "alchol", console = T)
duncan.test(attractmodel3, "alchol", console = T)
SNK.test(attractmodel3, "alchol", console = T)

#plotting interaction
plot(attractmodel3)

