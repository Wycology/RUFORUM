# LINEAR REGRESSION IN R

library(ggpubr)
library(ggplot2)

# Linear Regression: Equation of a line

# setting directory

# dataframe and importing data

eggp <- read.csv('data/eggp.csv')
eggp

# Visualization

plot(eggp$Fooduptake, eggp$Wateruptake, pch = 0, cex = 1.2, col = "blue")

# or

scatter.smooth(x = eggp$Fooduptake, y = eggp$Wateruptake)

ggplot(eggp, aes(x = Fooduptake, y = Wateruptake)) +
  geom_point() +
  stat_smooth()

# split the above command
#----------------------
# step 1
egraph <- ggplot(eggp, aes(x = Fooduptake, y = Wateruptake)) +
  geom_point()

egraph

# step2
# Add the regression line using geom_smooth()
# and typing in lm as your method for creating the line

egraph <- egraph + geom_smooth(method = "lm", col = "black")
egraph

# step3 Add the equation for the regression line.

egraph <- egraph + stat_regline_equation(label.x = 3, label.y = 5)
egraph

# step 4 Make the graph ready for publication
# We can add some style parameters
# using theme_bw() and making custom labels using labs().

egraph +
  theme_bw() +
  labs(title = "Wateruptake is a function of  Fooduptake",
       x = "FoodUptake",
       y = "Wateruptake")

#----------------------------------------------------------
# computation
# R function lm() 

# Wateruptake= b0+ b1 *Fooduptake

model <- lm(Wateruptake ~ Fooduptake, data = eggp)
model 
summary(model) 

# or

model2 <- aov(Wateruptake ~ Fooduptake, data = eggp)
summary(model2)

# b0 =-63.513, b1=2.733
# regression equation would be Wateruptake=-63.513+2.733*Fooduptake
# the intercept (b0) is -63.513. It can be interpreted 
# as the predicted amount of water taken by birds for zero food uptake.

# the regression beta coefficient for the variable
# Fooduptake (b1), also known as the slope, is 2.733.
# This means that, for unit increment in Fooduptake, the birds will increase 
# their water uptake by 2.733ml/bird.

#Regression line

ggplot(eggp, aes(Fooduptake, Wateruptake)) +
  geom_point() +
  stat_smooth(method = lm)

# Model summary
# We start by displaying the statistical
# summary of the model using the R function summary():

summary(model)

# Residual standard error (RSE), R-squared (R2) and the F-statistic are 
# metrics that are used to check how well the model fits to our data.

# The statistical hypotheses are as follow:

# Null hypothesis (H0): the coefficients are equal 
# to zero (i.e., no relationship between wateruptake and Fooduptake)
# Alternative Hypothesis (Ha): the coefficients are
# not equal to zero (i.e., there is a relationship between wateruptake and Fooduptake)

# In our example, both the p-values for the intercept 
# and the predictor variable are highly significant, 
# so we can reject the null hypothesis and which means that there
# is a significant linear relationship between wateruptake and Fooduptake.

confint(model) # Confidence interval for the whole model
#########################################################################

# Checking assumptions under linear regression

model2 <- aov(Wateruptake ~ Fooduptake, data = eggp)
summary(model2)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(model2)  # Plot the model information to check for the assumptions of the model

shapiro.test(model2$residuals)# testing for normality of residuals

####################################################################################
# Multiple regression
# Scatter plot in one page/all graphs in one page
par(mfrow = c(2, 2))
plot(eggp)

# Multiple Regression analysis
model3 <- lm(Wateruptake ~ Fooduptake + Eggproduction, data = eggp)

summary(model3)

confint(model3,  level = 0.95)

# Alternatively

model4 <- lm(Wateruptake ~., data = eggp)
summary(model4)

#--------------------------------------------------------------
# dealing with categorical variables
uptake <- read.csv("data/indicator.csv")
uptake

uptake$Species1 <- ifelse(uptake$Species == "pinusc", 0, 1)

colnames(uptake)
plot(Uptake ~ Time, data = uptake)
mod <- lm(Uptake~ Time + Species1, data = uptake)
summary(mod)

#----------------------------------------------------------------
# Polynomial regression
hard <- read.csv("data/polyreg.csv")
hard
colnames(hard)
plot(Tensiles.Y.~ HWconcn.X., data = hard)


mod1 <- lm(Tensiles.Y.~ HWconcn.X., data = hard)
summary(mod1)

par(mfrow = c(2,2))
plot(mod1)

# Quadratic regression
hard$hwcont <- (hard$HWconcn.X.)^2

mod2 <- lm(Tensiles.Y.~ HWconcn.X. + hwcont, data = hard)
summary(mod2)

par(mfrow = c(2, 2))
plot(mod2)

graphics.off()
#''''''''''''''''''''''''''''''''''''''''''''''''''''''
