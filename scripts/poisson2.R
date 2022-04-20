library(faraway)
library(qcc)

data("gala")

write.csv(gala, file = "data/gala.csv") # exporting tdata

gala <- read.csv("data/gala.csv")

summary(gala)

hist(gala$Species, breaks = 10, xlab = "Species Count", main = "Distribution of Species")

boxplot(gala$Species, main = "Boxplot of Species")

boxplot(log(gala$Species), main = "Boxplot of natural log of Species")

### MORE TO MORE ADDITIONAL NOTES #######

# Create a poisson model
fit <- glm(Species ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent, 
         data = gala, family = poisson())

summary(fit)

# Based on the above analysis, we find that variables Endemics,
# Area, and Nearest are significant and only
# their inclusion is sufficient to build the 
# right Poisson regression model. as there p-values < 0.05

fit.reduced <- glm(Species ~ Endemics + Area + Nearest, data = gala, family = poisson())
summary(fit.reduced)

# The output produces deviance, regression parameters,
# and standard errors. We can see that each of the parameters
# is significant at p < 0.05 level

## Interpreting coefficients #############
coef(fit.reduced)

# In Poisson regression, the dependent variable is modeled as
# the log of the conditional mean loge(l). 
# The regression parameter of 0.0355 for Endemics
# indicates that a one-unit increase in the variable 
# is associated with a 0.04 increase in the log mean number of Species,
# holding other variables constant. 
# The intercept is a log mean number of Species
# when each of the predictors equals zero.

# However, it is much easier to interpret the regression 
# coefficients in the original scale of the dependent variable
# (number of Species, rather than log number of Species). 
# The exponentiation of the coefficients will allow an easy 
# interpretation. This is done as follows.
exp(coef(fit.reduced))

# From the above findings, we can say that one unit increase
# in Area multiples the expected number of species by 0.9999,
# and a unit increase in the number of endemic species represented
# by Endemics multiplies the number of species by 1.0361.
# The most important aspect of Poisson regression is that
# exponentiated parameters have a multiplicative rather
# than an additive effect on the response variable.

qcc.overdispersion.test(gala$Species, type = "poisson")

fit.od <- glm(Species ~ Endemics + Area + Nearest, data = gala, family = quasipoisson())
summary(fit.od)