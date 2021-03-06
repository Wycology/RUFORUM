library(olsrr)
library(faraway)

data(state)
statedata <- data.frame(state.x77, row.names = state.abb) 

statedata
summary(statedata)

g <- lm(Life.Exp ~., data = statedata)
summary(g)

forward <- ols_step_forward_p(g, penter = 0.05)
forward

# Using aic

forward <- ols_step_forward_aic(g, detail = TRUE)
forward


# Backward elimination

back.p <- ols_step_backward_p(g, prem = 0.05)
back.p

back.aic <- ols_step_backward_aic(g, details = TRUE)
back.aic

# Stepwise method

both.p <- ols_step_both_p(g, pent = 0.05, prem = 0.05)
both.p

# Using aic

both.aic <- ols_step_both_aic(g, details = TRUE)
both.aic

# using option all subsets possible

all <- ols_step_all_possible(g)
all

# Creating a data frame for results

as.data.frame(all)

# To obtain plots of mallow's cp and other indices

plot(all)

# Using the best subset regression

best <- ols_step_best_subset(g)
best
