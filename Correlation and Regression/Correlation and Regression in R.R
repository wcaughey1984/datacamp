# 
# title:    Correlation and Regression in R
# purpose:  (Knowledge Development) learning correlation and regression in R
# author:   Billy Caughey
# date:     2020.12.13 - Initial Build 
#

##### Library #####
library(openintro)
library(tidyverse)
library(datasets)

##### Visualizing Bivariate Relationships #####

# both variables are numerical 
# Output Variable = Response Variable (Y)
# Input Variable = Explanatory Variable (X)

# Scatterplot is used to describe x vs y
# Response Variable = Vertical 
# Explanatory Variable = Horizontal
# cut() = function to break up numerical variables 

##### Scatterplots #####

# Scatterplot of weight vs. weeks
ncbirths %>%
    ggplot(aes(x = weeks, y = weight)) +
    geom_point()

##### Boxplots as discretized/conditioned scatterplots #####
# Boxplot of weight vs. weeks
ggplot(data = ncbirths, 
       aes(x = cut(weeks, breaks = 5), y = weight)) + 
    geom_boxplot()

##### Charactering Bivariate Relationships #####

# Form: linear vs non-linear
# Direction: Positive vs negative
# Strength: How much scatter is present?
# Outliers: Points that don't fit the overall pattern 

##### Creating Scatterplots #####

# Mammals scatterplot 
ggplot(data = mammals,
       aes(x = brain_wt,
           y = body_wt)) +
    geom_point()

# Baseball player scatterplot 
ggplot(data = mlbbat10,
       aes(x = obp,
           y = slg)) +
    geom_point()

# body dimensions scatterplot 
ggplot(data = bdims,
       aes(x = hgt,
           y = wgt,
           color = factor(sex))) +
    geom_point()

# Smoking scatterplot 
ggplot(data = smoking,
       aes(x = age,
           y = amt_weekdays)) +
    geom_point()

##### Transformations #####

# Scatterplot with coord_trans()
ggplot(data = mammals,
       aes(x = body_wt,
           y = brain_wt)) + 
    geom_point() +
    coord_trans(x = "log10", y = "log10")

# Scatterplot with scale_x_log10() and scale_y_log10()
ggplot(data = mammals, 
       aes(x = body_wt,
           y = brain_wt)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() 

##### Identfying Outliers #####

# Filter for AB greater than or equal to 200
ab_gt_200 <- mlbbat10 %>%
    filter(at_bat >= 200) 

# Scatterplot of SLG vs. OBP
ggplot(ab_gt_200, aes(x = obp, y = slg)) +
    geom_point()

# Identify the outlying player
ab_gt_200 %>%
    filter(obp < 0.2)

##### Computing Correlation #####

# Compute correlation
ncbirths %>%
    summarize(N = n(), r = cor(weight, mage))

# Compute correlation for all non-missing pairs
ncbirths %>%
    summarize(N = n(), r = cor(___, ___, use = ___))


##### The Anscombe Dataset #####

# Be very aware of what is going on. 
# My eyes will tell me one thing, but the math will say something different

##### Exploring Anscombe #####

# Compute properties of Anscombe
Anscombe %>%
    group_by(set) %>%
    summarize(
        N = n(), 
        mean_of_x = mean(x), 
        std_dev_of_x = sd(x), 
        mean_of_y = mean(y), 
        std_dev_of_y = sd(y), 
        correlation_between_x_and_y = cor(x,y)
    )

##### Perception of correlation #####

# Run this and look at the plot
ggplot(data = mlbBat10, aes(x = OBP, y = SLG)) +
    geom_point()

# Correlation for all baseball players
mlbBat10 %>%
    summarize(N = n(), 
              r = cor(OBP, SLG))

# Correlation for all players with at least 200 ABs
mlbBat10 %>%
    filter(AB >= 200) %>%
    summarize(N = n(), r = cor(OBP, SLG))

# Run this and look at the plot
ggplot(data = bdims, aes(x = hgt, y = wgt, color = factor(sex))) +
    geom_point() 

# Correlation of body dimensions
bdims %>%
    group_by(sex) %>%
    summarize(N = n(), 
              r = cor(hgt, wgt))

# Run this and look at the plot
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
    geom_point() + scale_x_log10() + scale_y_log10()

# Correlation among mammals, with and without log
mammals%>%
    summarize(N = n(), 
              r = cor(BodyWt, BrainWt), 
              r_log = cor(log(BodyWt), log(BrainWt)))

##### Interpretation and Correlation #####

# Correlation is not causation

##### Spurious Correlations #####

# Correlation does not imply causation!
# Spurious correlations are when variables move together but are not related
# Time is an obvious confounder! Be skeptical of correlation when time is used.
# Be on the lookout for spurious correlations

##### Spurious correlation in random data #####

# Create faceted scatterplot
noise %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~ z)

# Compute correlations for each dataset
noise_summary <- noise %>%
    group_by(z) %>%
    summarize(N = n(), 
              spurious_cor = cor(x, y))

# Isolate sets with correlations above 0.2 in absolute strength
noise_summary %>%
    filter(abs(spurious_cor) > 0.2)

##### Visualization of Linear Models #####

# Start with scatterplot 
# use geom_abline(intercept, slope)
# Criteria to determine how good the 'fit' is
# Least Squares Criteria: geom_smooth(method = "lm")

##### The Best Fit Line #####

# Scatterplot with regression line
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE)

##### Uniqueness of least squares line #####

# Estimate optimal value of my_slope
add_line(my_slope = 1)

##### Understanding Linear Models #####

# yhat = b0 + x1 * b1
# e = y - yhat

##### Regression model output terminology #####

64.594 - 0.591 * 92.4

##### Fitting a linear model "by hand" #####

# Print bdims_summary
bdims_summary

# Add slope and intercept
bdims_summary %>%
    mutate(slope = r * (sd_wgt / sd_hgt), 
           intercept = mean_wgt - slope * mean_hgt)

##### Regression vs. Regression to the mean #####

# Height of children vs. height of father
ggplot(data = Galton_men, aes(x = father, y = height)) +
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_smooth(method = "lm", se = FALSE)

# Height of children vs. height of mother
ggplot(data = Galton_women, aes(x = mother, y = height)) +
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_smooth(method = "lm", se = FALSE)

##### Interpretation of Regression #####

# Be very careful of the units and scale

##### Fitting simple linear models #####

# Linear model for weight as a function of height
lm(wgt ~ hgt, data = bdims)

# Linear model for SLG as a function of OBP
lm(SLG ~ OBP, data = mlbBat10)

# Log-linear model for body weight as a function of brain weight
lm(log(BodyWt) ~ log(BrainWt), data = mammals)

##### Your linear model object #####

# coef(): extracts the coefficients from a regression model
# summary(): sumamrize the linear models
# fitted.values(): extracts the fitted values of a linear model
# residuals: actual y - exprected y, residuals() function
# package broom
# augment(): pulls in all the pieces to evaluate a model

##### The lm Summary Output #####

# Show the coefficients
coef(mod)

# Show the full output
summary(mod)

##### Fitted Values and Residuals #####

# Mean of weights equal to mean of fitted values?
mean(bdims$wgt) == mean(fitted.values(mod))

# Mean of the residuals
mean(residuals(mod))

##### Tidy your linear model #####

# Load broom
library(broom)

# Create bdims_tidy
bdims_tidy <- broom::augment(mod)

# Glimpse the resulting data frame
dplyr::glimpse(bdims_tidy)

##### Using your linear model #####

# predict(lm, newdata): returns the prediction made by the model

# example using broom
isrs <- broom::augment(mod, newdata = new_data)
ggplot(data = textbooks, aes(x = amazNew, y = uclaNew)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_point(data = isrs, aes(y = .fitted), size = 3, color = "red") # the new point to put on the value

##### Making Predictions #####

# Print ben
print(ben)

# Predict the weight of ben
predict(mod, newdata = ben)

##### Adding a regression line to a plot manually #####

# Add the line to the scatterplot
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
    geom_point() + 
    geom_abline(data = coefs, 
                aes(intercept = `(Intercept)`, slope = hgt),  
                color = "dodgerblue")

##### Assessing Model Fit #####

# Use the Least Squares Criteria
# SSE : sum(.resid^2)
# SSE_also : (n() - 1) & var(.resid)
# RMSE = sqrt(SSE / (n - 2))
# Interpretation: Our model predicts within RMSE (units) of the truth

##### Standard error of residuals #####

# View summary of model
summary(mod)

# Compute the mean of the residuals
mean(residuals(mod))

# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))

##### Comparing Model Fits #####

# Null Model: lm(y ~ 1, data), 1 is the input (bases the model on the average of y)
# Test Model: lm(y ~ x, data), x is the input (model we want to show is better)
# SSE(Null Model) / SSE(Test Model) is a ratio to determine how well our model really is.
# This ratio, with some extra features, is R^2 as the coefficient of determination
# High R^2 can be overfit... becareful 
# "Essential all models are wrong, but some are useful." - George Box

##### Assessing simple linear model fit #####

# View model summary
summary(mod)

# Compute R-squared
bdims_tidy %>%
    summarize(var_y = var(wgt), var_e = var(hgt)) %>%
    mutate(R_squared = 1 - var(wgt)/var(hgt))

##### Linear vs Average #####

# Compute SSE for null model
mod_null %>%
    summarize(SSE = sum(.resid^2))

# Compute SSE for regression model
mod_hgt %>%
    summarize(SSE = sum(.resid^2))

##### Unusual Points #####

# leverage: relationship between the actual explanatory variable and the mean of the explanatory variable
# This means, values with a small leverage are closer to the mean
# Also means, values with a large leverage are farther away from the mean
# augment(mod) -> .hat shows the leverage
# A point can have high leverage but not be influential
# Cook's Distance : mathematically computes the leverage and influence of a point (augment(mod) -> .cooksd)

##### Leverage #####

# Rank points of high leverage
mod %>%
    augment() %>%
    arrange(desc(.hat)) %>%
    head()

##### Influence #####

# Rank influential points

augment(mod) %>%
    arrange(desc(.cooksd)) %>%
    head()

##### Removing Outliers #####

# Create nontrivial_players
nontrivial_players <- mlbBat10 %>%
    filter(AB >= 10, 
           OBP < .500)

# Fit model to new data
mod_cleaner <- lm(SLG ~ OBP, data = nontrivial_players)

# View model summary
summary(mod_cleaner)

# Visualize new model
nontrivial_players %>%
    ggplot(aes(x = OBP, y = SLG)) +
    geom_point() +
    geom_smooth(method = "lm")

##### High leverage points #####

# Rank high leverage points
mod %>%
    augment() %>%
    arrange(desc(.hat), .cooksd) %>%
    head()










