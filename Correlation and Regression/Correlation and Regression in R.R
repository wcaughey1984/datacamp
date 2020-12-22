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
ggplot(data = ___, aes(x = ___, y = ___)) + 
    ___ + 
    ___(method = ___, se = FALSE)













































