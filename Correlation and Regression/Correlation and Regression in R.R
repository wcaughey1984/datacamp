# 
# title:    Correlation and Regression in R
# purpose:  (Knowledge Development) learning correlation and regression in R
# author:   Billy Caughey
# date:     2020.12.13 - Initial Build 
#

##### Library #####
library(openintro)
library(tidyverse)

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


























