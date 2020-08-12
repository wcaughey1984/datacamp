# 
# Title:    Intermediate Data Visualization with ggplot2
# Purpose:  Code for Statistics Layer section of Data Visualization
# Author:   Billy Caughey 
# Date:     2020.08.06 - Initial build 
#           2020.08.08 - Still learning
#

##### Libraries #####
library(tidyverse)

##### Smoothing #####

# View the structure of mtcars
str(mtcars)

# Using mtcars, draw of scatter plot of mpg vs wt
ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

# Amend the plot to add a smooth layer
ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    geom_smooth()

# Amend the plot. Use lin. reg. smoothing; turn off std err ribbon
ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Amend the plot. Swap geom_smooth() for stat_smooth()
ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE)

##### Grouping Variables #####

mtcars <- mtcars %>%
    mutate(fcyl = factor(cyl))

# Using mtcars, plot mpg vs. wt, colored by fcyl

ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
    geom_point() +
    stat_smooth(method = "lm", se = F)

# Amend the plot to add antoher smooth layer with dummy grouping

ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
    geom_point() +
    stat_smooth(method = "lm", se = F) +
    stat_smooth(aes(group = 1), method = "lm", se = F)

##### Modifying stat_smooth #####

# Explore the effect of span argument on the LOESS curve 

ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    stat_smooth(color = "red", span = 0.9, se = F) +
    stat_smooth(color = "green", span = 0.6, se = F) +
    stat_smooth(color = "blue", span = 0.3, se = F)

# Amond the plot to color by fcyl

ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
    geom_point() +
    stat_smooth(se = F) +
    stat_smooth(method = "lm", se = F)

# Compare Loess to LM over the whole set

ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
    geom_point() +
    # Map color to dummy variable "All"
    stat_smooth(aes(color = "All"), se = FALSE) +
    # Note, group = 1 DOES NOT produce the same plot
    stat_smooth(method = "lm", se = FALSE)

##### Modifying stat_smooth (2) #####

library(car)

Vocab <- Vocab %>%
    mutate(year_group = ifelse(year >= 1974 & year <= 1995,
                               "[1974,1995]","(1995,2016]"),
           year_group = factor(year_group))

# Using Vocab, plot vocabulary vs. education, colored by year group
ggplot(Vocab, aes(x = education, y = vocabulary, color = year_group)) +
    # Add jittered points with transparency 0.25
    geom_jitter(alpha = 0.25) +
    # Add a smooth lin. reg. line (with ribbon)
    stat_smooth(method = "lm")

# mapp the fill color to year_group, set line size = 2
ggplot(Vocab, aes(x = education, y = vocabulary, color = year_group)) +
    # Add jittered points with transparency 0.25
    geom_jitter(alpha = 0.25) +
    # Add a smooth lin. reg. line (with ribbon)
    stat_smooth(aes(fill = year_group), size = 2, method = "lm")

##### Quantile #####

# Map the 5th, 50th, and 95th quantile

ggplot(Vocab, aes(x = education, y = vocabulary)) + 
    geom_jitter(alpha = 0.25) +
    stat_quantile(quantiles = c(0.05, 0.5, 0.95))

# Plot year_group 

ggplot(Vocab, aes(x = education, y = vocabulary, color = year_group)) + 
    geom_jitter(alpha = 0.25) +
    stat_quantile(quantiles = c(0.05, 0.5, 0.95))

##### Using stat_sum #####

# Run this, look at the plot, then update it
ggplot(Vocab, aes(x = education, y = vocabulary)) +
    # Replace this with a sum stat
    geom_jitter(alpha = 0.25)

# Run this, look at the plot, then update it
ggplot(Vocab, aes(x = education, y = vocabulary)) +
    # Replace this with a sum stat
    stat_sum(alpha = 0.25)

# Modify the size aesthetic with a appropriate scale function

ggplot(Vocab, aes(x = education, y = vocabulary)) +
    stat_sum(alpha = 0.25) +
    scale_size(rang = c(1,10))

# Inside stat_sum(), set size = ..prop..

ggplot(Vocab, aes(x = education, y = vocabulary)) +
    stat_sum(aes(size = ..prop..))

# Amend the plot to group by education

ggplot(Vocab, aes(x = education, y = vocabulary, group = education)) +
    stat_sum(aes(size = ..prop..))

##### Preparations #####

# Define position objects
# 1. Jitter with width 0.2
posn_j <- position_jitter(width = 0.2)

# 2. Dodge with width 0.1
posn_d <- position_dodge(width = 0.1)

# 3. Jitter-dodge with jitter.width 0.2 and dodge.width 0.1
posn_jd <- position_jitterdodge(jitter.width = 0.2, dodge.width =  0.1)

# Create the plot base: wt vs. fcyl, colored by fam
p_wt_vs_fcyl_by_fam <- ggplot(mtcars, aes(x = fcyl, y = wt, color = fam))

# Add a point layer
p_wt_vs_fcyl_by_fam +
    geom_point()

##### Using Position Objects #####

# Add jittering only
p_wt_vs_fcyl_by_fam +
    geom_point(position = posn_j)

# Add dodging only 
p_wt_vs_fcyl_by_fam +
    geom_point(position = posn_d)

# Add jittering and dodging only 
p_wt_vs_fcyl_by_fam +
    geom_point(position = posn_jd)

##### Plotting Variations #####

p_wt_vs_fcyl_by_fam_jit +
    # Add a summary stat of std deviation limits
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), position = posn_d) 

p_wt_vs_fcyl_by_fam_jit +
    # Add a summary stat of std deviation limits
    stat_summary(fun.data = mean_sdl, 
                 fun.args = list(mult = 1), 
                 position = posn_d,
                 geom = "errorbar") 

p_wt_vs_fcyl_by_fam_jit +
    # Add a summary stat of std deviation limits
    stat_summary(fun.data = mean_sdl, 
                 fun.args = list(mult = 1), 
                 position = posn_d,
                 geom = "errorbar") +
    stat_summary(fun.data = mean_cl_normal,
                 position = posn_d)

##### Coordinates #####


























