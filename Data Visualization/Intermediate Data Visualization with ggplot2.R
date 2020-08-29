#
# Title:    Intermediate Data Visualization with ggplot2
# Purpose:  Code for Statistics Layer section of Data Visualization
# Author:   Billy Caughey
# Date:     2020.08.06 - Initial build
#           2020.08.08 - Still learning
#           2020.08.14 - Facets
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

## Zooming In ##

# Run the code, view the plot, then update it
ggplot(mtcars, aes(x = wt, y = hp, color = fam)) +
    geom_point() +
    geom_smooth() +
# Add a continuous x scale from 3 to 6
    scale_x_continuous(limits = c(3, 6))

# Update the plot by adding a Cartesian coordinate system with x limits

ggplot(mtcars, aes(x = wt, y = hp, color = fam)) +
    geom_point() +
    geom_smooth() +
    coord_cartesian(xlim = c(3,6))

## Aspect Ratio I: 1:1 Ratios ##

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = FALSE) +
    # Fix the coordinate ratio
    coord_fixed()

## Aspect Ratio II: settings ratio ##

sun_plot +
    coord_fixed()

sun_plot +
    coord_fixed(ratio = 20)

## Expand and Clip ##

# Add cartesian coordinates with zero expansion, to remove all buffer margins on both the x and y axes

ggplot(mtcars, aes(wt, mpg)) +
    geom_point(size = 2) +
    # Add Cartesian coordinates with zero expansion
    coord_cartesian(expand = 0) +
    theme_classic()

# Set expand = 09, clip = "off", turn axis.line off

ggplot(mtcars, aes(wt, mpg)) +
    geom_point(size = 2) +
    # Turn clipping off
    coord_cartesian(expand = 0, clip = "off") +
    theme_classic() +
    # Remove axis lines
    theme(axis.line = element_blank())

## Log-transforming scales ##

# Produce a scatter plot of brainwt vs. bodywt
ggplot(msleep, aes(x = bodywt, y = brainwt)) +
    geom_point() +
    ggtitle("Raw Values")

# Add scale_*_*() functions
ggplot(msleep, aes(bodywt, brainwt)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    ggtitle("Scale_ functions")

# Perform a log10 coordinate system transformation
ggplot(msleep, aes(bodywt, brainwt)) +
    geom_point() +
    coord_trans(x = "log10", y = "log10")

## Adding stats to transformed scales ##

# Plot with a scale_*_*() function:
ggplot(msleep, aes(bodywt, brainwt)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Add a log10 x scale
    scale_x_log10() +
    # Add a log10 y scale
    scale_y_log10() +
    ggtitle("Scale functions")

# Plot with transformed coordinates
ggplot(msleep, aes(bodywt, brainwt)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Add a log10 coordinate transformation for x and y axes
    coord_trans(x = "log10", y = "log10")

# Notice that if I change transform after smooth and not scale, the trend line is loopy!

## Useful double axes ##

# Using airquality, plot Temp vs. Date
ggplot(airquality, aes(x = Date, y = Temp)) +
    # Add a line layer
    geom_line() +
    labs(x = "Date (1973)", y = "Fahrenheit")

# Define breaks (Fahrenheit)
y_breaks <- c(59, 68, 77, 86, 95, 104)

# Convert y_breaks from Fahrenheit to Celsius
y_labels <- (y_breaks - 32) * (5/9)

# Create a secondary x-axis
secondary_y_axis <- sec_axis(
    # Use identity transformation
    trans = identity,
    name = "Celsius",
    # Define breaks and labels as above
    breaks = y_breaks,
    labels = y_labels
)

# Examine the object
secondary_y_axis

# From previous step
y_breaks <- c(59, 68, 77, 86, 95, 104)
y_labels <- (y_breaks - 32) * 5 / 9
secondary_y_axis <- sec_axis(
    trans = identity,
    name = "Celsius",
    breaks = y_breaks,
    labels = y_labels
)

# Update the plot
ggplot(airquality, aes(Date, Temp)) +
    geom_line() +
    # Add the secondary y-axis
    scale_y_continuous(sec.axis = secondary_y_axis) +
    labs(x = "Date (1973)", y = "Fahrenheit")

## Flipping Axes II ##

# Plot fcyl bars, filled by fam
ggplot(mtcars, aes(fcyl, fill = fam)) +
    # Place bars side by side
    geom_bar(position = "dodge")

# apply coord_flip()

# Plot fcyl bars, filled by fam
ggplot(mtcars, aes(fcyl, fill = fam)) +
    # Place bars side by side
    geom_bar(position = "dodge") +
    coord_flip()

# partially overlapping bars

ggplot(mtcars, aes(fcyl, fill = fam)) +
    # Set a dodge width of 0.5 for partially overlapping bars
    geom_bar(position = position_dodge(width = 0.5)) +
    coord_flip()

## Flipping axes II ##

# Plot of wt vs. car
ggplot(mtcars, aes(x = car, y = wt)) +
    # Add a point layer
    geom_point() +
    labs(x = "car", y = "weight")

# Flip the axes to set car to the y axis
ggplot(mtcars, aes(car, wt)) +
    geom_point() +
    labs(x = "car", y = "weight") +
    coord_flip()

## Pie Charts ##

# Run the code, view the plot, then update it
ggplot(mtcars, aes(x = 1, fill = fcyl)) +
    geom_bar() +
    # Add a polar coordinate system
    coord_polar(theta = "y")


ggplot(mtcars, aes(x = 1, fill = fcyl)) +
    # Reduce the bar width to 0.1
    geom_bar(width = 0.1) +
    coord_polar(theta = "y") +
    # Add a continuous x scale from 0.5 to 1.5
    scale_x_continuous(limits = c(0.5, 1.5))

## Wind rose plots ##

# Using wind, plot wd filled by ws
ggplot(wind, aes(x = wd, fill = ws)) +
    # Add a bar layer with width 1
    geom_bar(width = 1)

# Convert to polar coordinates:
ggplot(wind, aes(wd, fill = ws)) +
    geom_bar(width = 1) +
    coord_polar(start = -pi/16)

##### Facets #####

## Facet layer basics ##

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    # Facet rows by am
    facet_grid(rows = vars(am), cols = vars(cyl))

## Many Variables ##

# See the interaction column
mtcars$fcyl_fam

# Color the points by fcyl_fam
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl_fam)) +
    geom_point() +
    # Use a paired color palette
    scale_color_brewer(palette = "Paired")

# Update the plot to map disp to size
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl_fam, size = disp)) +
    geom_point() +
    scale_color_brewer(palette = "Paired")

# Update the plot
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl_fam, size = disp)) +
    geom_point() +
    scale_color_brewer(palette = "Paired") +
    # Grid facet on gear and vs
    facet_grid(rows = vars(gear), cols = vars(vs))

# Formula Notation

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    # Facet rows by am using formula notation
    facet_grid(am ~ .)

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    # Facet rows by am using formula notation
    facet_grid(. ~ cyl)

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    # Facet rows by am and columns by cyl using formula notation
    facet_grid(am ~ cyl)

##### labeling facets #####

# Plot wt by mpg
ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    # The default is label_value
    facet_grid(cols = vars(cyl),
               labeller = label_both)

# Plot wt by mpg
ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    # The default is label_value
    facet_grid(cols = vars(cyl),
               labeller = label_context)

# Plot wt by mpg
ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    # Two variables
    facet_grid(cols = vars(vs, cyl), labeller = label_context)

##### Setting Order #####

# Make factor, set proper labels explictly
mtcars$fam <- factor(mtcars$am, labels = c(`0` = "automatic",
                                           `1` = "manual"))

# Default order is alphabetical
ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    facet_grid(cols = vars(fam))

# Make factor, set proper labels explictly, and
# manually set the label order
mtcars$fam <- factor(mtcars$am,
                     levels = c(1, 0),
                     labels = c(`1` = "manual",`0` = "automatic"))

# View again
ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    facet_grid(cols = vars(fam))

##### Variable plotting spaces 1: continuous spaces #####

# Fixed scales
ggplot(mtcars, aes(wt, mpg)) +
    geom_point() + 
    # Facet columns by cyl 
    facet_grid(cols = vars(cyl))

# Free x-axis scales 

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() + 
    # Facet columns by cyl 
    facet_grid(cols = vars(cyl),
               scales = "free_x")

# Facet the rows by 'cyl' and free the y-axis

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() + 
    # Swap cols for rows; free the y-axis scales
    facet_grid(rows = vars(cyl), 
               scales = "free_y")

##### Variable plotting spaces 2: Categorical Variables #####

# in practice, the facet behavior works the same with numerical and categorical variables.

ggplot(mtcars, aes(x = mpg, y = car, color = fam)) +
    geom_point() +
    # Facet rows by gear
    facet_grid(rows = vars(gear))

ggplot(mtcars, aes(x = mpg, y = car, color = fam)) +
    geom_point() +
    # Facet rows by gear
    facet_grid(rows = vars(gear),
               scales = "free_y",
               space = "free_y")

##### Wrapping for many levels #####

ggplot(Vocab, aes(x = education, y = vocabulary)) +
    stat_smooth(method = "lm", se = FALSE) +
    # Create facets, wrapping by year, using vars()
    facet_wrap(vars(year))

ggplot(Vocab, aes(x = education, y = vocabulary)) +
    stat_smooth(method = "lm", se = FALSE) +
    # Create facets, wrapping by year, using vars()
    facet_wrap(~ year)

ggplot(Vocab, aes(x = education, y = vocabulary)) +
    stat_smooth(method = "lm", se = FALSE) +
    # Create facets, wrapping by year, using vars()
    facet_wrap(~ year,
               ncol = 11)

##### Margin Plots #####
ggplot(mtcars, aes(x = wt, y = mpg)) + 
    geom_point() +
    # Facet rows by fvs and cols by fam
    facet_grid(gear ~ fvs + fam)

ggplot(mtcars, aes(x = wt, y = mpg)) + 
    geom_point() +
    # Update the facets to add margins
    facet_grid(rows = vars(fvs), cols = vars(fam))

ggplot(mtcars, aes(x = wt, y = mpg)) + 
    geom_point() +
    # Update the facets to add margins
    facet_grid(rows = vars(fvs, fam), 
               cols = vars(gear), 
               margins = TRUE)
