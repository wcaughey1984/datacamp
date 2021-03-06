#
# Title:    Exploratory Data Analysis
# Purpose:  (Knowledge Development) Learning how to do EDA in R
# Author:   Billy Caughey
# Date:     2020.12.03 - Initial build
#

##### Libraries #####

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(openintro)

##### Data Sets #####

cars <- read.csv("https://assets.datacamp.com/production/course_1796/datasets/cars04.csv")
comics <- read.csv("https://assets.datacamp.com/production/course_1796/datasets/comics.csv")
life <- read.csv("https://assets.datacamp.com/production/course_1796/datasets/life_exp_raw.csv")

##### Exploring cateogorical data #####

# levels(): be able to see the levels of a factor variable
# Contingency Table: table()
# Graphics in this course will be from ggplot

##### Contingency Table Review #####

# Print the first rows of the data
print(comics)

# Check levels of align
levels(comics$align)

# Check the levels of gender
levels(comics$gender)

# Create a 2-way contingency table
tab <- table(comics$align, comics$gender)

##### Dropping Levels #####

# Load dplyr
library(dplyr)

# Print tab
print(tab)

# Remove align level
comics_filtered <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()

# See the result
comics_filtered

##### Side-by-side barcharts #####

# Load ggplot2
library(ggplot2)

# Create side-by-side barchart of gender by alignment
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar(position = "dodge")

# Create side-by-side barchart of alignment by gender
ggplot(comics, aes(fill = align, x = gender)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))

##### Counts vs Proportions #####

# prop.table()
# 1 : Condition on Rows
# 2 : Condition on Columns

ggplot(comics, aes(x = id, fill = align)) + # Condition on id
    geom_bar(position = "fill") +
    ylab("proportion")


ggplot(comics, aes(x = align, fill = id)) + # Condition on align
    geom_bar(position = "fill") +
    ylab("proportion")

##### Conditional Proportions #####

tab <- table(comics$align, comics$gender)
options(scipen = 999, digits = 3) # Print fewer digits
prop.table(tab)     # Joint proportions
prop.table(tab, 2)  # Conditional on columns

##### Counts vs proportions (2) #####

# Plot of gender by align
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar()

# Plot proportion of gender, conditional on align
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar(position = "fill") +
  ylab("proportion")

##### Distribution of one variable #####

table(comics$id)

ggplot(comics, aes(x = id)) +
    geom_bar()

tab_cnt = table(comics$id, comics$align)
tab_cnt

# Faceting
ggplot(comics, aes(x = id)) +
    geom_bar() +
    facet_wrap(~ align)

# Generally a good idea to stick to barcharts rather than pie charts

##### Marginal Barchart #####

# Change the order of the levels in align
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))

# Create plot of align
ggplot(data = comics, aes(x = align)) + 
  geom_bar()

##### conditional barchart #####

# Plot of alignment broken down by gender
ggplot(comics, aes(x = align)) + 
  geom_bar() +
  facet_wrap(~ gender)

##### Improve Pie Chart #####

# Put levels of flavor in descending order
lev <- c("apple", "key lime", "boston creme", "blueberry", "cherry", "pumpkin", 
         "strawberry")
pies$flavor <- factor(pies$flavor, levels = lev)

# Create barchart of flavor
ggplot(pies, aes(x = flavor)) + 
  geom_bar(fill = "chartreuse") + 
  theme(axis.text.x = element_text(angle = 90))

##### Exploring Numerical Data #####

str(cars)

# Dotplot 
ggplot(cars, aes(x = weight)) +
  geom_dotplot(dotsize = 0.4)

# histogram 
ggplot(cars, aes(x = weight)) +
  geom_histogram(dotsize = 0.4)

# density plot 
ggplot(cars, aes(x = weight)) +
  geom_density(dotsize = 0.4)

# boxplot 
ggplot(cars, aes(x = 1, y = weight)) +
  geom_boxplot() +
  coord_flip()

# Faceted histogram 

ggplot(cars, aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_wrap(~ pickup)

##### Faceted Histogram #####

# Load package
library(ggplot2)

# Learn data structure
str(cars)

# Create faceted histogram
ggplot(cars, aes(x = city_mpg)) +
  geom_histogram() +
  facet_wrap(~ suv)

##### Boxplots and density plots #####

# Filter cars with 4, 6, 8 cylinders
common_cyl <- filter(cars, ncyl %in% c(4, 6, 8))

# Create box plots of city mpg by ncyl
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Create overlaid density plots for same data
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)

##### Distribution of One Variable #####

cars2 <- cars %>%
  filter(eng_size < 2.0)

ggplot(cars2, aes(x = hwy_mpg)) +
  geom_histogram()

cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram()

cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram(binwidth = 5)

cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_density()

cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram(bw = 5)

# Best binwidth and bandwidth? Play around with them and see 

##### Marginal and conditional histograms #####

# Create hist of horsepwr
cars %>%
  ggplot(aes(horsepwr)) +
  geom_histogram() +
  ggtitle("Histogram of Horse Power")

# Create hist of horsepwr for affordable cars
cars %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(horsepwr)) +
  geom_histogram() +
  xlim(c(90, 550)) +
  ggtitle("Histogram of Horse Power, MSRP < 25k")

##### Three Bandwidths #####

# Create hist of horsepwr with binwidth of 3
cars %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Histogram, binwidth 3")

# Create hist of horsepwr with binwidth of 30
cars %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 30) +
  ggtitle("Histogram, binwidth 30")

# Create hist of horsepwr with binwidth of 60
cars %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 60) +
  ggtitle("Histogram, binwidth 60")

##### Box Plots #####

# boxplot: 1Q, Median, and 3Q 

# Construct box plot of msrp
cars %>%
  ggplot(aes(x = 1, y = msrp)) +
  geom_boxplot()

# Exclude outliers from data
cars_no_out <- cars %>%
  filter(msrp < 100000)

# Construct box plot of msrp using the reduced dataset
cars_no_out %>%
  ggplot(aes(x = 1, y = msrp)) +
  geom_boxplot()

##### Plot selection #####

# Create plot of city_mpg
cars %>%
  ggplot(aes(x = 1, y = city_mpg)) +
  geom_boxplot()

# Create plot of width
cars %>% 
  ggplot(aes(x = width)) +
  geom_density()

##### Visualizations in higher dimensions #####

# plotting 3 variables requires use of the facet_grips and facet_wraps 

##### 3 variable plot #####

# Facet hists using hwy mileage and ncyl
common_cyl %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_grid(ncyl ~ suv) +
  ggtitle("Histogram of hwy_mpg, ncyl vs suv")

##### Measures of Center #####

# Center Mean: sum of observations / number of observations, mean()
# Center Median: Middle value in a sorted data, median()
# Center Mode: Most common observation of the data, mode()

# Mean is the balance point of the data 
# The mean is sensitive to outliers 

# group_by() %>% summarize() are a powerful combination

##### Calculating center measures #####

# Create dataset of 2007 data
gap2007 <- filter(gapminder, year == 2007)

# Compute groupwise mean and median lifeExp
gap2007 %>%
  group_by(continent) %>%
  summarize(mean(lifeExp),
            median(lifeExp))

# Generate box plots of lifeExp for each continent
gap2007 %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot()

##### Measures of Variability #####

# choose to decide the center is the mean
# Var(x) = sum((x - mean(x))^2) / (n - 1)
# sd(x) = sqrt(var(x))
# IQR(x) = q3 - q1
# Range(x) = max(x) - min(x)

# Most commonly used statistics is the sd(x)
# Var(x) and sd(x) are sensitive to outliers since they are based on averages
# IQR(x) is less sensitive to outliers

##### Calculate spread measures #####

# Compute groupwise measures of spread
gap2007 %>%
  group_by(continent) %>%
  summarize(sd(lifeExp),
            IQR(lifeExp),
            n())

# Generate overlaid density plots
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)

##### Choose measures for center and spread #####

# Compute stats for lifeExp in Americas
gap2007 %>%
  filter(continent == "Americas") %>%
  summarize(mean(lifeExp),
            sd(lifeExp))

# Compute stats for population
gap2007 %>%
  summarize(median(pop),
            IQR(pop))

##### Shape and Transformations #####

# Center
# Spread
# Shape: modality (number of humps)
# Skew: Look for the tail (Right Tail, Left Tail, Symmetric)

# If there is a tail, I can use the log(x) transformation

##### Transformations #####

# Create density plot of old variable
gap2007 %>%
  ggplot(aes(x = pop)) +
  geom_density()

# Transform the skewed pop variable
gap2007 <- gap2007 %>%
  mutate(log_pop = log(pop))

# Create density plot of new variable
gap2007 %>%
  ggplot(aes(x = log_pop)) +
  geom_density()

##### Outliers #####

# Outliers: Have extreme values from the bulk of information
# When I see a tail in density plot, use a comparative boxplot 
# It is good to consider outliers away from the distribution 

##### Identifying Outliers #####

# Filter for Asia, add column indicating outliers
gap_asia <- gap2007 %>%
  filter(continent == "Asia") %>%
  mutate(is_outlier = (lifeExp < 50))

# Remove outliers, create box plot of lifeExp
gap_asia %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = 1, y = lifeExp)) +
  geom_boxplot()

##### Introducting the data #####

# What characteristics of an email are indicators if an email is spam?
# Visuals: histograms, boxplots

##### Spam and num_char #####

# Load packages

library(ggplot2)
library(dplyr)
library(openintro)

# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarize(median(num_char),
            IQR(num_char))

# Create plot
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()

##### Spam and num_char interpretation #####

# what did I learn from the boxplots?
# the median of non-spam emails are longer than spam emails

##### Spam and !!! #####

# Compute center and spread for exclaim_mess by spam

email %>%
    group_by(spam) %>%
    summarize(median(exclaim_mess),
              IQR(exclaim_mess))

# Create plot for spam and exclaim_mess

email %>%
    mutate(log_exclaim_mess = log(exclaim_mess + 0.01)) %>%
    ggplot(aes(x = log_exclaim_mess)) +
    geom_histogram() +
    facet_wrap(~ spam)

##### Check-in 1 #####

# Zero inflation
# Analyze two components seperates (0 vs non-0)
# Categorize the zero inflation 

##### Collapsing Levels #####

# Create plot of proportion of spam by image
email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = "fill")

##### Data Integrity #####

# Test if images count as attachments
sum(email$image > email$attach)


##### Answering questions with chains #####

# Question 1
email %>%
  filter(dollar > 0) %>%
  group_by(spam) %>%
  summarize(median(num_char),
            mean(num_charr))

# Question 2
email %>%
  filter(dollar > 10) %>%
  ggplot(aes(x = spam)) +
  geom_bar()

##### Check-in 2 #####

##### What's in a number? #####

# Reorder levels
email$number_reordered <- factor(email$number, levels = c('none', 'small', 'big'))

# Construct plot of number_reordered
ggplot(email, aes(x = number_reordered)) +
  geom_bar() +
  facet_wrap(~ spam)

##### What's in a number interpretation #####











