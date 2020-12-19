# 
# Title:    Case Study - Exploratory Data Analysis in R
# Purpose:  (Knowledge Development) Case study of UN voting
# Author:   Billy Caughey
# Date:     2020.12.12 - Initial Build 
#

##### Libraries #####
library(tidyverse)
library(unvotes)

votes <- un_votes %>%
    inner_join(un_roll_calls, by = "rcid") %>%
    mutate(vote = as.numeric(vote))


##### The United Nations Voting Dataset #####

# UN General Assemply
# Row: country voting pair
# rcid: roll call id
# session: which year long session 
# vote: 1 = yes, 9 = not member yet
# ccode: country code

##### Filtering Rows #####

# Load the dplyr package
library(dplyr)

# Print the votes dataset
print(votes)

# Filter for votes that are "yes", "abstain", or "no"
votes %>%
    filter(vote <= 3)

##### Adding a year column #####

# Add another %>% step to add a year column
votes %>%
    filter(vote <= 3) %>%
    mutate(year = 1945 + session)

##### Adding a country column #####

# Load the countrycode package
library(countrycode)

# Convert country code 100
countrycode(100, "cown", "country.name")

# Add a country column within the mutate: votes_processed
votes_processed <- votes %>%
    filter(vote <= 3) %>%
    mutate(year = session + 1945,
           votes_processed = countrycode(country_code, "cown", "country.name"))


##### Grouping and Summarizing #####

# Percentage of Yes Votes 
# Summarize: takes many rows and converts them into one
# percentage_yes = mean(vote == 1)
# Remember the group_by + summarize combo!

##### Summarizing the full dataset #####

# Print votes_processed

votes_processed

# Find total and fraction of "yes" votes

votes_processed %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1))


##### Summarizing by year #####

# Change this code to summarize by year
votes_processed %>%
    group_by(year) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1))

##### Summarizing by country #####

# Summarize by country: by_country
by_country <- votes_processed %>%
    group_by(country) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1))

##### Sorting and filtering summarized data #####

# Get used to using the following verbs at all stages of an analysis
# summarize
# filter
# arrange
# group_by

##### Sorting by percentage of "yes" votes #####

# You have the votes summarized by country
by_country <- votes_processed %>%
    group_by(country) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1))

# Print the by_country dataset
by_country

# Sort in ascending order of percent_yes
by_country %>%
    arrange(percent_yes)

# Now sort in descending order
by_country %>%
    arrange(desc(percent_yes))

##### Filtering summarized output #####

# Filter out countries with fewer than 100 votes
by_country %>%
    arrange(percent_yes) %>%
    filter(total >= 100)

##### Vizualization with ggplot2 #####

# Steps to Exploratory Data Analysis 
# 1 - Cleaning Data Analysis 
# 2 - Visualization 

##### Plotting a line over time #####

# Define by_year
by_year <- votes_processed %>%
    group_by(year) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1))

# Load the ggplot2 package
library(ggplot2)

# Create line plot
ggplot(by_year, aes(x = year, y = percent_yes)) +
    geom_line()

##### Other ggplot2 layers #####

# Change to scatter plot and add smoothing curve
ggplot(by_year, aes(year, percent_yes)) +
    geom_point() +
    geom_smooth()

##### Visualizing by country #####

# summarize by year and country 

##### Summarizing by year and coutnry #####

# Group by year and country: by_year_country
by_year_country <- votes_processed %>%
    group_by(year, country) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1))

##### Plotting just the UK over time #####

# Start with by_year_country dataset
by_year_country <- votes_processed %>%
    group_by(year, country) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1))

# Print by_year_country
by_year_country

# Create a filtered version: UK_by_year
UK_by_year <- by_year_country %>%
    filter(country == "United Kingdom of Great Britain and Northern Ireland")

# Line plot of percent_yes over time for UK only
UK_by_year %>%
    ggplot(aes(x = year, y = percent_yes)) +
    geom_line()

##### Plotting Multiple Countries #####

# Vector of four countries to examine
countries <- c("United States of America", 
               "United Kingdom of Great Britain and Northern Ireland",
               "France", 
               "India")

# Filter by_year_country: filtered_4_countries
filtered_4_countries <- by_year_country %>%
    filter(country %in% countries)

# Line plot of % yes in four countries
filtered_4_countries %>%
    ggplot(aes(x = year, y = percent_yes, color = country)) +
    geom_line()

##### Faceting by Country #####

# ~ means explained by 
# scales = "free_y" makes each y-axis independent of each other

##### Faceting the time series #####

# Vector of six countries to examine
countries <- c("United States of America", 
               "United Kingdom of Great Britain and Northern Ireland",
               "France", 
               "Japan", 
               "Brazil", 
               "India")

# Filtered by_year_country: filtered_6_countries
filtered_6_countries <- by_year_country %>%
    filter(country %in% countries)

# Line plot of % yes over time faceted by country
filtered_6_countries %>%
    ggplot(aes(x = year, y = percent_yes)) +
    geom_line() +
    facet_wrap(~ country)

##### Linear Regression #####

# lm: linear model 
# summary(lm) : sumnmarizes the lm object 
# "Visualizations can suprise you, but it doesn't scale well. 
#  Modeling scales well, but it can't surprise you." - Hadley Wickam

##### Linear Regression on the United States #####

# Percentage of yes votes from the US by year: US_by_year
US_by_year <- by_year_country %>%
    filter(country == "United States of America")

# Print the US_by_year data
US_by_year

# Perform a linear regression of percent_yes by year: US_fit
US_fit <- lm(percent_yes ~ year, data = US_by_year)

# Perform summary() on the US_fit object
summary(US_fit)

##### Tidying models with broom #####

# A model fit is a 'messy' object 
library(broom)
tidy(US_fit)

##### Tidying a linear regression model #####

# Load the broom package
library(broom)

# Call the tidy() function on the US_fit object
tidy(US_fit)

##### Combining models for multiple countries #####

# Linear regression of percent_yes by year for US
US_by_year <- by_year_country %>%
    filter(country == "United States of America")
US_fit <- lm(percent_yes ~ year, US_by_year)

# Fit model for the United Kingdom
UK_by_year <- by_year_country %>%
    filter(country == "United Kingdom of Great Britain and Northern Ireland")
UK_fit <- lm(percent_yes ~ year, UK_by_year)

# Create US_tidied and UK_tidied
US_tidied <- tidy(US_fit)
UK_tidied <- tidy(UK_fit)

# Combine the two tidied models
US_tidied %>%
    bind_rows(UK_tidied)

##### Nesting for multiple models #####

# fitting model for each country 
# start with by_year_country 
# tidyr::nest = nests data by variable (This means I'm creating a list)
# tidyr::unnest = opens the data back up to the top level 

##### Nesting a data frame #####

# Load the tidyr package
library(tidyr)

# Nest all columns besides country
by_year_country %>%
    nest(-country)

##### List columns #####

# All countries are nested besides country
nested <- by_year_country %>%
    nest(-country)

# Print the nested data for Brazil
nested$data[[7]]

##### Unnesting #####

# All countries are nested besides country
nested <- by_year_country %>%
    nest(-country)

# Unnest the data column to return it to its original form
nested %>%
    unnest()

##### Fitting Multiple Models #####

library(purrr)
v <- list(1, 2, 3)
map(v, ~ . * 10)

dat1 <- by_year_country %>%
    nest(-country) %>%
    mutate(models = map(data, ~ lm(percent_yes ~ year, .)))

dat2 <- dat1 %>%
    mutate(tidied = map(models, tidy))

dat3 <- dat2 %>%
    unnest(tidied)

##### Performing linear regression on each nested dataset #####

# Load tidyr and purrr
library(tidyr)
library(purrr)

# Perform a linear regression on each item in the data column
by_year_country %>%
    nest(-country) %>%
    mutate(model = map(data, ~ lm(percent_yes ~ year, .)))

##### Tidy each linear regression model #####

# Load the broom package
library(broom)

# Add another mutate that applies tidy() to each model
by_year_country %>%
    nest(-country) %>%
    mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
           tidied = map(model, tidy))

##### Unnesting a data frame #####

# Add one more step that unnests the tidied column
country_coefficients <- by_year_country %>%
    nest(-country) %>%
    mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
           tidied = map(model, tidy)) %>%
    unnest(tidied)

# Print the resulting country_coefficients variable
country_coefficients

##### Working with many tidy models #####

country_coefficients %>%
    filter(term == "year") %>%
    filter(p.adjust(p.value) < 0.05) # multiple test correction

##### Filtering model terms #####

# Print the country_coefficients dataset
country_coefficients

# Filter for only the slope terms
country_coefficients %>%
    filter(term == "year")

##### Filtering for significant countries #####

# Filter for only the slope terms
slope_terms <- country_coefficients %>%
    filter(term == "year")

# Add p.adjusted column, then filter

slope_terms %>%
    mutate(p.adjusted = p.adjust(p.value)) %>%
    filter(p.adjusted < 0.05)

##### Sorting by slope #####

# Filter by adjusted p-values
filtered_countries <- country_coefficients %>%
    filter(term == "year") %>%
    mutate(p.adjusted = p.adjust(p.value)) %>%
    filter(p.adjusted < .05)

# Sort for the countries increasing most quickly
filtered_countries %>%
    arrange(estimate)

# Sort for the countries decreasing most quickly
filtered_countries %>%
    arrange(desc(estimate))

##### Joining Datasets with inner_join #####

# Load dplyr package
library(dplyr)

# Print the votes_processed dataset
votes_processed

# Print the descriptions dataset
descriptions

# Join them together based on the "rcid" and "session" columns
votes_joined <- votes_processed %>%
    inner_join(descriptions, by = c("rcid", "session"))

##### Filtering the joined dataset #####

# Filter for votes related to colonialism
votes_joined %>%
    filter(co == 1)

##### Visualizing coloninalism votes #####

# Load the ggplot2 package
library(ggplot2)

# Filter, then summarize by year: US_co_by_year
US_co_by_year <- votes_joined %>%
    filter(country == "United States") %>%
    filter(co == 1) %>%
    group_by(year) %>%
    summarize(percent_yes = mean(vote == 1))

# Graph the % of "yes" votes over time
US_co_by_year %>%
    ggplot(aes(x = year, y = percent_yes)) +
    geom_line()

##### Tidy Data #####

# gather(key, new variable, values)
# key: this variable will contain the names of the fields of the values
# new variable: the new variable that will contain values
# values: the cell level information being put into new_variable 

##### Using gather to tidy a dataset #####

# Load the tidyr package
library(tidyr)

# Gather the six me/nu/di/hr/co/ec columns
votes_joined %>%
    gather(topic, has_topic, me:ec)

# Perform gather again, then filter
votes_gathered <- votes_joined %>%
    gather(topic, has_topic, me:ec) %>%
    filter(has_topic == 1)

##### Recoding the topics #####

# Replace the two-letter codes in topic: votes_tidied
votes_tidied <- votes_gathered %>%
    mutate(topic = recode(topic,
                             "me" = "Palestinian conflict",
                             "nu" = "Nuclear weapons and nuclear material",
                             "di" = "Arms control and disarmament",
                             "hr" = "Human rights",
                             "co" = "Colonialism",
                             "ec" = "Economic development"))

##### Summarize by country, year, and topic #####

# Print votes_tidied
votes_tidied

# Summarize the percentage "yes" per country-year-topic
by_country_year_topic <- votes_tidied %>%
    group_by(country, year, topic) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1)) %>%
    ungroup()

# Print by_country_year_topic
by_country_year_topic

##### Visualizing trends in topics for one country #####

# Load the ggplot2 package
library(ggplot2)

# Filter by_country_year_topic for just the US
US_by_country_year_topic <- by_country_year_topic %>%
    filter(country == "United States")

# Plot % yes over time for the US, faceting by topic
US_by_country_year_topic %>%
    ggplot(aes(x = year, y = percent_yes)) +
    geom_line() +
    facet_wrap(~ topic)

##### Tidy modeling by topic and country #####

##### Nesting by topic and country #####

# Load purrr, tidyr, and broom
library(purrr)
library(tidyr)
library(broom)

# Print by_country_year_topic
by_country_year_topic 

# Fit model on the by_country_year_topic dataset
country_topic_coefficients <- by_country_year_topic %>%
    nest(-country, - topic) %>%
    mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
           tidied = map(model, tidy)) %>%
    unnest(tidied)

# Print country_topic_coefficients
country_topic_coefficients

##### Interpreting tidy models #####

# Create country_topic_filtered

country_topic_filtered <- country_topic_coefficients %>%
    filter(term == "year") %>%
    mutate(p.adjusted = p.adjust(p.value)) %>%
    filter(p.adjusted < 0.05)

##### Steepest trends by topic #####

country_topic_filtered %>%
    arrange(estimate)

##### Checking models visually #####

# Create vanuatu_by_country_year_topic
vanuatu_by_country_year_topic <- by_country_year_topic %>%
    filter(country == "Vanuatu")

# Plot of percentage "yes" over time, faceted by topic
vanuatu_by_country_year_topic %>%
    ggplot(aes(x = year, y = percent_yes)) +
    geom_line() +
    facet_wrap(~ topic)





























































