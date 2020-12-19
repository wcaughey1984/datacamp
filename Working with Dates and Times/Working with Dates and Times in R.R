#####################################################################################
# Title:    Working with Dates and Times in R                                       #
# Purpose:  (KD) Learning to become the worlds best R programmer                    #
# Author:   Billy Caughey                                                           #
# Date:     2020.11.09 - Initial Build                                              #
#####################################################################################

#################################
##### Introduction to dates #####
#################################

# Global Standard: ISO 8601: YYYY-MM-DD

##### Specifying Dates #####

# The date R 3.0.0 was released
x <- "2013-04-03"

# Examine structure of x
str(x)

# Use as.Date() to interpret x as a date
x_date <- as.Date(x)

# Examine structure of x_date
str(x_date)

# Store April 10 2014 as a Date
april_10_2014 <- as.Date("2014-04-10")

##### Automatic Import #####

# Load the readr package
library(readr)

# Use read_csv() to import rversions.csv
releases <- read_csv("rversions.csv")

# Examine the structure of the date column
str(releases$date)

# Load the anytime package
library(anytime)

# Various ways of writing Sep 10 2009
sep_10_2009 <- c("September 10 2009", "2009-09-10", "10 Sep 2009", "09-10-2009")

# Use anytime() to parse sep_10_2009
anytime(sep_10_2009)

##### Plotting #####

library(ggplot2)

# Set the x axis to the date column
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major)))

# Limit the axis to between 2010-01-01 and 2014-01-01
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  xlim(as.Date("2010-01-01"), as.Date("2014-01-01"))

# Specify breaks every ten years and labels with "%Y"
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")

##### Arithmetic and logical operations #####

# Find the largest date
last_release_date <- max(releases$date)

# Filter row for last release
last_release <- filter(releases, last_release_date == date)

# Print last_release
print(last_release)

# How long since last release?
Sys.Date() - last_release_date

##### Getting datetimes into R #####

# Use as.POSIXct to enter the datetime
as.POSIXct("2010-10-01 12:12:00")

# Use as.POSIXct again but set the timezone to `"America/Los_Angeles"`
as.POSIXct("2010-10-01 12:12:00", tz = "America/Los_Angeles")

# Use read_csv to import rversions.csv
releases <- read_csv("rversions.csv")

# Examine structure of datetime column
str(releases$datetime)

##### Datetimes behave nicely too #####

# Import "cran-logs_2015-04-17.csv" with read_csv()
logs <- read_csv("cran-logs_2015-04-17.csv")

# Print logs
print(logs)

# Store the release time as a POSIXct object
release_time <- as.POSIXct("2015-04-16 07:13:33", tz = "UTC")

# When is the first download of 3.2.0?
logs %>%
  filter(datetime == release_time,
    r_version == "3.2.0")

# Examine histograms of downloads by version
ggplot(logs, aes(x = datetime)) +
  geom_histogram() +
  geom_vline(aes(xintercept = as.numeric(release_time)))+
  facet_wrap(~ r_version, ncol = 1)

########################################
##### Parsing dates with lubridate #####
########################################

##### Selecting the right parsing function #####

library(lubridate)

# Parse x
x <- "2010 September 20th" # 2010-09-20
ymd(x)

# Parse y
y <- "02.01.2010"  # 2010-01-02
dmy(y)

# Parse z
z <- "Sep, 12th 2010 14:00"  # 2010-09-12T14:00
mdy_hm(z)

##### Specifying an order with parse_date_time() #####

# Specify an order string to parse x
x <- "Monday June 1st 2010 at 4pm"
parse_date_time(x, orders = "ABdYIp")

# Specify order to include both "mdy" and "dmy"
two_orders <- c("October 7, 2001", "October 13, 2002", "April 13, 2003",
  "17 April 2005", "23 April 2017")
parse_date_time(two_orders, orders = c("mdy", "dmy"))

# Specify order to include "dOmY", "OmY" and "Y"
short_dates <- c("11 December 1282", "May 1372", "1253")
parse_date_time(short_dates, orders = c("dOmY", "OmY", "Y"))

##### Importing Daily Weather Data #####

library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)

# Import CSV with read_csv()
akl_daily_raw <- read_csv("akl_weather_daily.csv")

# Print akl_daily_raw
print(akl_daily_raw)

# Parse date
akl_daily <- akl_daily_raw %>%
  mutate(date = as_date(date))

# Print akl_daily
print(akl_daily)

# Plot to check work
ggplot(akl_daily, aes(x = date, y = max_temp)) +
  geom_line()

##### Import hourly weather data #####

library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)

# Import "akl_weather_hourly_2016.csv"
akl_hourly_raw <- read_csv("akl_weather_hourly_2016.csv")

# Print akl_hourly_raw
print(akl_hourly_raw)

# Use make_date() to combine year, month and mday
akl_hourly  <- akl_hourly_raw  %>%
  mutate(date = make_date(year = year, month = month, day = mday))

# Parse datetime_string
akl_hourly <- akl_hourly  %>%
  mutate(
    datetime_string = paste(date, time, sep = "T"),
    #datetime = as_date(datetime_string)
    #This probably was working but wasn't producing the correct answers
    datetime = ymd_hms(datetime_string)
  )

# Print date, time and datetime columns of akl_hourly
akl_hourly %>% select(date, time, datetime)

# Plot to check work
ggplot(akl_hourly, aes(x = datetime, y = temperature)) +
  geom_line()

##### Extracting parts of a datetime #####

# year() = pulls the year of the date element
# month() = pulls the month of the date element
# day() = pulls the day of the date element
# hour(), min(), and sec()
# wday = weekday of the date element
# yday = Julian day
# tz() = time zone

# year(x) <- 2017x resets the year of a date element

# leap_year(), am(), pm() are boolean functions
# quarter() and semester() show which quarter or semester the date element is in

##### What can you Extract #####

# Examine the head() of release_time
head(release_time)

# Examine the head() of the months of release_time
head(month(release_time))

# Extract the month of releases
month(release_time) %>% table()

# Extract the year of releases
year(release_time) %>% table()

# How often is the hour before 12 (noon)?
mean(hour(release_time) < 12)

# How often is the release in am?
mean(am(release_time))

##### Adding useful labels #####

library(ggplot2)

# Use wday() to tabulate release by day of the week
wday(releases$datetime) %>% table()

# Add label = TRUE to make table more readable
wday(releases$datetime, label = TRUE) %>% table()

# Create column wday to hold labelled week days
releases$wday <- wday(releases$datetime, label = TRUE)

# Plot barchart of weekday by type of release
ggplot(releases, aes(wday)) +
  geom_bar() +
  facet_wrap(~ type, ncol = 1, scale = "free_y")

##### Extracting for plotting #####

library(ggplot2)
library(dplyr)
library(ggridges)

# Add columns for year, yday and month
akl_daily <- akl_daily %>%
  mutate(
    year = year(date),
    yday = yday(date),
    month = month(date, label = T))

# Plot max_temp by yday for all years
ggplot(data = akl_daily, aes(x = yday, y = max_temp)) +
  geom_line(aes(group = year), alpha = 0.5)

# Examine distribution of max_temp by month
ggplot(data = akl_daily, aes(x = max_temp, y = month, height = ..density..)) +
  geom_density_ridges(stat = "density")

##### Extracting for filtering and summarizing #####

# Create new columns hour, month and rainy
akl_hourly <- akl_hourly %>%
  mutate(
    hour = hour(datetime),
    month = month(datetime, label = T),
    rainy = weather == "Precipitation"
  )

# Filter for hours between 8am and 10pm (inclusive)
akl_day <- akl_hourly %>%
  filter(hour >= 8, hour <= 22)

# Summarise for each date if there is any rain
rainy_days <- akl_day %>%
  group_by(month, date) %>%
  summarise(
    any_rain = any(rainy)
  )

# Summarise for each month, the number of days with rain
rainy_days %>%
  summarise(
    days_rainy = sum(any_rain)
  )

##### Rounding datetimes #####

# rounding a date element will always give another date element
# round_date() closest day
# ceiling_date() rounds up
# floor_date() rounds down

##### Practice Rounding #####

r_3_4_1 <- ymd_hms("2016-05-03 07:13:28 UTC")

# Round down to day
floor_date(r_3_4_1, unit = "day")

# Round to nearest 5 minutes
round_date(r_3_4_1, unit = "5 minutes")

# Round up to week
ceiling_date(r_3_4_1, unit = "week")

# Subtract r_3_4_1 rounded down to day
r_3_4_1 - floor_date(r_3_4_1, unit = "day"

##### Rounding with weather data #####

# Create day_hour, datetime rounded down to hour
akl_hourly <- akl_hourly %>%
  mutate(
    day_hour = floor_date(datetime, unit = "hour")
  )

# Count observations per hour
akl_hourly %>%
  count(day_hour)

# Find day_hours with n != 2
akl_hourly %>%
  count(day_hour) %>%
  filter(n != 2) %>%
  arrange(desc(n))
