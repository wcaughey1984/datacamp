library(tidyverse)
library(dplyr)
library(lubridate)
library(visdat)
library(assertive)

##### Data Uniformity #####

head(accounts)

formats <- c("%Y-%m-%d", "%B %d, %Y")

accounts %>%
  mutate(date_opened_clean = parse_date_time(date_opened, orders = formats))

##### Currency Uniformity #####

accounts %>%
    ggplot(aes(x = date_opened, y = total)) +
    geom_point()

accounts %>%
    left_join(account_offices, by = "id")

accounts %>%
    left_join(account_offices, by = "id") %>%
    mutate(total_usd = ifelse(office == "Tokyo", total / 104, total))

accounts %>%
    left_join(account_offices, by = "id") %>%
    mutate(total_usd = ifelse(office == "Tokyo", total / 104, total))

accounts %>%
    left_join(account_offices, by = "id") %>%
    mutate(total_usd = ifelse(office == "Tokyo", total / 104, total)) %>%
    ggplot(aes(x = date_opened, y = total_usd)) +
    geom_point()

##### Validating Totals #####

accounts %>%
    mutate(theoretical_total = fund_A + fund_B + fund_C) %>%
    filter(total != theoretical_total)

##### Validating Age #####

accounts %>%
    mutate(theoretical_age = floor(as.numeric(date_opened %--% today(), "years"))) %>%
    filter(acct_age != theoretical_age)

##### Visualizing missing data #####

vis_miss(accounts)

accounts %>%
    mutate(missing_inv = is.na(inv_amount)) %>%
    group_by(missing_inv) %>%
    summarize(avg_age = mean(age, na.rm = T))

accounts %>%
    arrange(age) %>%
    vis_miss()

##### Treating missing data #####

accounts_clean <- accounts %>%
    filter(!is.na(cust_id))

accounts_clean <- accounts %>%
    filter(!is.na(cust_id)) %>%
    mutate(acct_amount_filled = ifelse(is.na(acct_amount), inv_amount * 5, acct_amount))

assert_all_are_not_na(accounts_clean$cust_id)

assert_all_are_not_na(accounts_clean$acct_amount_filled)

##### Small distance, Small difference #####

library(stringdist)
stringdist("las angelos", "los angeles", method = "dl")
stringdist("las angelos", "los angeles", method = "lcs")
stringdist("las angelos", "los angeles", method = "jaccard")

##### Fixing Typos with string distince #####

library(smss)
library(fuzzyjoin)

zagat %>%
    count(city)

##### Pair Blocking #####

library(reclin)
pair_blocking(x = zagat, y = fodors)
pair_blocking(x = zagat, y = fodors, blocking_var = "city")

##### Comparing Pairs #####

pair_blocking(x = zagat, y = fodors, blocking_var = "city") %>%
    compare_pairs(by = "name", default_comparator = lcs())

pair_blocking(x = zagat, y = fodors, blocking_var = "city") %>%
    compare_pairs(by = c("name", "phone", "addr"), default_comparator = jaro_winkler())

##### Putting it Together #####

pair_blocking(zagat, fodors, blocking_var = "city") %>%
    compare_pairs(by = "name", default_comparator = jaro_winkler()) %>%
    score_problink()

pair_blocking(zagat, fodors, blocking_var = "city") %>%
    compare_pairs(by = "name", default_comparator = jaro_winkler()) %>%
    score_problink() %>%
    select_n_to_m()

pair_blocking(zagat, fodors, blocking_var = "city") %>%
    compare_pairs(by = "name", default_comparator = jaro_winkler()) %>%
    score_problink() %>%
    select_n_to_m() %>%
    link()