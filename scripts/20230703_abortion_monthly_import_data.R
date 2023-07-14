library(tidyverse)
library(readxl)


# script to import monthly abortion data scraped from Indiana Terminated Pregnancy Reports between
# 2014 and 2022. Initial project gathering data from 2014:2021 can be found at
# https://github.com/tedschurter/indiana_abortion/tree/main


# import 2022 data - scraped from https://www.in.gov/health/vital-records/files/2022-TPR-Annual.pdf

mn_ab <- read_xlsx("external_data/2022_termination_data.xlsx", sheet = 11)

# add year
mn_ab$year <- 2022

# change column name
mn_ab$nonres <- mn_ab$non_res

# reorder columns
mn_ab <- mn_ab %>% select(c(year, month, res, nonres))

# check months for errors
unique(mn_ab$month)
n_distinct(mn_ab$month)


# import data from 2014:2021 - previously scraped, cleaned via
# https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20230516_monthly_totals_wr.R

# url for 2014: 2021 monthly data for residents and non residents 
mn_14_21_url <- "https://raw.githubusercontent.com/tedschurter/indiana_abortion/main/Exported_Data/month_ab_clean.csv"

mn_14_21 <- read.csv(mn_14_21_url)

# to match format of two dataframes: add year; non_res to nonres to mn_ab; order columns

# check months for errors

unique(mn_14_21$month)
n_distinct(mn_14_21$month)

# check years for errors
unique(mn_14_21$year)
n_distinct(mn_14_21$year)

# bind two dataframes
mnth <- rbind(mn_ab, mn_14_21)

# add column for total monthly abortions
mnth <- mnth %>% mutate(tot = res+nonres)

# check number of rows (nine years * 12 months should be 108 rows)
nrow(mnth)

# should be 12 distinct months
n_distinct(mnth$month)

# should be nine years
n_distinct(mnth$year)
unique(mnth$year)

# clean up
rm(mn_ab, mn_14_21)




# write to csv
write_csv(mnth, "exported_data/monthly_abortions_14_22.csv")
