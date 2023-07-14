library(tidyverse)
library(readxl)


# Script imports abortion totals for nonresidents by state and year
# from Indiana Terminated Pregnancy Reports:
# https://www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/


# data scraped from 2022 report: 
nres_22 <- read_xlsx("external_data/2022_termination_data.xlsx", sheet = 14)

# add 2022 year column to nres_22 

nres_22$year <- 2022

# import previous data from 2019 to 2021. 

# url for 2019 to 2021 data 
nres_19_21_url <- "https://raw.githubusercontent.com/tedschurter/indiana_abortion/main/Exported_Data/nonresident_ab_19_21.csv"

nres_19_21 <- read_csv(nres_19_21_url)

# bind two dataframes together

nres <- rbind(nres_19_21, nres_22) 

# clean up
rm(nres_22, nres_19_21, nres_19_21_url)

# check years - should be 4 
nres %>% distinct(year)

# check states - should be six
nres %>% distinct(state)

# check column types
str(nres)

# write csv

write_csv(nres, "exported_data/nonresident_ab_19_22.csv")
