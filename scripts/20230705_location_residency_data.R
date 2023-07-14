library(tidyverse)
library(readxl)

# Import data of abortion providers scraped from Indiana's Terminated Pregnancy 
# reports.

# The format of the data changed for the years involved which requires importing 
# years with identical formats first, then standardizing the format, then combining 
# them together. 

# run a loop to combine years with identically structured data  ####
# The years 2018:2020 included total and resident count columns.

# path to file
path <- "external_data/provider_residency.xlsx"

# dataframe of sheet names for loop
names <- data.frame(excel_sheets(path))

# add column named sheets
names$sheets <- names$excel_sheets.path. 

# remove exce_sheets.path. column
names$excel_sheets.path. <- NULL

# filter to years 2018:2020 
names_18_20 <- names %>% filter(sheets != "2022_prov_res" & 
                                  sheets != "2021_prov_res")


for (i in 1:length(names_18_20$sheets)){
  
  ploc <- read_xlsx("external_data/provider_residency.xlsx",
                    sheet = excel_sheets(path)[i]) %>% 
    mutate(year = str_sub(excel_sheets(path)[i], start = 1L, end = 4L)) %>% 
    select(year, county, facility, resident, total) %>% 
    # remove percents included within () and commas
    # regex: \\(c._\\) removes parentheses and all content inside it
    # |, removes comma separating 4 digit numbers
    
    mutate_at(
      vars(4:5),
      funs(as.numeric(str_remove_all(., pattern = "\\(.+\\)|,"))))
  
  if (i == 1) {
    ploc2 <- ploc }
  
    else { ploc2 <- rbind(ploc, ploc2)
    }
}

# rename dataframe to reflect years 
ploc_18_20 <- ploc2

# clean up 
rm(ploc, ploc2, names, names_18_20)

# create nonresident column by subtracting resident column from total column

ploc_18_20 <- ploc_18_20 %>% select(year, county, facility, resident, total) %>% 
  mutate(nonresident = total-resident, .after = resident)


# 2021 and 2022 data includes count by resident, nonresident and total 

# read in 2021 data

ploc_21 <- read_xlsx("external_data/provider_residency.xlsx", sheet = 4) %>% 
  mutate(year = str_sub(excel_sheets(path)[4], start = 1L, end = 4L)) %>% 
  select(year, county, facility, resident, nonresident, total) %>% 
  # remove percents included within () and commas
  # regex: \\(c._\\) removes parentheses and all content inside it
  # |, removes comma separating 4 digit numbers
  
  mutate_at(
    vars(4:6),
    funs(as.numeric(str_remove_all(., pattern = "\\(.+\\)|,"))))


# read in 2021 data

ploc_22 <- read_xlsx("external_data/provider_residency.xlsx", sheet = 5) %>% 
  mutate(year = str_sub(excel_sheets(path)[5], start = 1L, end = 4L)) %>% 
  select(year, county, facility, resident, nonresident, total) %>% 
  # remove percents included within () and commas
  # regex: \\(c._\\) removes parentheses and all content inside it
  # |, removes comma separating 4 digit numbers
  
  mutate_at(
    vars(4:6),
    funs(as.numeric(str_remove_all(., pattern = "\\(.+\\)|,"))))

# bind all three dataframes together
ploc <- rbind(ploc_22, ploc_21, ploc_18_20)

# clean up unnecessary dataframes
rm(ploc_22, ploc_21, ploc_18_20, path)

# check facility names
unique(ploc$facility)

# PP of Bloomington has a few of\rBloomington entries

# removes extra spaces from facility entries
ploc$facility <- str_squish(ploc$facility)

# check facilities again
unique(ploc$facility)

# Planned Parenthood of Indianapolis (Georgetown) can be recoded; the same facility 
# was listed in reports in different ways

ploc <- ploc %>%
  mutate(facility = case_when(
    facility == "Planned Parenthood of Georgetown" ~ "Planned Parenthood of Indianapolis",
    facility == "Planned Parenthood of Indianapolis (Georgetown)" ~ "Planned Parenthood of Indianapolis",
    .default = facility))

# check facility names again
unique(ploc$facility)

write_csv(ploc, "exported_data/provider_residency.csv")




 