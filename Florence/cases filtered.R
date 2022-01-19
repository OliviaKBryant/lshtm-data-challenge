# Load the tidyverse
setwd('/Users/fm/Public/HDS/Data Challenge')
library(tidyverse)

# Load in data
names <- c("areaCode", "areaName", "areaType", "date", "newcasesPub", "NewcasesSpec")
cases <- read_csv('nation_2021-10-01.csv', col_names = names, skip = 1) 
glimpse(cases)

# Summarise new cases (by published date) by month
cases %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(summary_variable = sum(newcasesPub))

