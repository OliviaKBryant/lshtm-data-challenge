## Exploratory data analysis on combined data

# Load tidyverse
library(tidyverse)

# Load in data
GP <- read_csv('GP_Prescriptions_Deprivation_RuralUrban.csv') 
glimpse(GP)

# Summarise items by month
GP %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(summary_variable = sum(items)) 
  # %>% filter(month == "2020-03-01")

