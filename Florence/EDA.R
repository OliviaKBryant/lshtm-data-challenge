## Exploratory data analysis on combined data

# Load tidyverse
setwd('/Users/fm/Public/HDS/Data Challenge/local data')
library(tidyverse)

# Load in data
GP <- read_csv('GP_Prescriptions_Deprivation_RuralUrban.csv') 
glimpse(GP)

# Summarise by month
ItemsByMonth <- GP %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(summary_variable = sum(items)) # %>% 
  # view() # %>% 
  # filter(month == "2020-03-01") 
colnames(ItemsByMonth)[2] <- "items"

# Plot an overall line graph
ggplot(data=ItemsByMonth, aes(x=month, y=items)) +
  geom_line(color="red")

# Plot with number of cases -> no correlation can be seen
ItemsCases <- merge(x=ItemsByMonth, y=CaseByMonth, by="month", all.x=T)
glimpse(ItemsCases)
ggplot(data=ItemsCases, aes(x=month)) +
  geom_line(aes(y=items), color="red") +
  geom_line(aes(y=cases), color='blue') +
  coord_cartesian(xlim=as.Date(c("2020-03-01", "2021-10-01")))




 

