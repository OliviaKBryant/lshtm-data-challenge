## Exploratory data analysis on combined data

# Load tidyverse
setwd('/Users/fm/Public/HDS/Data Challenge/local data/Analysis Dataset')
library(tidyverse)
library(zoo)

# Load in data
GP <- read_csv('GP_corticosterioid_prescriptions.csv') 
CCG <- read_csv("CCG_corticosterioid_prescriptions.csv")
Region <- read_csv("NHS_England_regions_corticosterioid_prescriptions.csv")

# Summarise by month
# GP
ItemsByMonthGP <- GP %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(summary_variable = sum(items)) # %>% 
  # view() # %>% 
  # filter(month == "2020-03-01") 
colnames(ItemsByMonthGP)[2] <- "itemsGP"

# CCG
ItemsByMonthCCG <- CCG %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(summary_variable = sum(items)) 
colnames(ItemsByMonthCCG)[2] <- "itemsCCG"

# NHS Region
ItemsByMonthRegion <- Region %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(summary_variable = sum(items)) 
colnames(ItemsByMonthRegion)[2] <- "itemsRegion"

# Look at total by different division methods
ItemsCheck <- left_join(ItemsByMonthGP, ItemsByMonthCCG, by='month') %>%
  left_join(., ItemsByMonthRegion, by='month') 
view(ItemsCheck)

colours <- c("GP"="red", "CCG"="orange", "Region"="yellow")

ggplot(data=ItemsCheck, aes(x=month)) +
  geom_line(aes(y=itemsGP, colour="GP"), size=1) + 
  geom_line(aes(y=itemsCCG, colour="CCG"), size=1) + 
  geom_line(aes(y=itemsRegion, colour="Region"), size=1) + 
  labs(x = "Date",
     y = "Items",
     color = "Legend") +
  scale_color_manual(values = colours)

# Months over different years
ItemsMY <- ItemsByMonthGP 
ItemsMY$month <- as.yearmon(ItemsMY$month)

ggplot(ItemsMY, aes(lubridate::month(month, label=TRUE, abbr=TRUE), 
                itemsGP, group=factor(lubridate::year(month)), 
                colour=factor(lubridate::year(month)))) +
  geom_line() +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()

# Load in COVID cases data (gov.uk)
# setwd('/Users/fm/Public/HDS/Data Challenge/local data')
names <- c("areaCode", "areaName", "areaType", "date", "newcasesPub", "NewcasesSpec")
cases <- read_csv('nation_2021-10-01.csv', col_names = names, skip = 1) 

# Summarise new cases (by published date) by month
CaseByMonth <- cases %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(summary_variable = sum(newcasesPub)) 
colnames(CaseByMonth)[2] <- "cases"

# Plot items prescribed with number of cases 
ItemsCases <- merge(x=ItemsByMonthGP, y=CaseByMonth, by="month", all.x=T)
ggplot(data=ItemsCases, aes(x=month)) +
  geom_line(aes(y=itemsGP), color="red") +
  geom_line(aes(y=cases), color='blue') +
  coord_cartesian(xlim=as.Date(c("2020-03-01", "2021-10-01")))
# No correlation can be seen
