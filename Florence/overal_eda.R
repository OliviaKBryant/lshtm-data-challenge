## Exploratory data analysis on combined data

# Load tidyverse
setwd('/Users/fm/Public/HDS/Data Challenge/local data/Analysis Dataset')
library(tidyverse)
library(magrittr)
library(zoo)
library(skimr)
library(ggthemes)

# Load in data
GP <- read_csv('GP_corticosterioid_prescriptions.csv')
GP %<>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(list_size != 0, #416046 obs to 413601 obs
         items_per_1k_pats < 200) # 413601 obs to 413432 obs
CCG <- read_csv("CCG_corticosterioid_prescriptions.csv")
CCG %<>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(list_size != 0, # 6374 obs to 6360 obs
         items_per_1k_pats < 200) # remain 6360 obs
Region <- read_csv("NHS_England_regions_corticosterioid_prescriptions.csv")
Region %<>%
  mutate(items_per_1k_pats = items/list_size *1000)

# Check that GP dataset only contains those with practice setting code 4
# Code 4 = GP Practice
# GP %>%
#   select(gp_id) %>%
#   unique()

# Summarise by month
# GP
ItemsByMonthGP <- GP %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(itemsGP = sum(items)/sum(list_size) *1000) # %>%
  # filter(month == "2020-03-01")

# CCG
ItemsByMonthCCG <- CCG %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(itemsCCG = sum(items)/sum(list_size) *1000)

# NHS Region
ItemsByMonthRegion <- Region %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(itemsRegion = sum(items)/sum(list_size) *1000)

# For both list size and items
# CCG / Regions > GP
# Main analysis to use Regions / CCG

# Look at total by different division methods
ItemsCheck <- ItemsByMonthGP %>%
  left_join(ItemsByMonthCCG, by='month') %>%
  left_join(ItemsByMonthRegion, by='month')
colours <- c("GP"="red", "CCG"="orange", "Region"="yellow")

ggplot(data=ItemsCheck, aes(x=month)) +
  geom_line(aes(y=itemsGP, colour="GP"), size=1) +
  geom_line(aes(y=itemsCCG, colour="CCG"), size=1) +
  geom_line(aes(y=itemsRegion, colour="Region"), size=1) +
  labs(x = "Month",
     y = "Prescriptions per 1,000 Patients",
     color = "Legend",
     title="Total Prescriptions by GP/CCG/Region") +
  scale_color_manual(values = colours)
ggsave("Total_Items_GP_CCG_Region.png")

# Months over different years
ItemsMY <- ItemsByMonthRegion
ItemsMY$month <- as.yearmon(ItemsMY$month)

ggplot(ItemsMY, aes(x=lubridate::month(month, label=TRUE, abbr=TRUE),
                    y=itemsRegion,
                group=factor(lubridate::year(month)),
                colour=factor(lubridate::year(month)))) +
  geom_line() +
  geom_point() +
  labs(x="Month", y="Prescriptioins per 1,000 patients on list", colour="Year",
       title="Corticosteroids Prescribed per 1,000 patients \nby Month of Year") +
  theme_fivethirtyeight() +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  scale_color_brewer(palette = 'Set2')
ggsave("Total_Items_Month_Year.png")

# Analysis on COVID cases (For Record Only)
# Load in COVID cases data (gov.uk)
setwd('/Users/fm/Public/HDS/Data Challenge/local data')
names <- c("areaCode", "areaName", "areaType", "date", "newcasesPub", "NewcasesSpec")
cases <- read_csv('nation_2021-10-01.csv', col_names = names, skip = 1)
glimpse(cases)

# Create items dataset (*NOT* per 1,000 patients on list)
ItemsGP <- GP %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(itemsGP = sum(items))

# Summarise new cases (by published date) by month
CaseByMonth <- cases %>%
  filter(areaName == "England") %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(newcaseno = sum(newcasesPub))

# Plot total England prescriptions VS number of cases
ItemsCases <- merge(x=ItemsGP, y=CaseByMonth, by="month", all.x=T)
ggplot(data=ItemsCases, aes(x=month)) +
  geom_line(aes(y=itemsGP, color="Items")) +
  geom_line(aes(y=newcaseno, color='New cases')) +
  coord_cartesian(xlim=as.Date(c("2020-01-01", "2021-10-01"))) +
  labs(x="Time", y="Number", color="Legend",
       title="Corticosteroids Prescriptions & \nMonthly New Cases in England") +
  theme_fivethirtyeight() +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  scale_color_brewer(palette = 'Set1')

# No correlation can be seen
ggsave("Total_Items_VS_COVID_Cases.png")