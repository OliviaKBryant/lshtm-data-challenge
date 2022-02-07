# ---
# title: Analysis on COVID cases and total corticosteroid prescriptions (Monthly)
# ---

library(tidyverse)
library(magrittr)
library(patchwork)
library(ggthemes)

theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.key = element_rect(fill = "white", colour = "white"))

# --------------------------------------------------------------------------------
# Load in COVID cases data 
names <- c("areaCode", "areaName", "areaType", "date", "newcasesPub", "newcasesSpec")
cases <- read_csv('nation_2021-10-01.csv', col_names = names, skip = 1)

# Summarise new cases (by published date) by month
case_by_month <- cases %>%
  filter(areaName == "England") %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(newcasesPub = sum(newcasesPub))

# Load analysis dataset for NHS regional data
regions <- read_csv("NHS_England_regions_corticosterioid_prescriptions.csv")

# Create items dataset (*NOT* per 1,000 patients)
items_total <- regions %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(items_total = sum(items))

# Plot total England prescriptions VS number of cases
ggplot() +
  geom_line(data=items_total, aes(x=month, y=items_total, color="Total Prescriptions")) +
  geom_line(data=case_by_month, aes(x=month, y=newcasesPub, color='New Cases By Publish Date')) +
  coord_cartesian(xlim=as.Date(c("2020-01-01", "2021-10-01"))) +
  labs(x="Month", 
       y="Number", 
       color="Legend",
       title="Systemic Corticosteroid Prescriptions & \nCOVID-19 Cases Jan 2020 to Oct 2021",
       subtitle = "Monthly data follow different trends",
       caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
                UK Health Security Agency. Coronavirus (COVID-19) in the UK. 2022.")

# Save in word size
ggsave("monthly_new_covid_cases.png", width = 8, height = 4.5, units = "in")
# No correlation can be seen