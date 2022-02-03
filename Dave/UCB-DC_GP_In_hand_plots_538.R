## Info ------------------------------------------------------------------------
##
## Script name: Data Challenge Plots - Babylon GP in Hand - case study
##
## Purpose of script: Plots plots to show time series of corticosteroid 
## prescription at GP in Hand. 
##
## Author: David Turner
##
## Date Created: 1 Feb 2022
##
## Notes -----------------------------------------------------------------------
##
## Data sources:
## https://openprescribing.net/
## https://digital.nhs.uk/services/organisation-data-service/file-downloads/gp-and-gp-practice-related-data
##          > https://files.digital.nhs.uk/assets/ods/current/epraccur.zip
##
## Style: fivethirtyeight
##   
## Setup -----------------------------------------------------------------------
library(tidyverse)
library(ggthemes)
setwd("~/HDS/Data Challange/UCB")
theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0))
##
## Load Data -------------------------------------------------------------------
pd_gp <- read_csv("data/Analysis Dataset/GP_corticosterioid_prescriptions.csv") 
## GP information for prescribing setting
pd_lon <- read_csv("data/Analysis Dataset/NHS_England_regions_corticosterioid_prescriptions.csv") %>%
  filter(name == "LONDON COMMISSIONING REGION") %>%
  select(date, items, list_size, name) %>%
  mutate(items_per_1k_pats = items/list_size *1000)
  
##
## Clean and Wrangle Data ------------------------------------------------------
##
pd_not_GPiH <- pd_gp %>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(list_size != 0,
         items_per_1k_pats < 200) %>%
  group_by(date) %>%
  summarise(items = sum(items), list_size = sum(list_size)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         name = "GP average")

pd_GPatH <- pd_gp %>%
  filter(gp_id == "E85124") %>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  select(date, items, list_size, items_per_1k_pats, name)

pd_gp_clean <- rbind(pd_lon, pd_not_GPiH, pd_GPatH)


##
## Plots -----------------------------------------------------------------------
##
## Plot 1: Time Series comparing urban and rural SCS items.
ggplot(pd_gp_clean, 
       aes(date, 
           items_per_1k_pats, colour = name)) +
  geom_line() +
  labs(title = 'Systemic Corticosteroids Prescriptions',
       subtitle = 'Babylon GP at Hand',
       y = "Items perscribed per 1000 patients",
       x = "",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2017") +
  geom_vline(xintercept = as.Date("2020-01-28"), # first case in the UK
             colour = "gray") +
  annotate("rect", # first lockdown
           fill = "gray", 
           alpha = 0.4,
           xmin = as.Date("2020-03-26"), 
           xmax = as.Date("2020-05-17"),
           ymin=-Inf, ymax=Inf) + 
  annotate("rect", # second lockdown
           fill = "gray", 
           alpha = 0.4, 
           xmin = as.Date("2020-11-05"), 
           xmax = as.Date("2020-12-02"),
           ymin=-Inf, 
           ymax=Inf) +
  annotate("rect", # third lockdown
           fill = "gray", 
           alpha = 0.4, 
           xmin = as.Date("2021-01-05"), 
           xmax = as.Date("2021-04-12"),
           ymin=-Inf, 
           ymax=Inf) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 5, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 2.4,
           alpha = 0.7,
           angle = -90,
           hjust = 1,
           vjust = 0) +
  xlim(as.Date("2019-01-01"), as.Date("2021-10-01"))+
  theme(panel.grid.major.x = element_blank())
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/GP_In_Hand_Line_538.png', width = 10, height = 5.625, units = "in")
##
##
##
## Plot 2: Box plot......
ggplot(pd_gp_clean, aes(items_per_1k_pats, colour = rural_urban_overall)) +
  geom_boxplot() +
  labs(title = 'Systemic Corticosteroids Prescriptions in Rural and Urban Areas',
       subtitle = 'Average Prescribing Rate\nApr 2019 - Oct 2021',
       y = "",
       x = "Items perscribed per 1000 patients",
       colour='',
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2017") +
  scale_colour_discrete(name="",
                        breaks=c("rural", "urban"),
                        labels=c("Rural", "Urban")) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('plots/Corticosteroids_Perscriptions_Rural_Urban_Box_538.png', width = 10, height = 5.625, units = "in")
## End