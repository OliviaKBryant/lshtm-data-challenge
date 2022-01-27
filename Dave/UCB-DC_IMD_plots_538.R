## Info ------------------------------------------------------------------------
##
## Script name: Data Challenge Plots - Indices of Multiple Deprivation (IMD)
##
## Purpose of script: Plots for systemic corticosteroids prescriptions in 
## English primary care by IMD decile.
##
## Author: David Turner
##
## Date Created: 25 Jan 2022
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
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"))
##
## Load Data -------------------------------------------------------------------
## Analysis dataset
pd_gp <- read_csv("analysis-data/GP_corticosterioid_prescriptions.csv") 
## GP information for prescribing setting
GPs <- read_csv("raw-data/epraccur.csv", col_names = F) %>%
  select(gp_id = X1, prescribing_setting = X26)
##
## Clean and Wrangle Data ------------------------------------------------------
##
pd_gp_clean <- pd_gp %>%
  left_join(GPs, by = "gp_id") %>%
  drop_na() %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         deprivation_decile = factor(deprivation_decile)) %>%
  filter(covid_period %in% c("pre-covid year 1",
                             "covid year 1",
                             "covid year 2"),
         prescribing_setting == 4,
         list_size != 0,
         items_per_1k_pats < 200) %>%
  group_by(date, deprivation_decile, covid_period) %>%
  summarise(items = sum(items), list_size = sum(list_size)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date),
         month = format(date,"%B"),
         year = format(date, "%Y"))
##
## Plots -----------------------------------------------------------------------
##
## Plot 1: Time Series comparing urban and rural SCS items.
ggplot(pd_gp_clean, 
       aes(date, 
           items_per_1k_pats, 
           colour = deprivation_decile )) +
  labs(title = 'Systemic Corticosteroids Prescriptions per IMD Decile',
       subtitle = 'Before and after the onset of COVID-19\nApr 2019 - Oct 2021',
       y = "SCS Prescriptions per 1000 patients",
       x = "",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2017") +
  expand_limits(x = as.Date("2019-03-18")) +
  ylim(8,22.5) +
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
           y = 20.1, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 2.4,
           alpha = 0.7,
           angle = -90,
           hjust = 1,
           vjust = 0) +
  geom_line(show.legend = TRUE) +
  scale_colour_discrete(name="IMD Decile",
                        breaks=c("rural", "urban"),
                        labels=c("Rural", "Urban")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 month") +
  theme(panel.grid.major.x = element_blank())
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('plots/Corticosteroids_Perscriptions_IMD_Line_538.png', width = 10, height = 5.625, units = "in")
##
## Plot 2: Box plot......
ggplot(pd_gp_clean, aes(items_per_1k_pats, colour = deprivation_decile)) +
  geom_boxplot() +
  labs(title = 'Systemic Corticosteroids Prescriptions per IMD Decile',
       subtitle = 'Average Prescribing Rate\nApr 2019 - Oct 2021',
       y = "",
       x = "SCS Prescriptions per 1000 patients",
       colour='',
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2017") +
  scale_colour_discrete(name="",
                        breaks=c("rural", "urban"),
                        labels=c("Rural", "Urban")) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('plots/Corticosteroids_Perscriptions_IMD_Box_538.png', width = 10, height = 5.625, units = "in")
## End
