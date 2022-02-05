## Info ------------------------------------------------------------------------
##
## Script name: Data Challenge Plots - Rural Urban Classification
##
## Purpose of script: Plots for systemic corticosteroids prescriptions in 
## English primary care by rural urban classification.
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
library(scales)
library(ggtext)
##
theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_markdown(),
             plot.caption = element_markdown(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.box.background = element_rect(fill = "white", colour = "white"),
             plot.title = element_markdown(),
             plot.subtitle = element_markdown())
##
## Load Data -------------------------------------------------------------------
## load cleaned data from clean.R
source("Dave/clean.R")
##
## Clean and Wrangle Data ------------------------------------------------------
##
pd_gp_clean <- pd_gp_clean %>%
  group_by(date, rural_urban_overall, covid_period) %>%
  summarise(items = sum(items), 
            list_size = sum(list_size)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date),
         month = format(date,"%B"),
         year = format(date, "%Y"))
##
## Plots -----------------------------------------------------------------------
##
## Plot 1 - line: Setup -----
ru_plot <- ggplot(pd_gp_clean, 
       aes(date, 
           items_per_1k_pats, 
           colour = rural_urban_overall )) +
  labs(title = "Systemic Corticosteroids Prescription Rates in <span style='color:#17BECF'>Rural</span> and <span style='color:#D32728'>Urban</span><br>Areas of England",
       subtitle = 'Prescription rates are higher in rural areas than urban areas',
       y = "Items per 1000 patients",
       x = "",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2017") +
  ylim(0,20) +
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "3 month",
               limits = c(as.Date("2019-01-01"), as.Date("2021-10-01")),
               expand=c(0,0)) + # holds the axis to the above lims
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none")
##
## Plot 1a - line: PowerPoint without annotations ------
ru_no_annot <- ru_plot + geom_line()
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/Rural_Urban_Line_pp_no_annot.png', 
       plot = ru_no_annot, 
       width = 10, 
       height = 5.625, 
       units = "in")
##
## Plot 1b - line: PowerPoint with annotations ------
ru_annot <- ru_plot + 
  geom_vline(xintercept = as.Date("2020-01-28"), # first case in the UK
             colour = "gray",
             alpha = 0.8) +
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
           size = 3,
           alpha = 0.7,
           angle = -90,
           hjust = 0.5,
           vjust = 0
  ) + 
  geom_line()
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/Rural_Urban_Line_pp_annot.png',
       plot = ru_annot,
       width = 10,
       height = 5.625,
       units = "in")
##
## Plot 1c - line: Word with annotations ------
## Save plot for a word document
ggsave('Dave/plots/Rural_Urban_Line_word_annot.png',
       plot = ru_annot + 
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12)),
       width = 8,
       height = 4.5,
       units = "in")
##
## Plot 2: Box plot ---------
ggplot(pd_gp_clean, aes(items_per_1k_pats, colour = rural_urban_overall)) +
  geom_boxplot() +
  labs(title = 'Systemic Corticosteroids Prescriptions in Rural and Urban Areas',
       subtitle = 'Average Prescribing Rate\nApr 2019 - Oct 2021',
       y = "",
       x = "Items per 1000 patients",
       colour='',
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2017") +
  scale_colour_discrete(name="",
                        breaks=c("rural", "urban"),
                        labels=c("Rural", "Urban")) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/Corticosteroids_Perscriptions_Rural_Urban_Box_538.png', width = 10, height = 5.625, units = "in")
## End