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
## `clean.R` must be run before this script
##
## Setup and load clean data ---------------------------------------------------
source("Dave/clean.R")
##
## Load other data -------------------------------------------------------------------
## London only data for visual comparison
pd_lon <- read_csv("data/Analysis Dataset/NHS_England_regions_corticosterioid_prescriptions.csv") %>%
  filter(name == "LONDON COMMISSIONING REGION") %>%
  select(date, items, list_size, name)
##
## Clean and Wrangle Data ------------------------------------------------------
##
## Cleaned data for all English GP surgeries
## Analysis dataset - gp level
##
pd_not_GPiH <- pd_gp_clean %>%
  filter(gp_id != "E85124") %>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(list_size != 0,
         items_per_1k_pats < 200) %>%
  group_by(date) %>%
  summarise(items = sum(items), list_size = sum(list_size)) %>%
  mutate(name = "eng_gp_average")
##
## 
pd_GPatH <- pd_gp_clean %>%
  filter(gp_id == "E85124") %>%
  select(date, items, list_size, name)
##
pd_gp_clean <- rbind(pd_lon, pd_not_GPiH, pd_GPatH) %>%
  mutate(items_per_1k_pats = items/list_size *1000)


##
## Plots -----------------------------------------------------------------------
##
## Plot 1: Time Series comparing urban and rural SCS items.
p <- ggplot(pd_gp_clean, 
       aes(date, 
           items_per_1k_pats, colour = factor(name))) +
  labs(title = "Systemic Corticosteroids Prescription Rates for <span style='color:#E69F00'>England</span>, <span style='color:#0072B2'>London</span> and <span style='color:#009E73'>Babaylon GP at Hand</span>",
       subtitle = 'Babylon GP at Hand, a mostly online GP practice based in Hammersmith, has a significantly lower prescription rate that in England or London',
       y = "Items perscribed per 1000 patients",
       x = "",
       caption = "Source: OpenPrescribing.net, The DataLab, University of Oxford, 2022") +
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
           y = 3.4, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 2.4,
           alpha = 0.7,
           angle = -90,
           hjust = 0.5,
           vjust = 0) +
  xlim(as.Date("2019-01-01"), as.Date("2021-10-01"))+
  scale_colour_manual(values = cbf_pal_6) +
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "3 month",
               limits = c(as.Date("2019-01-01"), as.Date("2021-10-01")),
               expand=c(0,0)) + # holds the axis to the above limits
  theme(plot.title = element_markdown(size = 15),
        plot.subtitle = element_markdown(size = 10),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_line(); p
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/GP_In_Hand_Line_pp.png', 
       plot = p,
       width = 10, 
       height = 5.625, 
       units = "in")
## Save plot for a word document
ggsave('Dave/plots/GP_In_Hand_Line_word.png',
       plot = p + 
         theme(plot.title = element_markdown(size = 13),
               plot.subtitle = element_markdown(size = 8)),
       width = 9,
       height = 5,
       units = "in")
## End
