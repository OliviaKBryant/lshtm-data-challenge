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
## `clean.R` must be run before this script
##
## Setup and load clean data ---------------------------------------------------
source("Dave/clean.R")
##
## London only data for visual comparison
pd_lon <- read_csv("data/Analysis Dataset/NHS_England_regions_corticosterioid_prescriptions.csv") %>%
  filter(name == "LONDON COMMISSIONING REGION") %>%
  select(date, items, list_size, name)
# LSOA to CCG lookup
LSOA_CCG <- read_csv("data/wrangling/Lower_Layer_Super_Output_Area_(2011)_to_Clinical_Commissioning_Group_to_Local_Authority_District_(April_2021)_Lookup_in_England.csv") %>%
  select(LSOA11CD, CCG21CD)
# CCR - to NHS region lookup
CCG_NHSER <- read_csv("data/wrangling/Clinical_Commissioning_Group_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv") %>%
  select(CCG21CD, NHSER21CDH, NHSER21NM)
## Summarise data by rural urban classification --------------------------------
##
pd_gp_ru <- pd_gp_clean %>%
  group_by(date, rural_urban_overall, covid_period) %>%
  summarise(items = sum(items), 
            list_size = sum(list_size)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date),
         month = format(date,"%B"),
         year = format(date, "%Y"))

pd_gp_ru_ex_lon <- pd_gp_clean %>%
  left_join(LSOA_CCG, by=c("LSOA_code" = "LSOA11CD")) %>%
  left_join(CCG_NHSER, by = "CCG21CD") %>%
  filter(NHSER21CDH != "Y56") %>%
  group_by(date, rural_urban_overall, covid_period) %>%
  summarise(items = sum(items), 
            list_size = sum(list_size)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date),
         month = format(date,"%B"),
         year = format(date, "%Y"))

pd_gp_ru_lon <- pd_gp_clean %>%
  left_join(LSOA_CCG, by=c("LSOA_code" = "LSOA11CD")) %>%
  left_join(CCG_NHSER, by = "CCG21CD") %>%
  filter(NHSER21CDH == "Y56") %>%
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
ru_plot <- ggplot(mapping = aes(date, 
           items_per_1k_pats, 
           colour = rural_urban_overall )) +
  labs(title = "Systemic Corticosteroids Prescription Rates in <span style='color:#D32728'>Rural</span> and <span style='color:#17BECF'>Urban</span><br>Areas of England",
       subtitle = 'Prescription rates are higher in rural areas than urban areas',
       y = "Items per 1000 patients",
       x = "",
       caption = "Source: OpenPrescribing.net, The DataLab, University of Oxford, 2022") +
  ylim(0,20) +
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "3 month",
               limits = c(as.Date("2019-01-01"), as.Date("2021-10-01")),
               expand=c(0,0)) + # holds the axis to the above limits
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none")
##
## Plot 1a - line: PowerPoint without annotations ------
ru_no_annot <- ru_plot + 
  geom_line(data = pd_gp_ru)
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
           y = 2.5, 
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
  geom_line(data = pd_gp_ru)
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
ggplot(pd_gp_ru, aes(items_per_1k_pats, colour = rural_urban_overall)) +
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
#ggsave('Dave/plots/Corticosteroids_Perscriptions_Rural_Urban_Box_538.png', width = 10, height = 5.625, units = "in")
##
## Plots excluding London-------------------------------------------------------
##
## Plot 1 - line: Setup -----
ru_plot_ex_lon <- ggplot(mapping = aes(date, items_per_1k_pats, colour = rural_urban_overall )) +
  labs(title = "Systemic Corticosteroids Prescription Rates in <span style='color:#D32728'>Rural</span> and <span style='color:#17BECF'>Urban</span><br>Areas of England - Excluding NHS Region London",
       subtitle = 'Excluding NHS London decreases the difference between rural and urban prescription rates.',
       y = "Items per 1000 patients",
       x = "",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022") +
  ylim(0,20) +
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "3 month",
               limits = c(as.Date("2019-01-01"), as.Date("2021-10-01")),
               expand=c(0,0)) + # holds the axis to the above limits
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none")
##
## Plot 1b - line: PowerPoint with annotations ------
ru_ex_lon <- ru_plot_ex_lon + 
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
           y = 2.5, 
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
  geom_line(data = pd_gp_ru_ex_lon)
##
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/Rural_Urban_Line_pp_ex_lon.png',
       plot = ru_ex_lon,
       width = 10,
       height = 5.625,
       units = "in")
##
## Plot 1c - line: Word with annotations ------
## Save plot for a word document
ggsave('Dave/plots/Rural_Urban_Line_word_ex_lon.png',
       plot = ru_ex_lon + 
         geom_line(data = pd_gp_ru, linetype = 2) +
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12)) +
         annotate(
           geom = "text", 
           x = as.Date("2019-02-01"), 
           y = 7.5, 
           label = "Prescription rates\nincluding London", 
           hjust = "left",
           color = "grey",
           size = 3) +
         annotate(geom = "segment",
                  x = as.Date("2019-02-01"),
                  xend = as.Date("2019-01-01"),
                  y = 9,
                  yend = 13.5,
                  color = "grey")
       ,
       width = 8,
       height = 4.5,
       units = "in")
## End