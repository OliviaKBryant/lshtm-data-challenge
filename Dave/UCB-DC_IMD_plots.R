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
## `clean.R` must be run before this script
##
## Setup and load clean data ---------------------------------------------------
source("Dave/clean.R")
##
## Wrangle Data ------------------------------------------------------
##
pd_gp_imd <- pd_gp_clean %>%
  group_by(date, deprivation_decile, covid_period) %>%
  summarise(items = sum(items), list_size = sum(list_size)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date),
         month = format(date,"%B"),
         year = format(date, "%Y"))
##
## Plots -----------------------------------------------------------------------
##
## Plot 1 - line: Setup -----
imd_plot <- ggplot(pd_gp_imd,
                   aes(date,
                       items_per_1k_pats,
                       group = factor(deprivation_decile),
                       colour = factor(deprivation_decile))) +
  labs(title = "Systemic Corticosteroids Prescription Rates in England per the Indices<br>of Multiple Deprivation (IMD) Decile",
       subtitle = "Prescription rates are very similar between IMD Deciles<br>
       IMD decile: 
       <span style='color:#A6CEE3'>1</span> | 
       <span style='color:#1F78B4'>2</span> |
       <span style='color:#B2DF8A'>3</span> | 
       <span style='color:#33A02C'>4</span> | 
       <span style='color:#FB9A99'>5</span> | 
       <span style='color:#E31A1C'>6</span> | 
       <span style='color:#FDBF6F'>7</span> | 
       <span style='color:#FF7F00'>8</span> | 
       <span style='color:#CAB2D6'>9</span> | 
       <span style='color:#6A3D9A'>10</span>
       ",
       y = "Items per 1000 patients",
       x = "",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022") +
  ylim(0,20) +
  scale_colour_manual(values = cbf_pal) +
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "3 month",
               limits = c(as.Date("2019-01-01"), as.Date("2021-10-01")),
               expand=c(0,0)) + # holds the axis to the above lims
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none")
##
## Plot 1a - line: PowerPoint without annotations ------
imd_no_annot <- imd_plot + geom_line()
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/IMD_Line_pp_no_annot.png',
       imd_no_annot,
       width = 10, 
       height = 5.625, 
       units = "in")
##
## Plot 1b - line: PowerPoint with annotations ------
imd_annot <- imd_plot + 
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
  geom_line()
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/IMD_Line_pp_annot.png',
       plot = imd_annot,
       width = 10,
       height = 5.625,
       units = "in")
##
## Plot 1c - line: Word with annotations ------
## Save plot for a word document
ggsave('Dave/plots/IMD_Line_word_no_annot.png',
       plot = imd_annot + 
         theme(plot.title = element_markdown(size = 15),
               plot.subtitle = element_markdown(size = 12)),
       width = 8,
       height = 4.5,
       units = "in")
##
## Plot 2: Box plot. -------
imd_box <- ggplot(pd_gp_imd, aes(x = items_per_1k_pats, 
                        y = factor(deprivation_decile),
                        group = factor(deprivation_decile),
                        colour = factor(deprivation_decile))) +
  geom_boxplot() +
  labs(title = "Average Systemic Corticosteroids Prescriptions per the Indices<br>of Multiple Deprivation (IMD) Decile",
  subtitle = "England - May 2018 to October 2019",
       y = "IMD Decile",
       x = "Items per 1000 patients",
       colour='',
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2017") +
  scale_colour_manual(values = cbf_pal) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(),
        legend.position = "none")
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/IMD_Box_pp.png', 
       plot = imd_box,
       width = 10, 
       height = 5.625, 
       units = "in")
## Save plot for Word document
ggsave('Dave/plots/IMD_Box_word.png',
       plot = imd_box +
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12)),
       width = 8,
       height = 4.5,
       units = "in")
## End
