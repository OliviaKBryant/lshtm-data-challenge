## Info ------------------------------------------------------------------------
##
## Script name: Accessing individual drugs from the OpenPrescribing API
##
## Purpose of script: To pull information from OpenPrescribing API on  
## corticosteroids by BNF Chemical Substance name.  
##
## Author: David Turner
##
## Date Created: 25 Jan 2022
##
## ## Notes -----------------------------------------------------------------------
##
## `clean.R` and `UCB-DC_pull_from_api.R` must be run before this script
##
## Setup and load clean data ---------------------------------------------------
source("Dave/clean.R")
source("Dave/UCB-DC_pull_from_api.R")
# Okabe-Ito colour blind friendly palette.
##
## plots ---------------
## Setup plot --------
p <- ggplot(mapping = aes(x= date,
                          y= items_per_1k_pats)) +
  labs(y = "Items per 1000 patients",
       x = "",
       colour = "",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022") +
  geom_vline(xintercept = as.Date("2020-01-28"), # first case in the UK
             colour = "gray",
             alpha = 0.8) +
  annotate("rect", # first lockdown
           fill = "gray", 
           alpha = 0.4,
           xmin = as.Date("2020-03-26"), 
           xmax = as.Date("2020-05-17"),
           ymin=0, 
           ymax=Inf) + 
  annotate("rect", # second lockdown
           fill = "gray", 
           alpha = 0.4, 
           xmin = as.Date("2020-11-05"), 
           xmax = as.Date("2020-12-02"),
           ymin=0, 
           ymax=Inf) +
  annotate("rect", # third lockdown
           fill = "gray", 
           alpha = 0.4, 
           xmin = as.Date("2021-01-05"), 
           xmax = as.Date("2021-04-12"),
           ymin=0, 
           ymax=Inf) +
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "3 month",
               limits = c(as.Date("2019-01-01"), as.Date("2021-10-01")),
               expand=c(0,0)) + # holds the axis to the above limits
  theme(panel.grid.major.x = element_blank()
        ,legend.position = "none"
  )
##
## Line plots ---------
## have to split it up for scale
## Line plot 1 -----
per_d_1 <- p + labs(title = "<span style='color:#D32728'>Prednisolone</span> Prescription Rates in England",
                    subtitle = "Prednisolone accounted for nearly 77% of all Systemic Corticosteroids Prescriptions in England") +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 1.5, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 3,
           alpha = 0.7,
           angle = -90,
           hjust = 0.5,
           vjust = 0)  +
  scale_y_continuous(limits = c(0,13),
                     breaks = c(0,3,6,9,12)) +
  geom_line(data = filter(pd_parent_compound,
                          parent_compound == "Prednisolone"), 
            aes(colour = parent_compound))
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/csc_per_drug_1_pp.png', 
       plot = per_d_1,
       width = 10, 
       height = 5.625, 
       units = "in")
## Save plot for a word document
ggsave('Dave/plots/csc_per_drug_1_word.png',
       plot = per_d_1 + 
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12)),
       width = 8,
       height = 4.5,
       units = "in")
##
## Line plot 2 ---------
per_d_2 <- p + labs(
  title = "Systemic Corticosteroids Prescription Prescription Rates in England",
  subtitle = "Not all Systemic Corticosteroids saw a drop in prescription rates during the COVID19 pandemic.<br><span style='color:#D55E00'>Methylprednisolone acetate</span>
  and <span style='color:#CC79A7'>Triamcinolone acetonide</span> are injectables only when used systemically,<br>
  <span style='color:#56B4E9'>Fludrocortisone acetate</span>, <span style='color:#0072B2'>Hydrocortisone</span>,
  <span style='color:#009E73'>Dexamethasone</span> and
  <span style='color:#E69F00'>Budesonide</span> have a variety of admission routes.") +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 0.875, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown"), 
           size = 2.5,
           alpha = 0.7,
           angle = -90,
           hjust = 0.5,
           vjust = 0)  +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_colour_manual(values = cbf_pal_6) +
  geom_line(data = filter(pd_cs,
                          bnf_cs %in% c(
                            "Fludrocortisone acetate",
                            "Methylprednisolone acetate",
                            "Hydrocortisone",
                            "Dexamethasone",
                            "Triamcinolone acetonide",
                            "Budesonide")),
            aes(colour = bnf_cs))
##
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/csc_per_drug_2_pp.png', 
       plot = per_d_2,
       width = 10, 
       height = 5.625, 
       units = "in")
## Save plot for a word document
ggsave('Dave/plots/csc_per_drug_2_word.png',
       plot = per_d_2 +
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12)),
       width = 8,
       height = 4.5,
       units = "in")
