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
## To install openprescribingR (accesses the OpenPrescribing API).
## Uncomment the following 2 lines to install from github. 
# library(devtools)
# devtools::install_github("fergustaylor/openprescribingR")
library(openprescribingR)
library(tidyverse)
library(ggthemes)
library(scales)

theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"))
##
## Read in mapping for CCG to region
map <- read_csv("data/wrangling/Clinical_Commissioning_Group_to_STPs_(April_2021)_Lookup_in_England.csv")
##
## Read in all BNF codes 
BNFcodes <- read_csv("data/wrangling/20220115_1642266807923_BNF_Code_Information.csv")
##
## Find BNF Chemical Substance codes in relevant sections.
  ## Codelists of partial BNF codes referring to chapters and sections 
par_bnf_cds <- c("^010502", "^0603", "^100102")
  ## Finding all `BNF Chemical Substance Codes` in `par_bnf_cds` chapters 
  ## and sections
bnf_csc <- BNFcodes %>% 
  subset(Reduce("|", lapply(par_bnf_cds, grepl, BNFcodes$`BNF Presentation Code`))) %>% 
  filter(str_detect(`BNF Chemical Substance`, "^DUMMY", negate = TRUE)) %>%
  pull(`BNF Chemical Substance Code`) %>%
  unique() 
##
## Pull data from API-----------------------------------------------------------
## Loop to pull information from the api per chem substance and retain the codes
  ## Empty data list for the loop
datalist = list()
  ## Loop to pull each substance per CCG
for (i in seq_along(bnf_csc)) {
  try(dat <- spending_by_CCG(bnf_csc[i])) # pull data from api
  dat$bnf_csc <- bnf_csc[i]  # add the BNF chem sub codes
  datalist[[i]] <- dat # add data to list
}
  ## bind list into dataframe, merge with chem sub names and CCG and data wrangle
epd_ccg <- do.call(rbind, datalist) %>%
  left_join(
    unique(select(BNFcodes,
                  "BNF Chemical Substance Code",
                  "BNF Chemical Substance")),
    by = c("bnf_csc" = "BNF Chemical Substance Code")) %>%
  rename(bnf_cs = 'BNF Chemical Substance') %>%
  mutate(date = as.Date(date),
         bnf_cs = factor(bnf_cs),
         chapter = substr(bnf_csc, start = 1, stop = 2),
         section = substr(bnf_csc, start = 1, stop = 4),
         para = substr(bnf_csc, start = 1, stop = 6)) %>%
  left_join(map, by = c("row_id" = "CCG21CDH"))
##
## aggregate on a national level and by parent compound
epd_national_parent_compound <- epd_ccg %>%
  mutate(parent_compound = word(bnf_cs, 1)) %>%
  group_by(parent_compound) %>%
  mutate(parent_compound_group = 
           ifelse(sum(items) > 25000,
                  parent_compound, "Other")) %>%
  ungroup() %>%
  group_by(date,
           parent_compound_group) %>%
  summarise(items = sum(items), 
            quantity = sum(quantity), 
            actual_cost = sum(actual_cost))
##
## plots ---------------
  ## Setup plot --------
p <- ggplot(mapping = aes(x=date,
                          y=items,
                          colour = parent_compound_group)) +
  labs(title = 'Systemic Corticosteroids Prescriptions per Compound',
       subtitle = 'Before and after the onset of COVID-19\nApr 2019 - Oct 2021',
       y = "Total Prescriptions across england",
       x = "",
       colour = "",
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
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "3 month",
               limits = c(as.Date("2019-03-01"), as.Date("2021-10-01"))) +
  theme(panel.grid.major.x = element_blank())
##
  ## Line plots ---------
    ## have to split it up for scale
      ## Line plot 1
p + geom_line(data = filter(epd_national_parent_compound,
                            parent_compound_group == "Prednisolone")) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 700000, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 3,
           alpha = 0.7,
           angle = -90,
           hjust = 0.5,
           vjust = 0) 
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/Corticosteroids_Perscriptions_per_drug_1.png', width = 10, height = 5.625, units = "in")
##
      ## Line plot 2
p + geom_line(data = filter(epd_national_parent_compound,
                            parent_compound_group %in% 
                              c("Fludrocortisone", 
                                "Methylprednisolone", 
                                "Hydrocortisone", 
                                "Dexamethasone", 
                                "Triamcinolone"))) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 60000, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 3,
           alpha = 0.7,
           angle = -90,
           hjust = 0,
           vjust = 0) 
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/Corticosteroids_Perscriptions_per_drug_2.png', width = 10, height = 5.625, units = "in")
##      
      ## Line plot 3
p + geom_line(data = filter(epd_national_parent_compound,
                            parent_compound_group %in% 
                              c("Budesonide", 
                                "Betamethasone", 
                                "Other"))) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 10000, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 3,
           alpha = 0.7,
           angle = -90,
           hjust = 0,
           vjust = 0) 
## Save plot to the size of a 16:9 PowerPoint slide
ggsave('Dave/plots/Corticosteroids_Perscriptions_per_drug_3.png', width = 10, height = 5.625, units = "in")