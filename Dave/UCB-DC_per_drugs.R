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
##
theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"))
##
## Analysis dataset - CCG level
list_size <- read_csv("data/Analysis Dataset/CCG_corticosterioid_prescriptions.csv")  %>% 
  select(date, list_size) %>% 
  group_by(date) %>% 
  summarise(list_size = sum(list_size))
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
  try(dat <- spending_by_code(bnf_csc[i])) # pull data from api
  dat$bnf_csc <- bnf_csc[i]  # add the BNF chem sub codes
  datalist[[i]] <- dat # add data to list
}
  ## bind list into dataframe, add list_size, 
  ## merge with chem sub names and CCG and data wrangle
pd_compound <- do.call(rbind, datalist) %>%
  mutate(date = as.Date(date)) %>%
  left_join( # for list size
    list_size, 
    by = "date") %>%
  drop_na() %>%
  left_join( # for BNF Chemical Substance name
    unique(select(BNFcodes,
                  "BNF Chemical Substance Code",
                  "BNF Chemical Substance")),
    by = c("bnf_csc" = "BNF Chemical Substance Code")) %>%
  rename(bnf_cs = 'BNF Chemical Substance') %>%
  mutate(bnf_cs = factor(bnf_cs),
         parent_compound = word(bnf_cs, 1)) %>%
  group_by(date, bnf_cs, parent_compound) %>%
  summarise(items = sum(items),
            list_size = sum(list_size)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date))
 

pd_parent_compound <- pd_compound %>%
  group_by(date,
           parent_compound,
           list_size) %>%
  summarise(items = sum(items), 
            quantity = sum(quantity), 
            actual_cost = sum(actual_cost)) %>%
  mutate(items_per_1k_pats = items/list_size *1000)

##
## aggregate on a national level and by parent compound
epd_national_parent_compound <- pd_ccg %>%
  mutate(parent_compound = word(bnf_cs, 1)) %>%
  group_by(parent_compound) %>%
  mutate(parent_compound_group = 
           ifelse(sum(items) > 25000,
                  parent_compound, "Other")) %>%
  ungroup() %>%
  group_by(date,
           parent_compound_group,
           list_size) %>%
  summarise(items = sum(items), 
            quantity = sum(quantity), 
            actual_cost = sum(actual_cost)) %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date),
         month = format(date,"%B"),
         year = format(date, "%Y"))
##
## plots ---------------
  ## Setup plot --------
p <- ggplot(mapping = aes(x= date,
                          y= items_per_1k_pats)) +
  labs(title = 'Systemic Corticosteroids Prescriptions per Compound',
       subtitle = 'Before and after the onset of COVID-19\nApr 2019 - Oct 2021',
       y = "Items perscribed per 1000 patients",
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
      ## Line plot 1 -----
p + geom_line(data = filter(pd_parent_compound,
                            parent_compound == "Prednisolone"), 
              aes(colour = parent_compound)) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 12, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 3,
           alpha = 0.7,
           angle = -90,
           hjust = 0.5,
           vjust = 0) 

p + geom_line(data = filter(pd_compound,
                            parent_compound == "Prednisolone"), 
              aes(colour = bnf_cs)) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 12, 
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
      ## Line plot 2 ---------
p + geom_line(data = filter(pd_compound,
                            parent_compound %in% 
                              c(#"Prednisolone",
                                "Fludrocortisone", 
                                "Methylprednisolone", 
                                "Hydrocortisone", 
                                "Dexamethasone", 
                                "Triamcinolone",
                                "Budesonide"
                                #"Budesonide",
                                #"Deflazacort",
                                #"Beclometasone",
                                #"Prednisone",
                                #"Cortisone"
                                ))) +
  annotate("text", # #
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 1, 
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
      ## Line plot 3 --------
p + geom_line(pd_compound,
              parent_compound %in% 
                c(#"Prednisolone",
                  #"Fludrocortisone", 
                  #"Methylprednisolone", 
                  #"Hydrocortisone", 
                  #"Dexamethasone", 
                  #"Triamcinolone",
                  #"Budesonide",
                  "Budesonide",
                  "Deflazacort",
                  "Beclometasone",
                  "Prednisone",
                  "Cortisone"
                )) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 11, 
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
# injectable only compounds
inject <- c("Dexamethasone phosphate", "Dexamethasone sodium phosphate", "Hydrocortisone sodium phosphate", "Hydrocortisone sodium succinate", "Prednisolone acetate", "Triamcinolone acetonide", "Triamcinolone hexacetonide")
## plot injectables
p + geom_line(
              data = filter(pd_compound, bnf_cs %in% inject), mapping = aes(colour = bnf_cs)) +
  annotate("text", # labels
           x = c(as.Date("2020-02-02"), 
                 as.Date("2020-03-29"), 
                 as.Date("2020-11-08"), 
                 as.Date("2021-01-08")), 
           y = 0.5, 
           label = c("1st C19 Case", 
                     "1st Lockdown",
                     "2nd Lockdown",
                     "3rd Lockdown") , 
           size = 3,
           alpha = 0.7,
           angle = -90,
           hjust = 0,
           vjust = 0) 
