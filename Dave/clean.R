## Info ------------------------------------------------------------------------
##
## Script name: Clean GP level data from OpenPrescribing
##
## Purpose of script: Clean and wrangle data to for plots in other scripts.
## 
##
## Author: David Turner
##
## Date Created: 5 Feb 2022
##
## Notes -----------------------------------------------------------------------
##
## Data sources:
## https://openprescribing.net/
## https://digital.nhs.uk/services/organisation-data-service/file-downloads/gp-and-gp-practice-related-data
##          > https://files.digital.nhs.uk/assets/ods/current/epraccur.zip
##
## Setup -----------------------------------------------------------------------
library(dplyr)
##
## Load Data -------------------------------------------------------------------
## Analysis dataset
pd_gp <- read_csv("data/Analysis Dataset/GP_corticosterioid_prescriptions.csv") 
## GP information for prescribing setting
GPs <- read_csv("data/epraccur.csv", col_names = F) %>%
  select(gp_id = X1, prescribing_setting = X26)
##
## Clean and Wrangle Data ------------------------------------------------------
##
pd_gp_clean <- pd_gp %>%
  left_join(GPs, by = "gp_id") %>% # add th 'prescribing setting (GP surgery type)
  drop_na() %>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(covid_period %in% c("pre-covid year 2", # selecting required time period 
                             "pre-covid year 1",
                             "covid year 1",
                             "covid year 2"),
         prescribing_setting == 4, # select prescribing setting of "GP Practice"
         list_size != 0, # drop GPs with no regestered patients
         items_per_1k_pats < 200) %>% # dropping GPs with unfeasibly high prescription rates
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date),
         month = format(date,"%B"),
         year = format(date, "%Y"))
##
## remove unneeded data
remove(GPs)
remove(pd_gp)