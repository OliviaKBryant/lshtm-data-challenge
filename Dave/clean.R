## Info ------------------------------------------------------------------------
##
## Script name: Setup and clean GP level data from OpenPrescribing
##
## Purpose of script: Load libraries, set plot themes and clean and wrangle data
## for plotting
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
library(tidyverse)
library(ggthemes)
library(scales)
library(ggtext)
library(RColorBrewer)
##
## set plot themes
theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_markdown(),
             plot.caption = element_markdown(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.box.background = element_rect(fill = "white", colour = "white"),
             plot.title = element_markdown(),
             plot.subtitle = element_markdown())
# Okabe-Ito colour blind friendly palette.
cbf_pal_6  <- c("#E69F00", # orange
                "#009E73", # bluishgreen
                "#56B4E9", # yellow
                "#0072B2", # blue
                "#D55E00", # vermillion
                "#CC79A7") # reddishpurple
# Colour blind friendly pallet of 10 colours
cbf_pal <- brewer.pal(10, "Paired")
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
  filter(prescribing_setting == 4, # select prescribing setting of "GP Practice"
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