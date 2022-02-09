## Info ------------------------------------------------------------------------
##
## Script name: Pull data from OpenPrescribing API
##
## Purpose of script: Loop to pull OpenPrescribing data at the BNF Chemical 
## Substance level
##
## Author: David Turner
##
## Date Created: 5 Feb 2022
##
## Notes -----------------------------------------------------------------------
## 
## Setup and load clean data ---------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
## To install openprescribingR (accesses the OpenPrescribing API), uncomment the
## following 2 lines to install from github. 
# library(devtools) # to install packages from github
# devtools::install_github("fergustaylor/openprescribingR")
library(openprescribingR)
##
## Pull patient numbers per CCG from the API and aggragate to national level
list_size <- list_size() %>%
  select(date, total_list_size) %>% 
  group_by(date) %>% 
  summarise(list_size = sum(total_list_size)) 
## Read in all BNF codes 
BNFcodes <- read_csv("data/wrangling/20220115_1642266807923_BNF_Code_Information.csv")
##
## Find BNF Chemical Substance codes in relevant sections.
## Codelists of partial BNF codes referring to chapters and sections 
par_bnf_cds <- c("^010502", "^0603", "^100102")
## Finding all `BNF Chemical Substance Codes` in `par_bnf_cds` chapters 
## and sections
bnf_csc <- BNFcodes %>% 
  subset( # find all relevant BNF Presentation Codes that start with `par_bnf_cds`
    Reduce("|",
           lapply(par_bnf_cds,
                  grepl,
                  BNFcodes$`BNF Presentation Code`))) %>% 
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
## bind list into dataframe, add list_size, add chemical substance names, data wrangle
pd_parent_compound <- do.call(rbind, datalist) %>%
  left_join( # add BNF Chemical Substance name
    unique(select(BNFcodes,
                  "BNF Chemical Substance Code",
                  "BNF Chemical Substance")),
    by = c("bnf_csc" = "BNF Chemical Substance Code")) %>%
  rename(bnf_cs = 'BNF Chemical Substance') %>%
  mutate(bnf_cs = factor(bnf_cs),
         parent_compound = factor(word(bnf_cs, 1))) %>%
  group_by(date, parent_compound) %>%
  summarise(items = sum(items)) %>%
  left_join( # add list sizes
    list_size, 
    by = "date") %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date))
##
###########
pd_cs <- do.call(rbind, datalist) %>%
  left_join( # add BNF Chemical Substance name
    unique(select(BNFcodes,
                  "BNF Chemical Substance Code",
                  "BNF Chemical Substance")),
    by = c("bnf_csc" = "BNF Chemical Substance Code")) %>%
  rename(bnf_cs = 'BNF Chemical Substance') %>%
  mutate(bnf_cs = factor(bnf_cs)) %>%
  group_by(date, bnf_cs) %>%
  summarise(items = sum(items)) %>%
  left_join( # add list sizes
    list_size, 
    by = "date") %>%
  mutate(items_per_1k_pats = items/list_size *1000,
         date = as.Date(date))
##########
##
## remove unneeded objects
rm(list = c("bnf_csc",
            "BNFcodes",
            "dat",
            "datalist",
            "i",
            "list_size",
            "p",
            "par_bnf_cds"))
