# ---
# title: Prevalence conditions and systemic corticosteroid prescriptions
# ---

library(fingertipsR)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(magrittr)
library(zoo)

theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"),
             panel.grid.major.x = element_blank())

# --------------------------------------------------------------------------------
# Load in data
ccg <- read_csv("CCG_corticosterioid_prescriptions.csv")
ccg %<>%
    mutate(items_per_1k_pats = items/list_size *1000) %>%
    filter(list_size != 0, # 6374 obs to 6360 obs
           items_per_1k_pats < 200) # remain 6360 obs
# Create 'year' variable for joining
ccg$year <- lubridate::year(ccg$date)

# --------------------------------------------------------------------------------
## % of people with long-standing health conditions
indid <- 351
dflshc <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
lshc <- dflshc %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod, TimeperiodSortable) %>%
  filter(grepl("^CCG", AreaType))
# Convert year from character to date
lshc$year <- as.Date(lshc$Timeperiod, format = "%Y") %>%
  lubridate::year()
# Convert CCG name to the same format as Openprescribing
lshc$AreaName %<>% toupper() %>%
  str_replace(pattern="&", replacement="AND") %>%
  str_replace(pattern="NHS NEWCASTLE AND GATESHEAD CCG",
              replacement="NHS NEWCASTLE GATESHEAD CCG")
lshc %<>%
  rename(name = AreaName, long_term = Value) %>%
  dplyr::select(name, year, long_term) 

# left join CCG file
itemslsc <- left_join(ccg, lshc, by=c("year", "name"))

# select Mar 2020
lshcmar2020 <- itemslsc %>%
    filter(date=="2020-03-01")

# plot correlation
ggplot(lshcmar2020,aes(x=long_term,y=items_per_1k_pats)) + 
    geom_point() + 
    geom_smooth(method="lm") +
    labs (x="% of Population with \nLong Standing Health Conditions",
          y= "Items per 1,000 Patients",
          title="Systemic Corticosteroid Prescriptions & \n% of Population with Long Standing Health Conditions \nby CCG (Mar 2020)",
          subtitle = paste("Pearson's R = ",
                           round(cor(lshcmar2020$long_term, 
                                     lshcmar2020$items_per_1k_pats, 
                                     method="pearson"),2)),
          caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
                Office for Health Improvement and Disparities. Public health profiles. 2022")
# Word size
ggsave("lshc_corticosteroids_ccg.png", width = 8, height = 4.5,units = "in")

# --------------------------------------------------------------------------------
## COPD
indid <- 253
dfcopd <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
copd <- dfcopd %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod, TimeperiodSortable) %>%
  filter(grepl("^CCG", AreaType))
# Convert year from character to date
copd$year <- as.Date(copd$Timeperiod, format = "%Y") %>%
  lubridate::year()
# Convert CCG name to the same format as Openprescribing
copd$AreaName %<>% toupper() %>%
  str_replace(pattern="&", replacement="AND") %>%
  str_replace(pattern="NHS NEWCASTLE AND GATESHEAD CCG",
              replacement="NHS NEWCASTLE GATESHEAD CCG")
copd %<>%
  rename(name = AreaName, copd_prevalence = Value) %>%
  dplyr::select(name, year, copd_prevalence) %>%
  distinct()

# left join CCG file
itemscopd <- left_join(ccg, copd, by=c("year", "name"))

# select Mar 2020
copdmar2020 <- itemscopd %>%
    filter(date=="2020-03-01")

# plot correlation
ggplot(copdmar2020,aes(x=copd_prevalence,y=items_per_1k_pats)) + 
    geom_point() + 
    geom_smooth(method="lm") +
    labs (x="COPD QOF Prevalence (%)",
          y= "Items per 1,000 Patients",
          title="Systemic Corticosteroid Prescriptions &\nCOPD QOF Prevalence by CCG (Mar 2020)",
          subtitle = paste("Pearson's R = ",
                           round(cor(copdmar2020$copd_prevalence, 
                                     copdmar2020$items_per_1k_pats, 
                                     method="pearson"),2)),
          caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
                Office for Health Improvement and Disparities. Public health profiles. 2022")
# Word size
ggsave("copd_corticosteroids_ccg.png", width = 8, height = 4.5,units = "in")

# --------------------------------------------------------------------------------
## Asthma
indid <- 90933
dfasthma <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
asthma <- dfasthma %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod, TimeperiodSortable) %>%
  filter(grepl("^CCG", AreaType))
# Convert year from character to date
asthma$year <- as.Date(asthma$Timeperiod, format = "%Y") %>%
  lubridate::year()
# Convert CCG name to the same format as Openprescribing
asthma$AreaName %<>% toupper() %>%
  str_replace(pattern="&", replacement="AND") %>%
  str_replace(pattern="NHS NEWCASTLE AND GATESHEAD CCG",
              replacement="NHS NEWCASTLE GATESHEAD CCG")
asthma %<>%
  rename(name = AreaName, asthma_prevalence = Value) %>%
  dplyr::select(name, year, asthma_prevalence) %>%
  distinct()

# left join CCG file
itemsasthma <- left_join(ccg, asthma, by=c("year", "name"))

# select Mar 2020
asthmamar2020 <- itemsasthma %>%
    filter(date=="2020-03-01")

# plot correlation
ggplot(asthmamar2020,aes(x=asthma_prevalence,y=items_per_1k_pats)) + 
    geom_point() + 
    geom_smooth(method="lm") +
    labs (x="Asthma Prevalence (%)",
          y= "Items per 1,000 Patients",
          title="Systemic Corticosteroid Prescriptions &\nAsthma QOF Prevalence by CCG (Mar 2020)",
          subtitle = paste("Pearson's R = ",
                           round(cor(asthmamar2020$asthma_prevalence, 
                                     asthmamar2020$items_per_1k_pats, 
                                     method="pearson"),2)),
          caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
                Office for Health Improvement & Disparities. Public Health Profiles. 2022")
# Word size
ggsave("asthma_corticosteroids_ccg.png",width = 8, height = 4.5,units = "in")

# --------------------------------------------------------------------------------
## Rheumatoid Arthritis
indid <- 91269
dfra <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
ra <- dfra %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod, TimeperiodSortable) %>%
  filter(grepl("^CCG", AreaType))
# Convert year from character to date
ra$year <- as.Date(ra$Timeperiod, format = "%Y") %>%
  lubridate::year()
# Convert CCG name to the same format as Openprescribing
ra$AreaName %<>% toupper() %>%
  str_replace(pattern="&", replacement="AND") %>%
  str_replace(pattern="NHS NEWCASTLE AND GATESHEAD CCG",
              replacement="NHS NEWCASTLE GATESHEAD CCG")
ra %<>%
  rename(name = AreaName, ra_prevalence = Value) %>%
  dplyr::select(name, year, ra_prevalence) %>%
  distinct()

# left join CCG file
itemsra <- left_join(ccg, ra, by=c("year", "name"))

# select Mar 2020
ramar2020 <- itemsra %>%
    filter(date=="2020-03-01")

# plot correlation
ggplot(ramar2020,aes(x=ra_prevalence,y=items_per_1k_pats)) + 
    geom_point() + 
    geom_smooth(method="lm") +
    labs (x="Rheumatoid Arthritis Prevalence (%)",
          y= "Items per 1,000 Patients",
          title="Systemic Corticosteroid Prescriptions &\nRheumatoid Arthritis QOF Prevalence \nby CCG (Mar 2020)",
          subtitle = paste("Pearson's R = ",
                           round(cor(ramar2020$ra_prevalence, 
                                     ramar2020$items_per_1k_pats, 
                                     method="pearson"),2)),
          caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
                Office for Health Improvement & Disparities. Public Health Profiles. 2022") 
# Word size
ggsave("ra_corticosteroids_ccg.png", width = 8, height = 4.5,units = "in")
