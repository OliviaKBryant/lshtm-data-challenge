# ---
# title: Population data and number of corticosteroid prescriptions / 1,000 patients
# ---

## Source: Public Health Profile from Office for Health Improvement and Disparities (OHID)
## https://fingertips.phe.org.uk
## Fingertips for R

## Install required packages
# install.packages("fingertipsR", repos = "https://dev.ropensci.org")
# install.packages("miniUI")
# install.packages("DT")
# install.packages("shinycssloaders")

library(fingertipsR)
library(tidyverse)
library(magrittr)
library(zoo)
library(MASS)
theme_set(theme_fivethirtyeight())
theme_update(plot.background = element_blank(),
             panel.background = element_blank(),
             legend.background = element_blank(),
             legend.key = element_blank(),
             axis.ticks.y = element_blank(),
             axis.line = element_blank(),
             axis.title = element_text(),
             panel.grid.major.x = element_blank())

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

# Load in data
CCG <- read_csv("CCG_corticosterioid_prescriptions.csv")
CCG %<>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(list_size != 0, # 6374 obs to 6360 obs
         items_per_1k_pats < 200) # remain 6360 obs
# Create 'year' variable for joining
CCG$year <- lubridate::year(CCG$date)

##################################################
# % with a long-standing health condition
indid <- 351
dflshc <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
lshc <- dflshc %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod, TimeperiodSortable) %>%
  filter(grepl("^CCG", AreaType))
lshc$year <- as.Date(lshc$Timeperiod, format = "%Y") %>%
  lubridate::year()
# Convert CCG name to the same format as Openprescribing
lshc$AreaName %<>% toupper() %>%
  str_replace(pattern="&", replacement="AND") %>%
  str_replace(pattern="NHS NEWCASTLE AND GATESHEAD CCG",
              replacement="NHS NEWCASTLE GATESHEAD CCG")
# Change column names for joining
lshc %<>%
  rename(name = AreaName, long_term = Value) %>%
  dplyr::select(name, year, long_term) 

lshcmar2020 <- left_join(CCG, lshc, by=c("year", "name")) %>%
  filter(date=="2020-03-01")
lshcmar2020$density <- get_density(lshcmar2020$long_term, 
                                   lshcmar2020$items_per_1k_pats, n = 100)
ggplot(lshcmar2020,aes(x=long_term,y=items_per_1k_pats,color=density)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_color_distiller(palette="Blues", direction=1) + 
  labs (x="% with a Long Standing \nHealth Condition",
        y= "Corticosteroid Prescriptions \nper 1,000 Patients",
        title="Corticosteroids Prescriptions & \n% with a Long Standing Health Condition \nby CCG (Mar 2020)",
        subtitle = paste("Pearson's R = ",
                         round(cor(lshcmar2020$long_term, 
                                   lshcmar2020$items_per_1k_pats, 
                                   method="pearson"),2)),
        caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
             Office for Health Improvement and Disparities. Public health profiles. 2022")
ggsave("lshc_corticosteroids_ccg.pdf")

##################################################
# % aged 65+ years
indid <- 336
df65 <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
age65 <- df65 %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod) %>%
  filter(grepl("^CCG", AreaType))
# Convert year from character to date
age65$year <- as.Date(age65$Timeperiod, format = "%Y") %>%
  lubridate::year()
# Convert CCG name to the same format as Openprescribing
age65$AreaName %<>% toupper() %>%
  str_replace(pattern="&", replacement="AND") %>%
  str_replace(pattern="NHS NEWCASTLE AND GATESHEAD CCG",
              replacement="NHS NEWCASTLE GATESHEAD CCG")
# Change column names for joining
age65 %<>%
  rename(name = AreaName, proportion65 = Value) %>%
  dplyr::select(name, year, proportion65) %>%
  distinct()

over65mar2020 <- left_join(CCG, age65, by=c("year", "name")) %>%
  filter(date=="2020-03-01")
over65mar2020$density <- get_density(over65mar2020$proportion65, 
                                     over65mar2020$items_per_1k_pats, n = 100)

ggplot(over65mar2020,aes(x=proportion65,y=items_per_1k_pats,color=density)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_color_distiller(palette="Blues", direction=1) +
  labs (x="% aged 65+ years",
        y= "Corticosteroid Prescriptions \nper 1,000 Patients",
        title="Corticosteroids Prescriptions & \n% aged 65+ years by CCG (Mar 2020)",
        subtitle = paste("Pearson's R = ",
                         round(cor(over65mar2020$proportion65, 
                                   over65mar2020$items_per_1k_pats, 
                                   method="pearson"),2)),
        caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
             Office for Health Improvement and Disparities. Public health profiles. 2022")
ggsave("over65_corticosteroids_ccg.pdf")

##################################################
# COPD: QOF prevalence (all ages)
indid <- 253
dfc <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
copd <- dfc %>%
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
copdmar2020 <- left_join(CCG, copd, by=c("year", "name")) %>%
  filter(date=="2020-03-01")
copdmar2020$density <- get_density(copdmar2020$copd_prevalence, 
                                   copdmar2020$items_per_1k_pats, n = 100)
ggplot(copdmar2020,aes(x=copd_prevalence,y=items_per_1k_pats,color=density)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_color_distiller(palette="Blues", direction=1) +
  labs (x="COPD Prevalence (%)",
        y= "Corticosteroid Prescriptions \nper 1,000 Patients",
        title="Corticosteroids Prescriptions \n& COPD: QOF prevalence (all ages) \nby CCG (Mar 2020)",
        subtitle = paste("Pearson's R = ",
                         round(cor(copdmar2020$copd_prevalence, 
                                   copdmar2020$items_per_1k_pats, 
                                   method="pearson"),2)),
        caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
             Office for Health Improvement and Disparities. Public health profiles. 2022")
ggsave("copd_corticosteroids_ccg.pdf")

##################################################s
# Asthma: QOF prevalence (6+ yrs)
indid <- 90933
dfa <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
asthma <- dfa %>%
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
asthmamar2020 <- left_join(CCG, asthma, by=c("year", "name")) %>%
  filter(date=="2020-03-01")
asthmamar2020$density <- get_density(asthmamar2020$asthma_prevalence, 
                                     asthmamar2020$items_per_1k_pats, n = 100)
ggplot(asthmamar2020,aes(x=asthma_prevalence,y=items_per_1k_pats,color=density)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_color_distiller(palette="Blues", direction=1) +
  labs (x="Asthma Prevalence (%)",
        y= "Corticosteroid Prescriptions \nper 1,000 Patients",
        title="Corticosteroids Prescriptions per 1,000 patients \n& Asthma: QOF prevalence (6+ yrs) \nby CCG (Mar 2020)",
        subtitle = paste("Pearson's R = ",
                         round(cor(asthmamar2020$asthma_prevalence, 
                                   asthmamar2020$items_per_1k_pats, 
                                   method="pearson"),2)),
        caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
             Office for Health Improvement and Disparities. Public health profiles. 2022")
ggsave("asthma_corticosteroids_ccg.pdf")

##################################################s
# Rheumatoid Arthritis: QOF prevalence (16+)
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
ramar2020 <- left_join(CCG, ra, by=c("year", "name")) %>%
  filter(date=="2020-03-01")
ramar2020$density <- get_density(ramar2020$ra_prevalence, 
                                 ramar2020$items_per_1k_pats, n = 100)
ggplot(ramar2020,aes(x=ra_prevalence,y=items_per_1k_pats,color=density)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_color_distiller(palette="Blues", direction=1) +
  labs (x="Rheumatoid Arthritis Prevalence (%)",
        y= "Corticosteroid Prescriptions \nper 1,000 Patients",
        title="Corticosteroids Prescriptions per 1,000 patients \n& Rheumatoid Arthritis QOF Prevalence (16+) \nby CCG (Mar 2020)",
        subtitle = paste("Pearson's R = ",
                         round(cor(ramar2020$ra_prevalence, 
                                   ramar2020$items_per_1k_pats, 
                                   method="pearson"),2)),
        caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
             Office for Health Improvement and Disparities. Public health profiles. 2022")
ggsave("ra_corticosteroids_ccg.pdf")