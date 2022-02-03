## Public Health Profile from Office for Health Improvement and Disparities (OHID)
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

# Extract profile id
profs <- profiles()
profs <- profs[grepl("National General Practice Profiles", profs$ProfileName),]
# head(profs)
profs$ProfileName %>%
    unique()

# Public Health Outcomes Framework id #19
# National General Practice Profiles id #20

# Search for indicators
profid <- 20
inds <- indicators(ProfileID = profid)
# head(inds)
# print(inds[grepl("Rheu", inds$IndicatorName), c("IndicatorID", "IndicatorName")])

# 351 % with a long-standing health condition
# 336 % aged 65+ years
# 93553 Deprivation score (IMD 2019)
# 295 Total QOF points
# 90933 Asthma: QOF prevalence (6+ yrs)
#   -> there was an expired dataset with all ages for asthma
# 253 COPD: QOF prevalence (all ages)
# 91269 Rheumatoid Arthritis: QOF prevalence (16+)

# Look up Area Type ID
# area_types() %>%
#     select(AreaTypeID, AreaTypeName) %>%
#     distinct() %>%
#     View()

# % long-standing health condition
indid <- 351
df <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
glimpse(df)
long_term <- df %>%
    dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod, TimeperiodSortable) %>%
    filter(grepl("^CCG", AreaType))
# Convert year from character to date
long_term$year <- as.Date(long_term$Timeperiod, format = "%Y") %>%
    lubridate::year()
# Convert CCG name to the same format as Openprescribing
long_term$AreaName %<>% toupper() %>%
    str_replace(pattern="&", replacement="AND") %>%
    str_replace(pattern="NHS NEWCASTLE AND GATESHEAD CCG",
                replacement="NHS NEWCASTLE GATESHEAD CCG")
long_term %<>%
    rename(name = AreaName, long_term = Value) %>%
    dplyr::select(name, year, long_term) %>%
    distinct()


# % > 65 (by year)
indid <- 336
df <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
glimpse(df)
age65 <- df %>%
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

# % over 65 in England
overall65 <- df %>%
    dplyr::select(AreaType, Value, Timeperiod) %>%
    filter(grepl("England", AreaType))
overall65$year <- as.Date(overall65$Timeperiod, format = "%Y") %>%
    lubridate::year()
overall65 %<>% rename(name = AreaType, proportion65o = Value)

# COPD
indid <- 253
df <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
copd <- df %>%
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

# Asthma
indid <- 90933
df <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
asthma <- df %>%
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
