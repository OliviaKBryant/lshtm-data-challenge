########################################
## Data Wrangling
## Olivia Bryant
########################################

setwd("~/Documents/LSHTM/Data Challenge/Initial EDA")
library(tidyverse)
library(plyr)

gp_practice_postcodes <- read.csv("Data/GPdata/gp_practice_postcodes.csv")
gp_prescriptions <- read.csv("Data/GPdata/GP Practice data.csv")
nrow(gp_prescriptions)
deprivation_indices <- read.csv("Data/GPData/2019_deprivation_postcode.csv")
rural_urban <- read.csv("Data/GPData/Rural_Urban_Classification_(2011)_of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv")

# add postcodes to GP practices
gp_add_postcodes <- join(gp_prescriptions, gp_practice_postcodes, by='id', type='left', match='all')
nrow(gp_add_postcodes)
# use postcodes to get deprivation indices and LSOA
gp_add_deprivation <- join(gp_add_postcodes, deprivation_indices, by='postcode', type='left', match='first')
# join on LSOA to get the rural/urban classification
gp_add_rural_urban <- join(gp_add_deprivation, rural_urban, by=c("LSOA11CD"="LSOA.code"), type='left', match='all')

# drop any duplicate columns
colnames(gp_add_rural_urban) <- c("postcode", "date", "gp_id", "name", "items", "cost", "list_size", "postcode_status", "LSOA_code", "LSOANAME", "deprivation_rank", "deprivation_decile", "LSOA_name", "rural_urban_class", "rural_urban_descr", "FID" )
clean_data <- gp_add_rural_urban[ , -which(names(gp_add_rural_urban) %in% c("LSOANAME", "FID"))]

# COVID year will start in March
covid_year_1 <- "2021-03-01"
covid_year_2 <- "2022-03-01"
covid_start <- "2020-03-01"
pre_covid_year_1 <- "2019-03-01"
pre_covid_year_2 <- "2018-03-01"
pre_covid_year_3 <- "2017-03-01"
pre_covid_year_4 <- "2016-03-01"
 
covid_period_status <- function(d) {
  print(d)
  if (d < pre_covid_year_3) {
    return("pre-covid year 4")
  } else if (d >= pre_covid_year_3 && d < pre_covid_year_2) {
    return("pre-covid year 3")
  } else if (d >= pre_covid_year_2 && d < pre_covid_year_1) {
    return("pre-covid year 2")
  } else if (d >= pre_covid_year_1 && d < covid_start) {
    return("pre-covid year 1")
  } else if (d >= covid_start && d < covid_year_1) {
    return("covid year 1")
  } else if (d >= covid_year_1 && d < covid_year_2) {
    return("covid year 2")
  }
}

# assign every row in the prescriptions dataset a covid period
clean_data$covid_period <- lapply(clean_data$date, covid_period_status)
# group rural/urban classifications into just rural or urban
clean_data$rural_urban_overall <- ifelse(grepl("Rural",clean_data$rural_urban_descr), "rural", "urban")   
clean_data <- apply(clean_data,2,as.character)
write.csv(clean_data,"Data/GPData/Analysis Dataset Updated/GP_corticosterioid_prescriptions.csv", row.names = TRUE)


## CCGs
ccg_data <- read.csv("Data/GPData/ccgs.csv")
ccg_data$covid_period <- lapply(ccg_data$date, covid_period_status)
colnames(ccg_data) <- c("date", "ccg_id", "name", "items", "cost", "list_size", "covid_period")
ccg_data<- apply(ccg_data,2,as.character)
write.csv(ccg_data,"Data/GPData/Analysis Dataset Updated/CCG_corticosterioid_prescriptions.csv", row.names = TRUE)

## NHS England regions
nhs_regions <- read.csv("Data/GPData/nhs_regions.csv")
nhs_regions$covid_period <- lapply(nhs_regions$date, covid_period_status)
colnames(nhs_regions) <- c("date", "ccg_id", "name", "items", "cost", "list_size", "covid_period")
nhs_regions<- apply(nhs_regions,2,as.character)
write.csv(nhs_regions,"Data/GPData/Analysis Dataset Updated/NHS_England_regions_corticosterioid_prescriptions.csv", row.names = TRUE)
