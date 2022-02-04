# ---
# title: Map for %  aged 65+ and number of corticosteroid prescriptions / 1,000 patients (Mar 2020)
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
library(sf)
library(scales)
library(patchwork)
library(ggthemes)

theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.key = element_blank(),
             axis.ticks.y = element_blank(),
             axis.line = element_blank())

# --------------------------------------------------------------------------------
### LOAD IN DATA
# % aged 65+ years
indid <- 336
# Fetch data from fingertips (takes time)
df65 <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
# Create regional dataset
age65 <- df65 %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod) %>%
  filter(grepl("^NHS region", AreaType))
# Convert year from character to date
age65$date <- as.Date(paste(age65$Timeperiod, "-01-01",sep="")) %>%
  lubridate::ymd()
# Convert region names to match government shapefiles
age65$AreaName <- substr(age65$AreaName, 1, nchar(age65$AreaName)-11)
age65 %<>%
  rename(name = AreaName, proportion65 = Value)
# # Change column names for joining
# age65 %<>%
#   rename(name = AreaName, proportion65 = Value) %>%
#   dplyr::select(name, year, proportion65) %>%
#   distinct()

# import NHS England region boundaries
NHS_regions_bound <- read_sf("~/NHS_England_Regions_(April_2019)_EN_BFC/NHS_England_Regions_(April_2019)_EN_BFC.shp")

# Regional
regions <- read_csv("~/NHS_England_regions_corticosterioid_prescriptions.csv")
regions %<>%
  mutate(prescription_rate_per_1000 = items/list_size *1000)

# change the regional names to match the government shapefiles
regions <- regions %>% 
  mutate(name = case_when(name == "LONDON COMMISSIONING REGION" ~ "London",
                          name == "EAST OF ENGLAND COMMISSIONING REGION" ~ "East of England",
                          name == "NORTH WEST COMMISSIONING REGION"  ~ "North West",
                          name ==  "SOUTH WEST COMMISSIONING REGION" ~ "South West",
                          name ==  "SOUTH EAST COMMISSIONING REGION" ~ "South East",
                          name ==  "MIDLANDS COMMISSIONING REGION" ~ "Midlands",
                          name ==  "NORTH EAST AND YORKSHIRE COMMISSIONING REGION" ~ "North East and Yorkshire")
  )

# --------------------------------------------------------------------------------
### MAPS: NHS REGIONS (Mar 2020)

##
##  Choropleth map plotting function
##
# input: date in "YYYY-MM-DD" format, lower limit of scale, upper limit of scale, boundary
# file and data

## Map for prescriptions
choropleth_maker <- function(date, low_lim, up_lim, boundary, data){
  data.filtered <- data[data['date'] == date,]
  data.filtered <- left_join(boundary, data.filtered, by = c("nhser19nm" = "name"))
  choro <- ggplot(data.filtered, aes(fill = prescription_rate_per_1000)) +
    geom_sf(color = "#ffffff", size = 0.1) +
    scale_fill_distiller(direction = 1, palette='Blues', limits=c(low_lim,up_lim), name="Items per 1000 patients") +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks=element_blank(),
    )
  choro
}

rxmap <- choropleth_maker("2020-03-01", 8, 18, NHS_regions_bound, regions)
rxmap

## Map for % aged 65+
choropleth_maker_a <- function(date, low_lim, up_lim, boundary, data){
  data.filtered <- data[data['date'] == date,]
  data.filtered <- left_join(boundary, data.filtered, by = c("nhser19nm" = "name"))
  choro <- ggplot(data.filtered, aes(fill = proportion65)) +
    geom_sf(color = "#ffffff", size = 0.1) +
    scale_fill_distiller(direction = 1, palette='Greens', limits=c(low_lim,up_lim), name="% aged 65+") +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks=element_blank(),
    )
  choro
}

# Choose "1 Jan" as date as % aged 65+ is updated yearly
agemap <- choropleth_maker_a("2020-01-01", 9, 22, NHS_regions_bound, age65)

## side-by-side plot of items per 1000 patients vs % aged 65+
patch <- rxmap + plot_spacer() + agemap + plot_layout(widths = c(4, 1 ,4))
patch + plot_annotation(
  title = 'Systemic Corticosteroid Prescriptions per 1000 patients \nand % aged 65+ (Mar 2020)',
  caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
               Office for Health Improvement and Disparities. Public health profiles. 2022"
  )
ggsave("over65_corticosteroids_region_map.png", width = 10, height = 5.625, units = "in")
