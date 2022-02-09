# ---
# title: %  aged 65+ and number of corticosteroid prescriptions / 1,000 patients (Mar 2020)
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
             panel.grid.major.x = element_blank())

# --------------------------------------------------------------------------------
### Load in data
# % aged 65+ years
indid <- 336
# Fetch data from fingertips (takes time)
df65 <- fingertips_data(IndicatorID = indid, AreaTypeID = "All")
# Create NHS region dataset
age65_r <- df65 %>%
  dplyr::select(AreaCode, AreaName, AreaType, Value, Timeperiod) %>%
  filter(grepl("^NHS region", AreaType))
# Convert year from character to date
age65_r$date <- as.Date(paste(age65_r$Timeperiod, "-01-01",sep="")) %>%
  lubridate::ymd()
# Convert region names to match government shapefiles
age65_r$AreaName <- substr(age65_r$AreaName, 1, nchar(age65_r$AreaName)-11)
age65_r %<>%
  rename(name = AreaName, proportion65 = Value)

# Create CCG dataset
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

# Import NHS England region boundaries
NHS_regions_bound <- read_sf("NHS_England_Regions_(April_2019)_EN_BFC.shp")

# OpenPrescribing CCG data 
ccg <- read_csv("CCG_corticosterioid_prescriptions.csv")
ccg %<>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(list_size != 0, # 6374 obs to 6360 obs
         items_per_1k_pats < 200) # remain 6360 obs
# Create 'year' variable for joining
ccg$year <- lubridate::year(ccg$date)

# OpenPrescribing NHS region data 
regions <- read_csv("NHS_England_regions_corticosterioid_prescriptions.csv")
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
### Scatterplot for correlation (CCG) (Mar 2020)

# Change column names for joining
age65 %<>%
  rename(name = AreaName, proportion65 = Value) %>%
  dplyr::select(name, year, proportion65) %>%
  distinct()

over65mar2020 <- left_join(ccg, age65, by=c("year", "name")) %>%
  filter(date=="2020-03-01")
ggplot(over65mar2020,aes(x=proportion65,y=items_per_1k_pats)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs (x="% aged 65+ years",
        y= "Items per 1000 Patients",
        title="Systemic Corticosteroid Prescriptions & \n% Aged 65+ Years by CCG (Mar 2020)",
        subtitle = paste("Pearson's R = ",
                         round(cor(over65mar2020$proportion65, 
                                   over65mar2020$items_per_1k_pats, 
                                   method="pearson"),2)),
        caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
                Office for Health Improvement & Disparities. Public Health Profiles. 2022"
  )
# PowerPoint size
ggsave("over65_corticosteroids_ccg.png", width = 10, height = 5.625, units = "in")
# Word size
ggsave("over65_corticosteroids_ccg_word.png", width = 8, height = 4.5, units = "in")

# --------------------------------------------------------------------------------
### Maps (NHS Regions) (Mar 2020)

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
    scale_fill_distiller(direction = 1, palette='Blues', limits=c(low_lim,up_lim), name="Items per \n1000 patients") +
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
agemap <- choropleth_maker_a("2020-01-01", 9, 22, NHS_regions_bound, age65_r)

## side-by-side plot of items per 1000 patients vs % aged 65+
patch <- rxmap + plot_spacer() + agemap + plot_layout(widths = c(4, 0.5 ,4))
patch + plot_annotation(
  title = 'Systemic Corticosteroid Prescriptions & \n% Aged 65+ Years by NHS Region (Mar 2020)',
  caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
                Office for Health Improvement & Disparities. Public Health Profiles. 2022."
  )
# PowerPoint size
ggsave("over65_corticosteroids_region_map.png", width = 10, height = 5.625, units = "in")
# Word size
ggsave("over65_corticosteroids_region_map_word.png", width = 8, height = 4.5, units = "in")

