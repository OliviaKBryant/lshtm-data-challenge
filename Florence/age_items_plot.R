# ---
# title: Relationship between % aged 65+ and 
#         Systemic corticosteroid prescriptions per 1000 patients
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
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.key = element_blank(),
             axis.ticks.y = element_blank(),
             axis.line = element_blank())

# Load in data
ccg <- read_csv("CCG_corticosterioid_prescriptions.csv")
ccg %<>%
  mutate(items_per_1k_pats = items/list_size *1000) %>%
  filter(list_size != 0, # 6374 obs to 6360 obs
         items_per_1k_pats < 200) # remain 6360 obs
# Create 'year' variable for joining
ccg$year <- lubridate::year(ccg$date)

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

over65mar2020 <- left_join(ccg, age65, by=c("year", "name")) %>%
  filter(date=="2020-03-01")
ggplot(over65mar2020,aes(x=proportion65,y=items_per_1k_pats)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_color_distiller(palette="Blues", direction=1) +
  labs (x="% aged 65+ years",
        y= "Corticosteroid Prescriptions \nper 1000 Patients",
        title="Systemic Corticosteroids Prescriptions & \n% aged 65+ years by CCG (Mar 2020)",
        subtitle = paste("Pearson's R = ",
                         round(cor(over65mar2020$proportion65, 
                                   over65mar2020$items_per_1k_pats, 
                                   method="pearson"),2)),
        caption = "Sources: OpenPrescribing.net, The DataLab, University of Oxford, 2022,
               Office for Health Improvement and Disparities. Public health profiles. 2022"
        )
ggsave("over65_corticosteroids_ccg.png", width = 10, height = 5.625, units = "in")
