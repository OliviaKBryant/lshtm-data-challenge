library(tidyverse)
library(ggthemes)
theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"))


input_directory <- "/Users/lukeconroy/OneDrive/Masters_UK/HDS/Semester_2/Data_Challenge/Raw_Data/Official_data/Analysis Dataset/"
setwd(input_directory)

## import and tidy
gpLevelData <- as_tibble(read.csv("GP_corticosterioid_prescriptions.csv")) %>%
    mutate(year = as.numeric(str_sub(date, 1, 4)), month = as.numeric(str_sub(date, 6, 7)), date=as.Date(date)) %>%
    rename(id = X)

yearChoices <-
    unique(gpLevelData$year)

## put a(n) (invisible) struture on the data
gpLevelDataGrouped <- group_by(gpLevelData, year, month, deprivation_decile, date)

## summarise by summing items in the grouped tibble
itemsYearMonthDD <- summarise_at(gpLevelDataGrouped, .vars=vars(items), .funs=list(sum))
itemsYearMonthDD <- mutate(itemsYearMonthDD)

#plot
ggplot(data=filter(itemsYearMonthDD, year==2017)) +
    geom_col(mapping=aes(x=date, y=items, fill=deprivation_decile)) +
    labs(title = "Systemic Corticosteroid Prescpriptions by IMD", caption = "Source: OpenPrescribing.net,     EBM DataLab, University of Oxford, 2017", x = "Month", y = "Items Prescribed") +
    scale_x_date(date_labels="%b",
                 date_breaks = "1 month") +
    theme(panel.grid.major.x = element_blank()) +
  scale_colour_fivethirtyeight()


## ccg data
##ccgLevelData <- as_tibble(read.csv("CCG_corticosterioid_prescriptions.csv"))
##ccgLevelData <- mutate(ccgLevelData, year = as.numeric(str_sub(date, 1, 4)))
