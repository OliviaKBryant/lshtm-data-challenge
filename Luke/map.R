library(shiny)
library(tidyverse)
library(ggthemes)
library(leaflet)

theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"))

input_directory <- "/Users/lukeconroy/OneDrive/Masters_UK/HDS/Semester_2/Data_Challenge/Raw_Data/Official_data/Analysis Dataset/"
setwd(input_directory)

##shape files for map
mapData <- as_tibble(read.csv("regional_shape.csv"))

## import and tidy
gpLevelData <- as_tibble(read.csv("GP_corticosterioid_prescriptions.csv")) %>%
    mutate(year = as.numeric(str_sub(date, 1, 4)), month = as.numeric(str_sub(date, 6, 7)), date=as.Date(date)) %>%
    rename(id = X)

yearChoices <- unique(gpLevelData$year)

## put a(n) (invisible) struture on the data
gpLevelDataGrouped <- group_by(gpLevelData, year, month, deprivation_decile, date)
gpLevelDataPresentation <- summarise_at(gpLevelDataGrouped, .vars=vars(items), .funs=list(sum))

ccgLevelData <- as.tibble(read.csv("CCG_corticosterioid_prescriptions.csv")) %>% mutate(year = as.numeric(str_sub(date, 1, 4)), month = as.numeric(str_sub(date, 6, 7)), date=as.Date(date))

ccgLevelDataGrouped <- group_by(gpLevelData, year, month, date)
ccgLevelDataPresentation <- summarise_at(gpLevelDataGrouped, .vars=vars(items), .funs=list(sum))


ui <- fluidPage(

    tabsetPanel(
        tabPanel("Map tab",
                 titlePanel("Visualise geographic patterns"),
                 leafletOutput("mainMap", height="95vh")
            
         )
    )
)


server <- function(input, output, session){

    output$mainMap <- renderLeaflet({

        leaflet(mapData) %>% addCircles(lng = ~long, lat=~lat) %>% addTiles()
        
        })
    
    }



shinyApp(ui, server)
