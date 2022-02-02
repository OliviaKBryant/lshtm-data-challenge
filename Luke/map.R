library(shiny)
library(tidyverse)
library(ggthemes)
library(leaflet)
library(rgdal)
library(sf)

## theme_set(theme_fivethirtyeight())
## theme_update(axis.title = element_text(),
##              plot.caption = element_text(hjust = 0, vjust = 0),
##              plot.background = element_rect(fill = "white", colour = "white"),
##              panel.background = element_rect(fill = "white", colour = "white"),
##              legend.background = element_rect(fill = "white", colour = "white"))

input_directory <- "/Users/lukeconroy/OneDrive/Masters_UK/HDS/Semester_2/Data_Challenge/Raw_Data/Official_data/Analysis Dataset/"
setwd(input_directory)

##shape files for map
regions_poly <- st_read("NHS_England_Regions_(April_2020)_Boundaries_EN_BFC.shp") %>% st_transform(4236)

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

regionalLevelData <- as_tibble(read.csv("NHS_England_regions_corticosterioid_prescriptions.csv") %>% mutate(year = as.numeric(str_sub(date, 1, 4)), month = as.numeric(str_sub(date, 6, 7)), date=as.Date(date))) %>%
    mutate(nhser20cd = case_when(
               (name == "EAST OF ENGLAND COMMISSIONING REGION") ~ "E40000007",
               (name == "LONDON COMMISSIONING REGION") ~ "E40000003",
               (name == "MIDLANDS COMMISSIONING REGION") ~ "E40000008",
               (name == "NORTH EAST AND YORKSHIRE COMMISSIONING REGION") ~ "E40000009",
               (name == "NORTH WEST COMMISSIONING REGION") ~ "E40000010",
               (name == "SOUTH EAST COMMISSIONING REGION") ~ "E40000005",
               (name == "SOUTH WEST COMMISSIONING REGION") ~ "E40000006"
           )
        )

regionalLevelDataGrouped <- group_by(regionalLevelData, nhser20cd, year)
regionalLevelDataPresentation <- summarise_at(regionalLevelDataGrouped, .vars=vars(items), .funs=list(sum))

tempDataset <- filter(regionalLevelDataPresentation, year==2017)
tempDataset <- merge(regions_poly, tempDataset, by.x="nhser20cd", by.y="nhser20cd")

##build map layers


ui <- fluidPage(

    tabsetPanel(
        tabPanel("Map tab",
                 titlePanel("Visualise geographic patterns"),
                 leafletOutput("mainMap", height="95vh")
            
         )
    )
)

server <- function(input, output, session){

bins <- c(850332, 1111593, 1156070, 1306996, 1381807, 1640072, 1836607, Inf)
pal <- colorBin("YlOrRd", domain = tempDataset$items, bins = bins)
    
    output$mainMap <- renderLeaflet({
        leaflet(regions_poly) %>% addTiles()  %>% addPolygons(
           fillColor = pal(tempDataset$items),
           weight = 2,
           opacity = 1,
           color = "white",
           dashArray = "3",
           fillOpacity = 0.7)
        
        })
    
    }


shinyApp(ui, server)
