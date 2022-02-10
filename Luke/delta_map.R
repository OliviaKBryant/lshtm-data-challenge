library(shiny)
library(tidyverse)
library(ggthemes)
library(leaflet)
library(rgdal)
library(sf)

input_directory <- "/Users/lukeconroy/OneDrive/Masters_UK/HDS/Semester_2/Data_Challenge/Raw_Data/Official_data/Analysis Dataset/"
setwd(input_directory)

##shape files for map
regions_poly <- st_read("NHS_England_Regions_(April_2020)_Boundaries_EN_BFC.shp") %>% st_transform(4236)
regions_coords <- as_tibble(read.csv("regional_shape.csv"))

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
regionalLevelDataMerged <- merge(regions_poly, regionalLevelDataPresentation, by.x="nhser20cd", by.y="nhser20cd")

yearChoices <- c(2017,2018,2019,2020,2021)
monthChoices <- unique(regionalLevelData$month)

regionalLevelDataRawMerged <- merge(regions_poly, regionalLevelData, by.x="nhser20cd", by.y="nhser20cd")
item_bins <- sort(unique(regionalLevelDataRawMerged$items))


ui <- fluidPage(

    tabsetPanel(
        tabPanel("Map tab",
                 titlePanel("Visualise geographic patterns"),
                 sliderInput("inputYear1", "Select Start Year", min=min(yearChoices), max=max(yearChoices), value=min(yearChoices), step=1),
                 sliderInput("inputMonth1", "Select Start Month", min=min(monthChoices), max=max(monthChoices), value=min(monthChoices), step=1),
                 sliderInput("inputYear2", "Select End Year", min=min(yearChoices), max=max(yearChoices), value=min(yearChoices), step=1),
                 sliderInput("inputMonth2", "Select End Month", min=min(monthChoices), max=max(monthChoices), value=min(monthChoices), step=1),

                 tableOutput("table1"),
                 downloadButton('download1',"Download delta"),
                 leafletOutput("mainMap", height="95vh")
            
         )
    )
)

server <- function(input, output, session){
    
# reactive object for the working dataset to reduce reloading
    currentDataSet1 <- reactive({
        
        ds1 <- filter(regionalLevelDataRawMerged, year==input$inputYear1, month==input$inputMonth1)
        ds2 <- filter(regionalLevelDataRawMerged, year==input$inputYear2, month==input$inputMonth2)
        delta <- abs(ds1$items - ds2$items)
        dsDelta <- mutate(ds1, items = delta)
            
    })

    output$table1 <- renderTable({
        
        activeData <- currentDataSet1()
        data.frame(Region=activeData$name, Prescriptions_Delta=activeData$items)
   
        
    })

    output$download1 <- downloadHandler(
        filename=function(){"delta.csv"},
        content=function(fname){
            activeData <- currentDataSet1()
            write.csv(activeData$items, fname)
        }
    )
    
    
    output$mainMap <- renderLeaflet({
        activeData <- currentDataSet1()
        currentBins <- sort(unique(activeData$items)) + c(2,3,4,5,6,7,8)
        print(currentBins)
        print(activeData)
        pal <- colorBin("YlOrRd", domain = activeData$items, bins = currentBins)
        leaflet(regions_poly) %>% addTiles()  %>% addPolygons(
           fillColor = pal(activeData$items),
           weight = 2,
           opacity = 1,
           color = "white",
           dashArray = "3",
           fillOpacity = 0.7) %>% addLegend(
                      pal =  colorBin("YlOrRd", domain = activeData$items, bins = currentBins), 
                      values = currentBins, 
                      opacity = 0.9, 
                      title = "Difference in number of items prescribed",
                      na.label = "No Data",
                      position = "topleft")
        
        })
    
}


shinyApp(ui, server)
