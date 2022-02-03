library(shiny)
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

#----------------------------------------------------------------------------------------#

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

regions_poly <- st_read("NHS_England_Regions_(April_2020)_Boundaries_EN_BFC.shp") %>% st_transform(4236)
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
item_bins <- sort(regionalLevelDataMerged$items)
#------------------------------------------------------------------------------------------#

# page description
ui <- fluidPage(

    tabsetPanel(
        tabPanel("Visualise Deprivation",
                 titlePanel("Prescribing patterns by deprivation measure dashboard"),
    
                 #drop down input to select dataset
                 selectInput("inputDataset1", "Select Aggregation Level", choices=c("gpLevelDataPresentation", "ccgLevelDataPresentation", "regionalLevelData")),
  
                 #slider input to select year
                 sliderInput("inputYear1", "Select Year", min=min(yearChoices), max=max(yearChoices), value=min(yearChoices), step=1),
 
                 #output element for graph
                 plotOutput("deprivationPlot")
 
                 ),
        
        tabPanel("Visualise CCG Level Prescribing",
                 titlePanel("Prescribing at GP/CCG/National Level by year"),
    
                 #drop down input to select dataset
                 selectInput("inputDataset2", "Select Aggregation Level", choices=c("gpLevelDataPresentation", "ccgLevelData", "regionalLevelData")),
  
                 #slider input to select year
                 sliderInput("inputYear2", "Select Year", min=min(yearChoices), max=max(yearChoices), value=min(yearChoices), step=1),
 
                 #output element for graph
                 plotOutput("simplePlot")
 
                 ),

        tabPanel("Map tab",
                 titlePanel("Visualise geographic patterns"),
                 
                 sliderInput("inputYear3", "Select Year", min=min(yearChoices), max=max(yearChoices), value=min(yearChoices), step=1),

                 verbatimTextOutput("text1"),
                 
                 leafletOutput("mainMap", height="95vh")
            
         )
    
        
      )
  )
    
# page functionality
server <- function(input, output, session) {
  
# reactive object for the working dataset to reduce reloading
currentDataset1 <- reactive({
    rawDS <- get((input$inputDataset1)) #defaults to searching the calling namespace
    filter(rawDS, year==input$inputYear1)
})

currentDataset2 <- reactive({
    rawDS <- get((input$inputDataset2)) #defaults to searching the calling namespace
    filter(rawDS, year==input$inputYear2)
})


currentDataSet3 <- reactive({       
        filter(regionalLevelDataMerged, year==input$inputYear3)

    })

## render the plot and assign to the output element
output$deprivationPlot <- renderPlot({
    ggplot(data=currentDataset1()) +
   geom_col(mapping=aes(x=date, y=items, fill=deprivation_decile)) +
labs(title = "Systemic Corticosteroid Prescpriptions by IMD", caption = "Source: OpenPrescribing.net, EBM Data Lab, University of Oxford, 2017", x = "Month", y = "Items Prescribed") +
   scale_x_date(date_labels="%b",
                date_breaks = "1 month") +
   theme(panel.grid.major.x = element_blank()) +
   scale_colour_fivethirtyeight()
     })

output$simplePlot <- renderPlot({
    ggplot(data=currentDataset2()) +
   geom_col(mapping=aes(x=month, y=items)) +
labs(title = "Total prescriptions by year", caption = "Source: OpenPrescribing.net, EBM Data Lab, University of Oxford, 2017", x = "Month", y = "Items Prescribed") +
   theme(panel.grid.major.x = element_blank()) +
   scale_colour_fivethirtyeight()
     })


    output$text1 <- renderText({

        sort(unique(currentDataSet3()$items))
        
        })
    
    output$mainMap <- renderLeaflet({
        activeData <- currentDataSet3()
        pal <- colorBin("YlOrRd", domain = activeData$items, bins = item_bins)
        leaflet(regions_poly) %>% addTiles()  %>% addPolygons(
           fillColor = pal(currentDataSet3()$items),
           weight = 2,
           opacity = 1,
           color = "white",
           dashArray = "3",
           fillOpacity = 0.7)
        
        })
    
    }

    
# render the plot and assign to the output element
## output$deprivationPlot <- renderPlot({
##      # cleaned_data <-  filter(currentDataset(), is.na(items) == FALSE )
##       ggplot(data=currentDataset()) + geom_bar(mapping = aes(x=deprivation_decile, y=items), stat="identity"    ) + scale_colour_fivethirtyeight()
      
##   })
  


shinyApp(ui, server)
