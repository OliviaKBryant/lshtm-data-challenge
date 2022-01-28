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

# import datasets from csv and tidy
gpLevelData <- as_tibble(read.csv("GP_corticosterioid_prescriptions.csv"))  #not using magrittr pipe as ESS on Emacs doesnt recognise
gpLevelData <- mutate(gpLevelData, year = as.numeric(str_sub(date, 1, 4)))
ccgLevelData <- as_tibble(read.csv("CCG_corticosterioid_prescriptions.csv"))
ccgLevelData <- mutate(ccgLevelData, year = as.numeric(str_sub(date, 1, 4)))
yearChoices <- unique(gpLevelData$year)

# page description
ui <- fluidPage(
  
  #drop down input to select dataset
  selectInput("inputDataset", "Select dataset", choices=c("gpLevelData", "ccgLevelData", "regionalLevelData")),
  
  #slider input to select year
  sliderInput("inputYear", "Select Year", min=min(yearChoices), max=max(yearChoices),
                                                  value=min(yearChoices), step=1),
  
  #plot output for graph
  plotOutput("deprivationPlot")
  
)

# page functionality
server <- function(input, output, session) {
  
  # reactive object for the working dataset to reduce reloading
  currentDataset <- reactive({
    rawDs <- get((input$inputDataset)) #defaults to searching the calling namespace
    filter(rawDs, year == input$inputYear)
    
  })
  
  # deprivationPlot: render a plot and assign to the output element
  output$deprivationPlot <- renderPlot({
    cleanedData <- filter(currentDataset(), is.na(items) == FALSE )
    ggplot(data=cleanedData) + geom_bar(mapping = aes(x=deprivation_decile, y=items), stat="identity") + scale_colour_fivethirtyeight()
  })
  
}

shinyApp(ui, server)
