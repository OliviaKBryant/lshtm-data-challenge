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

# import datasets from csv and tidy
## gpLevelData <- as_tibble(read.csv("GP_corticosterioid_prescriptions.csv"))  #not using magrittr pipe as ESS on Emacs doesnt recognise
## gpLevelData <- mutate(gpLevelData, year = as.numeric(str_sub(date, 1, 4)))

## import and tidy
gpLevelData <- as_tibble(read.csv("GP_corticosterioid_prescriptions.csv")) %>%
    mutate(year = as.numeric(str_sub(date, 1, 4)), month = as.numeric(str_sub(date, 6, 7)), date=as.Date(date)) %>%
    rename(id = X)

yearChoices <-
    unique(gpLevelData$year)

## put a(n) (invisible) struture on the data
gpLevelDataGrouped <- group_by(gpLevelData, year, month, deprivation_decile, date)
gpLevelDataPresentation <- summarise_at(gpLevelDataGrouped, .vars=vars(items), .funs=list(sum))

ccgLevelData <- as_tibble(read.csv("CCG_corticosterioid_prescriptions.csv"))
ccgLevelData <- mutate(ccgLevelData, year = as.numeric(str_sub(date, 1, 4)))
yearChoices <- unique(gpLevelData$year)

#------------------------------------------------------------------------------------------#

# page description
ui <- fluidPage(
  
  #drop down input to select dataset
  selectInput("inputDataset", "Select dataset", choices=c("gpLevelDataPresentation", "ccgLevelData", "regionalLevelData")),
  
  #slider input to select year
  sliderInput("inputYear", "Select Year", min=min(yearChoices), max=max(yearChoices), value=min(yearChoices), step=1),
  
  #output element for graph
  plotOutput("deprivationPlot")
  
)

# page functionality
server <- function(input, output, session) {
  
# reactive object for the working dataset to reduce reloading
currentDataset <- reactive({
    rawDS <- get((input$inputDataset)) #defaults to searching the calling namespace
    filter(rawDS, year==input$inputYear)
    })

## render the plot and assign to the output element
output$deprivationPlot <- renderPlot({
    ggplot(data=currentDataset()) +
   geom_col(mapping=aes(x=date, y=items, fill=deprivation_decile)) +
labs(title = "Systemic Corticosteroid Prescpriptions by IMD", caption = "Source: OpenPrescribing.net, EBM Data Lab, University of Oxford, 2017", x = "Month", y = "Items Prescribed") +
   scale_x_date(date_labels="%b",
                date_breaks = "1 month") +
   theme(panel.grid.major.x = element_blank()) +
   scale_colour_fivethirtyeight()
     })
    
    
# render the plot and assign to the output element
## output$deprivationPlot <- renderPlot({
##      # cleaned_data <-  filter(currentDataset(), is.na(items) == FALSE )
##       ggplot(data=currentDataset()) + geom_bar(mapping = aes(x=deprivation_decile, y=items), stat="identity"    ) + scale_colour_fivethirtyeight()
      
##   })
  
}

shinyApp(ui, server)
