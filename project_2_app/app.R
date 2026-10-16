#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(shinydashboard)

ui <- page_sidebar(
  
    # Application title
    titlePanel("Melbourne Housing Data"),
    
    sidebar = sidebar(
      #Making widget to select categorical variable
      selectInput(
        'cat',
        label = 'Choose a categorical variable',
        choices =
          list(
            'Type',
            'Method',
            'Regionname',
            'Councilarea'
          ),
        selected = 'Type'
      ),
      selectInput(
        'num',
        label = 'Choose a numeric variable',
        choices =
          list(
            'Rooms',
            'Price',
            'Distance'
          ),
        selected = 'Rooms'
      ),
      conditionalPanel(condition = "input.num == 'Rooms'",
                       sliderInput('roomrange', 'Select Range', min = 1, max = 31, value = c(1,31)
                       )),
      conditionalPanel(condition = "input.num == 'Price'",
                       sliderInput('pricerange', 'Select Range', min = 85000, max = 11200000, value = c(85000, 11200000)
                       )),
      conditionalPanel(condition = "input.num == 'Distance'",
                       sliderInput('distancerange', 'Select Range', min = 0, max = 64.1, value = c(0, 64.1)
                       ))
    ),
    tabsetPanel(
      tabPanel('About', 'The purpose of this app is to explore the different relationships between variables relating to this dataset, as well as to summarize the values within the variables.
               The data used in this app was scraped from the real estate website Domain. It provides data for many variables relating to the location, type, and price of the building, as well as who sold it and when.
               More information about this dataset can be found at this link: https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market
               The sidebar of this app is meant for you to subset different variables and give the specific range of values for each variable that you would like to see.
               The Data Download tab is meant to give you a place that you can view and download this dataset.
               The Data Exploration tab is meant to give you different numeric and graphical summaries based on the subsetted variables you selected in the sidebar.'),
      tabPanel('Data Download', 'Contents'),
      tabPanel('Data Exploration', 'Contents')
    )
    


)

# Define server logic
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
