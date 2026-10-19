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
library(readr)
library(tidyverse)
library(dplyr)
library(DT)
library(shinycssloaders)

housing_data <- read_csv('MELBOURNE_HOUSE_PRICES_LESS.csv')

ui <- page_sidebar(
  
    # Application title
    titlePanel("Melbourne Housing Data"),
    
    sidebar = sidebar(
      #Making widget to select categorical variable
      radioButtons(
        'cat',
        label = 'Choose a categorical variable',
        choices =
          list(
            'All',
            'Type',
            'Regionname'
            ),
        selected = 'All'
      ),
      #Making a widget to select numeric variable
      radioButtons(
        'num',
        label = 'Choose a numeric variable',
        choices =
          list(
            'All',
            'Price',
            'Distance'
          ),
        selected = 'All'
      ),
      conditionalPanel(condition = "input.num == 'Price'",
                       sliderInput('pricerange', 'Select Range', min = 85000, max = 11200000, value = c(85000, 11200000)
                       )),
      conditionalPanel(condition = "input.num == 'Distance'",
                       sliderInput('distancerange', 'Select Range', min = 0, max = 64.1, value = c(0, 64.1)
                       )),
      conditionalPanel(condition = "input.num == 'All'",
                       sliderInput('allrange_price', 'Select Price Range', min = 85000, max = 11200000, value = c(85000, 11200000)),
                       sliderInput('allrange_distance', 'Select Distance Range', min = 0, max = 64.1, value = c(0, 64.1))
                       )
    ),
    tabsetPanel(
      tabPanel('About', 
               p('The purpose of this app is to display and summarize data from the Melbourne Housing Market dataset.'),
               p('This data was scraped from the publicly available records of the Australian real estate website Domain. More information about this dataset can be found at the link below:'),
               p('https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market'),
               p('The first sidebar widget is for subsetting categorical variables from this dataset. The second is for subsetting the numeric variables from this dataset.'),
               p('The Data Download tab in the main body of this app is there to provide a resource for you to look at the dataset and download it.'),
               p('The Data Exploration tab is to display categorical and numeric data summmaries for this dataset. The summaries displayed will depend on the variables subsetted.')),
      tabPanel('Data Download',
               downloadLink('housing_download', 'Download'),
               br(),
               withSpinner(
               dataTableOutput('housing_table')),
               ),
      tabPanel('Data Exploration', 'This tab is for displaying summaries of the variables that you subsetted in the sidebar.',
               br(),
               tabsetPanel(
                 tabPanel('Contingency Tables',
                          br(),
                          tabsetPanel(
                            tabPanel('One-Way',
                                     withSpinner(tableOutput('one_way'))),
                            tabPanel('Two-Way',
                                     withSpinner(tableOutput('two_way')))
                          )),
                 tabPanel('Numerical Summaries',
                          br(),
                          tabsetPanel(
                            tabPanel('Mean',
                                     withSpinner(textOutput('mean'))),
                            tabPanel('Median',
                                     withSpinner(textOutput('med'))),
                            tabPanel('Standard Deviation',
                                     withSpinner(textOutput('sd')))
                          )),
                 tabPanel('Plots',
                          br(),
                          tabsetPanel(
                            tabPanel('Stacked Barplot',
                                     withSpinner(plotOutput('stacked_barplot'))),
                            tabPanel('Boxplot',
                                     withSpinner(plotOutput('boxplot'))),
                            tabPanel('Histogram',
                                     withSpinner(plotOutput('histogram'))),
                            tabPanel('Scatterplot',
                                     withSpinner(plotOutput('scatterplot'))),
                            tabPanel('Side-by-Side Barplot',
                                     withSpinner(plotOutput('side_by_side'))),
                            tabPanel('Heatmap',
                                     withSpinner(plotOutput('heatmap')))
                          ))
    ),
    
    
  )))
    

# Define server logic
server <- function(input, output, session) {
  #Rendering the dataset into a data table in the 'Data Download' tab
  output$housing_table <- renderDataTable({
    datatable(housing_data)
  })
  output$housing_download <- downloadHandler(
    filename = function(){
      paste('housing_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con){
      write.csv(housing_data, con)
    }
  )
  
  
  
  # rooms_filter <- reactive({
  #   housing_data |>
  #     filter(
  #       Rooms >= input$roomrange[1],
  #       Rooms <= input$roomrange[2]
  #     )
  # })
  # 
  # price_filter <- reactive({
  #   housing_data |>
  #     filter(
  #       Price >= input$pricerange[1],
  #       Price <= input$pricerange[2]
  #     )
  # })
  # 
  # distance_filter <- reactive({
  #   housing_data |>
  #     filter(
  #       Distance >= input$distancerange[1],
  #       Distance <= input$distancerange[2]
  #     )
  # })
  # 
  # cat_filter <- reactive({
  #   if(input$cat == 'Type'){
  #     type_data = housing_data$Type
  #   }else if(input$cat == 'Method'){
  #     method_data = housing_data$Method
  #   }else if(input$cat == 'Regionname'){
  #     region_data = housing_data$Regionname
  #   }else if(input$cat == 'Councilarea'){
  #     council_data = housing_data$CouncilArea
  #   }
  #   
  # })

 # #Calculating mean, median, and sd
 #  summaries <- reactive({
 #    req(input$num)
 #    if(input$num == 'Rooms'){
 #      room_data <- housing_data[housing_data$Rooms %in% as.numeric(input$num), ]
 #      room_mean = mean(room_data, na.rm = TRUE)
 #      room_med = median(room_data, na.rm = TRUE)
 #      room_sd = sd(room_data, na.rm = TRUE)
 #      return(list(room_mean, room_med, room_sd))
 #    }else if(input$num == 'Price'){
 #      price_data <- housing_data[housing_data$Price %in% as.numeric(input$num), ]
 #      price_mean = mean(price_data, na.rm = TRUE)
 #      price_med = median(price_data, na.rm = TRUE)
 #      price_sd = sd(price_data, na.rm = TRUE)
 #      return(list(price_mean, price_med, price_sd))
 #    }else if(input$num == 'Distance'){
 #      distance_data <- housing_data[housing_data$Distance %in% as.numeric(input$num), ]
 #      distance_mean = mean(distance_data, na.rm = TRUE)
 #      distance_med = median(distance_data, na.rm = TRUE)
 #      distance_sd = sd(distance_data, na.rm = TRUE)
 #      return(list(distance_mean, distance_med, distance_sd))
 #    }
 #  })
 # 
 #  output$num_summaries <- renderText({
 #    summaries()
 #  })

}


# Run the application 
shinyApp(ui = ui, server = server)
