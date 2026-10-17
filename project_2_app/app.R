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

housing_data <- read_csv('MELBOURNE_HOUSE_PRICES_LESS.csv')

ui <- page_sidebar(
  
    # Application title
    titlePanel("Melbourne Housing Data"),
    
    sidebar = sidebar(
      #Making widget to select categorical variable
      checkboxGroupInput(
        'cat',
        label = 'Choose a categorical variable',
        choices =
          list(
            'Type' = housing_data$Type,
            'Method' = housing_data$Method,
            'Regionname' = housing_data$Regionname,
            'Councilarea' = housing_data$Councilarea
          ),
        selected = 'Type'
      ),
      #Making a widget to select numeric variable
      checkboxGroupInput(
        'num',
        label = 'Choose a numeric variable',
        choices =
          list(
            'Rooms' = housing_data$Rooms,
            'Price' = housing_data$Price,
            'Distance' = housing_data$Distance
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
               dataTableOutput(outputId = 'housing_table'),
               ),
      tabPanel('Data Exploration', 'This tab is for displaying summaries of the variables that you subsetted in the sidebar.',
               br(),
               tabsetPanel(
                 tabPanel('Contingency Tables',
                          br(),
                          tabsetPanel(
                            tabPanel('One-Way',
                                     tableOutput('one_way')),
                            tabPanel('Two-Way',
                                     tableOutput('two_way'))
                          )),
                 tabPanel('Numerical Summaries', 'content'),
                 tabPanel('Plots',
                          plotOutput('scatterplot'),
                          plotOutput('stacked_barplot'),
                          plotOutput('heatmap'),
                          plotOutput('boxplot'),
                          plotOutput('histogram'),
                          plotOutput('faceted_barplot'))
               ))
    ),
    
    
  )
    

# Define server logic
server <- function(input, output) {
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
  
  output$one_way <- renderTable({
    table(input$num | input$cat)
  })
  
  
  
  output$scatterplot <- renderPlot({
    ggplot(data = housing_data, aes_string(x = input$num[1], y = input$num[2])) +
      geom_point() + geom_smooth(method = lm)
  })
  
  output$stacked_barplot <- renderPlot({
    ggplot(data = housing_data, aes_string(x = input$cat[1], fill = input$cat[2]))
  })
  
  output$heatmap <- renderPlot({
    ggplot(data = housing_data, aes_string(x = input$cat | input$num, y = input$cat | input$num, fill = input$cat | input$num)) +
      geom_tile(color = 'lightblue') +
      scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
      theme_minimal() +
      labs(title = 'Heatmap for Melbourne Housing Data')
  })

  output$boxplot <- renderPlot({
    ggplot(data = housing_data, aes_string(x = input$cat, y = input$num, fill = input$cat))
  })
  
  output$histogram <- renderPlot({
    ggplot(data = housing_data, aes_string(x = input$num)) +
      geom_density()
  })
  
  output$faceted_barplot <- renderPlot({
    ggplot(data = housing_data, aes_string(x = input$cat)) +
      geom_bar(position = 'dodge') +
      labs(x = '') +
      scale_fill_discrete('') +
      facet_wrap(~ '')
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
