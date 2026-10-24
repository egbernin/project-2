
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
      #Including conditional panels for the sliders
      conditionalPanel(condition = "input.num == 'Price'",
                       sliderInput('pricerange', 'Select Range', min = 85000, max = 11200000, value = c(85000, 11200000)
                       )),
      conditionalPanel(condition = "input.num == 'Distance'",
                       sliderInput('distancerange', 'Select Range', min = 0, max = 64.1, value = c(0, 64.1)
                       )),
      conditionalPanel(condition = "input.num == 'All'",
                       sliderInput('pricerange', 'Select Price Range', min = 85000, max = 11200000, value = c(85000, 11200000)),
                       sliderInput('distancerange', 'Select Distance Range', min = 0, max = 64.1, value = c(0, 64.1))
                       ),
      #Making an action button that will subset data
      actionButton('subset_button', 'Subset Data'),
      
      #Making a radio button for modifier variables for plotting data
      radioButtons(
        'modifier',
        label = 'Choose a modifier variable',
        choices =
          list(
            'Type',
            'Regionname',
            'Price',
            'Distance',
            'None'
          ),
        selected = 'None'
      )
    ),
    tabsetPanel(
      #Making tab for 'About' section
      tabPanel('About', 
               p('The purpose of this app is to display and summarize data from the Melbourne Housing Market dataset.'),
               p('This data was scraped from the publicly available records of the Australian real estate website Domain. More information about this dataset can be found at the link below:'),
               p('https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market'),
               p('The first sidebar widget is for subsetting categorical variables from this dataset. The second is for subsetting the numeric variables from this dataset. The third is for choosing a modifier variable for the plots.'),
               p('The Data Download tab in the main body of this app is there to provide a resource for you to look at the dataset and download it.'),
               p('The Data Exploration tab is to display categorical and numeric data summmaries for this dataset. The summaries displayed will depend on the variables subsetted.')),
      #Making tab for 'Data Download' section
      tabPanel('Data Download',
               downloadLink('housing_download', 'Download'),
               br(),
               withSpinner(dataTableOutput('housing_table'))
               ),
      #Making tab for 'Data Exploration' section
      tabPanel('Data Exploration', 'This tab is for displaying summaries of the variables that you subsetted in the sidebar.',
               br(),
               tabsetPanel(
                 #Making a tab for the contingency tables
                 tabPanel('Contingency Tables',
                          br(),
                          tabsetPanel(
                            tabPanel('One-Way',
                                     br(),
                                     withSpinner(tableOutput('one_way'))),
                            tabPanel('Two-Way',
                                     br(),
                                     withSpinner(tableOutput('two_way')))
                          )),
                 #Making a tab for the numerical summaries
                 tabPanel('Numerical Summaries',
                          br(),
                          withSpinner(tableOutput('num_summaries'))
                          ),
                 #Making a tab for the plots
                 tabPanel('Plots',
                          br(),
                          tabsetPanel(
                            #Making a stacked barplot tab
                            tabPanel('Stacked Barplot',
                                     br(),
                                     withSpinner(plotOutput('stacked_barplot'))),
                            #Making a boxplot tab
                            tabPanel('Boxplot',
                                     br(),
                                     withSpinner(plotOutput('boxplot'))),
                            #Making a histogram tab
                            tabPanel('Histogram',
                                     br(),
                                     withSpinner(plotOutput('histogram'))),
                            #Making a scatterplot tab
                            tabPanel('Scatterplot',
                                     br(),
                                     withSpinner(plotOutput('scatterplot'))),
                            #Making a side-by-side barplot tab
                            tabPanel('Side-by-Side Barplot',
                                     br(),
                                     withSpinner(plotOutput('side_by_side'))),
                            #Making a heatmap tab
                            tabPanel('Heatmap',
                                     br(),
                                     withSpinner(plotOutput('heatmap')))
                          ))
    )
    
    
  )))
    

# Define server logic
server <- function(input, output, session) {

  #Making the downlaod link
  output$housing_download <- downloadHandler(
    filename = function(){
      paste('housing_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con){
      write.csv(housing_data, con)
    }
  )
  
  #Reactive function for filtering the data
  filtered_data <- reactiveValues({
    req(input$cat, input$num)
    switch(input$cat,
           'All' = housing_data(c[4,10]),
           'Type' = housing_data[4],
           'Regionname' = housing_data[10])
    switch(input$num,
           'All' = housing_data(c[5,12]),
           'Price' = housing_data[5],
           'Distance' = housing_data[12])
    return(c(input$cat, as.numeric(input$num)))
  })
  
  #ObserveEvent for the price range slider
  observeEvent(input$pricerange, {
    df <- filtered_data()
    df %>%
      filter(Price >= input$pricerange[1] & Price <= input$pricerange[2])
  })
  
  #ObserveEvent function for the distance range slider
  observeEvent(input$distancerange, {
    df <- filtered_data()
    df %>%
      filter(Distance >= input$distancerange[1] & Distance <= input$pricerange[2])
  })
  
  #Reactive function for the modifier variable
  modifier_var <- reactiveValues({
    if(input$modifier == 'Type'){
      modifier <- housing_data[4]
    }else if(input$modifier == 'Regionname'){
      modifier <- housing_data[10]
    }else if(input$modifier == 'Price'){
      modifier <- as.numeric(housing_data[5])
    }else if(input$modifer == 'Distance'){
      modifier <- as.numeric(housing_data[12])
    }else{
      modifier = ''
    }
    
    return(modifier)
    
  })
  
  #Output for housing table
  output$housing_table <- renderDataTable({
    input$subset_button
    df <- filtered_data()
    df
  })

  #Output for one way contingency table
  output$one_way <- renderTable({
    input$subset_button
    df <- filtered_data()
    print(input$cat)
    table(df[input$cat])
  })
  
  #Output for numeric summaries
  output$num_summaries <- renderTable({
    input$subset_button
    df <- filtered_data()
    df %>%
      group_by(input$cat) %>%
      drop_na(input$cat) %>%
      summarize(mean_num = mean(input$num, na.rm = TRUE), sd_num = sd(input$num, na.rm = TRUE), med_num = median(input$num, na.rm = TRUE))
  })
  
  #Output for histogram
   output$histogram <- renderPlot({
    input$subset_button
    df <- filtered_data()
    ggplot(df %>%
             drop_na(!!(isolate(sym(input$num)))), aes(x = !!isolate(sym(input$num)))) +
      geom_density() +
      labs(title = 'Histogram of Selected Numeric Variable(s)', x = 'Numeric Variable', y = 'Distribution')
  })
  
  #Output for stacked barplot
  output$stacked_barplot <- renderPlot({
    input$subset_button
    df <- filtered_data()
    ggplot(data = df %>%
             drop_na(!!(isolate(sym(input$cat))), !!(isolate(sym(input$num))), !!(isolate(sym(input$modifier)))), aes(x = !!(isolate(sym(input$cat))), y = !!(isolate(sym(input$num))), fill = !!(isolate(sym(input$modifier))))) +
      geom_col() +
      labs(title = 'Stacked Barplot of Selected Variables', x = 'Categorical Variable', y = 'Numeric Variable', fill = 'Modifier Variable')
  })
  
  #Output for boxplot
  output$boxplot <- renderPlot({
    input$subset_button
    df <- filtered_data()
    ggplot(data = df %>%
             drop_na(!!(isolate(sym(input$num))), !!(isolate(sym(input$cat))), !!(isolate(sym(input$modifier)))), 
             geom_boxplot(aes(x = !!(isolate(sym(input$num)))), y = !!(isolate(sym(input$cat))), fill = !!(isolate(sym(input$modifier))))) 
      labs(title = 'Boxplot of Selected Variables')
  })
  
  #Output for scatterplot
  output$scatterplot <- renderPlot({
    input$subset_button
    df <- filtered_data()
    modifier <- modifier_var()
    ggplot(df %>%
             drop_na(!!(isolate(sym(input$num))), !!(isolate(sym(modifier)))), aes(x = !!(isolate(sym(input$num))), color = !!(isolate(sym(modifier))))) +
           geom_point() +
           labs(title = 'Scatterplot of Selected Variables')
           
  })
  
  #Output for side by side barplot
  output$side_by_side <- renderPlot({
    input$subset_button
    df <- filtered_data()
    ggplot(df %>%
             drop_na(!!(isolate(sym(input$cat))), !!(isolate(sym(input$modifier))), aes(x = !!(isolate(sym(input$cat))), fill = !!(isolate(sym(input$modifier)))))) +
      geom_bar(position = 'dodge')
    labs(title = 'Side by Side Barplot of Selected Variables', x = 'Categorical Variable', fill = 'Modifier Variable')
  })
  
  #Output for heatmap
  output$heatmap <- renderPlot({
    input$subset_button
    df <- filtered_data()
    ggplot(df %>%
             drop_na(!!(isolate(sym(input$cat))), !!(isolate(sym(input$modifier)))), aes(x = !!(isolate(sym(input$cat))), fill = !!(isolate(sym(input$modifier))))) +
      geom_tile(color = 'lightblue') +
      scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
      theme_minimal() +
      labs(title = 'Heatmap of Selected Variables', x = 'Categorical Variable', fill = 'Modifier Variable')
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
