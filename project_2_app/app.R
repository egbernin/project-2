
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
  #Rendering the dataset into a data table in the 'Data Download' tab

  
  output$housing_download <- downloadHandler(
    filename = function(){
      paste('housing_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con){
      write.csv(housing_data, con)
    }
  )
  
  filtered_data <- reactive({
    #Categorical variable filter
    if(input$cat == 'All'){
      cat_sub <- housing_data[c(4,10)]
    }else if(input$cat == 'Type'){
      cat_sub <- housing_data[4]
    }else if(input$cat == 'Regionname'){
      cat_sub <- housing_data[10]
    }
    
    #Numeric variable filter
    if(input$num == 'All'){
      num_sub <- housing_data[c(5,12)]
    }else if(input$num == 'Price'){
      num_sub <- housing_data[5]
    }else if(input$num == 'Distance'){
      num_sub <- housing_data[12]
    }
    
    #Modifier variable filter
    if(input$modifier == 'Type'){
      mod_sub <- housing_data[4]
    }else if(input$modifier == 'Regionname'){
      mod_sub <- housing_data[10]
    }else if(input$modifier == 'Price'){
      mod_sub <- housing_data[5]
    }else if(input$modifier == 'Distance'){
      mod_sub <- housing_data[12]
    }

  })
  
  subset_slider <- reactiveValues(
    price_slider = NULL,
    distance_slider = NULL
  )
  

  
  output$housing_table <- renderDataTable({
    input$subset_button
    datatable(filtered_data)
  })
 
  
  
  
  
  
  
  
   # summaries <- reactive({
  #   num_sym <- sym(input$num)
  #   cat_sym <- sym(input$cat)
  #   num_data <- isolate(num_sym)
  #   cat_data <- isolate(cat_sym)
  #   housing_data |>
  #     group_by(cat_data) |>
  #     drop_na(cat_data) |>
  #     summarize(mean_value = mean(num_data, na.rm = TRUE), sd_value = sd(num_data, na.rm = TRUE), med_value = median(num_data, na.rm = TRUE))
  # })
  # 
  # 
  # output$num_summaries <- renderTable({
  #   input$summary_button
  #   summaries()
  # })
  # 
  # 
  # 
  # 
  
  #Output for histogram
   output$histogram <- renderPlot({
    input$subset_button
    x_data <- sym(input$num)
    histogram_data <- isolate(x_data)
    ggplot(data = housing_data |>
             drop_na(!!histogram_data), aes(x = !!histogram_data)) +
      geom_density() +
      labs(title = 'Histogram of Selected Numeric Variable(s)', x = 'Numeric Variable', y = 'Distribution')
  })
  
  #Output for stacked barplot
  output$stacked_barplot <- renderPlot({
    input$subset_button
    cat_sym <- sym(input$cat)
    num_sym <- sym(input$num)
    mod_sym <- sym(input$modifier)
    stacked_cat <- isolate(cat_sym)
    stacked_num <- isolate(num_sym)
    stacked_mod <- isolate(mod_sym)
    ggplot(data = housing_data |>
             drop_na(!!stacked_cat,!!stacked_num, !!stacked_mod), aes(x = !!stacked_cat, y = !!stacked_num, fill = !!stacked_mod)) +
      geom_col() +
      labs(title = 'Stacked Barplot of Selected Variables', x = 'Categorical Variable', y = 'Numeric Variable', fill = 'Modifier Variable')
  })
  
  output$boxplot <- renderPlot({
    input$subset_button
    num_sym <- sym(input$num)
    boxplot_num <- isolate(num_sym)
    ggplot(data = housing_data |>
             drop_na(!!boxplot_num), 
             geom_boxplot(aes(x = !!boxplot_num))) 
      labs(title = 'Boxplot of Selected Variables')
  })
  
  output$scatterplot <- renderPlot({
    input$subset_button
    num_sym <- sym(input$num)
    mod_sym <- sym(input$modifier)
    scatter_num <- isolate(num_sym)
    scatter_mod <- isolate(mod_sym)
    ggplot(housing_data |>
             drop_na(!!scatter_num, !!scatter_mod), aes(x = !!scatter_num, color = !!scatter_mod),
           geom_point(),
           labs(title = 'Scatterplot of Selected Variables')
           )
  })
  
  output$side_by_side <- renderPlot({
    input$subset_button
    cat_sym <- sym(input$cat)
    mod_sym <- sym(input$modifier)
    bar_cat <- isolate(cat_sym)
    bar_mod <- isolate(mod_sym)
    ggplot(housing_data |>
             drop_na(!!bar_cat, !!bar_mod), aes(x = !!bar_cat, fill = !!bar_mod)) +
      geom_bar(position = 'dodge')
    labs(title = 'Side by Side Barplot of Selected Variables', x = 'Categorical Variable', fill = 'Modifier Variable')
  })
  
  output$heatmap <- renderPlot({
    input$subset_button
    cat_sym <- sym(input$cat)
    mod_sym <- sym(input$modifier)
    heatmap_cat <- isolate(cat_sym)
    heatmap_mod <- isolate(mod_sym)
    ggplot(housing_data |>
             drop_na(!!heatmap_cat, !!heatmap_mod), aes(x = !!heatmap_cat, fill = !!heatmap_mod)) +
      geom_tile(color = 'lightblue') +
      scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
      theme_minimal() +
      labs(title = 'Heatmap of Selected Variables', x = 'Categorical Variable', fill = 'Modifier Variable')
  })

  
 
  
  # WORK IN PROGRESS
  #  output$boxplot <- renderPlot({
  #   input$box_button
  #   num_sym <- sym(input$num)
  #   boxplot_data_num <- isolate(num_sym)
  #   ggplot(data = housing_data |>
  #            drop_na(!!boxplot_data_num) +
  #            geom_boxplot(aes(x = !!boxplot_data_num))) +
  #     labs(title = 'Boxplot of Selected Variables', x = 'Numeric Variable')
  # })
  # 
  # # 
  # output$scatterplot <- renderPlot({
  #   input$scatter_button
  #   num_sym <- sym(input$num)
  #   scatter_num <- isolate(num_sym)
  #   ggplot(data = housing_data |>
  #            drop_na(!!scatter_num), aes(x = !!scatter_num)) +
  #     geom_point() +
  #     labs(title = 'Scatterplot of Selected Variables', x = 'Numeric Variable')
  # })
  # 
  
  
  
  
  # 
  # 
  # WORK IN PROGRESS
  

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


 # #Calculating mean, median, and sd
 #  summaries <- reactive({
 #    req(input$num)

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
