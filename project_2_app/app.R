
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
            'Type',
            'Regionname',
            'Both'
            ),
        selected = 'Type'
      ),
      
      conditionalPanel(condition = "input.cat == 'Type'",
                       checkboxGroupInput(
                         'type_levels',
                         'Select Levels',
                         choices =
                           list(
                             'h',
                             't',
                             'u'
                           )
                       )
                         ),
      conditionalPanel(condition = "input.cat == 'Regionname'",
                       checkboxGroupInput(
                         'region_levels',
                         'Select Levels',
                         choices = 
                           list(
                             'Eastern Metropolitan',
                             'Eastern Victoria',
                             'Northern Metropolitan',
                             'Northern Victoria',
                             'South-Eastern Metropolitan',
                             'Western Metropolitan',
                             'Western Victoria'
                           )
                       )),
      conditionalPanel(condition = "input.cat == 'Both'",
                       checkboxGroupInput(
                         'type_all',
                         'Select Type Levels',
                         choices =
                           list(
                             'h',
                             't',
                             'u'
                           )
                       ),
                       checkboxGroupInput(
                         'region_all',
                         'Select Regionname Levels',
                         choices =
                           list(
                             'Eastern Metropolitan',
                             'Eastern Victoria',
                             'Northern Metropolitan',
                             'Northern Victoria',
                             'South-Eastern Metropolitan',
                             'Western Metropolitan',
                             'Western Victoria'
                           )
                       )),
      #Making a widget to select numeric variable
      radioButtons(
        'num',
        label = 'Choose a numeric variable',
        choices =
          list(
            'Rooms',
            'Price',
            'Both'
          ),
        selected = 'Rooms'
      ),
      #Including conditional panels for the sliders
      conditionalPanel(condition = "input.num == 'Rooms'",
                       sliderInput('roomrange', 'Select Range', min = 1, max = 31, value = c(1, 31)
                       )),
      conditionalPanel(condition = "input.num == 'Price'",
                       sliderInput('pricerange', 'Select Range', min = 85000, max = 11200000, value = c(85000, 11200000)
                       )),
      conditionalPanel(condition = "input.num == 'Both'",
                       sliderInput('roomrange_all', 'Select Room Range', min = 1, max = 31, value = c(1, 31)),
                       sliderInput('pricerange_all', 'Select Price Range', min = 85000, max = 11200000, value = c(85000, 11200000))),
 
      #Making an action button that will subset data
      actionButton('subset_button', 'Subset and Plot Data'),
      
      #Making a radio button for modifier variables for plotting data
      radioButtons(
        'modifier',
        label = 'Choose a modifier variable',
        choices =
          list(
            'Type',
            'Regionname',
            'Rooms',
            'Price'
          ),
        selected = 'Type'
      )
    ),
    tabsetPanel(
      #Making tab for 'About' section
      tabPanel('About', 
               p('The purpose of this app is to display and summarize data from the Melbourne Housing Market dataset.'),
               p('This data was scraped from the publicly available records of the Australian real estate website Domain. More information about this dataset can be found at the link below:'),
               p('https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market'),
               p('The first sidebar widget is for subsetting categorical variables from this dataset. The second is for subsetting the numeric variables from this dataset. The third is for choosing a modifier variable for the plots. This variable will not be subsetted in the data table.'),
               p('The Data Download tab in the main body of this app is there to provide a resource for you to look at the dataset and download it.'),
               p('The Data Exploration tab is to display categorical and numeric data summmaries for this dataset. The summaries displayed will depend on the variables subsetted.'),
               p("An image of Domain's logo is below:"),
               imageOutput('logo')),
      #Making tab for 'Data Download' section
      tabPanel('Data Download',
               downloadLink('housing_download', 'Download'),
               br(),
               p('Note that this data table includes variables that will not be used in plotting, such as Suburb, Address, Date, etc.'),
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
                                     p('This graph requires at least one categorical variable, one numeric variable, and a modifier.'),
                                     br(),
                                     withSpinner(plotOutput('stacked_barplot'))),
                            #Making a boxplot tab
                            tabPanel('Boxplot',
                                     br(),
                                     p('This graph requires at least one categorical variable, one numeric variable, and a modifier.'),
                                     br(),
                                     withSpinner(plotOutput('boxplot'))),
                            #Making a histogram tab
                            tabPanel('Histogram',
                                     br(),
                                     p('This graph requires at least one numeric variable.'),
                                     br(),
                                     withSpinner(plotOutput('histogram'))),
                            #Making a scatterplot tab
                            tabPanel('Scatterplot',
                                     br(),
                                     p('This graph requires at least one numeric variable and a modifier.'),
                                     br(),
                                     withSpinner(plotOutput('scatterplot'))),
                            #Making a side-by-side barplot tab
                            tabPanel('Side-by-Side Barplot',
                                     br(),
                                     p('This graph requires at least one categorical variable and a modifier.'),
                                     br(),
                                     withSpinner(plotOutput('side_by_side'))),
                            #Making a heatmap tab
                            tabPanel('Heatmap',
                                     br(),
                                     p('This graph requires at least one categorical variable and a modifier.'),
                                     br(),
                                     withSpinner(plotOutput('heatmap')))
                          ))
    )
    
    
  )))
    

# Define server logic
server <- function(input, output) {

  output$logo <- renderImage(
    {
    list(src = 'Picture_for_project_2.png')
  },
  deleteFile = FALSE
  )
  
  
  #Making the downlaod link
  output$housing_download <- downloadHandler(
    filename = function(){
      paste('housing_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con){
      write.csv(housing_data, con)
    }
  )
  
  #ADD CHECKBOXES, SUBSET ROWS NOT COLUMNS, PUT EVERYTHING IN FILTERED_DATA, SEE SCREENSHOT, ISOLATE THINGS
  
  #Reactive function for filtering the data
  filtered_data <- reactive({
    req(input$subset_button)
    type_vector <- c(input$type_levels)
    region_vector <- c(input$region_levels)
    all_region_vector <- c(input$region_all)
    all_type_vector <- c(input$type_all)

    if(input$cat == 'Both'){
      subsetted_cat <- housing_data %>%
        filter(
          Type %in% all_type_vector,
          Type %in% all_type_vector,
          Type %in% all_type_vector,
          Regionname %in% all_region_vector,
          Regionname %in% all_region_vector,
          Regionname %in% all_region_vector,
          Regionname %in% all_region_vector,
          Regionname %in% all_region_vector,
          Regionname %in% all_region_vector,
          Regionname %in% all_region_vector
        )
    }else if(input$cat == 'Type'){
      subsetted_cat <- housing_data %>%
        filter(
          Type %in% type_vector,
          Type %in% type_vector,
          Type %in% type_vector
        )
    }else if(input$cat == 'Regionname'){
      subsetted_cat <- housing_data %>%
        filter(
          Regionname %in% region_vector,
          Regionname %in% region_vector,
          Regionname %in% region_vector,
          Regionname %in% region_vector,
          Regionname %in% region_vector,
          Regionname %in% region_vector,
          Regionname %in% region_vector
        )
    }
    
    if(input$num == 'Rooms'){
      slider_return <- subsetted_cat[subsetted_cat$Rooms >= isolate(input$roomrange[1]) & subsetted_cat$Rooms <= isolate(input$roomrange[2]), ]
    }else if(input$num == 'Price'){
      slider_return <- subsetted_cat[subsetted_cat$Price >= isolate(input$pricerange[1]) & subsetted_cat$Price <= isolate(input$pricerange[2]), ]
    }else if(input$num == 'Both'){
      slider_return <- subsetted_cat[subsetted_cat$Price >= isolate(input$pricerange_all[1]) & subsetted_cat$Price <= isolate(input$pricerange_all[2]) &
                                      subsetted_cat$Rooms >= isolate(input$roomrange_all[1]) & subsetted_cat$Rooms <= isolate(input$roomrange_all[2]), ]
    }
    
    return(slider_return)
  })
  
  
  #Output for housing table
  output$housing_table <- renderDataTable({
    filtered_data()
  })
  

  #Reactive function for the modifier variable
  modifier_var <- reactive({
    req(input$modifier)
    subset_modifier <- c(input$modifier)
    housing_data %>%
      select(all_of(subset_modifier))
  })
  
  #Output for one way contingency table
  output$one_way <- renderTable({
    req(input$cat, input$subset_button)
    df <- filtered_data()
    print(str(df))
    table(df[subsetted_cat])
  })
  
  #Output for two way contingency table
  output$two_way <- renderTable({
    req(input$cat, input$modifier, input$subset_button)
    df <- filtered_data()
    modifier <- modifier_var()
    print(str(df))
    table(df[subsetted_cat], modifier)
  })
  
  #Output for numeric summaries
  output$num_summaries <- renderTable({
    req(input$num, input$cat, input$subset_button)
    df <- filtered_data()
    df %>%
      group_by(!!sym(subsetted_cat)) %>%
      drop_na(!!sym(subsetted_cat)) %>%
      summarize(mean_num = mean(!!(isolate(sym(slider_return))), na.rm = TRUE), sd_num = sd(!!(isolate(sym(slider_return))), na.rm = TRUE), med_num = median(!!(isolate(sym(slider_return))), na.rm = TRUE))
  })
  
  #Output for histogram
   output$histogram <- renderPlot({
    req(input$num, input$subset_button)
    df <- filtered_data()
    ggplot(df %>%
             drop_na(!!(isolate(sym(slider_return)))), aes(x = !!(isolate(sym(slider_return))))) +
      geom_density() +
      labs(title = 'Histogram of Selected Numeric Variable(s)', x = 'Numeric Variable', y = 'Distribution')
  })
  
  #Output for stacked barplot
  output$stacked_barplot <- renderPlot({
    req(input$cat, input$num, input$modifier, input$subset_button)
    df <- filtered_data()
    modifier <- modifier_var()
    ggplot(data = df %>%
             drop_na(!!(isolate(sym(subsetted_cat))), !!(isolate(sym(slider_return))), !!(isolate(sym(modifier)))), aes(x = !!(isolate(sym(subsetted_cat)))), y = !!(isolate(sym(slider_return))), fill = !!(isolate(sym(modifier)))) +
      geom_col() +
      labs(title = 'Stacked Barplot of Selected Variables', x = 'Categorical Variable', y = 'Numeric Variable', fill = 'Modifier Variable')
  })
  
  #Output for boxplot
  output$boxplot <- renderPlot({
    req(input$cat, input$num, input$modifier, input$subset_button)
    df <- filtered_data()
    modifier <- modifier_var()
    ggplot(data = df %>%
             drop_na(!!(isolate(sym(slider_return))), !!(isolate(sym(subsetted_cat))), !!(isolate(sym(modifier)))) + 
             geom_boxplot(aes(x = !!(isolate(sym(slider_return))), y = !!(isolate(sym(subsetted_cat))), fill = !!(isolate(sym(modifier)))))) +
      labs(title = 'Boxplot of Selected Variables')
  })
  
  #Output for scatterplot
  output$scatterplot <- renderPlot({
    req(input$num, input$modifier, input$subset_button)
    df <- filtered_data()
    modifier <- modifier_var()
    ggplot(df %>%
             drop_na(!!(isolate(sym(slider_return))), !!sym(isolate(modifier))), aes(x = !!(isolate(sym(slider_return))), color = !!(isolate(sym(modifier))))) +
           geom_point() +
           labs(title = 'Scatterplot of Selected Variables')
           
  })
  
  #Output for side by side barplot
  output$side_by_side <- renderPlot({
    req(input$cat, input$modifier, input$subset_button)
    df <- filtered_data()
    modifier <- modifier_var()
    ggplot(df %>%
             drop_na(!!(isolate(sym(subsetted_cat))), !!(isolate(sym(modifier)))), aes(x = !!(isolate(sym(subsetted_cat))), fill = !!(isolate(sym(modifier))))) +
      geom_bar(position = 'dodge')
    labs(title = 'Side by Side Barplot of Selected Variables', x = 'Categorical Variable', fill = 'Modifier Variable')
  })
  
  #Output for heatmap
  output$heatmap <- renderPlot({
    req(input$cat, input$modifier, input$subset_button)
    df <- filtered_data()
    modifier <- modifier_var()
    ggplot(df %>%
             drop_na(!!(isolate(sym(subsetted_cat))), !!(isolate(sym(modifier)))), aes(x = !!(isolate(sym(subsetted_cat))), fill = !!(isolate(sym(modifier))))) +
      geom_tile(color = 'lightblue') +
      scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
      theme_minimal() +
      labs(title = 'Heatmap of Selected Variables', x = 'Categorical Variable', fill = 'Modifier Variable')
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
