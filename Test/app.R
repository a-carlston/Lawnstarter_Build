library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(openxlsx)
library(plotly)

# Load the data
sys.source("../00_Scripts/functions.R", envir = env <- new.env())

data <- env[["customer_count_data"]]() 


# Define the server logic
server <- function(input, output) {
    
    # Create a reactive value to hold the user input
    userInput <- reactive({
        as.numeric(input$userInput)
    })
    
    # Modify the data dataframe to include the user input column
    data_with_input <- reactive({
        data %>% mutate(User_Input = userInput() * `2024`)
    })
    
    # Render the table
    output$table <- DT::renderDataTable({
        data_with_input() %>%
            mutate(Date = format(Date, "%b")) %>%
            group_by(Date) %>%
            tidyr::pivot_wider(
                names_from = Year,
                values_from = Customer_Count,
                names_prefix = "Year_",
                values_fill = 0
            ) %>%
            mutate(across(everything(), ~ format(.x, big.mark = ","))) %>%
            rename_with(~ gsub("Year_", "", .), starts_with("Year_")) %>%
            datatable(
                extensions = c('Buttons', 'Scroller'),
                options = list(
                    dom = 'Blrtip',
                    buttons = list(
                        list(
                            extend = 'excel',
                            text = 'Export to Excel'
                        ),
                        list(
                            extend = 'csv',
                            text = 'Export to CSV'
                        )
                    ),
                    defaultPageSize = 50,
                    scrollY = "450px",
                    scrollX = TRUE,
                    scroller = TRUE,
                    rowStyle = "function(index, row, dt) { if (index % 2 === 1) { return { classes: 'even' }; } }",
                    style = "font-size: 12px;",
                    rownames = FALSE  # Exclude row names column
                )
            )
    })
    
    # ...
    
}

# Define the UI
ui <- fluidPage(
    titlePanel("Phone_Forecast"),
    sidebarLayout(
        sidebarPanel(
            numericInput("userInput", "User Input:", value = NULL)  # Add this line for user input
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Table",
                    DT::dataTableOutput("table")
                ),
                tabPanel(
                    "Chart",
                    plotlyOutput("chart")
                )
            )
        )
    )
)

# Run the application
shinyApp(ui = ui, server = server)