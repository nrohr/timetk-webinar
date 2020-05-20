#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidyquant)
library(timetk)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stock Price Anomaly Detection"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("stock",
                      "Stock ticker symbol:",
                      value = "AMZN"),
            
            dateRangeInput("date",
                      "Date Range",
                      start = "2014-01-01",
                      end = Sys.Date()),
            
            numericInput("alpha",
                         "Alpha parameter for anomaly detection",
                         min = .01, 
                         max = .5, 
                         step = .01,
                         value = .05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tsPlot"),
           plotOutput("anomalyPlot"),
           dataTableOutput("anomalyTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    stockReturns <- reactive({
        stock_returns <- 
            input$stock %>% 
            tq_get(get = "stock.prices", from = input$date[1], to = input$date[2]) %>% 
            group_by(symbol) %>% 
            tq_transmute(adjusted, periodReturn, period = "daily", col_rename = "returns")
    })

    output$tsPlot <- renderPlot({
        stockReturns() %>%
            plot_time_series(date, returns, .color_var = year(date),
                             .interactive = FALSE, .color_lab = "Year")
    })
    
    output$anomalyPlot <- renderPlot({
        stockReturns() %>% 
            plot_anomaly_diagnostics(date, returns, .interactive = FALSE, .alpha = input$alpha)
    })
    
    output$anomalyTable <- renderDataTable({
        stockReturns() %>% 
            tk_anomaly_diagnostics(date, returns) %>% 
            filter(anomaly == "Yes") %>% 
            select(date, observed, anomaly) %>% 
            arrange(-abs(observed))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
