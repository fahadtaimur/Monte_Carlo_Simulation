library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(shinyjs)
library(ggthemes)
library(shinyWidgets)
library(DT)
library(data.table)
library(quantmod)
library(timetk)

source(file="functions.R")


# Define UI for application that draws a histogram
ui <- navbarPage(

    # Title
    title = "Stock Price - Monte-Carlo Simulation",
    inverse = FALSE,
    collapsible = TRUE,
    
    theme = shinytheme("cerulean"),
    
    tabPanel(
        # Header ----
        title = "Simulator",
        
        # UI Application ----
        div(
            class = "container",
            
            # spacing from the top
            column(width = 12,
                   div(
                       br()
                   )), 
            
            column(
                width = 4, 
                # wellPanel 
                shiny::wellPanel(
                    
                    # stock picker
                    pickerInput(inputId = "stock_selection", 
                                label   = "Select A Stock", 
                                choices = c("AAPL", "NVDA", "SQ", "SPY", "BND"), 
                                multiple = FALSE,
                                selected = "TSLA", 
                                options = pickerOptions(
                                    actionsBox = FALSE,
                                    liveSearch = TRUE,
                                    size=10
                                )),
                    
                    
                    # Date from
                    dateInput(inputId = "date1",
                              label = "Select Beginning Date",
                              value = as.Date("2020-01-01")),
                    
                    
                    # Date to
                    dateInput(inputId = "date2",
                              label = "Select Ending Date", 
                              value = Sys.Date()),
                    
                    # Expected Volatility
                    textInput(inputId = "volatility", 
                              label = "Enter expected volatility", 
                              value = "0.5"),
                    
                    
                    
                    # Number of Simulations
                    pickerInput(inputId = "trials", 
                                label   = "Select Number of Trials", 
                                choices = c(10, 50, 100, 500, 1000, 5000, 10000), 
                                multiple = FALSE,
                                selected = 1000, 
                                options = pickerOptions(
                                    actionsBox = FALSE,
                                    liveSearch = TRUE,
                                    size=10
                                )),
                    
                    # Number of Simulations
                    pickerInput(inputId = "forecast_days", 
                                label   = "Select Forecast Days", 
                                choices = c(15, 30, 60), 
                                multiple = FALSE,
                                selected = 30, 
                                options = pickerOptions(
                                    actionsBox = FALSE,
                                    liveSearch = TRUE,
                                    size=10
                                )),
                    
                    
                )
            ),
            
            
            
            # Table / Plot Area ----
            column(
                width = 8,
                
                div(
                    # Text Above
                    div(class="text-center", h4("Monte-Carlo Simulation Results")),
                    tabsetPanel(id="display", 
                                
                                
                                tabPanel(title = "Histogram", 
                                         plotlyOutput(outputId = "plotly_plot")), 
                                
                                tabPanel(title = "Density Plot", 
                                         plotlyOutput(outputId = "plotly_plot_density")), 
                                
                                tabPanel(title = "Time Series Plot", 
                                         plotlyOutput(outputId = "time_series_plot")), 
                                
                                
                                tabPanel(title = "All Results",
                                         dataTableOutput("Table")
                                ), 
                                
                                tabPanel(title = "Summarized Results", 
                                         dataTableOutput("Simulation_Summary"))
                                
                    )
                )
            ), 
            
            # Details ----
            div(
                class = "container",
                id = "summary",
                column(
                    width = 12,
                    div(
                        class = "panel",
                        div(class = "panel-header", h4("FYI")),
                        div(
                            class = "panel-body",
                            style = "color: #ffffff; background-color: #3e3f3a;",
                            p("Utilize the input selections on the left panel to modify the settings for the Monte-Carlo simulation. 
                              The average price and growth rate for the stock is calculated based on the starting and ending dates. The default
                              volatility is 50%, feel free to adjust higher or lower as you please.")
                        )
                    )
                )
            )
            
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Event Reactivity Setup ----
    
    
    # Get Stock Data / Analyze ----
    data <- reactive({
        getSymbols(input$stock_selection, src="yahoo", 
                   from = input$date1,
                   to = input$date2, 
                   auto.assign = FALSE) %>%
            as.data.frame() %>%
            janitor::clean_names() %>%
            rownames_to_column(var = "date") %>%
            mutate(date = lubridate::ymd(date)) %>%
            select(date, contains("adjusted")) %>%
            set_names("Date", "Adjusted")
    })
    
    ## Average Price
    average_price <- reactive({
        data() %>%
            summarise(average_price = mean(Adjusted)) %>%
            pull()
        })
    
    ## Standard Deviation
    volatility <- reactive({
        data() %>%
            summarise(vol = sd(Adjusted)) %>%
            pull()
    })
    
    ## Rate of Return
    ror <- reactive({
        
        ((data() %>%
            select("Adjusted") %>%
            slice(length(Adjusted)) %>% 
             pull()) / (data() %>%
            select("Adjusted") %>%
            slice(1) %>% pull())-1)

    })
    
    # Perform Monte-Carlo Simulation ----
    stock_simulation <- reactive(
        replicate(n = input$trials, 
                  stock_price_model(cp = average_price(),    # convert to daily mean
                                    days = 252,              # Yearly Days of trading
                                    gr = ror(),              # % return based on selected dates
                                    vol = as.numeric(input$volatility), 
                                    forecast_days = input$forecast_days)
                  )
    )
    
    
    # Table to show simulation results ---- 
    output$Table <- renderDataTable(stock_simulation() %>% 
                                        tibble() %>% 
                                        set_names(nm = "Results") %>% 
                                        mutate(Results = round(Results, digits = 0)))
    
    # Table to show simulation summary ----
    output$Simulation_Summary <- renderDataTable(
        stock_simulation() %>%
            tibble() %>%
            set_names(nm = "Results") %>%
            summarise(
                AveragePrice = mean(Results), 
                StdError = sd(Results)/sqrt(n()), 
                StdDeviation = sd(Results),
                Minimum = min(Results),
                Maximum = max(Results), 
                `10th_Percentile` = quantile(Results, probs = 0.1),
                `90th_Percentile` = quantile(Results, probs = 0.9)
            ) %>%
            map_df(.x = ., .f = ~round(., digits = 1))
    )
    
    # Plot - Histogram ----
    output$plotly_plot <- renderPlotly({
        hist_plotter(df=stock_simulation(), current_price = average_price())
    })
    
    # Plot - Density ----
    output$plotly_plot_density <- renderPlotly({
        density_plotter(df=stock_simulation(), current_price = average_price())
    })
    
    # Plot - Time Series ----
    output$time_series_plot <- renderPlotly({
        data() %>%
            plot_time_series(.date_var = Date, .value = Adjusted)
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
