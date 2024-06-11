library(shiny)
library(ggplot2)
library(forecast)
library(tseries)
library(DT)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "TimeVista"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-line")),
      menuItem("Decomposition", tabName = "decomposition", icon = icon("layer-group")),
      menuItem("SARIMA Model", tabName = "sarima", icon = icon("cogs")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("stethoscope")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-area")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Help", tabName = "help", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .navbar {
          background-color: 	#cadbef; /* Change this color to your desired header color */
        }
         .skin-blue .main-header .logo {
          background-color: 	#b2d0f3; /* Change this color to your desired header color */
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "upload",
              fileInput("file1", "Choose  File (Optional)",
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              checkboxInput("header", "Header", TRUE),
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                           selected = ","),
              selectInput("builtin", "Or Select a Built-in Dataset",
                          choices = c("None", "AirPassengers (Monthly)", "nottem (Monthly)",
                                      "EuStockMarkets (Daily)", "WWWusage (Daily)", "BJsales (Daily)",
                                      "UKgas (Quarterly)", "lynx (Yearly)", "UKLungDeaths (Yearly)",
                                      "DNase (Yearly)", "sunspot.year (Yearly)", "precip (Yearly)")),
              selectInput("column", "Select Time Series Column", choices = NULL),
              actionButton("resetOptions", "Reset Options")
      ),
      tabItem(tabName = "dataset",
              DTOutput("dataTable"),
              downloadButton("downloadData", "Download Dataset", icon = icon("download"))
      ),
      tabItem(tabName = "exploration",
              plotOutput("tsPlot"),
              verbatimTextOutput("summary"),
              uiOutput("tsPlotEquation"),
              downloadButton("downloadTsPlot", "Download Time Series Plot", icon = icon("download")),
              downloadButton("downloadSummary", "Download Summary", icon = icon("download"))
      ),
      tabItem(tabName = "decomposition",
              plotOutput("decompPlot"),
              uiOutput("decompEquation"),
              downloadButton("downloadDecompPlot", "Download Decomposition Plot", icon = icon("download"))
      ),
      tabItem(tabName = "sarima",
              verbatimTextOutput("sarimaModel"),
              uiOutput("sarimaEquation"),
              downloadButton("downloadSARIMAModel", "Download SARIMA Model", icon = icon("download"))
      ),
      tabItem(tabName = "diagnostics",
              plotOutput("diagPlot", height = "600px"),
              downloadButton("downloadDiagPlot", "Download Diagnostics Plot", icon = icon("download"))
      ),
      tabItem(tabName = "forecasting",
              numericInput("h", "Forecast Horizon (periods):", 12),
              plotOutput("forecastPlot"),
              uiOutput("forecastEquation"),
              downloadButton("downloadForecastPlot", "Download Forecast Plot", icon = icon("download"))
      ),
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Decomposition Plot",
                  plotOutput("dashboardDecompPlot"),
                  width = 6
                ),
                box(
                  title = "SARIMA Model",
                  verbatimTextOutput("dashboardSarimaModel"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Forecasting Plot",
                  plotOutput("dashboardForecastPlot", height = "600px"),
                  width = 12
                )
              ),
              fluidRow(
                column(12, align = "center",
                       downloadButton("downloadDashboard", "Download Dashboard", icon = icon("download"))
                )
              )
      ),
      tabItem(tabName = "help",
              fluidRow(
                box(
                  title = "Help",
                  h4("About the App"),
                  p("This Time Series Analysis application allows users to upload a CSV file or select from built-in datasets to perform various types of time series analysis including data exploration, decomposition, SARIMA modeling, diagnostics, and forecasting."),
                  h4("How to Use the App"),
                  tags$ol(
                    tags$li("Navigate to the 'Dataset' tab to upload your CSV file or select a built-in dataset."),
                    tags$li("Use the 'Data Exploration' tab to visualize the time series and view summary statistics."),
                    tags$li("In the 'Decomposition' tab, view the seasonal decomposition of the time series."),
                    tags$li("Use the 'SARIMA Model' tab to fit and view the summary of a SARIMA model."),
                    tags$li("The 'Diagnostics' tab provides diagnostic plots for the fitted SARIMA model."),
                    tags$li("In the 'Forecasting' tab, specify the forecast horizon and view the forecast plot."),
                    tags$li("The 'Dashboard' tab combines decomposition, SARIMA model summary, and forecasting plots.")
                  ),
                  h4("Known Drawbacks"),
                  tags$ul(
                    tags$li("The app might not handle very large datasets efficiently."),
                    tags$li("Only supports univariate time series analysis."),
                    tags$li("Limited options for customizing plots."),
                    tags$li("The app searchs for SARIMA models using function 'auto.arima' which won't be the best model all the time")
                  ),
                  width = 12
                )
              )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # Reactive expression for storing the loaded data
  loadedData <- reactiveVal(NULL)
  
  # Function to load data from file or built-in datasets
  loadData <- function() {
    if (is.null(input$file1) && input$builtin == "None") {
      return(NULL)
    } else if (!is.null(input$file1)) {
      read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    } else {
      switch(input$builtin,
             "AirPassengers (Monthly)" = data.frame(Time = time(AirPassengers), Passengers = as.numeric(AirPassengers)),
             "nottem (Monthly)" = data.frame(Time = time(nottem), Temperature = as.numeric(nottem)),
             "EuStockMarkets (Daily)" = data.frame(Time = time(EuStockMarkets), EuStockMarkets),
             "WWWusage (Daily)" = data.frame(Time = time(WWWusage), WWWusage),
             "BJsales (Daily)" = data.frame(Time = time(BJsales), BJsales),
             "UKgas (Quarterly)" = data.frame(Time = time(UKgas), Consumption = as.numeric(UKgas)),
             "lynx (Yearly)" = data.frame(Time = time(lynx), lynx),
             "UKLungDeaths (Yearly)" = data.frame(Time = time(UKLungDeaths), UKLungDeaths),
             "DNase (Yearly)" = data.frame(Time = time(DNase), DNase),
             "sunspot.year (Yearly)" = data.frame(Time = time(sunspot.year), sunspot.year),
             "precip (Yearly)" = data.frame(Time = time(precip), precip)
      )
    }
  }
  
  # Load data initially
  observeEvent(input$builtin, {
    loadedData(loadData())
  })
  
  observeEvent(input$file1, {
    loadedData(loadData())
  })
  
  # Reset options button action
  observeEvent(input$resetOptions, {
    updateSelectInput(session, "column", choices = NULL)
    loadedData(NULL)
    updateRadioButtons(session, "sep", selected = ",")
    updateCheckboxInput(session, "header", value = TRUE)
    updateSelectInput(session, "builtin", selected = "None")
  })
  
  # Update column choices based on loaded data
  observe({
    if (!is.null(loadedData())) {
      updateSelectInput(session, "column", choices = colnames(loadedData()))
    }
  })
  
  # Reactive expression for time series data
  tsData <- reactive({
    req(loadedData(), input$column)
    ts(loadedData()[[input$column]], frequency = ifelse(input$builtin == "nottem (Monthly)", 12, ifelse(input$builtin == "UKgas (Quarterly)", 4, 12)))
  })
  
  # Render data table
  output$dataTable <- renderDT({
    loadedData()
  })
  
  # Render time series plot
  output$tsPlot <- renderPlot({
    req(tsData())
    autoplot(tsData()) + ggtitle("Time Series Plot") +
      theme_minimal() +
      theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render time series plot equation
  output$tsPlotEquation <- renderUI({
    req(tsData())
    withMathJax(
      helpText('The time series plot is displayed using the equation: $$Y_t = \\text{data}[t]$$ where \\( Y_t \\) represents the time series data at time \\( t \\).')
    )
  })
  
  # Render summary of time series
  output$summary <- renderPrint({
    req(tsData())
    cat("Summary of the Data Set\n") # Title
    summary(tsData())
  })
  
  # Render decomposition plot
  output$decompPlot <- renderPlot({
    req(tsData())
    ts_decomp <- decompose(tsData())
    autoplot(ts_decomp) + ggtitle("Decomposition Plot") +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })
  
  # Render decomposition equation
  output$decompEquation <- renderUI({
    req(tsData())
    withMathJax(
      helpText('The decomposition plot displays the following components:
                $$Y_t = T_t + S_t + R_t$$ 
                where \\( Y_t \\) is the observed time series, \\( T_t \\) is the trend component, \\( S_t \\) is the seasonal component, and \\( R_t \\) is the residual component.')
    )
  })
  
  # Render SARIMA model summary
  output$sarimaModel <- renderPrint({
    req(tsData())
    fit <- auto.arima(tsData())
    summary(fit)
  })
  
  # Render SARIMA model equation
  output$sarimaEquation <- renderUI({
    req(tsData())
    fit <- auto.arima(tsData())
    withMathJax(
      helpText(paste('The SARIMA model fitted is: $$ARIMA(', fit$arma[1], ', ', fit$arma[6], ', ', fit$arma[2], ') \\times (', fit$arma[3], ', ', fit$arma[7], ', ', fit$arma[4], ')_{', fit$arma[5], '}$$'))
    )
  })
  
  # Render diagnostics plot for SARIMA model
  output$diagPlot <- renderPlot({
    req(tsData())
    fit <- auto.arima(tsData())
    tsdiag(fit)
  })
  
  # Render forecast plot
  output$forecastPlot <- renderPlot({
    req(tsData(), input$h)
    fit <- auto.arima(tsData())
    forecasted <- forecast(fit, h = input$h)
    autoplot(forecasted) + ggtitle("Forecast Plot") +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })
  
  # Render forecast equation
  output$forecastEquation <- renderUI({
    req(tsData(), input$h)
    fit <- auto.arima(tsData())
    forecasted <- forecast(fit, h = input$h)
    withMathJax(
      helpText(paste('The forecast is made using the SARIMA model: $$ARIMA(', fit$arma[1], ', ', fit$arma[6], ', ', fit$arma[2], ') \\times (', fit$arma[3], ', ', fit$arma[7], ', ', fit$arma[4], ')_{', fit$arma[5], '}$$'))
    )
  })
  
  # Render decomposition plot for dashboard
  output$dashboardDecompPlot <- renderPlot({
    req(tsData())
    ts_decomp <- decompose(tsData())
    autoplot(ts_decomp) + ggtitle("Decomposition Plot") + theme_minimal()
  })
  
  # Render SARIMA model summary for dashboard
  output$dashboardSarimaModel <- renderPrint({
    req(tsData())
    fit <- auto.arima(tsData())
    summary(fit)
  })
  
  # Render forecast plot for dashboard
  output$dashboardForecastPlot <- renderPlot({
    req(tsData(), input$h)
    fit <- auto.arima(tsData())
    forecasted <- forecast(fit, h = input$h)
    autoplot(forecasted) + ggtitle("Forecast Plot") + theme_minimal()
  })
  
  # Download dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(loadedData(), file, row.names = FALSE)
    }
  )
  
  # Download time series plot
  output$downloadTsPlot <- downloadHandler(
    filename = function() {
      paste("time_series_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = autoplot(tsData()) + ggtitle("Time Series Plot") + theme_minimal())
    }
  )
  
  # Download summary
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste("summary-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(capture.output(summary(tsData())), file)
    }
  )
  
  # Download decomposition plot
  output$downloadDecompPlot <- downloadHandler(
    filename = function() {
      paste("decomposition_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ts_decomp <- decompose(tsData())
      ggsave(file, plot = autoplot(ts_decomp) + ggtitle("Decomposition Plot") + theme_minimal())
    }
  )
  
  # Download SARIMA model summary
  output$downloadSARIMAModel <- downloadHandler(
    filename = function() {
      paste("sarima_model-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      fit <- auto.arima(tsData())
      writeLines(capture.output(summary(fit)), file)
    }
  )
  
  # Download diagnostics plot
  output$downloadDiagPlot <- downloadHandler(
    filename = function() {
      paste("diagnostics_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      fit <- auto.arima(tsData())
      png(file)
      tsdiag(fit)
      dev.off()
    }
  )
  
  # Download forecast plot
  output$downloadForecastPlot <- downloadHandler(
    filename = function() {
      paste("forecast_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      fit <- auto.arima(tsData())
      forecasted <- forecast(fit, h = input$h)
      ggsave(file, plot = autoplot(forecasted) + ggtitle("Forecast Plot") + theme_minimal())
    }
  )
  
  # Download dashboard
  output$downloadDashboard <- downloadHandler(
    filename = function() {
      paste("dashboard-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 14, height = 10)
      
      # Decomposition Plot
      ts_decomp <- decompose(tsData())
      print(autoplot(ts_decomp) + ggtitle("Decomposition Plot") + theme_minimal())
      
      # SARIMA Model Summary
      fit <- auto.arima(tsData())
      cat("SARIMA Model Summary\n")
      cat(capture.output(summary(fit)), sep = "\n")
      
      # Forecast Plot
      forecasted <- forecast(fit, h = input$h)
      print(autoplot(forecasted) + ggtitle("Forecast Plot") + theme_minimal())
      
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
