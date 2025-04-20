library(shiny)
library(ggplot2)
library(caret)

# ---------- [ Helper Function ] ---------- #
identify_variable_types <- function(df) {
  qualitative <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  quantitative <- names(df)[sapply(df, is.numeric)]
  list(Qualitative = qualitative, Quantitative = quantitative)
}
# ---------------------------------------------



# ---------------------------------------------


# ---------------------------------------------



# ---------------------------------------------


# ---------- [ Helper Function ] ---------- #



#
# Define UI
ui <- fluidPage(
  titlePanel("Auto Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file", accept = ".csv"),
      uiOutput("response_selector"),
      selectInput("outlier_method", "Outlier Detection Method", 
                  choices = c("IQR", "zscore"), selected = "IQR"),
      actionButton("analyze", "Run Analysis")
    ),
    mainPanel(
      verbatimTextOutput("var_types"),
      verbatimTextOutput("missing_summary"),
      verbatimTextOutput("outliers"),
      uiOutput("plots_ui"),
      verbatimTextOutput("model_output")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  df <- reactiveVal(NULL)
  analysis_plots <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = TRUE)
    df(data)
    
    updateSelectInput(session, "response", choices = names(data))
  })
  
  output$response_selector <- renderUI({
    req(df())
    selectInput("response", "Select Response Variable (optional)", 
                choices = c("None", names(df())), selected = "None")
  })
  
  observeEvent(input$analyze, {
    req(df())
    data <- df()
    response_var <- if (input$response == "None") NULL else input$response
    
    # Save outputs to display in dashboard
    types <- capture.output(print(identify_variable_types(data)))
    output$var_types <- renderText({ paste(types, collapse = "\n") })
    
    data <- impute_missing_values(data)
    df(data)  # Save imputed version
    missing_info <- sapply(data, function(x) sum(is.na(x)))
    output$missing_summary <- renderText({ paste(capture.output(print(missing_info)), collapse = "\n") })
    
    outliers <- capture.output(print(detect_outliers(data, method = input$outlier_method)))
    output$outliers <- renderText({ paste(outliers, collapse = "\n") })
    
    # Plots
    plots <- visualize_variables(data)
    analysis_plots(plots)
    
    output$plots_ui <- renderUI({
      req(analysis_plots())
      plot_output_list <- lapply(names(analysis_plots()), function(col) {
        plotname <- paste0("plot_", col)
        output[[plotname]] <- renderPlot({ analysis_plots()[[col]] })
        plotOutput(plotname, height = "300px")
      })
      do.call(tagList, plot_output_list)
    })
    
    # Predictive model
    if (!is.null(response_var)) {
      model_summary <- capture.output(predictive_model(data, response_var))
      output$model_output <- renderText({ paste(model_summary, collapse = "\n") })
    } else {
      output$model_output <- renderText("No response variable selected.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
