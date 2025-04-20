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
impute_missing_values <- function(df) {
  mode_function <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  for (col in names(df)) {
    if (any(is.na(df[[col]]))) {
      if (is.numeric(df[[col]])) {
        df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
      } else {
        df[[col]][is.na(df[[col]])] <- mode_function(df[[col]])
      }
    }
  }
  return(df)
}


# ---------------------------------------------
detect_outliers <- function(df, method = "IQR") {
  outlier_list <- list()
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      if (method == "IQR") {
        Q1 <- quantile(df[[col]], 0.25)
        Q3 <- quantile(df[[col]], 0.75)
        IQR_val <- Q3 - Q1
        outliers <- df[[col]] < (Q1 - 1.5 * IQR_val) | df[[col]] > (Q3 + 1.5 * IQR_val)
      } else if (method == "zscore") {
        z <- scale(df[[col]])
        outliers <- abs(z) > 3
      }
      outlier_list[[col]] <- which(outliers)
    }
  }
  return(outlier_list)
}

# ---------------------------------------------
visualize_variables <- function(df) {
  library(ggplot2)
  plots <- list()
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      p <- ggplot(df, aes_string(x = col)) +
        geom_histogram(fill = "steelblue", bins = 30) +
        ggtitle(paste("Histogram of", col))
    } else {
      p <- ggplot(df, aes_string(x = col)) +
        geom_bar(fill = "tomato") +
        ggtitle(paste("Bar plot of", col))
    }
    plots[[col]] <- p
  }
  return(plots)
}


# ---------------------------------------------

predictive_model <- function(df, response) {
  df <- na.omit(df)
  
  # Drop high-cardinality or identifier columns if present
  drop_cols <- intersect(names(df), c("competitorname", "ip_address"))
  df <- df[, !names(df) %in% drop_cols]
  
  if (nrow(df) < 5) {
    stop("Dataset too small after filtering. Cannot proceed with modelling.")
  }
  
  # Check if response is binary or continuous
  if (is.numeric(df[[response]]) && length(unique(df[[response]])) > 2) {
    is_binary <- FALSE
  } else {
    df[[response]] <- as.factor(df[[response]])
    is_binary <- TRUE
  }
  
  formula <- as.formula(paste(response, "~ ."))
  
  set.seed(123)
  trainIndex <- createDataPartition(df[[response]], p = .8, list = FALSE)
  train <- df[trainIndex, ]
  test <- df[-trainIndex, ]
  
  if (is_binary) {
    model <- glm(formula, data = train, family = "binomial")
    pred <- predict(model, test, type = "response")
    pred_class <- ifelse(pred > 0.5, 1, 0)
    acc <- mean(pred_class == as.numeric(as.character(test[[response]])))
    print(paste("Accuracy:", round(acc, 3)))
  } else {
    model <- lm(formula, data = train)
    pred <- predict(model, test)
    rmse <- sqrt(mean((test[[response]] - pred)^2))
    print(paste("RMSE:", round(rmse, 3)))
  }
  
  print(summary(model))
}



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
    output$model_output <- renderText({
      if (!is.null(response_var)) {
        tryCatch({
          model_summary <- capture.output(predictive_model(data, response_var))
          paste(model_summary, collapse = "\n")
        }, error = function(e) {
          paste("Model error:", e$message)
        })
      } else {
        "No response variable selected."
      }
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
