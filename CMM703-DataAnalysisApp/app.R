library(shiny)
library(ggplot2)
library(caret)
library(shinythemes)

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



# Defining the UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # You can try: cerulean, journal, sandstone etc.
  
  titlePanel("🧠 Auto Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "📂 Upload CSV file", accept = ".csv"),
      uiOutput("response_selector"),
      selectInput("outlier_method", "📊 Outlier Detection Method", 
                  choices = c("IQR", "zscore"), selected = "IQR"),
      actionButton("analyze", "🚀 Run Analysis"),
      actionButton("fix_types", "🛠 Fix Variable Types"),
      width = 3
    ),
    
    mainPanel(
      tags$h4("📌 Variable Types"),
      verbatimTextOutput("var_types"),
      tags$hr(),
      
      tags$h4("📌 Missing Values (per column)"),
      verbatimTextOutput("missing_summary"),
      tags$hr(),
      
      tags$h4("📌 Univariate Outliers (Index positions)"),
      verbatimTextOutput("outliers"),
      tags$hr(),
      
      tags$h4("📌 Variable-wise Visualizations"),
      uiOutput("plots_ui"),
      tags$hr(),
      
      tags$h4("📌 Predictive Model Summary"),
      verbatimTextOutput("model_output"),
      width = 9
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
  
  observeEvent(input$fix_types, {
    req(df())
    
    showModal(modalDialog(
      title = "🔧 Convert Variable Type",
      selectInput("var_to_convert", "Select Variable", choices = names(df())),
      selectInput("new_type", "Convert to Type", choices = c("numeric", "factor", "character")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_conversion", "Convert")
      )
    ))
  })
  
  # ✅ Place this block immediately after the one above
  observeEvent(input$confirm_conversion, {
    removeModal()
    current_df <- df()
    var <- input$var_to_convert
    type <- input$new_type
    
    # Apply conversion
    tryCatch({
      if (type == "numeric") {
        current_df[[var]] <- as.numeric(as.character(current_df[[var]]))
      } else if (type == "factor") {
        current_df[[var]] <- as.factor(current_df[[var]])
      } else if (type == "character") {
        current_df[[var]] <- as.character(current_df[[var]])
      }
      df(current_df)  # Save back to reactiveVal
      showNotification(paste("✅", var, "converted to", type), type = "message")
    }, error = function(e) {
      showNotification(paste("⚠️ Error converting variable:", e$message), type = "error")
    })
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
