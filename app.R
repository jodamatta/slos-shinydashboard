library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(MLmetrics)
library(ranger)
library(caretEnsemble)
library(httr)
library(magrittr)

load_SLOSModel <- function() {
  
  old <- options()
  on.exit(options(old))
  
  options(timeout = 6000)
  url <- "https://github.com/igor-peres/ICU-Length-of-Stay-Prediction/releases/download/v2.0.0/SLOS_small.RData"
  temp_file <- tempfile(fileext = ".RData")
  response <- GET(url, write_disk(temp_file, overwrite = TRUE))
  
  if (http_error(response)) {
    stop("Failed to download the model. HTTP error: ", status_code(response))
  }
  
  load(temp_file)
  return(small_model)
}

predict_and_evaluate <- function(data) {
  small_model <- load_SLOSModel()
  
  predictions <- predict(small_model, newdata = data)
  
  Observations <- data.frame(Observations = data$UnitLengthStay_trunc)
  Predictions <- data.frame(pred = predictions)
  
  RMSE_value <- MLmetrics::RMSE(Predictions$pred, Observations$Observations)
  MAE_value <- MLmetrics::MAE(Predictions$pred, Observations$Observations)
  R2_value <- MLmetrics::R2_Score(Predictions$pred, Observations$Observations)
  comparison <- cbind(Observations, Predictions)
  
  return(list(predictions = data.frame(predictions), comparison = data.frame(comparison), 
              RMSE = RMSE_value, MAE = MAE_value, R2 = R2_value))
}

SLOS <- function(data) {
  # errors 
  required_columns <- c("UnitCode", "UnitLengthStay_trunc")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (nrow(data) == 0) {
    stop("Error: Input data must contain at least one row.")
  }
  
  if (length(missing_columns) > 0) {
    stop("Error: Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  
  # SLOS function
  eval_results <- predict_and_evaluate(data)
  observations <- eval_results$comparison$Observations
  predictions <- eval_results$predictions$pred
  
  df_model_pred <- data.frame(observations = observations, predictions = predictions) %>%
    dplyr::mutate(predictions = dplyr::case_when(
      predictions < 0 ~ 0,
      predictions > 21 ~ 21,
      TRUE ~ predictions
    ))
  
  df_unit_slos <- df_model_pred %>%
    dplyr::bind_cols(dplyr::select(data, UnitCode)) %>%
    dplyr::group_by(UnitCode) %>%
    dplyr::summarise(admissoes = dplyr::n(),
                     soma_los_obs = sum(observations),
                     soma_los_esp = sum(predictions)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(SLOS = soma_los_obs / soma_los_esp) %>%
    stats::na.omit()
  
  plot_SLOS_obs_prev <- ggplot2::ggplot(df_unit_slos) +
    ggplot2::geom_point(ggplot2::aes(x = soma_los_esp, y = soma_los_obs), color = "gray40") +
    ggplot2::geom_smooth(ggplot2::aes(x = soma_los_esp, y = soma_los_obs)) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1), linetype = "dashed") +
    ggplot2::labs(x = "Sum of predicted ICU LoS",
                  y = "Sum of observed ICU LoS",
                  title = "Grouped LoS per Unit (days)") +
    ggplot2::theme_bw()
  
  x_range <- range(df_unit_slos$admissoes, na.rm = TRUE)
  x_padding <- diff(x_range) * 0.1 
  xlim <- c(max(0, x_range[1] - x_padding), x_range[2] + x_padding)
  
  y_range <- range(df_unit_slos$SLOS, na.rm = TRUE)
  y_padding <- diff(y_range) * 0.1 
  ylim <- c(max(0, y_range[1] - y_padding), y_range[2] + y_padding)
  
  # Compute R^2 value
  r_squared <- cor(df_unit_slos$soma_los_esp, df_unit_slos$soma_los_obs)^2
  
  # Compute median and interquartile range of SLOS
  slos_summary <- df_unit_slos %>%
    dplyr::summarise(
      Q1 = quantile(SLOS, 0.25, na.rm = TRUE),
      Q3 = quantile(SLOS, 0.75, na.rm = TRUE)
    ) %>%
    as.list()
  
  slos_summary$IQR <- slos_summary$Q3 - slos_summary$Q1
  
  # Count units within Q1-Q3 range
  units_within_IQR <- df_unit_slos %>%
    dplyr::filter(SLOS >= slos_summary$Q1, SLOS <= slos_summary$Q3) %>%
    dplyr::count()
  
  theta = sum(df_unit_slos$soma_los_obs) / sum(df_unit_slos$soma_los_esp)
  
  # Using ems::funnel to generate funnel plot
  funnel_plot <- ems::funnel(unit = df_unit_slos$UnitCode,
                             y = df_unit_slos$SLOS,
                             y.type = "SRU",
                             o = df_unit_slos$soma_los_obs,
                             e = df_unit_slos$soma_los_esp,
                             theta = sum(df_unit_slos$soma_los_obs) / sum(df_unit_slos$soma_los_esp),
                             n = df_unit_slos$admissoes,
                             method = "normal", option = "rate", plot = FALSE, direct = TRUE)
  
  plot(funnel_plot)
  
  return(list(df_unit_slos = df_unit_slos, 
              plot_SLOS_obs_prev = plot_SLOS_obs_prev, 
              funnel_plot = funnel_plot,
              r_squared = r_squared,
              slos_summary = slos_summary,
              units_within_IQR = units_within_IQR,
              theta = theta))
}


ui <- dashboardPage(
  dashboardHeader(title = "ICU Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Patient Predictor", tabName = "predictor", icon = icon("stethoscope")),
      menuItem("ICU Efficiency Analysis", tabName = "efficiency", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { min-height: 100vh !important; }
    "))),
    
    tabItems(
      tabItem(tabName = "efficiency",
              fluidRow(
                box(title = "Upload Data", status = "primary", solidHeader = TRUE, width = 3,
                    fileInput("data_file", "Upload RData File", accept = c(".RData")),
                    textOutput("eval_status"),
                    conditionalPanel(
                      condition = "output.predictions_ready",
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("R² Value: "), textOutput("r_squared", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("Median SLOS: "), textOutput("median_slos", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("Q1: "), textOutput("q1", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("Q3: "), textOutput("q3", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("IQR: "), textOutput("iqr", inline = TRUE)
                      ),
                      
                      downloadButton("download_rdata", "Download Predictions (RData)"),
                      downloadButton("download_csv", "Download Predictions (CSV)")
                    )
                ),
                
                
                box(title = "Analysis Plots", status = "info", solidHeader = TRUE, width = 9,
                    plotOutput("funnel_plot"),
                    plotOutput("slos_plot")
                )
              )
      ),
      
      tabItem(tabName = "predictor",
              fluidRow(
                box(title = "Patient Information", status = "primary", solidHeader = TRUE, width = 3,
                    pickerInput("gender", "Gender:", choices = c("Female", "Male"), selected = "Female"),
                    numericInput("age", "Age (>= 16):", value = 30, min = 16),
                    pickerInput("admission_source", "Source of ICU Admission:", 
                                choices = c("Cardiovascular intervention room", "Emergency room", 
                                            "Operating room", "Other", "Other unit at your hospital", 
                                            "Ward/Floor"), selected = "Other"),
                    pickerInput("admission_type", "Type of ICU Admission:", 
                                choices = c("Medical", "Emergency Surgery", "Scheduled Surgery")),
                    pickerInput("cirrhosis", "Cirrhosis:", choices = c("ChildAB", "ChildC", "No"), selected = "No"),
                    pickerInput("crf", "Chronic Renal Failure:", choices = c("Dialysis", "NoDialysis", "No"), selected = "No"),
                    pickerInput("ChfNyha", "Cardiac Heart Failure:", choices = c("Class23", "Class4", "No"), selected = "No"),
                    numericInput("lowest_gcs", "Lowest Glasgow Coma Scale (3-15):", value = 10, min = 3, max = 15)
                ),
              
                box(title = "Clinical Measurements", status = "warning", solidHeader = TRUE, width = 4,
                    numericInput("urea", "Urea (first 1h):", value = 20, min = 0),
                    numericInput("length_hospital_stay", "Length of Hospital Stay Prior to ICU (days):", value = 5, min = 0),
                    numericInput("creatinine", "Highest Creatinine (first 1h)", value = 0, min = 0),
                    numericInput("high_heart", "Highest Heart Rate (first 1h):", value = 0, min = 0, max = 200),
                    numericInput("high_temp", "Highest temperature (first 1h):", value = 0, min = 0, max = 50),
                    numericInput("high_leuko", "Highest Leukocyte count (first 1h):", value = 0, min = 0),
                    numericInput("high_resp", "Highest Respiratory Rate (first 1h):", value = 0, min = 0)
                ),
                
                box(title = "Comorbidities", status = "danger", solidHeader = TRUE, width = 5,
                    checkboxInput("dementia", "Dementia", FALSE),
                    checkboxInput("aids", "AIDS", FALSE),
                    checkboxInput("alcohol", "Alcoholism", FALSE),
                    checkboxInput("hypertension", "Arterial Hypertension", FALSE),
                    checkboxInput("asthma", "Asthma", FALSE),
                    checkboxInput("arrhythmia", "Cardiac Arrhythmia", FALSE),
                    checkboxInput("aaf", "Acute Atrial Fibrillation", FALSE),
                    checkboxInput("aki", "Acute Kidney Injury", FALSE),
                    checkboxInput("angina", "Angina", FALSE),
                    checkboxInput("asystole", "Asystole", FALSE),
                    checkboxInput("transplant", "Cardiac Transplant", FALSE),
                    checkboxInput("chemo", "Chemotherapy", FALSE),
                    checkboxInput("caf", "Chronic Atrial Fibrillation", FALSE),
                    checkboxInput("ventilation", "Non-invasive Ventilation at admission (first 24h)", FALSE),
                    checkboxInput("resp_failure", "Respiratory Failure at admission (first 24h)", FALSE),
                    checkboxInput("is_mechanical_ventilation", "Mechanical Ventilation", FALSE),
                    checkboxInput("is_vasopressors", "Vasopressors at admission (first 24h)", FALSE)
                )
              ),
              
              fluidRow(
                box(width = 4, 
                    textOutput("predict_status"),
                    actionButton("predict", "Predict", icon = icon("calculator"), class = "btn-success")
                    ),
                box(title = "Predicted Length of Stay:", status = "success", solidHeader = TRUE, width = 4,
                    verbatimTextOutput("prediction"))
              )
      )
    )
  )
)

server <- function(input, output) {
  status <- reactiveValues(eval = "", predict = "")
  
  ### ICU EFFICIENCY
  
  observeEvent(input$data_file, {
    status$eval <- "Evaluating..."  
  })
  
  output$eval_status <- renderText({ req(status$eval); status$eval })
  
  data_reactive <- reactive({
    
    req(input$data_file)
    
    env <- new.env()
    load(input$data_file$datapath, envir = env)
    
    obj_name <- ls(env)[1]  
    data <- env[[obj_name]]
    
    validate(
      need(is.data.frame(data) || is.tibble(data), 
           "The uploaded file must contain a data frame.")
    )
    
    return(data)
  })
  
  result_metrics_reactive <- reactive({
    req(data_reactive())  
    status$eval <- ""  
    result <- SLOS(data_reactive())  
    return(result)  
  })
  
  output$funnel_plot <- renderPlot({
    req(result_metrics_reactive())  # 
    plot(result_metrics_reactive()$funnel_plot)  
  })
  
  
  output$slos_plot <- renderPlot({
    req(result_metrics_reactive())
    plot(result_metrics_reactive()$plot_SLOS_obs_prev)
  })
  
  output$r_squared <- renderText({
    req(result_metrics_reactive())
    round(result_metrics_reactive()$r_squared, 4)
  })
  
  output$median_slos <- renderText({
    req(result_metrics_reactive())
    round(result_metrics_reactive()$theta, 4)
  })
  
  output$q1 <- renderText({
    req(result_metrics_reactive())
    paste(round(result_metrics_reactive()$slos_summary$Q1, 4))
  })
  
  output$q3 <- renderText({
    req(result_metrics_reactive())
    paste(round(result_metrics_reactive()$slos_summary$Q3, 4))
  })
  
  output$iqr <- renderText({
    req(result_metrics_reactive())
    paste(round(result_metrics_reactive()$slos_summary$IQR, 4))
  })
  
  output$predictions_ready <- reactive({
    return(!is.null(result_metrics_reactive()))
  })
  outputOptions(output, "predictions_ready", suspendWhenHidden = FALSE)
  
  output$download_rdata <- downloadHandler(
    filename = function() { "SLOS_predictions.RData" },
    content = function(file) {
      predictions <- result_metrics_reactive()$slos$df_unit_slos
      save(predictions, file = file)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() { "SLOS_predictions.csv" },
    content = function(file) {
      predictions <- result_metrics_reactive()$slos$df_unit_slos
      write.csv(predictions, file, row.names = FALSE)
    }
  )
  
  output$eval_status <- renderText({ status$eval })
  
  ### PATIENT PREDICTOR
  
  observeEvent(input$predict, {
    status$predict <- "Predicting..."  
  })
  
  output$predict_status <- renderText({ req(status$predict); status$predict })
  
  observeEvent(input$predict, {
    status$predict <- ""
    predictors <- data.frame(
      UnitCode = "00",
      UnitLengthStay_trunc = 0,
      Gender = factor(ifelse(input$gender == "Female", "F", "M"), levels = c("F", "M")),
      Age = input$age,
      AdmissionSourceName = input$admission_source,
      AdmissionTypeName = input$admission_type,
      LowestGlasgowComaScale1h = input$lowest_gcs,
      Urea = input$urea,
      LengthHospitalStayPriorUnitAdmission = input$length_hospital_stay,
      IsMechanicalVentilation = factor(as.integer(input$is_mechanical_ventilation), levels = c(1, 0)),
      IsVasopressors = factor(as.integer(input$is_vasopressors), levels = c(1, 0)),
      HighestCreatinine1h = as.numeric(input$creatinine),
      IsNonInvasiveVentilation = factor(as.integer(input$ventilation), levels = c(1, 0)),
      IsRespiratoryFailure = factor(as.integer(input$resp_failure), levels = c(1, 0)),
      IsDementia = factor(as.integer(input$dementia), levels = c(1, 0)),
      HighestHeartRate1h = as.numeric(input$high_heart),
      HighestTemperature1h = as.numeric(input$high_temp),
      ChfNyha = factor(input$ChfNyha),
      HighestLeukocyteCount1h = as.numeric(input$high_leuko),
      HighestRespiratoryRate1h = as.numeric(input$high_resp),
      IsAcuteAtrialFibrilation = factor(as.integer(input$aaf), levels = c(1, 0)),
      IsAcuteKidneyInjury = factor(as.integer(input$aki), levels = c(1, 0)),
      IsAids = factor(as.integer(input$aids), levels = c(1, 0)),
      IsAlcoholism = factor(as.integer(input$alcohol), levels = c(1, 0)),
      IsAngina = factor(as.integer(input$angina), levels = c(1, 0)),
      IsArterialHypertension = factor(as.integer(input$hypertension), levels = c(1, 0)),
      IsAsthma = factor(as.integer(input$asthma), levels = c(1, 0)),
      IsAsystole = factor(as.integer(input$asystole), levels = c(1, 0)),
      IsCardiacArrhythmia = factor(as.integer(input$arrhythmia), levels = c(1, 0)),
      IsCardiacTransplant = factor(as.integer(input$transplant), levels = c(1, 0)),
      IsChemotherapy = factor(as.integer(input$chemo), levels = c(1, 0)),
      IsChronicAtrialFibrilation = factor(as.integer(input$caf), levels = c(1, 0)),
      IsCirrhosis = factor(input$cirrhosis),
      IsCrf = factor(input$crf)
    )
    
    predicted_los <- predict_and_evaluate(predictors)
    
    output$prediction <- renderText({
      paste0(round(predicted_los$predictions, 2), " days")
    })
    output$predict_status <- renderText({ status$predict })
  })
}

shinyApp(ui, server, options = list(height = 980))
