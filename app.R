library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ems)
library(httr)
library(httr2)

url <- "https://slos-api-48722054238.us-central1.run.app"

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
    
    #########################
    #       SLOS TEST       #
    #########################
    # result <- SLOS(data_reactive())
    
    temp_file <- tempfile(fileext =".rds")
    saveRDS(data_reactive(), temp_file)
    url_SLOS <- paste(url, "/SLOS_API", sep ="")
    
    response <- request(url_SLOS) %>%
      req_body_multipart(file = upload_file(temp_file)) %>%
      req_timeout(200) %>%
      req_perform()
    
    temp_rds_file <- tempfile(fileext = ".rds")
    
    writeBin(resp_body_raw(response), temp_rds_file)
    
    result <- readRDS(temp_rds_file)
    
    unlink(temp_rds_file)
    
    return(result)  
  })
  
  output$funnel_plot <- renderPlot({
    req(result_metrics_reactive())  
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
    #######################
    #   TEST PREDICTION   #
    #######################
    # predicted_los <- predict_and_evaluate(predictors)
    url_pne <- paste(url, "/predict_and_evaluate_API", sep="")
    
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(predictors, temp_file)
    
    response <- request("https://slos-api-48722054238.us-central1.run.app/predict_and_evaluate_API") %>%
      req_body_multipart(file = upload_file(temp_file)) %>%
      req_timeout(200) %>%
      req_perform()
    
    temp_rds_file <- tempfile(fileext = ".rds")
    
    writeBin(resp_body_raw(response), temp_rds_file)
    
    predicted_los <- readRDS(temp_rds_file)
    unlink(temp_file)
    
    output$prediction <- renderText({
      paste0(round(predicted_los$predictions, 2), " days")
    })
    output$predict_status <- renderText({ status$predict })
  })
}

shinyApp(ui, server, options = list(height = 980))
