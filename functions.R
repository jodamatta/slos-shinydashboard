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
