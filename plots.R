mainerset <- readRDS("daily_province_simulation.RDS")
swet <- readRDS("weekly_province_simulation.RDS")

forecast_plotter <- function(data, type) {
  color_stuff <- paste0(type, " Model Predicted Cases")
  ggplot(data, aes(x = date)) +
    # Shaded area for 90% confidence interval
    # geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = "90% CI"), alpha = 0.2) +
    # Shaded area for 50% confidence interval
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
    # Shaded area for 20% confidence interval
    geom_ribbon(aes(ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
    # Line for the median (predicted cases)
    geom_line(aes(y = median, color = "Predicted Cases"), linetype = "dashed") +
    # Line for the true value (actual cases)
    geom_line(aes(y = true_value, color = "Actual Cases")) +
    geom_vline(xintercept = min(data$date) + 70, linetype = "dashed") +
    # geom_point(aes(y = true_value)) +
    labs(title = paste0(type, " Model New Cases per Day"),
         x = "Date",
         y = "New reports per day",
         fill = "Confidence Interval",
         color = "Legend") +
    scale_fill_manual(values = c("90% CI" = "purple", "50% CI" = "orange", "20% CI" = "green")) +
    scale_color_manual(values = c("Actual Cases" = "red", "Predicted Cases" = "blue")) +
    theme_minimal() +
    # scale_y_log10() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))
}
forecast_plotter(mainerset$SouthAfrica_national$forecast, type = "Daily")
forecast_plotter(swet$SouthAfrica_national$forecast, type = "Weekly")

multiforecast_plotter <- function(df1, df2, aggregation){
  ggplot() + 
    # geom_ribbon(data = df1, aes(x = date, ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
    # Shaded area for 20% confidence interval
    # geom_ribbon(data = df1, aes(x = date,ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
    # Line for the median (predicted cases)
    
    # geom_vline(data = df1, xintercept = min(data$date) + 70, linetype = "dashed") +
    # guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))
    # geom_ribbon(data = df2, aes(x = date,ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
    # Shaded area for 20% confidence interval
    # geom_ribbon(data = df2, aes(x = date,ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
    # Line for the median (predicted cases)
    geom_line(data = df1, aes(x = date,y = median, color = "Daily Model Predicted Cases"), linetype = "dashed") +
    geom_line(data = df2, aes(x = date,y = median, color = "Weekly Model Predicted Cases"), linetype = "dashed") +
    # Line for the true value (actual cases)
    # geom_line(data = df1, aes(y = true_value, color = "Actual Cases")) +
    geom_vline(xintercept = min(df2$date) + 70, linetype = "dashed", color="violet") +
    # geom_point(aes(y = true_value)) +
    geom_line(data = df1, aes(x = date,y = true_value, color = "Actual Cases")) +
    labs(title = paste0("New Reports per Day with Aggregation ", aggregation, " Forecasting"),
         x = "Date",
         y = "New reports per day",
         fill = "Confidence Interval",
         color = "Legend") +
    scale_fill_manual(values = c("90% CI" = "purple", "50% CI" = "orange", "20% CI" = "green")) +
    scale_color_manual(values = c("Actual Cases" = "red", "Weekly Model Predicted Cases" = "blue", "Daily Model Predicted Cases" = "black")) +
    theme_minimal() +
    theme(legend.position = "bottom") # +
    # guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))
}

multiforecast_plotter(mainerset$SouthAfrica_national$forecast, swet$SouthAfrica_national$forecast, aggregation = "Before")
multiforecast_plotter(mainerset$SouthAfrica$forecast, swet$SouthAfrica$forecast, aggregation = "After")
# multiforecast_plotter(swet$SouthAfrica$forecast, swet$SouthAfrica$forecast, aggregation = "After")

national_multiforecast_plotter <- function(df1, df2){
  ggplot() + 
    # geom_ribbon(data = df1, aes(x = date, ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
    # Shaded area for 20% confidence interval
    # geom_ribbon(data = df1, aes(x = date,ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
    # Line for the median (predicted cases)
    
    # geom_vline(data = df1, xintercept = min(data$date) + 70, linetype = "dashed") +
    # guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))
    # geom_ribbon(data = df2, aes(x = date,ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
    # Shaded area for 20% confidence interval
    # geom_ribbon(data = df2, aes(x = date,ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
    # Line for the median (predicted cases)
    geom_line(data = df1, aes(x = date,y = median, color = "Aggregation before Forecast Predicted Cases"), linetype = "dashed") +
    geom_line(data = df2, aes(x = date,y = median, color = "Aggregation after Forecast Predicted Cases"), linetype = "dashed") +
    # Line for the true value (actual cases)
    # geom_line(data = df1, aes(y = true_value, color = "Actual Cases")) +
    geom_vline(xintercept = min(df2$date) + 70, linetype = "dashed", color="violet") +
    # geom_point(aes(y = true_value)) +
    geom_line(data = df1, aes(x = date,y = true_value, color = "Actual Cases")) +
    labs(title = "New Reports per Day",
         x = "Date",
         y = "New reports per day",
         fill = "Confidence Interval",
         color = "Legend") +
    scale_fill_manual(values = c("90% CI" = "purple", "50% CI" = "orange", "20% CI" = "green")) +
    scale_color_manual(values = c("Actual Cases" = "red", "Aggregation before Forecast Predicted Cases" = "blue", "Aggregation after Forecast Predicted Cases" = "black")) +
    theme_minimal() +
    theme(legend.position = "bottom") # +
  # guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))
}

# national_multiforecast_plotter(mainerset$SouthAfrica_national$forecast, mainerset$SouthAfrica$forecast)


extract_crps <- function(score_df, type){
  actual_df <- score_df %>%
    filter(model == type) %>%
    select(date, crps)
  return(actual_df)
}

weekly_weekly_crps <- extract_crps(downscale_results$total_scored, "weekly")
daily_weekly_crps <- extract_crps(downscale_results$total_scored, "daily")
weekly_daily_crps <- extract_crps(upscale_results$total_scored, "weekly")
daily_daily_crps <- extract_crps(upscale_results$total_scored, "daily")

names(weekly_weekly_crps)[2] <- "weekly_weekly_crps"
names(daily_weekly_crps)[2] <- "daily_weekly_crps"
names(weekly_daily_crps)[2] <- "weekly_daily_crps"
names(daily_daily_crps)[2] <- "daily_daily_crps"

merged_crps <- weekly_weekly_crps %>%
  full_join(daily_weekly_crps, by = "date") %>%
  full_join(weekly_daily_crps, by = "date") %>%
  full_join(daily_daily_crps, by = "date") %>%
  full_join(select(mainerset$SouthAfrica$forecast, c("date", "true_value")), by="date")

crps_plotter <- function(score_df) {
  coeff <- 2
  
  # Filter out NA values for each line individually
  score_df_filtered_red <- score_df %>% filter(!is.na(weekly_daily_crps))
  score_df_filtered_blue <- score_df %>% filter(!is.na(weekly_weekly_crps))
  score_df_filtered_green <- score_df %>% filter(!is.na(daily_daily_crps))
  score_df_filtered_yellow <- score_df %>% filter(!is.na(daily_weekly_crps))
  
  ggplot(score_df, aes(x = date)) +
    
    geom_line(aes(y = true_value), linetype = "dashed") +
    geom_vline(xintercept = min(score_df$date) + 70, linetype = "dashed") +
    geom_line(data = score_df_filtered_red, aes(y = weekly_daily_crps / coeff), color = "red") +
    geom_line(data = score_df_filtered_blue, aes(y = weekly_weekly_crps / coeff), color = "blue") +
    # geom_line(data = score_df_filtered_green, aes(y = daily_daily_crps / coeff), color = "green") +
    # geom_line(data = score_df_filtered_yellow, aes(y = daily_weekly_crps / coeff), color = "yellow") +
    
    labs(title = "New Reports per Day",
         x = "Date",
         y = "New reports per day") +
    
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_y_continuous(
      name = "Actual Cases",
      sec.axis = sec_axis(~ . * coeff, name = "Continuous Ranked Probability Score (CRPS)"),
      limits = c(0, 100000 / coeff)
    )
    # scale_y_log10()
}

# Example call to the function with your data
crps_plotter(merged_crps)
