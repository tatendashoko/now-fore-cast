forecast_plotter <- function(data){
    ggplot(data, aes(x = date)) +
  # Shaded area for 90% confidence interval
  # geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = "90% CI"), alpha = 0.2) +
  # Shaded area for 50% confidence interval
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
  # Shaded area for 20% confidence interval
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
  # Line for the median
  geom_line(aes(y = median), color = "blue") +
  geom_line(aes(y = true_value), linetype = "dashed", color = "red") +
  geom_vline(xintercept = min(data$date) + 70, linetype = "dashed") +
  # geom_point(aes(y = true_value)) +
  labs(title = "New Reports per Day",
       x = "Date",
       y = "New reports per day",
       fill = "Confidence Interval") +
  scale_fill_manual(values = c("90% CI" = "purple", "50% CI" = "orange", "20% CI" = "green")) +
  theme_minimal() +
  # scale_y_log10() +
  theme(legend.position = "bottom")
}

forecast_plotter(mainerset$SouthAfrica$forecast)

crps_plotter <- function(forecast, scores){
  ggplot(forecast, aes(x = date)) +
  # Shaded area for 90% confidence interval
#   geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = "90% CI"), alpha = 0.2) +
  # Shaded area for 50% confidence interval
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
  # Shaded area for 20% confidence interval
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
  # Line for the median
  geom_line(aes(y = median), color = "blue") +
  geom_line(aes(y = true_value), linetype = "dashed") +
  geom_vline(xintercept = min(data$date) + 70, linetype = "dashed") +
  geom_point(aes(y = true_value)) +
  labs(title = "New Reports per Day",
       x = "Date",
       y = "New reports per day",
       fill = "Confidence Interval") +
  scale_fill_manual(values = c("90% CI" = "purple", "50% CI" = "orange", "20% CI" = "green")) +
  theme_minimal() +
  # scale_y_log10() +
  theme(legend.position = "bottom")
}