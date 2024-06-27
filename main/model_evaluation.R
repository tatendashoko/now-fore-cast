# Scoring Using CRPS
downscaler <- function(forecast_data) {
    forecast_data <- forecast_data %>%
    mutate(week = floor_date(as.Date(date), "week") + days(6)) %>%
    group_by(week, sample) %>%
    mutate(prediction = ifelse(date == max(date), sum(prediction, na.rm = TRUE), NA)) %>%
    mutate(true_value = ifelse(date == max(date), sum(true_value, na.rm = TRUE), NA)) %>%
    filter(true_value != "Invalid Number") %>%
    ungroup() %>%
    select(-week)
}

scorer <- function(weekly_forecasts, daily_forecasts, mode="downscale"){

if (mode == "downscale") {
    weekly_forecasts <- downscaler(weekly_forecasts)
    daily_forecasts <- downscaler(daily_forecasts)
}

total_forecast <- rbind(weekly_forecasts, daily_forecasts)

total_scored <- score(total_forecast, by="model")
# score_summary <- summarise_scores(total_scored)

pairwise <- pairwise_comparison(total_scored, by = "model", metric = "crps")

plot_pairwise_comparison(pairwise, type = "mean_scores_ratio") +
  facet_wrap(~model)

return(list(pairwise = pairwise, total_scored = total_scored))
}

results <- scorer(Eastern_Cape_daily_forecast$scoring, Eastern_Cape_weekly_forecast$scoring, mode="upscale")
b <- results$pairwise
c <- results$total_scored