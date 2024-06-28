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

upscale_results <- scorer(swet$SouthAfrica_national$scoring, mainerset$SouthAfrica_national$scoring, mode="upscale")
downscale_results <- scorer(swet$SouthAfrica_national$scoring, mainerset$SouthAfrica_national$scoring, mode="downscale")

extract_crps <- function(score_df, type){
  actual_df <- score_df %>%
    filter(model == type) %>%
    # mutate(crps = if(type == "weekly") crps/14 else crps/2) %>%
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
