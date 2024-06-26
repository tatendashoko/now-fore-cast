eastern_province_data <- select(Eastern_Cape_weekly_incidence, c("date", "weekly_incidence"))

#packages

pacman::p_load(here,
               rstan,
               tidyverse,
               janitor,
               EpiNow2,
               data.table,
               scoringutils
               )

require(data.table)
library("EpiNow2")
library("rstan")
library("scoringutils")

options(mc.cores = 4)
# converting days to weeks
incubation_period <- LogNormal(mean = 5/7, sd = 1/7, max = 2) 
# generation_time <- LogNormal(mean = 5.2/7, sd = 1.72/7, max = 10/7) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
generation_time <- Gamma(mean = 7.12/7, sd = 1.72/7, max=10/7) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
reporting_delay <- LogNormal(mean = 2/7, sd = 1/7, max = 10/7)

delay <- incubation_period + reporting_delay
rt_prior <- list(mean = 3, sd = 0.1) #assume sd for now


reported_province_cases <- function(df, start_day=1, end_day){
  cases <- df %>%
    ungroup() %>%
    slice(start_day:end_day) %>%
    # select(date, weekly_incidence) %>%
    rename(confirm = weekly_incidence) %>%
    select(date, confirm)
  return(cases)
}

reported_eastern_cape_cases <- reported_province_cases(eastern_province_data, start_day = 14, end_day = 14+60)

obs_stuff <- obs_opts(
  family = c("negbin", "poisson"),
  phi = list(mean = 0, sd = 1),
  weight = 1,
  week_effect = TRUE,
  week_length = 7,
  scale = 1,
  na = "accumulate",
  likelihood = TRUE,
  return_likelihood = FALSE
)

# sampling_stuff <- stan_sampling_opts(
#   cores = getOption("mc.cores", 1L),
#   warmup = 20,
#   samples = 100,
#   chains = 2,
#   control = list(),
#   save_warmup = FALSE,
#   seed = as.integer(runif(1, 1, 1e+08)),
#   future = FALSE,
#   max_execution_time = Inf,
# )


weekly_def <- epinow(reported_eastern_cape_cases,
  generation_time = generation_time_opts(generation_time),
  delays = delay_opts(delay),
  rt = rt_opts(prior = rt_prior),
  obs = obs_stuff,
  horizon = 14,
  # stan = sampling_stuff
)
# plot(weekly_def)
actual_eastern_cape_cases <- reported_province_cases(eastern_province_data, start_day = 14, end_day = 14+60+14)

weekly_data <- summary(weekly_def, output = "estimated_reported_cases")
# data <- summary(def, type = "parameters", params = "infections")

weekly_data[, date := as.Date(date)]
setDT(actual_eastern_cape_cases)
actual_eastern_cape_cases[, date := as.Date(date)]

# Add actual cases to forecast data
weekly_data[actual_eastern_cape_cases, on = "date", actual_cases := i.confirm]
data <- weekly_data
ggplot(data, aes(x = date)) +
  # Shaded area for 90% confidence interval
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = "90% CI"), alpha = 0.2) +
  # Shaded area for 50% confidence interval
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
  # Shaded area for 20% confidence interval
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
  # Line for the median
  geom_line(aes(y = median), color = "blue") +
  geom_line(aes(y = actual_cases), linetype = "dashed") +
  geom_point(aes(y = actual_cases)) +
  geom_vline(xintercept = min(data$date) + 8*7, linetype = "dashed") +
  labs(title = "New Reports per Day",
       x = "Date",
       y = "New reports per day",
       fill = "Confidence Interval") +
  scale_fill_manual(values = c("90% CI" = "purple", "50% CI" = "orange", "20% CI" = "green")) +
  theme_minimal() +
  # scale_y_log10() +
  theme(legend.position = "bottom")

forecast_long <- melt(data, id.vars = c("date", "actual_cases"), 
                    measure.vars = c("median", "lower_20", "lower_50", "lower_90", "upper_20", "upper_50", "upper_90"), 
                    variable.name = "type", value.name = "value")

forecast_long[, quantile := fifelse(type == "median", 0.5,
                            fifelse(type == "lower_90", 0.1,
                            fifelse(type == "lower_50", 0.25,
                            fifelse(type == "lower_20", 0.4,
                            fifelse(type == "upper_20", 0.6,
                            fifelse(type == "upper_50", 0.75,
                            fifelse(type == "upper_90", 0.9, NA_real_)))))))]
forecast_long[, prediction := value]
forecast_long[, true_value := actual_cases]
forecast_long[, model := "weekly"]

# Filter out rows with NA quantile values if any
forecast_long <- forecast_long[!is.na(quantile)]

# Evaluate the forecasts
weekly_scored <- select(forecast_long, c("date", "quantile", "prediction", "true_value", "model"))
total_scored <- rbind(daily_scored, weekly_scored)
scores <- score(total_scored, metrics = NULL)

score(forecast_data)

# Summarize the scores
score_summary <- summarise_scores(scores, by = "model", relative_skill = TRUE, relative_skill_metric = "interval_score")

## Print the score summary
print(score_summary)

pairwise <- pairwise_comparison(scores, by = "model", metric = "interval_score")

plot_pairwise_comparison(pairwise, type = "interval_score") +
  facet_wrap(~model)

interval_score(
  true_values = data$actual_cases,
  lower = data$lower_90,
  upper = data$upper_90,
  interval_range = 90,
  weigh = TRUE
)

plot_wis(scores, x = "model", relative_contributions = TRUE, flip = FALSE)
plot_score_table(scores, y = "model", by = NULL, metrics = NULL)

# Scoring Using CRPS
forecast_data <- select(def[["estimates"]][["samples"]], c("date", "sample", "value"))

forecast_data[actual_eastern_cape_cases, on = "date", actual_cases := i.confirm]
forecast_data[, model := "weekly"]

forecast_data <- forecast_data[sample > 250] %>%
  rename(prediction = value, true_value = actual_cases)

forecaster <- forecast_data %>%
             group_by(date, sample) %>%
             summarize(prediction = mean(prediction, na.rm = TRUE), .groups = 'drop') %>%
          
setDT(forecaster)
forecaster[actual_eastern_cape_cases, on = "date", true_value := i.confirm]
forecaster[, model := "weekly"] 

forecaster <- forecaster %>% filter(true_value != " Invalid Number") %>% select(-rn)

weekly_scored <- score(forecaster)
score_summary <- summarise_scores(weekly_scored)