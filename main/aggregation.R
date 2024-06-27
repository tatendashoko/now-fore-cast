# aggregator <- function(list_of_province_data){
#     for(province in names(list_of_province_data)) {
#         forecast <- list_of_province_data$province$forecast
#     }
# }
library(purrr)
# Function to sum relevant columns
sum_columns_forecast <- function(df1, df2) {
  if ("forecast" %in% names(df1)) df1 <- df1$forecast
  setDT(df1) %>%
    full_join(df2$forecast, by = c("date", "type"), suffix = c("_df1", "_df2")) %>%
    mutate(
      median = coalesce(median_df1, 0) + coalesce(median_df2, 0),
      lower_90 = coalesce(lower_90_df1, 0) + coalesce(lower_90_df2, 0),
      lower_50 = coalesce(lower_50_df1, 0) + coalesce(lower_50_df2, 0),
      lower_20 = coalesce(lower_20_df1, 0) + coalesce(lower_20_df2, 0),
      upper_20 = coalesce(upper_20_df1, 0) + coalesce(upper_20_df2, 0),
      upper_50 = coalesce(upper_50_df1, 0) + coalesce(upper_50_df2, 0),
      upper_90 = coalesce(upper_90_df1, 0) + coalesce(upper_90_df2, 0),
      true_value = coalesce(true_value_df1, 0) + coalesce(true_value_df2, 0)
    ) %>%
    setDT() %>%
    select(date, type, median, lower_90, lower_50, lower_20, upper_20, upper_50, upper_90, true_value)
}
combined_df <- reduce(swet, sum_columns_forecast)
sum_columns_score <- function(df1, df2) {
  if ("scoring" %in% names(df1)) df1 <- df1$scoring
  setDT(df1) %>%
    full_join(df2$scoring, by = c("date", "model", "sample"), suffix = c("_df1", "_df2")) %>%
    mutate(
      prediction = coalesce(prediction_df1, 0) + coalesce(prediction_df2, 0),
      true_value = coalesce(true_value_df1, 0) + coalesce(true_value_df2, 0)
    ) %>%
    setDT() %>%
    select(date, sample, prediction, true_value, model)
}
# Initialize the accumulator with the first data frame

anotherone <- reduce(swet, sum_columns_score)
# accumulator <- mainerset[[1]]
# # Use reduce to apply the sum_columns function across all data frames in the list
# # Loop through the rest of the data frames and accumulate the sums
# for (i in 2:length(mainerset)) {
#   accumulator <- sum_columns(accumulator, mainerset[[i]])
#   print(a)
# }