path <- getwd()  # Check the current working directory
library("glue")
# Source required scripts
source(glue("{path}/dependencies.R"))
source(glue("{path}/main/get_data.R"))
source(glue("{path}/main/data_organization.R"))
source(glue("{path}/main/model_forcasting.R"))
source(glue("{path}/main/forecast_data_selection.R"))

options(mc.cores=4)

#' @title The pipeline function
#' 
#' @param province_data a data.frame that has ...
#' 
#' @param pred_size a positive integer; the number of days to use
#' for training
#' 
#' @param pred_window a positive integer; the number of days to forecast
#' 
#' 
pipeline <- function(
  province_data, pred_size=70, pred_window=14, type = c("daily", "weekly"), 
  no_of_slides=-1
) {
  
  type <- match.arg(type)
  
  data_length <- length(province_data$date)
  slide_slack <- as.integer(data_length - pred_size)

  pred_window <- as.integer(pred_window)
    
  if (no_of_slides*pred_window > slide_slack) {
    warning("Requested more slides than possible.")
    no_of_slides <- -1
  }
  
  # Generate the sequence for sliding windows
  slider <- seq(0, slide_slack, by = pred_window)
  if (no_of_slides > 0) slider <- slider[1:no_of_slides]
    
      # Initialize an empty list to store predictions
    prediction_list <- list()
    
    # Predict for each sliding window and store predictions in the list
    previous_slide_end <- NULL

    previous_chains_forecast_data <- NULL
    previous_data <- NULL

    for (x in slider) {
        print(glue(">>>>>>>>>> Slider {x} >>>>>>>>>>>>>>>>>>>>>>>>>>"))

        # Add actual cases to forecast data
        reported_cases <- reported_province_cases(province_data, start_day = x, end_day = as.integer(x + pred_size), type = type)
        reported_cases[, date := as.Date(date)]
        
        first_non_na_index <- which(!is.na(reported_cases$confirm))[1]
        # Check if the first non-NA value is 0
        if (reported_cases$confirm[first_non_na_index] == 0) {
          # Remove rows before the first non-NA value
          reported_cases <- reported_cases[(first_non_na_index+1):nrow(reported_cases), ]
        }
        # prepend arbitrary value due to NAs
        # new_row <- data.table(date = as.Date(min(reported_cases$date) - 1), confirm = 0)
        # Bind the new row to the existing data frame
        # reported_cases <- rbindlist(list(new_row, reported_cases), use.names = TRUE, fill = TRUE)
        actual_cases <- reported_province_cases(province_data, start_day = x+pred_size+1, end_day = as.integer(x + pred_size + pred_window), type = "daily")
        actual_cases[, date := as.Date(date)]

        # Check the slide interval
        if (!is.null(previous_slide_end)) {
            slide_interval <- as.integer(x) - previous_slide_end
            if (slide_interval != pred_window) {
                print(glue("Error: Slide interval is not {pred_window} days"))
                next 
            }
        }
        previous_slide_end <- as.integer(x)

        def <- model(reported_cases, generation_time, delay, rt_prior, pred_window)
        # est_dt <- def$estimated_reported_cases$samples[sample > 250][date > max(date)-pred_window]
        # setnames(est_dt, "cases", "prediction")
        # est_dt[actual_cases, on = .(date), true_value := confirm]
        
        data <- summary(def, output = "estimated_reported_cases")
        data[, date := as.Date(date)]
        data[actual_cases, on = "date", true_value := i.confirm]

        if (x == 0) previous_data <- data
        last_date_previous <- max(previous_data$date)
        previous_data <- rbind(previous_data, data[date > last_date_previous])
        
        forecast_data <- select(def[["estimates"]][["samples"]], c("date", "sample", "value"))
        forecast_data[, date := as.Date(date)]
        forecast_data <- forecast_data[sample > 250] %>%
            rename(prediction = value)

        combined_chains_forecast_data <- forecast_data %>%
                    group_by(date, sample) %>%
                    summarize(prediction = mean(prediction, na.rm = TRUE), .groups = 'drop')
                
        setDT(combined_chains_forecast_data)
        combined_chains_forecast_data[actual_cases, on = "date", true_value := i.confirm]
        combined_chains_forecast_data[, model := type] 

        combined_chains_forecast_data <- combined_chains_forecast_data %>% filter(true_value != "Invalid Number")
        
        if (x == 0) previous_chains_forecast_data <- combined_chains_forecast_data

        # Find the last date in the previous
        last_date_previous <- max(previous_chains_forecast_data$date)

        # Filter the first data frame to include only rows after the last date in the second data frame
        previous_chains_forecast_data <- rbind(previous_chains_forecast_data, combined_chains_forecast_data[date > last_date_previous])
        
    }
    # Store the result in the list
    prediction_list <- list(scoring = previous_chains_forecast_data, forecast = previous_data)
    return(prediction_list)
}

# Initialize the script and store the results
daily_province_simulation <- list()
weekly_province_simulation <- list()
#simulation of each province
provinces <- c("Limpopo", "North West", "Mpumalanga", "Free State")


simulator <- function(province_name, type = "daily", no_of_slides=1) {
    print(glue("---------------------------- Simulation for {province}---------------------------------"))
    if (type == "daily") province_variable <- gsub(" ", "_", province)
    else province_variable <- paste0(gsub(" ", "_", province), "_weekly_incidence")
    dataset <- get(province_variable)
    result <- pipeline(dataset, pred_size=70, pred_window=14, type=type, no_of_slides=no_of_slides)
}

# for (province in provinces){
# # assign(paste0(gsub(" ", "_", province), "_daily_forecast"), 
# #         simulator(province, type = "daily", no_of_slides=20))
# # daily_province_simulation[[as.character(province)]] <- get(paste0(gsub(" ", "_", province), "_daily_forecast"))
# # saveRDS(get("daily_province_simulation", envir = .GlobalEnv), "new_daily_province_simulation.Rds")

# assign(paste0(gsub(" ", "_", province), "_weekly_forecast"), 
#         simulator(province, type = "weekly", no_of_slides=20))
# weekly_province_simulation[[as.character(province)]] <- get(paste0(gsub(" ", "_", province), "_weekly_forecast"))
# saveRDS(get("weekly_province_simulation", envir = .GlobalEnv), "new_weekly_province_simulation.Rds")
# }

#national daily simulation 
national_daily_simulation <- pipeline(national_data, pred_size=70, pred_window=14, type="daily", no_of_slides=20)
saveRDS(get("national_daily_simulation", envir = .GlobalEnv), "new_daily_national_simulation.Rds")

#national weekly simulation
# national_simulation <- pipeline(weekly_national_data, pred_size=70, pred_window=14, type="weekly", no_of_slides=20)
# saveRDS(get("national_simulation", envir = .GlobalEnv), "new_weekly_national_simulation.Rds")
