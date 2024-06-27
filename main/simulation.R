path <- getwd()  # Check the current working directory
library("glue")
# Source required scripts
source(glue("{path}/dependencies.R"))
source(glue("{path}/main/get_data.R"))
source(glue("{path}/main/data_organization.R"))
source(glue("{path}/main/model_forcasting.R"))
source(glue("{path}/main/forecast_data_selection.R"))

options(mc.cores=4)

# Define the pipeline function
pipeline <- function(province_data, pred_size=60, pred_window=14, type="daily", no_of_slides=-1) {
    # Filter data for the specified province
   
    data <- province_data
        
    data_length <- length(data$date)
    slide_slack <- as.integer(data_length - pred_size)

    # Generate the sequence for sliding windows
    if ((no_of_slides != -1) & (no_of_slides <= slide_slack)) slider <- seq(0, slide_slack, by=(as.integer(pred_window)))[1:no_of_slides]
    else slider <- seq(0, slide_slack, by=(as.integer(pred_window)))
    
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
        
        # prepend arbitrary value due to NAs
        # new_row <- data.table(date = as.Date(min(reported_cases$date) - 1), confirm = 0)
        # Bind the new row to the existing data frame
        # reported_cases <- rbindlist(list(new_row, reported_cases), use.names = TRUE, fill = TRUE)
        
        actual_cases <- reported_province_cases(province_data, start_day = x, end_day = as.integer(x + pred_size + pred_window), type = "daily")
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
# provinces <- ("Northern Cape")

simulator <- function(province_name, type = "daily", no_of_slides=1) {
    print(glue("---------------------------- Simulation for {province}---------------------------------"))
    if (type == "daily") province_variable <- gsub(" ", "_", province)
    else province_variable <- paste0(gsub(" ", "_", province), "_weekly_incidence")
    dataset <- get(province_variable)
    result <- pipeline(dataset, pred_size=70, pred_window=14, type=type, no_of_slides=no_of_slides)
}

for (province in provinces){
assign(paste0(gsub(" ", "_", province), "_daily_forecast"), 
        simulator(province, type = "daily", no_of_slides=20))
daily_province_simulation[[as.character(province)]] <- get(paste0(gsub(" ", "_", province), "_daily_forecast"))
saveRDS(get("daily_province_simulation", envir = .GlobalEnv), "new_daily_province_simulation.Rds")

# assign(paste0(gsub(" ", "_", province), "_weekly_forecast"), 
#         simulator(province, type = "weekly", no_of_slides=3))
# weekly_province_simulation[[as.character(province)]] <- get(paste0(gsub(" ", "_", province), "_weekly_forecast"))
# saveRDS(get("weekly_province_simulation", envir = .GlobalEnv), "new_weekly_province_simulation.Rds")
}
