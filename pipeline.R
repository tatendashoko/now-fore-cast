# Load necessary packages
pacman::p_load(here,
               rstan,
               tidyverse,
               janitor,
               EpiNow2,
               data.table,
               scoringutils,
               glue
               )

# Source required scripts
source("init_analysis.R")
source("data_chunks.R")
options(mc.cores=4)

# Define the pipeline function
pipeline <- function(province_name="Eastern Cape", pred_size=60, pred_window=14) {

    # Filter data for the specified province
    data <- province_data_filtered %>%
        filter(province == province_name) 
    
    data_length <- length(data$date)
    slide_slack <- as.integer(data_length - pred_size)

    # Generate the sequence for sliding windows
    slider <- seq(0, slide_slack, by = as.integer(pred_window ))[1:3]
    
    # Initialize an empty list to store predictions
    prediction_list <- list()
    
    # Predict for each sliding window and store predictions in the list
    previous_slide_end <- NULL

    for (x in slider) {
        print(glue(">>>>>>>>>> Slider {x} >>>>>>>>>>>>>>>>>>>>>>>>>>"))

        # Add actual cases to forecast data
        reported_cases <- reported_province_cases(province_name, start_day = x, end_day = as.integer(x + pred_size))
        actual_cases <- reported_province_cases(province_name, start_day = x, end_day = as.integer(x + pred_size + pred_window))

        # Check the slide interval
        if (!is.null(previous_slide_end)) {
            slide_interval <- as.integer(x) - previous_slide_end
            if (slide_interval != pred_window) {
                print(glue("Error: Slide interval is not {pred_window} days"))
                next 
            }
        }
        previous_slide_end <- as.integer(x)

        def <- epinow(reported_cases,
                      generation_time = generation_time_opts(generation_time),
                      delays = delay_opts(delay),
                      rt = rt_opts(prior = rt_prior),
                      horizon = pred_window)

        data <- summary(def, output = "estimated_reported_cases")
        data[, date := as.Date(date)]
        actual_cases[, date := as.Date(date)]

        data[actual_cases, on = "date", actual_cases := i.confirm]

        # Store the result in the list
        prediction_list[[as.character(x)]] <- data
    }
    
    return(prediction_list)
}

# Initialize the script and store the results
provinces <- unique(province_data$province)
province_simulation <- list()

#simulation of each province
for (province in provinces){
print(glue("---------------------------- Simulation for {province}---------------------------------"))
result <- pipeline(province_name = as.character(province) ,pred_size=100, pred_window=20)
province_simulation[[as.character(province)]] <- result
}

