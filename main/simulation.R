path <- getwd()  # Check the current working directory

# Source required scripts
source(glue("{path}/dependencies.R"))
source(glue("{path}/main/get_data.R"))
source(glue("{path}/main/data_organization.R"))
source(glue("{path}/main/model_forcasting.R"))

options(mc.cores=4)

# Define the pipeline function
pipeline <- function(province_data, pred_size=60, pred_window=14, type= "daily") {
    # Filter data for the specified province
   
    data <- province_data
        
    data_length <- length(data$date)
    slide_slack <- as.integer(data_length - pred_size)

    # Generate the sequence for sliding windows
    slider <- seq(0, slide_slack, by=(as.integer(pred_window)))[1:3]
    
    # Initialize an empty list to store predictions
    prediction_list <- list()
    
    # Predict for each sliding window and store predictions in the list
    previous_slide_end <- NULL

    for (x in slider) {
        print(glue(">>>>>>>>>> Slider {x} >>>>>>>>>>>>>>>>>>>>>>>>>>"))

        # Add actual cases to forecast data
        reported_cases <- reported_province_cases(province_data, start_day = x, end_day = as.integer(x + pred_size), type = type)
        actual_cases <- reported_province_cases(province_data, start_day = x, end_day = as.integer(x + pred_size + pred_window), type = "daily")

        # Check the slide interval
        if (!is.null(previous_slide_end)) {
            slide_interval <- as.integer(x) - previous_slide_end
            if (slide_interval != pred_window) {
                print(glue("Error: Slide interval is not {pred_window} days"))
                next 
            }
        }
        previous_slide_end <- as.integer(x)

        # def <- epinow(reported_cases,
        #               generation_time = generation_time_opts(generation_time),
        #               delays = delay_opts(delay),
        #               rt = rt_opts(prior = rt_prior),
        #               horizon = pred_window)
        if (type == "daily") def <- model(reported_cases, daily_generation_time, daily_delay, rt_prior, pred_window)
        else def <- model(reported_cases, weekly_generation_time, weekly_delay, rt_prior, pred_window)

        data <- summary(def, output = "estimated_reported_cases")
        data[, date := as.Date(date)]
        setDT(actual_cases)
        actual_cases[, date := as.Date(date)]

        data[actual_cases, on = "date", actual_cases := i.confirm]

        # Store the result in the list
        prediction_list[[as.character(x)]] <- data
    }
    
    return(prediction_list)
}

# Initialize the script and store the results
province_simulation <- list()
weekly_province_simulation <- list()
#simulation of each province
for (province in provinces){
print(glue("---------------------------- Simulation for {province}---------------------------------"))
# result <- pipeline(get(gsub(" ", "_", province)), pred_size=100, pred_window=20)
# province_simulation[[as.character(province)]] <- result

# Weekly simulations
province_variable <- paste0(gsub(" ", "_", province), "_weekly_incidence")
weekly_dataset <- get(province_variable)
weekly_result <- pipeline(weekly_dataset, pred_size=100, pred_window=20, type="weekly")
weekly_province_simulation[[as.character(province)]] <- weekly_result
}
