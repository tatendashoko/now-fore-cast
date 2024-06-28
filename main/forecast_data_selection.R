reported_province_cases <- function(province_data, start_day=1, end_day, type="daily"){
  if(type == "daily"){
    cases <- province_data %>%
        # filter(province == province) %>%
        ungroup() %>%
        slice(start_day:end_day) %>%
        select(date, incidence) %>%
        rename(confirm = incidence)
  } else {
      cases <- province_data %>%
        ungroup() %>%
        slice(start_day:end_day) %>%
        rename(confirm = weekly_incidence) %>%
        select(date, confirm)
  }
  setDT(cases)
  return(cases)
}
