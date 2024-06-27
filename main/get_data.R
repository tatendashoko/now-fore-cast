## NICD data

province_data_raw <- read_csv(here("./data/province_data.csv")) %>% 
  clean_names()

require(data.table)

province_data <- setDT(province_data_raw)

province_data[
  order(date),
  incidence := c(cumulative_cases[1], diff(cumulative_cases)),
  by = province_id
]
