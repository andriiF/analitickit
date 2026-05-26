Instrukcja:

PLiki: 
  train.csv
  stores.csv
  holidays_events.csv

Lokalizacja plików: "data"

WORKFLOW:
sales_data <- load_sales_data("data/")

validation_before <- validate_sales_ts(sales_data)

clean_data <- clean_sales_ts(sales_data)

validation_after <- validate_sales_ts(clean_data)

metrics <- compute_sales_metrics(clean_data)

plot <- plot_sales_trends(clean_data)

summary <- create_management_summary(clean_data)

logic_result <- sales_ts_logic(
  clean_data,
  city = "Quito",
  store_type = "D",
  start_date = "2016-01-01",
  end_date = "2017-08-15"
)

prognosis <- create_prognosis(
  clean_data,
  store_nbr = 1,
  family = "AUTOMOTIVE",
  horizon = 30,
  period = "day"
)
