Instrukcja:

PLiki: 
  train.csv
  stores.csv
  holidays_events.csv

Lokalizacja plików: "data"

sales_data <- load_sales_data("data/")
validation <- validate_sales_ts(sales_data)

metrics <- sales_data |>
  clean_sales_ts() |>
  compute_sales_metrics()
