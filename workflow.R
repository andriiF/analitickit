# Full analitickit workflow
# Run from the package root: source("workflow.R")

data_path <- "data"

if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
  devtools::load_all(quiet = TRUE)
} else {
  library(analitickit)
}

# 1. Load data
sales_data <- load_sales_data(data_path)
message("Loaded ", nrow(sales_data$train), " train rows")

# 2. Validate (before cleaning)
validation_before <- validate_sales_ts(sales_data)
message(
  "Validation before cleaning: ",
  if (validation_before$passed) "OK" else "issues found"
)

# 3. Clean data
clean_data <- clean_sales_ts(
  sales_data,
  handle_duplicates = "aggregate",
  fill_missing_dates = TRUE,
  sort = TRUE
)

validation_after <- validate_sales_ts(clean_data)
message(
  "Validation after cleaning: ",
  if (validation_after$passed) "OK" else "issues found"
)

# 4. Business metrics
metrics <- compute_sales_metrics(clean_data)
metrics_stores <- compute_sales_metrics(clean_data, group_vars = "store_nbr")
metrics_families <- compute_sales_metrics(clean_data, group_vars = "family")

message("Total sales: ", metrics$total_sales)

# 5. Visualizations
plot <- plot_sales_trends(clean_data, aggregate_period = "month")

# 6. Management summary
summary <- create_management_summary(clean_data, period = "month")

# 7. Segment analysis
logic_result <- sales_ts_logic(
  clean_data,
  city = "Quito",
  store_type = "D",
  start_date = "2016-01-01",
  end_date = "2017-08-15",
  group_vars = c("store_nbr", "family"),
  plot_group_var = "family",
  aggregate_period = "month"
)

# 8. Forecast (Prophet)
if (requireNamespace("prophet", quietly = TRUE)) {
  prognosis <- create_prognosis(
    clean_data,
    store_nbr = 1,
    family = "AUTOMOTIVE",
    horizon = 30,
    period = "day"
  )
  message("Forecast ready (", nrow(prognosis$forecast), " rows)")
} else {
  message("Package 'prophet' not installed — forecast skipped")
  prognosis <- NULL
}

message("Workflow finished.")
