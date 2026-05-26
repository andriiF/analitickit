pkgname <- "analitickit"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('analitickit')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("clean_sales_ts")
### * clean_sales_ts

flush(stderr()); flush(stdout())

### Name: clean_sales_ts
### Title: Clean sales time series data
### Aliases: clean_sales_ts

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D 
##D clean_data <- clean_sales_ts(
##D   sales_data,
##D   handle_duplicates = "aggregate",
##D   fill_missing_dates = TRUE,
##D   aggregate_period = "month"
##D )
##D 
##D clean_data$train
## End(Not run)




cleanEx()
nameEx("compute_sales_metrics")
### * compute_sales_metrics

flush(stderr()); flush(stdout())

### Name: compute_sales_metrics
### Title: Compute sales business metrics
### Aliases: compute_sales_metrics

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D clean_data <- clean_sales_ts(sales_data)
##D 
##D compute_sales_metrics(clean_data)
##D 
##D compute_sales_metrics(
##D   clean_data,
##D   group_vars = c("store_nbr", "family"),
##D   moving_average_window = 30
##D )
## End(Not run)




cleanEx()
nameEx("create_management_summary")
### * create_management_summary

flush(stderr()); flush(stdout())

### Name: create_management_summary
### Title: Create management sales summary
### Aliases: create_management_summary

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D clean_data <- clean_sales_ts(sales_data)
##D 
##D summary <- create_management_summary(clean_data, period = "month")
##D 
##D summary$overview
##D summary$best_store
##D summary$fastest_growing_category
## End(Not run)




cleanEx()
nameEx("create_prognosis")
### * create_prognosis

flush(stderr()); flush(stdout())

### Name: create_prognosis
### Title: Create sales prognosis using Prophet
### Aliases: create_prognosis

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D clean_data <- clean_sales_ts(sales_data)
##D 
##D prognosis <- create_prognosis(
##D   clean_data,
##D   store_nbr = 1,
##D   family = "AUTOMOTIVE",
##D   horizon = 30,
##D   period = "day"
##D )
##D 
##D prognosis$forecast
## End(Not run)




cleanEx()
nameEx("load_sales_data")
### * load_sales_data

flush(stderr()); flush(stdout())

### Name: load_sales_data
### Title: Load sales data
### Aliases: load_sales_data

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D 
##D sales_data$train
##D sales_data$stores
##D sales_data$holidays_events
## End(Not run)




cleanEx()
nameEx("plot_sales_trends")
### * plot_sales_trends

flush(stderr()); flush(stdout())

### Name: plot_sales_trends
### Title: Plot sales trends
### Aliases: plot_sales_trends

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D clean_data <- clean_sales_ts(sales_data)
##D 
##D plot_sales_trends(clean_data)
##D 
##D plot_sales_trends(
##D   clean_data,
##D   group_var = "family",
##D   aggregate_period = "month"
##D )
##D 
##D plot_sales_trends(
##D   clean_data,
##D   group_var = "store_nbr",
##D   add_moving_average = TRUE,
##D   moving_average_window = 30
##D )
## End(Not run)




cleanEx()
nameEx("sales_ts_logic")
### * sales_ts_logic

flush(stderr()); flush(stdout())

### Name: sales_ts_logic
### Title: Run sales time series analysis logic
### Aliases: sales_ts_logic

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D clean_data <- clean_sales_ts(sales_data)
##D 
##D result <- sales_ts_logic(
##D   clean_data,
##D   city = "Quito",
##D   store_type = "D",
##D   start_date = "2016-01-01",
##D   end_date = "2017-08-15",
##D   group_vars = c("store_nbr", "family"),
##D   plot_group_var = "family",
##D   aggregate_period = "month"
##D )
##D 
##D result$metrics
##D result$plot
## End(Not run)




cleanEx()
nameEx("validate_sales_ts")
### * validate_sales_ts

flush(stderr()); flush(stdout())

### Name: validate_sales_ts
### Title: Validate sales time series data
### Aliases: validate_sales_ts

### ** Examples

## Not run: 
##D sales_data <- load_sales_data("data/")
##D validation <- validate_sales_ts(sales_data)
##D 
##D validation$passed
##D validation$missing_values
##D validation$duplicates
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
