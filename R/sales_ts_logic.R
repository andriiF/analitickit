#' Run sales time series analysis logic
#'
#' Runs a higher-level sales time series analysis workflow on selected stores,
#' metadata groups, and time periods.
#'
#' The function filters sales data by store metadata such as city, state,
#' store type, cluster, store number, and date range. It then optionally
#' computes sales metrics using [compute_sales_metrics()] and creates a sales
#' trend plot using [plot_sales_trends()].
#'
#' @param data A named list returned by [load_sales_data()] or
#'   [clean_sales_ts()]. It should contain `train` and `stores` tibbles.
#' @param city Character vector or `NULL`. Optional city filter.
#' @param state Character vector or `NULL`. Optional state filter.
#' @param store_type Character vector or `NULL`. Optional store type filter.
#' @param cluster Integer or numeric vector or `NULL`. Optional cluster filter.
#' @param store_nbr Integer or numeric vector or `NULL`. Optional store number
#'   filter.
#' @param start_date Date or character. Optional start date filter.
#' @param end_date Date or character. Optional end date filter.
#' @param group_vars Character vector or `NULL`. Grouping variables passed to
#'   [compute_sales_metrics()]. Defaults to `c("store_nbr", "family")`.
#' @param plot_group_var Character or `NULL`. Grouping variable passed to
#'   [plot_sales_trends()]. Defaults to `"family"`.
#' @param aggregate_period Character. Time aggregation period passed to
#'   [plot_sales_trends()]. One of `"day"`, `"week"`, or `"month"`.
#'   Defaults to `"month"`.
#' @param moving_average_window Integer. Window size used by metrics and,
#'   optionally, the plot. Defaults to `7`.
#' @param add_moving_average Logical. Should the trend plot include a moving
#'   average line? Defaults to `FALSE`.
#' @param run_metrics Logical. Should sales metrics be computed?
#'   Defaults to `TRUE`.
#' @param run_plot Logical. Should a sales trend plot be created?
#'   Defaults to `TRUE`.
#'
#' @return A named list with:
#' \describe{
#'   \item{filtered_data}{Filtered sales data object with `train`, `stores`,
#'   and optionally `holidays_events`.}
#'   \item{metrics}{Metrics returned by [compute_sales_metrics()], or `NULL`
#'   if `run_metrics = FALSE`.}
#'   \item{plot}{A `ggplot` object returned by [plot_sales_trends()], or `NULL`
#'   if `run_plot = FALSE`.}
#'   \item{filters}{A list describing filters used in the analysis.}
#' }
#'
#' @details
#' This function is intended as a convenience wrapper for repeated analyses
#' on selected subsets of stores and time periods.
#'
#' Filtering by `city`, `state`, `store_type`, and `cluster` requires the
#' `stores` metadata table. The function uses the internal sales schema
#' defined by `sales_schema()`, so column names are resolved dynamically.
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#' clean_data <- clean_sales_ts(sales_data)
#'
#' result <- sales_ts_logic(
#'   clean_data,
#'   city = "Quito",
#'   store_type = "D",
#'   start_date = "2016-01-01",
#'   end_date = "2017-08-15",
#'   group_vars = c("store_nbr", "family"),
#'   plot_group_var = "family",
#'   aggregate_period = "month"
#' )
#'
#' result$metrics
#' result$plot
#' }
#'
#' @export
sales_ts_logic <- function(data,
                           city = NULL,
                           state = NULL,
                           store_type = NULL,
                           cluster = NULL,
                           store_nbr = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           group_vars = c("store_nbr", "family"),
                           plot_group_var = "family",
                           aggregate_period = c("month", "week", "day"),
                           moving_average_window = 7,
                           add_moving_average = FALSE,
                           run_metrics = TRUE,
                           run_plot = TRUE) {
  aggregate_period <- match.arg(aggregate_period)

  if (!is.list(data) || is.null(data$train) || is.null(data$stores)) {
    stop(
      "Input data must be a named list containing `train` and `stores` tables.",
      call. = FALSE
    )
  }

  if (!is.logical(run_metrics) || length(run_metrics) != 1) {
    stop("`run_metrics` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(run_plot) || length(run_plot) != 1) {
    stop("`run_plot` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(add_moving_average) || length(add_moving_average) != 1) {
    stop("`add_moving_average` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(moving_average_window) || length(moving_average_window) != 1) {
    stop("`moving_average_window` must be a single numeric value.", call. = FALSE)
  }

  moving_average_window <- as.integer(moving_average_window)

  if (moving_average_window < 1) {
    stop("`moving_average_window` must be greater than or equal to 1.", call. = FALSE)
  }

  filtered_data <- filter_sales_by_logic(
    data = data,
    city = city,
    state = state,
    store_type = store_type,
    cluster = cluster,
    store_nbr = store_nbr,
    start_date = start_date,
    end_date = end_date
  )

  if (nrow(filtered_data$train) == 0) {
    stop("No sales observations found for selected filters.", call. = FALSE)
  }

  metrics <- NULL
  plot <- NULL

  if (run_metrics) {
    metrics <- compute_sales_metrics(
      data = filtered_data,
      group_vars = group_vars,
      moving_average_window = moving_average_window
    )
  }

  if (run_plot) {
    plot <- plot_sales_trends(
      data = filtered_data,
      group_var = plot_group_var,
      aggregate_period = aggregate_period,
      add_moving_average = add_moving_average,
      moving_average_window = moving_average_window
    )
  }

  list(
    filtered_data = filtered_data,
    metrics = metrics,
    plot = plot,
    filters = list(
      city = city,
      state = state,
      store_type = store_type,
      cluster = cluster,
      store_nbr = store_nbr,
      start_date = start_date,
      end_date = end_date,
      group_vars = group_vars,
      plot_group_var = plot_group_var,
      aggregate_period = aggregate_period
    )
  )
}

filter_sales_by_logic <- function(data,
                                  city = NULL,
                                  state = NULL,
                                  store_type = NULL,
                                  cluster = NULL,
                                  store_nbr = NULL,
                                  start_date = NULL,
                                  end_date = NULL) {
  train_cols <- sales_columns("train")
  stores_cols <- sales_columns("stores")

  date_col <- train_cols[["date"]]
  train_store_col <- train_cols[["store_nbr"]]

  stores_store_col <- stores_cols[["store_nbr"]]
  city_col <- stores_cols[["city"]]
  state_col <- stores_cols[["state"]]
  type_col <- stores_cols[["type"]]
  cluster_col <- stores_cols[["cluster"]]

  train <- data$train
  stores <- data$stores



  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
  }

  stores_filtered <- stores

  if (!is.null(city)) {
    stores_filtered <- stores_filtered |>
      dplyr::filter(.data[[city_col]] %in% .env$city)
  }

  if (!is.null(state)) {
    stores_filtered <- stores_filtered |>
      dplyr::filter(.data[[state_col]] %in% .env$state)
  }

  if (!is.null(store_type)) {
    stores_filtered <- stores_filtered |>
      dplyr::filter(.data[[type_col]] %in% .env$store_type)
  }

  if (!is.null(cluster)) {
    stores_filtered <- stores_filtered |>
      dplyr::filter(.data[[cluster_col]] %in% .env$cluster)
  }

  if (!is.null(store_nbr)) {
    stores_filtered <- stores_filtered |>
      dplyr::filter(.data[[stores_store_col]] %in% .env$store_nbr)
  }

  selected_stores <- unique(stores_filtered[[stores_store_col]])

  train_filtered <- train |>
    dplyr::filter(.data[[train_store_col]] %in% selected_stores)


  if (!is.null(start_date)) {
    train_filtered <- train_filtered |>
      dplyr::filter(.data[[date_col]] >= .env$start_date)
  }

  if (!is.null(end_date)) {
    train_filtered <- train_filtered |>
      dplyr::filter(.data[[date_col]] <= .env$end_date)
  }

  result <- data
  result$train <- train_filtered
  result$stores <- stores_filtered

  result
}

