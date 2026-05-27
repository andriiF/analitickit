#' Compute sales business metrics
#'
#' Computes business metrics for sales time series data used by the
#' `analitickit` package.
#'
#' The function calculates total sales, average sales, sales variability,
#' promotion-related metrics, moving average summaries, peak statistics,
#' and growth metrics. Metrics can be calculated globally or by selected
#' grouping variables such as store number or product family.
#'
#' @param data A named list returned by [load_sales_data()] or
#'   [clean_sales_ts()]. It should contain a `train` tibble.
#' @param group_vars Character vector. Optional grouping variables used to
#'   calculate metrics by group. Defaults to `NULL`, which calculates global
#'   metrics for the whole dataset.
#' @param moving_average_window Integer. Window size used to calculate moving
#'   average metrics. Defaults to `7`.
#'
#' @return A tibble with computed business metrics.
#'
#' @details
#' The function uses the internal sales schema defined by `sales_schema()`,
#' so column names are resolved dynamically from the package schema.
#'
#' Peak-related metrics are calculated using local maxima in the sales time
#' series. A peak is defined as a point where sales are greater than the
#' previous and next observations within the same group.
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#' clean_data <- clean_sales_ts(sales_data)
#'
#' compute_sales_metrics(clean_data)
#'
#' compute_sales_metrics(
#'   clean_data,
#'   group_vars = c("store_nbr", "family"),
#'   moving_average_window = 30
#' )
#' }
#'
#' @export
compute_sales_metrics <- function(data,
                                  group_vars = NULL,
                                  moving_average_window = 7) {
  if (!is.list(data) || is.null(data$train)) {
    stop("Input data must be a named list containing a `train` table.", call. = FALSE)
  }

  if (!is.numeric(moving_average_window) || length(moving_average_window) != 1) {
    stop("`moving_average_window` must be a single numeric value.", call. = FALSE)
  }

  moving_average_window <- as.integer(moving_average_window)

  if (moving_average_window < 1) {
    stop("`moving_average_window` must be greater than or equal to 1.", call. = FALSE)
  }

  train <- data$train
  cols <- sales_columns("train")

  date_col <- cols[["date"]]
  sales_col <- cols[["sales"]]
  promo_col <- cols[["onpromotion"]]

  allowed_group_vars <- unname(cols)
  if (!is.null(group_vars)) {
    invalid_group_vars <- setdiff(group_vars, names(train))

    if (length(invalid_group_vars) > 0) {
      stop(
        "Unknown grouping variable(s): ",
        paste(invalid_group_vars, collapse = ", "),
        call. = FALSE
      )
    }
  }

  train <- train |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(c(group_vars, date_col)))
    )

  train_with_features <- add_metric_features(
    train = train,
    group_vars = group_vars,
    date_col = date_col,
    sales_col = sales_col,
    promo_col = promo_col,
    moving_average_window = moving_average_window
  )

  summarise_sales_metrics(
    train = train_with_features,
    group_vars = group_vars,
    date_col = date_col,
    sales_col = sales_col,
    promo_col = promo_col
  )
}


add_metric_features <- function(train,
                                group_vars,
                                date_col,
                                sales_col,
                                promo_col,
                                moving_average_window) {
  if (is.null(group_vars)) {
    train <- train |>
      dplyr::mutate(.metric_group = "all")

    group_vars <- ".metric_group"
  }

  train |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::arrange(.data[[date_col]], .by_group = TRUE) |>
    dplyr::mutate(
      sales_lag = dplyr::lag(.data[[sales_col]]),
      sales_lead = dplyr::lead(.data[[sales_col]]),
      is_peak = dplyr::if_else(
        !is.na(.data$sales_lag) &
          !is.na(.data$sales_lead) &
          .data[[sales_col]] > .data$sales_lag &
          .data[[sales_col]] > .data$sales_lead,
        TRUE,
        FALSE
      ),
      moving_average = calculate_moving_average(
        .data[[sales_col]],
        moving_average_window
      ),
      is_promoted = .data[[promo_col]] > 0
    ) |>
    dplyr::ungroup()
}

summarise_sales_metrics <- function(train,
                                    group_vars,
                                    date_col,
                                    sales_col,
                                    promo_col) {
  internal_group <- FALSE

  if (is.null(group_vars)) {
    group_vars <- ".metric_group"
    internal_group <- TRUE
  }

  result <- train |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::summarise(
      start_date = min(.data[[date_col]], na.rm = TRUE),
      end_date = max(.data[[date_col]], na.rm = TRUE),
      observations = dplyr::n(),
      active_sales_days = sum(.data[[sales_col]] > 0, na.rm = TRUE),

      total_sales = sum(.data[[sales_col]], na.rm = TRUE),
      average_sales = mean(.data[[sales_col]], na.rm = TRUE),
      median_sales = stats::median(.data[[sales_col]], na.rm = TRUE),
      min_sales = min(.data[[sales_col]], na.rm = TRUE),
      max_sales = max(.data[[sales_col]], na.rm = TRUE),

      sales_sd = stats::sd(.data[[sales_col]], na.rm = TRUE),
      sales_variance = stats::var(.data[[sales_col]], na.rm = TRUE),
      sales_cv = dplyr::if_else(
        average_sales == 0,
        NA_real_,
        sales_sd / average_sales
      ),

      promoted_observations = sum(.data$is_promoted, na.rm = TRUE),
      promotion_share = mean(.data$is_promoted, na.rm = TRUE),
      total_onpromotion = sum(.data[[promo_col]], na.rm = TRUE),
      promoted_sales = sum(.data[[sales_col]][.data$is_promoted], na.rm = TRUE),
      promoted_sales_share = dplyr::if_else(
        total_sales == 0,
        NA_real_,
        promoted_sales / total_sales
      ),

      moving_average_last  = last_non_missing(.data$moving_average),
      moving_average_mean = mean(.data$moving_average, na.rm = TRUE),

      peak_count = sum(.data$is_peak, na.rm = TRUE),
      avg_days_between_peaks = calculate_avg_days_between_peaks(
        dates = .data[[date_col]],
        is_peak = .data$is_peak
      ),

      first_sales = dplyr::first(.data[[sales_col]]),
      last_sales = dplyr::last(.data[[sales_col]]),
      absolute_growth = last_sales - first_sales,
      percent_growth = dplyr::if_else(
        first_sales == 0,
        NA_real_,
        absolute_growth / first_sales
      ),
      .groups = "drop"
    )

  if (internal_group) {
    result <- result |>
      dplyr::select(-dplyr::all_of(".metric_group"))
  }

  result
}

last_non_missing <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  dplyr::last(x)
}

calculate_moving_average <- function(x, window) {
  if (length(x) < window) {
    return(rep(NA_real_, length(x)))
  }

  as.numeric(
    stats::filter(
      x,
      rep(1 / window, window),
      sides = 1
    )
  )
}
calculate_avg_days_between_peaks <- function(dates, is_peak) {
  peak_days <- as.integer(as.Date(dates[is_peak %in% TRUE]))

  if (length(peak_days) < 2L) {
    return(NA_real_)
  }

  gaps <- diff(peak_days)

  if (length(gaps) == 0L) {
    return(NA_real_)
  }

  # Integer day gaps avoid lubridate duration/difftime in dplyr summarise.
  as.double(mean(gaps))
}
