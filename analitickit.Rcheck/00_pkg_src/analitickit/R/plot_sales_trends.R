#' Plot sales trends
#'
#' Creates sales trend visualizations for sales time series data used by the
#' `analitickit` package.
#'
#' The function plots sales over time, with optional temporal aggregation,
#' grouping, and moving average smoothing.
#'
#' @param data A named list returned by [load_sales_data()] or
#'   [clean_sales_ts()]. It should contain a `train` tibble.
#' @param group_var Character or `NULL`. Optional grouping variable used to
#'   draw separate trend lines. Common values are `"store_nbr"` or `"family"`.
#'   Defaults to `NULL`.
#' @param aggregate_period Character. Time aggregation period. One of
#'   `"day"`, `"week"`, or `"month"`. Defaults to `"day"`.
#' @param add_moving_average Logical. Should a moving average line be added?
#'   Defaults to `FALSE`.
#' @param moving_average_window Integer. Window size for the moving average.
#'   Defaults to `7`.
#'
#' @return A `ggplot` object.
#'
#' @details
#' The function uses the internal sales schema defined by `sales_schema()`,
#' so column names are resolved dynamically from the package schema.
#'
#' Sales are aggregated by date and, if provided, by `group_var`.
#' When `aggregate_period = "week"` or `"month"`, dates are converted to the
#' beginning of the selected period using [lubridate::floor_date()].
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#' clean_data <- clean_sales_ts(sales_data)
#'
#' plot_sales_trends(clean_data)
#'
#' plot_sales_trends(
#'   clean_data,
#'   group_var = "family",
#'   aggregate_period = "month"
#' )
#'
#' plot_sales_trends(
#'   clean_data,
#'   group_var = "store_nbr",
#'   add_moving_average = TRUE,
#'   moving_average_window = 30
#' )
#' }
#'
#' @export
plot_sales_trends <- function(data,
                              group_var = NULL,
                              aggregate_period = c("day", "week", "month"),
                              add_moving_average = FALSE,
                              moving_average_window = 7) {
  aggregate_period <- match.arg(aggregate_period)

  if (!is.list(data) || is.null(data$train)) {
    stop("Input data must be a named list containing a `train` table.", call. = FALSE)
  }

  if (!is.null(group_var) && length(group_var) != 1) {
    stop("`group_var` must be `NULL` or a single column name.", call. = FALSE)
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

  train <- data$train
  cols <- sales_columns("train")

  date_col <- cols[["date"]]
  sales_col <- cols[["sales"]]

  if (!date_col %in% names(train)) {
    stop("Date column not found in train data: ", date_col, call. = FALSE)
  }

  if (!sales_col %in% names(train)) {
    stop("Sales column not found in train data: ", sales_col, call. = FALSE)
  }

  if (!is.null(group_var) && !group_var %in% names(train)) {
    stop("Unknown grouping variable: ", group_var, call. = FALSE)
  }

  plot_data <- prepare_sales_trend_data(
    train = train,
    date_col = date_col,
    sales_col = sales_col,
    group_var = group_var,
    aggregate_period = aggregate_period,
    add_moving_average = add_moving_average,
    moving_average_window = moving_average_window
  )

  build_sales_trend_plot(
    plot_data = plot_data,
    date_col = date_col,
    group_var = group_var,
    add_moving_average = add_moving_average
  )
}



prepare_sales_trend_data <- function(train,
                                     date_col,
                                     sales_col,
                                     group_var = NULL,
                                     aggregate_period = "day",
                                     add_moving_average = FALSE,
                                     moving_average_window = 7) {
  if (aggregate_period == "day") {
    period_dates <- train[[date_col]]
  } else {
    period_dates <- lubridate::floor_date(
      train[[date_col]],
      unit = aggregate_period
    )
  }

  train <- train |>
    dplyr::mutate(
      "{date_col}" := period_dates
    )

  group_cols <- if (is.null(group_var)) {
    date_col
  } else {
    c(date_col, group_var)
  }

  plot_data <- train |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::summarise(
      sales = sum(.data[[sales_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols))
    )

  if (add_moving_average) {
    if (is.null(group_var)) {
      plot_data <- plot_data |>
        dplyr::arrange(.data[[date_col]]) |>
        dplyr::mutate(
          moving_average = calculate_plot_moving_average(
            .data$sales,
            moving_average_window
          )
        )
    } else {
      plot_data <- plot_data |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::arrange(.data[[date_col]], .by_group = TRUE) |>
        dplyr::mutate(
          moving_average = calculate_plot_moving_average(
            .data$sales,
            moving_average_window
          )
        ) |>
        dplyr::ungroup()
    }
  }

  plot_data
}

calculate_plot_moving_average <- function(x, window) {
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

build_sales_trend_plot <- function(plot_data,
                                   date_col,
                                   group_var = NULL,
                                   add_moving_average = FALSE) {
  if (is.null(group_var)) {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data[[date_col]], y = .data$sales)
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = "Sales trend",
        x = "Date",
        y = "Sales"
      )
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[[date_col]],
        y = .data$sales,
        colour = as.factor(.data[[group_var]]),
        group = .data[[group_var]]
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = "Sales trend",
        x = "Date",
        y = "Sales",
        colour = group_var
      )
  }

  if (add_moving_average) {
    if (is.null(group_var)) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$moving_average),
          linetype = "dashed",
          na.rm = TRUE
        )
    } else {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(
            y = .data$moving_average,
            colour = as.factor(.data[[group_var]]),
            group = .data[[group_var]]
          ),
          linetype = "dashed",
          na.rm = TRUE
        )
    }
  }

  p +
    ggplot2::theme_minimal()
}
