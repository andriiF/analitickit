#' Create sales prognosis using Prophet
#'
#' Creates sales forecasts based on historical sales trends using the
#' `prophet` package.
#'
#' The function aggregates sales time series data by date and optionally filters
#' the input data by store number and product family before fitting a Prophet
#' model.
#'
#' @param data A named list returned by [load_sales_data()] or
#'   [clean_sales_ts()]. It should contain a `train` tibble.
#' @param store_nbr Integer or numeric vector, or `NULL`. Optional store number
#'   filter.
#' @param family Character vector, or `NULL`. Optional product family filter.
#' @param horizon Integer. Number of future periods to forecast.
#'   Defaults to `30`.
#' @param period Character. Forecast period frequency. One of `"day"`,
#'   `"week"`, or `"month"`. Defaults to `"day"`.
#' @param include_history Logical. Should historical dates be included in the
#'   returned forecast? Defaults to `TRUE`.
#' @param yearly_seasonality Logical or character. Passed to
#'   [prophet::prophet()]. Defaults to `"auto"`.
#' @param weekly_seasonality Logical or character. Passed to
#'   [prophet::prophet()]. Defaults to `"auto"`.
#' @param daily_seasonality Logical or character. Passed to
#'   [prophet::prophet()]. Defaults to `"auto"`.
#'
#' @return A named list with:
#' \describe{
#'   \item{model}{A fitted Prophet model object.}
#'   \item{forecast}{A tibble with forecast results, including `ds`, `yhat`,
#'   `yhat_lower`, and `yhat_upper`.}
#'   \item{history}{A tibble used to fit the model, with columns `ds` and `y`.}
#'   \item{filters}{A list describing filters and forecast settings.}
#' }
#'
#' @details
#' Prophet expects a data frame with two columns: `ds` for dates and `y` for
#' the numeric target variable. This function converts the package sales data
#' into that format internally.
#'
#' Sales are aggregated to the selected `period` before model fitting.
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#' clean_data <- clean_sales_ts(sales_data)
#'
#' prognosis <- create_prognosis(
#'   clean_data,
#'   store_nbr = 1,
#'   family = "AUTOMOTIVE",
#'   horizon = 30,
#'   period = "day"
#' )
#'
#' prognosis$forecast
#' }
#'
#' @export
create_prognosis <- function(data,
                             store_nbr = NULL,
                             family = NULL,
                             horizon = 30,
                             period = c("day", "week", "month"),
                             include_history = TRUE,
                             yearly_seasonality = "auto",
                             weekly_seasonality = "auto",
                             daily_seasonality = "auto") {
  period <- match.arg(period)

  if (!requireNamespace("prophet", quietly = TRUE)) {
    stop(
      "Package `prophet` is required for create_prognosis(). ",
      "Install it with install.packages('prophet').",
      call. = FALSE
    )
  }

  if (!is.list(data) || is.null(data$train)) {
    stop("Input data must be a named list containing a `train` table.", call. = FALSE)
  }

  if (!is.numeric(horizon) || length(horizon) != 1) {
    stop("`horizon` must be a single numeric value.", call. = FALSE)
  }

  horizon <- as.integer(horizon)

  if (horizon < 1) {
    stop("`horizon` must be greater than or equal to 1.", call. = FALSE)
  }

  if (!is.logical(include_history) || length(include_history) != 1) {
    stop("`include_history` must be TRUE or FALSE.", call. = FALSE)
  }

  history <- prepare_prophet_history(
    data = data,
    store_nbr = store_nbr,
    family = family,
    period = period
  )

  if (nrow(history) < 2) {
    stop(
      "Not enough observations to fit a Prophet model. ",
      "At least 2 observations are required after filtering.",
      call. = FALSE
    )
  }

  model <- prophet::prophet(
    df = history,
    yearly.seasonality = yearly_seasonality,
    weekly.seasonality = weekly_seasonality,
    daily.seasonality = daily_seasonality
  )

  frequency <- prognosis_frequency(period)

  future <- prophet::make_future_dataframe(
    m = model,
    periods = horizon,
    freq = frequency,
    include_history = include_history
  )

  forecast <- stats::predict(model, future)

  forecast_tbl <- tibble::as_tibble(forecast)

  list(
    model = model,
    forecast = forecast_tbl,
    history = history,
    filters = list(
      store_nbr = store_nbr,
      family = family,
      horizon = horizon,
      period = period,
      include_history = include_history
    )
  )
}


prepare_prophet_history <- function(data,
                                    store_nbr = NULL,
                                    family = NULL,
                                    period = c("day", "week", "month")) {
  period <- match.arg(period)

  train_cols <- sales_columns("train")

  date_col <- train_cols[["date"]]
  store_col <- train_cols[["store_nbr"]]
  family_col <- train_cols[["family"]]
  sales_col <- train_cols[["sales"]]

  train <- data$train

  if (!is.null(store_nbr)) {
    train <- train |>
      dplyr::filter(.data[[store_col]] %in% .env$store_nbr)
  }

  if (!is.null(family)) {
    train <- train |>
      dplyr::filter(.data[[family_col]] %in% .env$family)
  }

  if (period == "day") {
    period_dates <- train[[date_col]]
  } else {
    period_dates <- lubridate::floor_date(train[[date_col]], unit = period)
  }

  history <- train |>
    dplyr::mutate(.period_date = period_dates) |>
    dplyr::group_by(.data$.period_date) |>
    dplyr::summarise(
      y = sum(.data[[sales_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      ds = as.Date(.data$.period_date),
      y = as.numeric(.data$y)
    ) |>
    dplyr::arrange(.data$ds)

  history
}
prognosis_frequency <- function(period = c("day", "week", "month")) {
  period <- match.arg(period)

  switch(
    period,
    day = "day",
    week = "week",
    month = "month"
  )
}
