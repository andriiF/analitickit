#' Clean sales time series data
#'
#' Cleans sales time series data used by the `analitickit` package.
#'
#' The function handles missing values, duplicated time series observations,
#' missing dates, optional sorting, and optional temporal aggregation.
#'
#' @param data A named list returned by [load_sales_data()]. It should contain
#'   three tibbles: `train`, `stores`, and `holidays_events`.
#' @param handle_duplicates Character. Method for handling duplicated
#'   observations for the same `date`, `store_nbr`, and `family`.
#'   One of `"aggregate"`, `"keep_first"`, or `"error"`.
#' @param fill_missing_dates Logical. Should missing dates be completed for
#'   each `store_nbr` and `family` time series? Defaults to `TRUE`.
#' @param fill_sales Numeric value used to replace missing sales values.
#'   Defaults to `0`.
#' @param fill_onpromotion Integer or numeric value used to replace missing
#'   promotion values. Defaults to `0`.
#' @param drop_missing_keys Logical. Should rows with missing key columns
#'   be removed? Key columns are `date`, `store_nbr`, and `family`.
#'   Defaults to `TRUE`.
#' @param sort Logical. Should the cleaned data be sorted by date, store,
#'   and product family? Defaults to `TRUE`.
#' @param aggregate_period Character. Optional temporal aggregation period.
#'   One of `"none"`, `"day"`, `"week"`, or `"month"`.
#'
#' @return A named list with the same structure as the input data, but with
#'   cleaned `train` data.
#'
#' @details
#' When `handle_duplicates = "aggregate"`, duplicated rows are aggregated by
#' summing `sales` and `onpromotion`.
#'
#' When `fill_missing_dates = TRUE`, the function completes missing daily dates
#' for each `store_nbr` and `family` combination. Missing `sales` and
#' `onpromotion` values are filled using `fill_sales` and `fill_onpromotion`.
#'
#' When `aggregate_period` is `"week"` or `"month"`, dates are converted to the
#' beginning of the selected period and sales are aggregated.
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#'
#' clean_data <- clean_sales_ts(
#'   sales_data,
#'   handle_duplicates = "aggregate",
#'   fill_missing_dates = TRUE,
#'   aggregate_period = "month"
#' )
#'
#' clean_data$train
#' }
#'
#' @export
clean_sales_ts <- function(data,
                           handle_duplicates = c("aggregate", "keep_first", "error"),
                           fill_missing_dates = TRUE,
                           fill_sales = 0,
                           fill_onpromotion = 0,
                           drop_missing_keys = TRUE,
                           sort = TRUE,
                           aggregate_period = c("none", "day", "week", "month")) {
  handle_duplicates <- match.arg(handle_duplicates)
  aggregate_period <- match.arg(aggregate_period)

  schema <- sales_schema()
  required_tables <- names(schema)

  structure_check <- check_required_tables(data, required_tables)

  if (!structure_check$passed) {
    stop(
      "Input data must contain tables: ",
      paste(required_tables, collapse = ", "),
      call. = FALSE
    )
  }

  train <- data$train
  cols <- sales_columns("train")

  train <- clean_missing_key_rows(
    train = train,
    drop_missing_keys = drop_missing_keys
  )

  train <- fill_sales_missing_values(
    train = train,
    fill_sales = fill_sales,
    fill_onpromotion = fill_onpromotion
  )

  train <- handle_train_duplicates(
    train = train,
    method = handle_duplicates
  )

  if (fill_missing_dates) {
    train <- complete_sales_dates(
      train = train,
      fill_sales = fill_sales,
      fill_onpromotion = fill_onpromotion
    )
  }

  if (aggregate_period != "none") {
    train <- aggregate_sales_period(
      train = train,
      period = aggregate_period
    )
  }

  if (sort) {
    date_col <- cols[["date"]]
    store_col <- cols[["store_nbr"]]
    family_col <- cols[["family"]]

    train <- train |>
      dplyr::arrange(
        .data[[date_col]],
        .data[[store_col]],
        .data[[family_col]]
      )
  }

  data$train <- train

  data
}


clean_missing_key_rows <- function(train, drop_missing_keys = TRUE) {
  if (!drop_missing_keys) {
    return(train)
  }

  cols <- sales_columns("train")

  date_col <- cols[["date"]]
  store_col <- cols[["store_nbr"]]
  family_col <- cols[["family"]]

  train |>
    dplyr::filter(
      !is.na(.data[[date_col]]),
      !is.na(.data[[store_col]]),
      !is.na(.data[[family_col]])
    )
}


fill_sales_missing_values <- function(train,
                                      fill_sales = 0,
                                      fill_onpromotion = 0) {
  cols <- sales_columns("train")

  sales_col <- cols[["sales"]]
  promo_col <- cols[["onpromotion"]]

  train |>
    dplyr::mutate(
      "{sales_col}" := tidyr::replace_na(.data[[sales_col]], fill_sales),
      "{promo_col}" := tidyr::replace_na(.data[[promo_col]], fill_onpromotion)
    )
}

handle_train_duplicates <- function(train,
                                    method = c("aggregate", "keep_first", "error")) {
  method <- match.arg(method)

  cols <- sales_columns("train")

  id_col <- cols[["id"]]
  date_col <- cols[["date"]]
  store_col <- cols[["store_nbr"]]
  family_col <- cols[["family"]]
  sales_col <- cols[["sales"]]
  promo_col <- cols[["onpromotion"]]

  duplicates <- train |>
    dplyr::count(
      .data[[date_col]],
      .data[[store_col]],
      .data[[family_col]],
      name = "n"
    ) |>
    dplyr::filter(.data$n > 1)

  if (nrow(duplicates) > 0 && method == "error") {
    stop(
      "Duplicated observations found for date, store and family. ",
      "Use handle_duplicates = 'aggregate' or 'keep_first'.",
      call. = FALSE
    )
  }

  if (method == "keep_first") {
    return(
      train |>
        dplyr::arrange(.data[[date_col]], .data[[store_col]], .data[[family_col]]) |>
        dplyr::distinct(
          .data[[date_col]],
          .data[[store_col]],
          .data[[family_col]],
          .keep_all = TRUE
        )
    )
  }

  train |>
    dplyr::group_by(
      .data[[date_col]],
      .data[[store_col]],
      .data[[family_col]]
    ) |>
    dplyr::summarise(
      "{id_col}" := dplyr::first(.data[[id_col]]),
      "{sales_col}" := sum(.data[[sales_col]], na.rm = TRUE),
      "{promo_col}" := sum(.data[[promo_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        id_col,
        date_col,
        store_col,
        family_col,
        sales_col,
        promo_col
      ))
    )
}

complete_sales_dates <- function(train,
                                 fill_sales = 0,
                                 fill_onpromotion = 0) {
  cols <- sales_columns("train")

  id_col <- cols[["id"]]
  date_col <- cols[["date"]]
  store_col <- cols[["store_nbr"]]
  family_col <- cols[["family"]]
  sales_col <- cols[["sales"]]
  promo_col <- cols[["onpromotion"]]

  date_grid <- train |>
    dplyr::group_by(
      .data[[store_col]],
      .data[[family_col]]
    ) |>
    dplyr::summarise(
      min_date = min(.data[[date_col]], na.rm = TRUE),
      max_date = max(.data[[date_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      "{date_col}" := list(seq(.data$min_date, .data$max_date, by = "day"))
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = dplyr::all_of(date_col)) |>
    dplyr::select(
      .data[[store_col]],
      .data[[family_col]],
      .data[[date_col]]
    )

  completed <- date_grid |>
    dplyr::left_join(
      train,
      by = c(store_col, family_col, date_col)
    ) |>
    dplyr::mutate(
      "{sales_col}" := tidyr::replace_na(.data[[sales_col]], fill_sales),
      "{promo_col}" := tidyr::replace_na(.data[[promo_col]], fill_onpromotion)
    )

  if (id_col %in% names(completed)) {
    completed <- completed |>
      dplyr::mutate(
        "{id_col}" := dplyr::row_number() - 1L
      )
  }

  completed |>
    dplyr::select(
      .data[[id_col]],
      .data[[date_col]],
      .data[[store_col]],
      .data[[family_col]],
      .data[[sales_col]],
      .data[[promo_col]]
    )
}
aggregate_sales_period <- function(train,
                                   period = c("day", "week", "month")) {
  period <- match.arg(period)

  cols <- sales_columns("train")

  id_col <- cols[["id"]]
  date_col <- cols[["date"]]
  store_col <- cols[["store_nbr"]]
  family_col <- cols[["family"]]
  sales_col <- cols[["sales"]]
  promo_col <- cols[["onpromotion"]]

  if (period == "day") {
    period_dates <- train[[date_col]]
  } else {
    period_dates <- lubridate::floor_date(train[[date_col]], unit = period)
  }

  aggregated <- train |>
    dplyr::mutate(
      "{date_col}" := period_dates
    ) |>
    dplyr::group_by(
      .data[[date_col]],
      .data[[store_col]],
      .data[[family_col]]
    ) |>
    dplyr::summarise(
      "{sales_col}" := sum(.data[[sales_col]], na.rm = TRUE),
      "{promo_col}" := sum(.data[[promo_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      "{id_col}" := dplyr::row_number() - 1L
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        id_col,
        date_col,
        store_col,
        family_col,
        sales_col,
        promo_col
      ))
    )

  aggregated
}
