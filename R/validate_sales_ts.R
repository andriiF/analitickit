#' Validate sales time series data
#'
#' Validates the quality and consistency of sales time series data used by
#' the `analitickit` package.
#'
#' The function checks the structure of the loaded dataset, required columns,
#' missing values, duplicates, date consistency, value ranges, store metadata
#' consistency, and time series frequency issues.
#'
#' @param data A named list returned by [load_sales_data()]. It should contain
#'   three tibbles: `train`, `stores`, and `holidays_events`.
#'
#' @return A named list with validation results:
#' \describe{
#'   \item{passed}{Logical value. `TRUE` if no critical issues were found.}
#'   \item{structure}{Information about required tables.}
#'   \item{columns}{Information about required columns.}
#'   \item{missing_values}{Number of missing values by table and column.}
#'   \item{duplicates}{Duplicated time series observations.}
#'   \item{dates}{Date range and date consistency checks.}
#'   \item{value_ranges}{Checks for invalid values.}
#'   \item{store_consistency}{Checks whether all stores from `train` exist in `stores`.}
#'   \item{frequency}{Checks for missing dates in each store and product family series.}
#' }
#'
#' @details
#' A duplicated time series observation is defined as more than one row for the
#' same combination of `date`, `store_nbr`, and `family`.
#'
#' Frequency issues are detected by checking whether each `store_nbr` and
#' `family` combination has a complete daily sequence between its minimum and
#' maximum date.
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#' validation <- validate_sales_ts(sales_data)
#'
#' validation$passed
#' validation$missing_values
#' validation$duplicates
#' }
#'
#' @export
validate_sales_ts <- function(data) {
  schema <- sales_schema()

  required_tables <- names(schema)

  structure_check <- check_required_tables(data, required_tables)

  if (!structure_check$passed) {
    return(list(
      passed = FALSE,
      structure = structure_check
    ))
  }

  columns_check <- list(
    train = check_required_columns_report(
      data$train,
      unname(schema$train$columns),
      "train"
    ),
    stores = check_required_columns_report(
      data$stores,
      unname(schema$stores$columns),
      "stores"
    ),
    holidays_events = check_required_columns_report(
      data$holidays_events,
      unname(schema$holidays_events$columns),
      "holidays_events"
    )
  )

  columns_passed <- all(vapply(
    columns_check,
    function(x) x$passed,
    logical(1)
  ))

  if (!columns_passed) {
    return(list(
      passed = FALSE,
      structure = structure_check,
      columns = columns_check
    ))
  }

  missing_values <- list(
    train = count_missing_values(data$train),
    stores = count_missing_values(data$stores),
    holidays_events = count_missing_values(data$holidays_events)
  )

  duplicates <- check_train_duplicates(data$train)

  dates <- list(
    train = check_date_column(data$train, schema$train$columns[["date"]]),
    holidays_events = check_date_column(
      data$holidays_events,
      schema$holidays_events$columns[["date"]]
    )
  )

  value_ranges <- check_value_ranges(data$train)

  store_consistency <- check_store_consistency(
    train = data$train,
    stores = data$stores
  )

  frequency <- check_sales_frequency(data$train)


  has_missing_values <- any(vapply(
    missing_values,
    function(x) any(x$missing_count > 0),
    logical(1)
  ))

  critical_issues <- c(
    has_missing_values,
    duplicates$duplicate_rows > 0,
    value_ranges$negative_sales_rows > 0,
    value_ranges$negative_onpromotion_rows > 0,
    value_ranges$invalid_store_nbr_rows > 0,
    store_consistency$missing_store_count > 0,
    frequency$series_with_missing_dates > 0,
    !dates$train$is_date,
    !dates$holidays_events$is_date
  )

  passed <- !any(critical_issues)

  list(
    passed = passed,
    has_issues = !passed,
    structure = structure_check,
    columns = columns_check,
    missing_values = missing_values,
    duplicates = duplicates,
    dates = dates,
    value_ranges = value_ranges,
    store_consistency = store_consistency,
    frequency = frequency
  )
}

check_required_tables <- function(data, required_tables) {
  if (!is.list(data)) {
    return(list(
      passed = FALSE,
      message = "Input data must be a named list.",
      missing_tables = required_tables
    ))
  }

  missing_tables <- setdiff(required_tables, names(data))

  list(
    passed = length(missing_tables) == 0,
    required_tables = required_tables,
    available_tables = names(data),
    missing_tables = missing_tables
  )
}
check_required_columns_report <- function(data, required_cols, table_name) {
  if (is.null(data)) {
    return(list(
      passed = FALSE,
      table = table_name,
      required_cols = required_cols,
      available_cols = character(0),
      missing_cols = required_cols
    ))
  }

  missing_cols <- setdiff(required_cols, names(data))

  list(
    passed = length(missing_cols) == 0,
    table = table_name,
    required_cols = required_cols,
    available_cols = names(data),
    missing_cols = missing_cols
  )
}

count_missing_values <- function(data) {
  tibble::tibble(
    column = names(data),
    missing_count = vapply(data, function(x) sum(is.na(x)), integer(1)),
    missing_share = vapply(data, function(x) mean(is.na(x)), numeric(1))
  )
}
check_train_duplicates <- function(train) {
  cols <- sales_columns("train")

  date_col <- cols[["date"]]
  store_col <- cols[["store_nbr"]]
  family_col <- cols[["family"]]

  duplicates <- train |>
    dplyr::count(
      .data[[date_col]],
      .data[[store_col]],
      .data[[family_col]],
      name = "n"
    ) |>
    dplyr::filter(.data$n > 1)

  list(
    duplicate_groups = nrow(duplicates),
    duplicate_rows = if (nrow(duplicates) == 0) {
      0L
    } else {
      sum(duplicates$n - 1L)
    },
    duplicated_keys = duplicates
  )
}
check_date_column <- function(data, date_col) {
  date_values <- data[[date_col]]
  is_date <- inherits(date_values, "Date")
  non_missing_dates <- date_values[!is.na(date_values)]

  list(
    is_date = is_date,
    min_date = if (is_date && length(non_missing_dates) > 0) {
      min(non_missing_dates)
    } else {
      NA
    },
    max_date = if (is_date && length(non_missing_dates) > 0) {
      max(non_missing_dates)
    } else {
      NA
    },
    unique_dates = length(unique(non_missing_dates)),
    missing_dates = sum(is.na(date_values))
  )
}

check_value_ranges <- function(train) {
  cols <- sales_columns("train")

  sales_col <- cols[["sales"]]
  promo_col <- cols[["onpromotion"]]
  store_col <- cols[["store_nbr"]]

  negative_sales <- train |>
    dplyr::filter(
      !is.na(.data[[sales_col]]),
      .data[[sales_col]] < 0
    )

  negative_onpromotion <- train |>
    dplyr::filter(
      !is.na(.data[[promo_col]]),
      .data[[promo_col]] < 0
    )

  invalid_store_nbr <- train |>
    dplyr::filter(
      !is.na(.data[[store_col]]),
      .data[[store_col]] <= 0
    )

  list(
    negative_sales_rows = nrow(negative_sales),
    negative_onpromotion_rows = nrow(negative_onpromotion),
    invalid_store_nbr_rows = nrow(invalid_store_nbr),
    negative_sales_examples = dplyr::slice_head(negative_sales, n = 10),
    negative_onpromotion_examples = dplyr::slice_head(
      negative_onpromotion,
      n = 10
    ),
    invalid_store_nbr_examples = dplyr::slice_head(
      invalid_store_nbr,
      n = 10
    )
  )
}
check_store_consistency <- function(train, stores) {
  train_cols <- sales_columns("train")
  stores_cols <- sales_columns("stores")

  train_store_col <- train_cols[["store_nbr"]]
  stores_store_col <- stores_cols[["store_nbr"]]

  train_stores <- unique(stats::na.omit(train[[train_store_col]]))
  metadata_stores <- unique(stats::na.omit(stores[[stores_store_col]]))

  missing_stores <- setdiff(train_stores, metadata_stores)
  unused_stores <- setdiff(metadata_stores, train_stores)

  list(
    missing_store_count = length(missing_stores),
    missing_stores = missing_stores,
    unused_store_count = length(unused_stores),
    unused_stores = unused_stores
  )
}

check_sales_frequency <- function(train) {
  cols <- sales_columns("train")

  date_col <- cols[["date"]]
  store_col <- cols[["store_nbr"]]
  family_col <- cols[["family"]]

  series_ranges <- train |>
    dplyr::filter(!is.na(.data[[date_col]])) |>
    dplyr::group_by(
      .data[[store_col]],
      .data[[family_col]]
    ) |>
    dplyr::summarise(
      min_date = min(.data[[date_col]]),
      max_date = max(.data[[date_col]]),
      observed_dates = dplyr::n_distinct(.data[[date_col]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      expected_dates = as.integer(.data$max_date - .data$min_date) + 1L,
      missing_dates = .data$expected_dates - .data$observed_dates,
      has_missing_dates = .data$missing_dates > 0
    )

  frequency_issues <- series_ranges |>
    dplyr::filter(.data$has_missing_dates)

  list(
    total_series = nrow(series_ranges),
    series_with_missing_dates = nrow(frequency_issues),
    total_missing_dates = sum(frequency_issues$missing_dates, na.rm = TRUE),
    frequency_issues = frequency_issues
  )
}
