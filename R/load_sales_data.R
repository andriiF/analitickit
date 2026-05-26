#' Load sales data
#'
#' Loads sales-related CSV files used by the `analitickit` package.
#'
#' The function reads three required files from the selected directory:
#' `train.csv`, `stores.csv`, and `holidays_events.csv`.
#'
#' The loaded data is returned as a named list of tibbles:
#' `train`, `stores`, and `holidays_events`.
#'
#' @param path A character string with the path to the directory containing
#'   the CSV files.
#' @param show_col_types Logical. Should column type messages from `readr`
#'   be displayed? Defaults to `FALSE`.
#'
#' @return A named list with three tibbles:
#' \describe{
#'   \item{train}{Training sales data with columns `id`, `date`, `store_nbr`,
#'   `family`, `sales`, and `onpromotion`.}
#'   \item{stores}{Store metadata with columns `store_nbr`, `city`, `state`,
#'   `type`, and `cluster`.}
#'   \item{holidays_events}{Holiday and event metadata with columns `date`,
#'   `type`, `locale`, `locale_name`, `description`, and `transferred`.}
#' }
#'
#' @details
#' This function uses `readr` from the tidyverse ecosystem to read CSV files
#' and assign appropriate column types.
#'
#' Expected files:
#' \itemize{
#'   \item `train.csv`
#'   \item `stores.csv`
#'   \item `holidays_events.csv`
#' }
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#'
#' sales_data$train
#' sales_data$stores
#' sales_data$holidays_events
#' }
#'
#' @export
load_sales_data <- function(path, show_col_types = FALSE) {
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path, call. = FALSE)
  }

  schema <- sales_schema()

  list(
    train = read_sales_csv(
      file = file.path(path, schema$train$file),
      col_types = schema$train$types,
      required_cols = unname(schema$train$columns),
      file_name = schema$train$file,
      show_col_types = show_col_types
    ),
    stores = read_sales_csv(
      file = file.path(path, schema$stores$file),
      col_types = schema$stores$types,
      required_cols = unname(schema$stores$columns),
      file_name = schema$stores$file,
      show_col_types = show_col_types
    ),
    holidays_events = read_sales_csv(
      file = file.path(path, schema$holidays_events$file),
      col_types = schema$holidays_events$types,
      required_cols = unname(schema$holidays_events$columns),
      file_name = schema$holidays_events$file,
      show_col_types = show_col_types
    )
  )
}
read_sales_csv <- function(file,
                           col_types,
                           required_cols,
                           file_name,
                           show_col_types = FALSE) {
  if (!file.exists(file)) {
    stop("Missing required file: ", file_name, call. = FALSE)
  }

  check_csv_columns(
    file = file,
    required_cols = required_cols,
    file_name = file_name
  )

  readr::read_csv(
    file = file,
    col_types = col_types,
    show_col_types = show_col_types
  )
}

check_csv_columns <- function(file, required_cols, file_name) {
  column_names <- names(
    readr::read_csv(
      file = file,
      n_max = 0,
      show_col_types = FALSE
    )
  )

  missing_cols <- setdiff(required_cols, column_names)

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s) in ",
      file_name,
      ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
