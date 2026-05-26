# Internal schema definition for analitickit data files
sales_schema <- function() {
  list(
    train = list(
      file = "train.csv",
      columns = c(
        id = "id",
        date = "date",
        store_nbr = "store_nbr",
        family = "family",
        sales = "sales",
        onpromotion = "onpromotion"
      ),
      types = readr::cols(
        id = readr::col_integer(),
        date = readr::col_date(),
        store_nbr = readr::col_integer(),
        family = readr::col_character(),
        sales = readr::col_double(),
        onpromotion = readr::col_integer()
      )
    ),
    stores = list(
      file = "stores.csv",
      columns = c(
        store_nbr = "store_nbr",
        city = "city",
        state = "state",
        type = "type",
        cluster = "cluster"
      ),
      types = readr::cols(
        store_nbr = readr::col_integer(),
        city = readr::col_character(),
        state = readr::col_character(),
        type = readr::col_character(),
        cluster = readr::col_integer()
      )
    ),
    holidays_events = list(
      file = "holidays_events.csv",
      columns = c(
        date = "date",
        type = "type",
        locale = "locale",
        locale_name = "locale_name",
        description = "description",
        transferred = "transferred"
      ),
      types = readr::cols(
        date = readr::col_date(),
        type = readr::col_character(),
        locale = readr::col_character(),
        locale_name = readr::col_character(),
        description = readr::col_character(),
        transferred = readr::col_logical()
      )
    )
  )
}

sales_columns <- function(table) {
  schema <- sales_schema()

  if (!table %in% names(schema)) {
    stop("Unknown table in schema: ", table, call. = FALSE)
  }

  schema[[table]]$columns
}
