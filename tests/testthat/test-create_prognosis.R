make_prognosis_sales_data <- function() {
  list(
    train = tibble::tibble(
      id = 1:10,
      date = as.Date(c(
        "2013-01-01", "2013-01-02", "2013-01-03", "2013-01-04", "2013-01-05",
        "2013-01-01", "2013-01-02", "2013-01-03", "2013-01-04", "2013-01-05"
      )),
      store_nbr = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L),
      family = c(
        "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE",
        "BABY CARE", "BABY CARE", "BABY CARE", "BABY CARE", "BABY CARE"
      ),
      sales = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28),
      onpromotion = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L)
    ),
    stores = tibble::tibble(
      store_nbr = c(1L, 2L),
      city = c("Quito", "Manta"),
      state = c("Pichincha", "Manabi"),
      type = c("D", "C"),
      cluster = c(13L, 5L)
    ),
    holidays_events = tibble::tibble(
      date = as.Date("2013-01-01"),
      type = "Holiday",
      locale = "Local",
      locale_name = "Quito",
      description = "Example holiday",
      transferred = FALSE
    )
  )
}


test_that("prepare_prophet_history creates ds and y columns", {
  sales_data <- make_prognosis_sales_data()

  result <- prepare_prophet_history(
    data = sales_data,
    store_nbr = 1L,
    family = "AUTOMOTIVE",
    period = "day"
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("ds", "y"))
  expect_equal(nrow(result), 5)
  expect_equal(result$ds, as.Date(c(
    "2013-01-01", "2013-01-02", "2013-01-03", "2013-01-04", "2013-01-05"
  )))
  expect_equal(result$y, c(10, 12, 14, 16, 18))
})


test_that("prepare_prophet_history aggregates sales without filters", {
  sales_data <- make_prognosis_sales_data()

  result <- prepare_prophet_history(
    data = sales_data,
    period = "day"
  )

  expect_equal(nrow(result), 5)
  expect_equal(result$y, c(30, 34, 38, 42, 46))
})

test_that("prepare_prophet_history supports monthly aggregation", {
  sales_data <- make_prognosis_sales_data()

  result <- prepare_prophet_history(
    data = sales_data,
    period = "month"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$ds, as.Date("2013-01-01"))
  expect_equal(result$y, sum(sales_data$train$sales))
})



test_that("prognosis_frequency maps periods to Prophet frequencies", {
  expect_equal(prognosis_frequency("day"), "day")
  expect_equal(prognosis_frequency("week"), "week")
  expect_equal(prognosis_frequency("month"), "month")
})
test_that("create_prognosis errors when train table is missing", {
  testthat::skip_if_not_installed("prophet")

  expect_error(
    create_prognosis(list()),
    "train"
  )
})


test_that("create_prognosis errors on invalid horizon", {
  testthat::skip_if_not_installed("prophet")

  sales_data <- make_prognosis_sales_data()

  expect_error(
    create_prognosis(
      sales_data,
      horizon = 0
    ),
    "greater than or equal to 1"
  )
})

test_that("create_prognosis returns model, forecast, history and filters", {
  testthat::skip_if_not_installed("prophet")

  sales_data <- make_prognosis_sales_data()

  result <- create_prognosis(
    sales_data,
    store_nbr = 1L,
    family = "AUTOMOTIVE",
    horizon = 3,
    period = "day",
    include_history = FALSE,
    yearly_seasonality = FALSE,
    weekly_seasonality = FALSE,
    daily_seasonality = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("model", "forecast", "history", "filters"))

  expect_s3_class(result$forecast, "tbl_df")
  expect_s3_class(result$history, "tbl_df")

  expect_true(all(c("ds", "yhat", "yhat_lower", "yhat_upper") %in% names(result$forecast)))
  expect_equal(nrow(result$forecast), 3)
  expect_equal(result$filters$horizon, 3)
})

test_that("create_prognosis errors when not enough observations are available", {
  testthat::skip_if_not_installed("prophet")

  sales_data <- make_prognosis_sales_data()

  expect_error(
    create_prognosis(
      sales_data,
      store_nbr = 999L,
      horizon = 3
    ),
    "Not enough observations"
  )
})



