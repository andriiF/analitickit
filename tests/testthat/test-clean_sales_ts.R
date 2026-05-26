test_that("clean_sales_ts fills missing sales and promotion values", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-02")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(NA_real_, 20),
      onpromotion = c(NA_integer_, 1L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(
    sales_data,
    fill_missing_dates = FALSE
  )

  expect_equal(result$train$sales, c(0, 20))
  expect_equal(result$train$onpromotion, c(0L, 1L))
})
test_that("clean_sales_ts removes rows with missing key values", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L, 3L),
      date = as.Date(c("2013-01-01", NA, "2013-01-03")),
      store_nbr = c(1L, 1L, NA_integer_),
      family = c("AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20, 30),
      onpromotion = c(0L, 1L, 0L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(
    sales_data,
    fill_missing_dates = FALSE,
    drop_missing_keys = TRUE
  )

  expect_equal(nrow(result$train), 1)
  expect_false(any(is.na(result$train$date)))
  expect_false(any(is.na(result$train$store_nbr)))
  expect_false(any(is.na(result$train$family)))
})

test_that("clean_sales_ts aggregates duplicated observations", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-01")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20),
      onpromotion = c(1L, 2L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(
    sales_data,
    handle_duplicates = "aggregate",
    fill_missing_dates = FALSE
  )

  expect_equal(nrow(result$train), 1)
  expect_equal(result$train$sales, 30)
  expect_equal(result$train$onpromotion, 3)
})
test_that("clean_sales_ts keeps first duplicated observation when requested", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-01")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20),
      onpromotion = c(1L, 2L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(
    sales_data,
    handle_duplicates = "keep_first",
    fill_missing_dates = FALSE
  )

  expect_equal(nrow(result$train), 1)
  expect_equal(result$train$id, 1L)
  expect_equal(result$train$sales, 10)
  expect_equal(result$train$onpromotion, 1L)
})

test_that("clean_sales_ts errors on duplicated observations when requested", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-01")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20),
      onpromotion = c(1L, 2L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  expect_error(
    clean_sales_ts(
      sales_data,
      handle_duplicates = "error",
      fill_missing_dates = FALSE
    ),
    "Duplicated observations found"
  )
})

test_that("clean_sales_ts completes missing dates", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-03")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 30),
      onpromotion = c(0L, 1L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(
    sales_data,
    fill_missing_dates = TRUE
  )

  expect_equal(nrow(result$train), 3)
  expect_true(as.Date("2013-01-02") %in% result$train$date)

  added_row <- result$train[result$train$date == as.Date("2013-01-02"), ]

  expect_equal(added_row$sales, 0)
  expect_equal(added_row$onpromotion, 0)
})
test_that("clean_sales_ts sorts data when requested", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(2L, 1L),
      date = as.Date(c("2013-01-02", "2013-01-01")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(20, 10),
      onpromotion = c(1L, 0L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(
    sales_data,
    fill_missing_dates = FALSE,
    sort = TRUE
  )

  expect_equal(result$train$date, as.Date(c("2013-01-01", "2013-01-02")))
})


test_that("clean_sales_ts aggregates data by month", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L, 3L),
      date = as.Date(c("2013-01-01", "2013-01-15", "2013-02-01")),
      store_nbr = c(1L, 1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20, 30),
      onpromotion = c(1L, 2L, 3L)
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(
    sales_data,
    fill_missing_dates = FALSE,
    aggregate_period = "month"
  )

  expect_equal(nrow(result$train), 2)
  expect_equal(result$train$date, as.Date(c("2013-01-01", "2013-02-01")))
  expect_equal(result$train$sales, c(30, 30))
  expect_equal(result$train$onpromotion, c(3, 3))
})
test_that("clean_sales_ts returns the same data object structure", {
  sales_data <- list(
    train = tibble::tibble(
      id = 1L,
      date = as.Date("2013-01-01"),
      store_nbr = 1L,
      family = "AUTOMOTIVE",
      sales = 10,
      onpromotion = 0L
    ),
    stores = tibble::tibble(
      store_nbr = 1L,
      city = "Quito",
      state = "Pichincha",
      type = "D",
      cluster = 13L
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

  result <- clean_sales_ts(sales_data, fill_missing_dates = FALSE)

  expect_named(result, c("train", "stores", "holidays_events"))
  expect_s3_class(result$train, "tbl_df")
  expect_s3_class(result$stores, "tbl_df")
  expect_s3_class(result$holidays_events, "tbl_df")
})
