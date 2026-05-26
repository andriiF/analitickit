test_that("validate_sales_ts passes valid sales data", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-02")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20),
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

  result <- validate_sales_ts(sales_data)

  expect_true(result$passed)
  expect_false(result$has_issues)
})

test_that("validate_sales_ts detects duplicate observations", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-01")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20),
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

  result <- validate_sales_ts(sales_data)

  expect_false(result$passed)
  expect_true(result$has_issues)
  expect_equal(result$duplicates$duplicate_groups, 1)
  expect_equal(result$duplicates$duplicate_rows, 1)
})

test_that("validate_sales_ts detects negative sales", {
  sales_data <- list(
    train = tibble::tibble(
      id = 1L,
      date = as.Date("2013-01-01"),
      store_nbr = 1L,
      family = "AUTOMOTIVE",
      sales = -10,
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

  result <- validate_sales_ts(sales_data)

  expect_false(result$passed)
  expect_equal(result$value_ranges$negative_sales_rows, 1)
})

test_that("validate_sales_ts detects missing dates in time series", {
  sales_data <- list(
    train = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date(c("2013-01-01", "2013-01-03")),
      store_nbr = c(1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20),
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

  result <- validate_sales_ts(sales_data)

  expect_false(result$passed)
  expect_equal(result$frequency$series_with_missing_dates, 1)
  expect_equal(result$frequency$total_missing_dates, 1)
})
