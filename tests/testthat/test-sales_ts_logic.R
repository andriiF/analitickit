make_logic_sales_data <- function() {
  list(
    train = tibble::tibble(
      id = 1:12,
      date = as.Date(c(
        "2013-01-01", "2013-01-02", "2013-01-03",
        "2013-01-01", "2013-01-02", "2013-01-03",
        "2013-02-01", "2013-02-02", "2013-02-03",
        "2013-02-01", "2013-02-02", "2013-02-03"
      )),
      store_nbr = c(
        1L, 1L, 1L,
        2L, 2L, 2L,
        1L, 1L, 1L,
        3L, 3L, 3L
      ),
      family = c(
        "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE",
        "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE",
        "BABY CARE", "BABY CARE", "BABY CARE",
        "BABY CARE", "BABY CARE", "BABY CARE"
      ),
      sales = c(
        10, 20, 30,
        100, 200, 300,
        5, 10, 15,
        50, 60, 70
      ),
      onpromotion = c(
        0L, 1L, 0L,
        1L, 1L, 0L,
        0L, 0L, 1L,
        1L, 0L, 0L
      )
    ),
    stores = tibble::tibble(
      store_nbr = c(1L, 2L, 3L),
      city = c("Quito", "Manta", "Quito"),
      state = c("Pichincha", "Manabi", "Pichincha"),
      type = c("D", "C", "A"),
      cluster = c(13L, 5L, 13L)
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

test_that("sales_ts_logic returns expected structure", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(sales_data)

  expect_type(result, "list")

  expect_named(
    result,
    c("filtered_data", "metrics", "plot", "filters")
  )

  expect_type(result$filtered_data, "list")
  expect_s3_class(result$metrics, "tbl_df")
  expect_s3_class(result$plot, "ggplot")
})

test_that("sales_ts_logic filters by city", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(
    sales_data,
    city = "Manta",
    run_plot = FALSE
  )

  expect_equal(unique(as.character(result$filtered_data$stores$city)), "Manta")
  expect_equal(unique(result$filtered_data$stores$store_nbr), 2L)
  expect_equal(unique(result$filtered_data$train$store_nbr), 2L)
  expect_null(result$plot)
})


test_that("sales_ts_logic filters by state and store type", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(
    sales_data,
    state = "Pichincha",
    store_type = "A",
    run_plot = FALSE
  )

  expect_equal(unique(result$filtered_data$stores$store_nbr), 3L)
  expect_true(all(result$filtered_data$train$store_nbr == 3L))
})

test_that("sales_ts_logic filters by date range", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(
    sales_data,
    start_date = "2013-02-01",
    end_date = "2013-02-02",
    run_plot = FALSE
  )

  expect_true(all(result$filtered_data$train$date >= as.Date("2013-02-01")))
  expect_true(all(result$filtered_data$train$date <= as.Date("2013-02-02")))
})

test_that("sales_ts_logic filters by store number", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(
    sales_data,
    store_nbr = 1L,
    run_plot = FALSE
  )

  expect_true(all(result$filtered_data$stores$store_nbr == 1L))
  expect_true(all(result$filtered_data$train$store_nbr == 1L))
})

test_that("sales_ts_logic can run metrics only", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(
    sales_data,
    run_metrics = TRUE,
    run_plot = FALSE
  )

  expect_s3_class(result$metrics, "tbl_df")
  expect_null(result$plot)
})

test_that("sales_ts_logic can run plot only", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(
    sales_data,
    run_metrics = FALSE,
    run_plot = TRUE
  )

  expect_null(result$metrics)
  expect_s3_class(result$plot, "ggplot")
})

test_that("sales_ts_logic passes group vars to compute_sales_metrics", {
  sales_data <- make_logic_sales_data()

  result <- sales_ts_logic(
    sales_data,
    group_vars = "store_nbr",
    run_plot = FALSE
  )

  expect_true("store_nbr" %in% names(result$metrics))
})

test_that("sales_ts_logic errors when required tables are missing", {
  expect_error(
    sales_ts_logic(list()),
    "train"
  )
})

test_that("sales_ts_logic errors on invalid moving average window", {
  sales_data <- make_logic_sales_data()

  expect_error(
    sales_ts_logic(
      sales_data,
      moving_average_window = 0
    ),
    "greater than or equal to 1"
  )
})

test_that("sales_ts_logic errors when filters return no observations", {
  sales_data <- make_logic_sales_data()

  expect_error(
    sales_ts_logic(
      sales_data,
      city = "Unknown city"
    ),
    "No sales observations found"
  )
})


