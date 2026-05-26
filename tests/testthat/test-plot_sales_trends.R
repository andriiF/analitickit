make_plot_sales_data <- function() {
  list(
    train = tibble::tibble(
      id = 1:6,
      date = as.Date(c(
        "2013-01-01",
        "2013-01-02",
        "2013-01-03",
        "2013-01-01",
        "2013-01-02",
        "2013-01-03"
      )),
      store_nbr = c(1L, 1L, 1L, 2L, 2L, 2L),
      family = c(
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE"
      ),
      sales = c(10, 20, 30, 100, 200, 300),
      onpromotion = c(0L, 1L, 0L, 0L, 1L, 0L)
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

make_plot_sales_data <- function() {
  list(
    train = tibble::tibble(
      id = 1:6,
      date = as.Date(c(
        "2013-01-01",
        "2013-01-02",
        "2013-01-03",
        "2013-01-01",
        "2013-01-02",
        "2013-01-03"
      )),
      store_nbr = c(1L, 1L, 1L, 2L, 2L, 2L),
      family = c(
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE"
      ),
      sales = c(10, 20, 30, 100, 200, 300),
      onpromotion = c(0L, 1L, 0L, 0L, 1L, 0L)
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

test_that("plot_sales_trends supports grouping by store", {
  sales_data <- make_plot_sales_data()

  result <- plot_sales_trends(
    sales_data,
    group_var = "store_nbr"
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_sales_trends supports monthly aggregation", {
  sales_data <- make_plot_sales_data()

  result <- plot_sales_trends(
    sales_data,
    aggregate_period = "month"
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_sales_trends supports moving average", {
  sales_data <- make_plot_sales_data()

  result <- plot_sales_trends(
    sales_data,
    add_moving_average = TRUE,
    moving_average_window = 2
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_sales_trends errors when train table is missing", {
  expect_error(
    plot_sales_trends(list()),
    "train"
  )
})

test_that("plot_sales_trends errors on unknown grouping variable", {
  sales_data <- make_plot_sales_data()

  expect_error(
    plot_sales_trends(
      sales_data,
      group_var = "unknown_column"
    ),
    "Unknown grouping variable"
  )
})

test_that("plot_sales_trends errors on invalid moving average window", {
  sales_data <- make_plot_sales_data()

  expect_error(
    plot_sales_trends(
      sales_data,
      add_moving_average = TRUE,
      moving_average_window = 0
    ),
    "greater than or equal to 1"
  )
})


test_that("prepare_sales_trend_data aggregates sales by day", {
  sales_data <- make_plot_sales_data()
  cols <- sales_columns("train")

  result <- prepare_sales_trend_data(
    train = sales_data$train,
    date_col = cols[["date"]],
    sales_col = cols[["sales"]],
    group_var = NULL,
    aggregate_period = "day"
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$sales, c(110, 220, 330))
})


test_that("prepare_sales_trend_data aggregates sales by group", {
  sales_data <- make_plot_sales_data()
  cols <- sales_columns("train")

  result <- prepare_sales_trend_data(
    train = sales_data$train,
    date_col = cols[["date"]],
    sales_col = cols[["sales"]],
    group_var = "store_nbr",
    aggregate_period = "day"
  )

  expect_equal(nrow(result), 6)

  store_1 <- result[result$store_nbr == 1L, ]
  store_2 <- result[result$store_nbr == 2L, ]

  expect_equal(store_1$sales, c(10, 20, 30))
  expect_equal(store_2$sales, c(100, 200, 300))
})









