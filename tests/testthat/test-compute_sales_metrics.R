make_metrics_sales_data <- function() {
  list(
    train = tibble::tibble(
      id = 1:5,
      date = as.Date(c(
        "2013-01-01",
        "2013-01-02",
        "2013-01-03",
        "2013-01-04",
        "2013-01-05"
      )),
      store_nbr = c(1L, 1L, 1L, 1L, 1L),
      family = c(
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE",
        "AUTOMOTIVE"
      ),
      sales = c(10, 20, 50, 20, 10),
      onpromotion = c(0L, 1L, 1L, 0L, 0L)
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
}

test_that("compute_sales_metrics computes global business metrics", {
  sales_data <- make_metrics_sales_data()

  result <- compute_sales_metrics(
    sales_data,
    moving_average_window = 3
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)

  expect_equal(result$total_sales, 110)
  expect_equal(result$average_sales, 22)
  expect_equal(result$median_sales, 20)
  expect_equal(result$min_sales, 10)
  expect_equal(result$max_sales, 50)
  expect_equal(result$active_sales_days, 5)
})

test_that("compute_sales_metrics computes promotion metrics", {
  sales_data <- make_metrics_sales_data()

  result <- compute_sales_metrics(sales_data)

  expect_equal(result$promoted_observations, 2)
  expect_equal(result$promotion_share, 2 / 5)
  expect_equal(result$total_onpromotion, 2)
  expect_equal(result$promoted_sales, 70)
  expect_equal(result$promoted_sales_share, 70 / 110)
})


test_that("compute_sales_metrics computes peak metrics", {
  sales_data <- make_metrics_sales_data()

  result <- compute_sales_metrics(sales_data)

  expect_equal(result$peak_count, 1)
  expect_true(is.na(result$avg_days_between_peaks))
})

test_that("avg_days_between_peaks returns double across grouped peaks", {
  sales_data <- list(
    train = tibble::tibble(
      id = 1:10,
      date = as.Date(c(
        "2013-01-01", "2013-01-02", "2013-01-03", "2013-01-04", "2013-01-05",
        "2013-01-01", "2013-01-02", "2013-01-03", "2013-01-04", "2013-01-05"
      )),
      store_nbr = rep(1L, 10),
      family = rep(c("AUTOMOTIVE", "BABY CARE"), each = 5),
      sales = c(1, 5, 1, 5, 1, 10, 20, 10, 20, 10),
      onpromotion = 0L
    )
  )

  result <- compute_sales_metrics(
    sales_data,
    group_vars = c("store_nbr", "family")
  )

  expect_type(result$avg_days_between_peaks, "double")
  expect_equal(result$peak_count, c(2L, 2L))
  expect_equal(result$avg_days_between_peaks, c(2, 2))
})


test_that("compute_sales_metrics computes moving average metrics", {
  sales_data <- make_metrics_sales_data()

  result <- compute_sales_metrics(
    sales_data,
    moving_average_window = 3
  )

  expect_equal(result$moving_average_last, mean(c(50, 20, 10)))
  expect_true(result$moving_average_mean > 0)
})

test_that("compute_sales_metrics supports grouping by store", {
  sales_data <- list(
    train = tibble::tibble(
      id = 1:4,
      date = as.Date(c(
        "2013-01-01",
        "2013-01-02",
        "2013-01-01",
        "2013-01-02"
      )),
      store_nbr = c(1L, 1L, 2L, 2L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE"),
      sales = c(10, 20, 100, 200),
      onpromotion = c(0L, 0L, 1L, 1L)
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

  result <- compute_sales_metrics(
    sales_data,
    group_vars = "store_nbr"
  )

  expect_equal(nrow(result), 2)
  expect_true("store_nbr" %in% names(result))

  store_1 <- result[result$store_nbr == 1L, ]
  store_2 <- result[result$store_nbr == 2L, ]

  expect_equal(store_1$total_sales, 30)
  expect_equal(store_2$total_sales, 300)
})

test_that("compute_sales_metrics supports grouping by store and family", {
  sales_data <- list(
    train = tibble::tibble(
      id = 1:4,
      date = as.Date(c(
        "2013-01-01",
        "2013-01-02",
        "2013-01-01",
        "2013-01-02"
      )),
      store_nbr = c(1L, 1L, 1L, 1L),
      family = c("AUTOMOTIVE", "AUTOMOTIVE", "BABY CARE", "BABY CARE"),
      sales = c(10, 20, 5, 15),
      onpromotion = c(0L, 0L, 1L, 1L)
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

  result <- compute_sales_metrics(
    sales_data,
    group_vars = c("store_nbr", "family")
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("store_nbr", "family") %in% names(result)))
})

test_that("compute_sales_metrics errors when train table is missing", {
  expect_error(
    compute_sales_metrics(list()),
    "train"
  )
})


test_that("compute_sales_metrics errors on unknown grouping variable", {
  sales_data <- make_metrics_sales_data()

  expect_error(
    compute_sales_metrics(
      sales_data,
      group_vars = "unknown_column"
    ),
    "Unknown grouping variable"
  )
})


test_that("compute_sales_metrics errors on invalid moving average window", {
  sales_data <- make_metrics_sales_data()

  expect_error(
    compute_sales_metrics(
      sales_data,
      moving_average_window = 0
    ),
    "greater than or equal to 1"
  )
})
