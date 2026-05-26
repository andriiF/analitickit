make_management_sales_data <- function() {
  list(
    train = tibble::tibble(
      id = 1:12,
      date = as.Date(c(
        "2013-01-01", "2013-01-02", "2013-02-01", "2013-02-02",
        "2013-01-01", "2013-01-02", "2013-02-01", "2013-02-02",
        "2013-01-01", "2013-01-02", "2013-02-01", "2013-02-02"
      )),
      store_nbr = c(
        1L, 1L, 1L, 1L,
        2L, 2L, 2L, 2L,
        3L, 3L, 3L, 3L
      ),
      family = c(
        "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE", "AUTOMOTIVE",
        "BABY CARE", "BABY CARE", "BABY CARE", "BABY CARE",
        "GROCERY", "GROCERY", "GROCERY", "GROCERY"
      ),
      sales = c(
        10, 10, 30, 30,
        100, 100, 50, 50,
        20, 20, 10, 10
      ),
      onpromotion = c(
        0L, 1L, 1L, 0L,
        1L, 1L, 0L, 0L,
        0L, 0L, 1L, 1L
      )
    ),
    stores = tibble::tibble(
      store_nbr = c(1L, 2L, 3L),
      city = c("Quito", "Manta", "Cuenca"),
      state = c("Pichincha", "Manabi", "Azuay"),
      type = c("D", "C", "A"),
      cluster = c(13L, 5L, 1L)
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

test_that("create_management_summary returns expected summary structure", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data)

  expect_type(result, "list")

  expect_named(
    result,
    c(
      "overview",
      "best_store",
      "worst_store",
      "fastest_growing_category",
      "largest_declining_category",
      "last_period_summary",
      "best_city",
      "best_store_type",
      "promotion_summary"
    )
  )
})

test_that("create_management_summary computes overview metrics", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data)

  expect_equal(result$overview$total_sales, 440)
  expect_equal(result$overview$observations, 12)
  expect_equal(result$overview$unique_stores, 3)
  expect_equal(result$overview$unique_families, 3)
})

test_that("create_management_summary identifies best and worst store", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data)

  expect_equal(result$best_store$store_nbr, 2L)
  expect_equal(result$best_store$total_sales, 300)

  expect_equal(result$worst_store$store_nbr, 3L)
  expect_equal(result$worst_store$total_sales, 60)
})

test_that("create_management_summary identifies fastest growing category", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data, period = "month")

  expect_equal(result$fastest_growing_category$family, "AUTOMOTIVE")
  expect_equal(result$fastest_growing_category$first_sales, 20)
  expect_equal(result$fastest_growing_category$last_sales, 60)
  expect_equal(result$fastest_growing_category$percent_growth, 2)
})


test_that("create_management_summary identifies largest declining category", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data, period = "month")

  expect_equal(result$largest_declining_category$family, "BABY CARE")
  expect_equal(result$largest_declining_category$previous_sales, 200)
  expect_equal(result$largest_declining_category$total_sales, 100)
  expect_equal(result$largest_declining_category$percent_change, -0.5)
})


test_that("create_management_summary computes last period summary", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data, period = "month")

  expect_equal(result$last_period_summary$period_date, as.Date("2013-02-01"))
  expect_equal(result$last_period_summary$total_sales, 180)
  expect_equal(result$last_period_summary$categories, 3)
})


test_that("create_management_summary identifies best city and store type", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data)

  expect_equal(result$best_city$city, "Manta")
  expect_equal(result$best_city$total_sales, 300)

  expect_equal(result$best_store_type$type, "C")
  expect_equal(result$best_store_type$total_sales, 300)
})


test_that("create_management_summary computes promotion summary", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data)

  expect_equal(result$promotion_summary$promoted_observations, 6)
  expect_equal(result$promotion_summary$total_onpromotion, 6)
  expect_equal(result$promotion_summary$promoted_sales, 260)
  expect_equal(result$promotion_summary$promoted_sales_share, 260 / 440)
})


test_that("create_management_summary errors when required tables are missing", {
  expect_error(
    create_management_summary(list()),
    "train"
  )
})

test_that("create_management_summary supports weekly period", {
  sales_data <- make_management_sales_data()

  result <- create_management_summary(sales_data, period = "week")

  expect_type(result, "list")
  expect_s3_class(result$last_period_summary, "tbl_df")
})
