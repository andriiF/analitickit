#' Create management sales summary
#'
#' Creates a business-oriented management summary for sales time series data
#' used by the `analitickit` package.
#'
#' The function identifies key business insights such as the best and worst
#' store, the fastest growing product category, the largest percentage decline
#' in the latest period, last period average sales, top-performing city, store
#' type performance, and promotion-related summary metrics.
#'
#' @param data A named list returned by [load_sales_data()] or
#'   [clean_sales_ts()]. It should contain `train` and `stores` tibbles.
#' @param period Character. Time aggregation period used for growth and
#'   decline calculations. One of `"week"` or `"month"`. Defaults to `"month"`.
#'
#' @return A named list with business summary tables:
#' \describe{
#'   \item{overview}{Overall sales summary.}
#'   \item{best_store}{Store with the highest total sales.}
#'   \item{worst_store}{Store with the lowest total sales.}
#'   \item{fastest_growing_category}{Product family with the highest percentage growth.}
#'   \item{largest_declining_category}{Product family with the largest percentage decline in the latest period.}
#'   \item{last_period_summary}{Sales summary for the latest available period.}
#'   \item{best_city}{City with the highest total sales.}
#'   \item{best_store_type}{Store type with the highest total sales.}
#'   \item{promotion_summary}{Promotion-related business metrics.}
#' }
#'
#' @details
#' Growth by category is calculated between the first and last available
#' aggregated periods for each product family.
#'
#' The largest percentage decline is calculated between the two latest
#' available aggregated periods for each product family.
#'
#' The function uses the internal sales schema defined by `sales_schema()`,
#' so column names are resolved dynamically from the package schema.
#'
#' @examples
#' \dontrun{
#' sales_data <- load_sales_data("data/")
#' clean_data <- clean_sales_ts(sales_data)
#'
#' summary <- create_management_summary(clean_data, period = "month")
#'
#' summary$overview
#' summary$best_store
#' summary$fastest_growing_category
#' }
#'
#' @export
create_management_summary <- function(data,
                                      period = c("month", "week")) {
  period <- match.arg(period)

  if (!is.list(data) || is.null(data$train) || is.null(data$stores)) {
    stop(
      "Input data must be a named list containing `train` and `stores` tables.",
      call. = FALSE
    )
  }

  train <- data$train
  stores <- data$stores

  train_cols <- sales_columns("train")
  stores_cols <- sales_columns("stores")

  date_col <- train_cols[["date"]]
  store_col <- train_cols[["store_nbr"]]
  family_col <- train_cols[["family"]]
  sales_col <- train_cols[["sales"]]
  promo_col <- train_cols[["onpromotion"]]

  city_col <- stores_cols[["city"]]
  state_col <- stores_cols[["state"]]
  type_col <- stores_cols[["type"]]
  cluster_col <- stores_cols[["cluster"]]

  sales_with_stores <- train |>
    dplyr::left_join(
      stores,
      by = store_col
    )

  period_sales <- prepare_management_period_sales(
    train = train,
    date_col = date_col,
    family_col = family_col,
    sales_col = sales_col,
    period = period
  )

  list(
    overview = management_overview(
      train = train,
      date_col = date_col,
      sales_col = sales_col,
      promo_col = promo_col,
      store_col = store_col,
      family_col = family_col
    ),
    best_store = get_store_performance(
      sales_with_stores = sales_with_stores,
      store_col = store_col,
      city_col = city_col,
      state_col = state_col,
      type_col = type_col,
      cluster_col = cluster_col,
      sales_col = sales_col,
      direction = "best"
    ),
    worst_store = get_store_performance(
      sales_with_stores = sales_with_stores,
      store_col = store_col,
      city_col = city_col,
      state_col = state_col,
      type_col = type_col,
      cluster_col = cluster_col,
      sales_col = sales_col,
      direction = "worst"
    ),
    fastest_growing_category = get_fastest_growing_category(
      period_sales = period_sales,
      family_col = family_col
    ),
    largest_declining_category = get_largest_declining_category(
      period_sales = period_sales,
      family_col = family_col
    ),
    last_period_summary = get_last_period_summary(
      period_sales = period_sales
    ),
    best_city = get_metadata_performance(
      sales_with_stores = sales_with_stores,
      metadata_col = city_col,
      sales_col = sales_col,
      direction = "best"
    ),
    best_store_type = get_metadata_performance(
      sales_with_stores = sales_with_stores,
      metadata_col = type_col,
      sales_col = sales_col,
      direction = "best"
    ),
    promotion_summary = get_promotion_summary(
      train = train,
      sales_col = sales_col,
      promo_col = promo_col
    )
  )
}

management_overview <- function(train,
                                date_col,
                                sales_col,
                                promo_col,
                                store_col,
                                family_col) {
  tibble::tibble(
    start_date = min(train[[date_col]], na.rm = TRUE),
    end_date = max(train[[date_col]], na.rm = TRUE),
    observations = nrow(train),
    total_sales = sum(train[[sales_col]], na.rm = TRUE),
    average_sales = mean(train[[sales_col]], na.rm = TRUE),
    median_sales = stats::median(train[[sales_col]], na.rm = TRUE),
    unique_stores = dplyr::n_distinct(train[[store_col]]),
    unique_families = dplyr::n_distinct(train[[family_col]]),
    total_onpromotion = sum(train[[promo_col]], na.rm = TRUE)
  )
}
get_store_performance <- function(sales_with_stores,
                                  store_col,
                                  city_col,
                                  state_col,
                                  type_col,
                                  cluster_col,
                                  sales_col,
                                  direction = c("best", "worst")) {
  direction <- match.arg(direction)

  result <- sales_with_stores |>
    dplyr::group_by(
      .data[[store_col]],
      .data[[city_col]],
      .data[[state_col]],
      .data[[type_col]],
      .data[[cluster_col]]
    ) |>
    dplyr::summarise(
      total_sales = sum(.data[[sales_col]], na.rm = TRUE),
      average_sales = mean(.data[[sales_col]], na.rm = TRUE),
      observations = dplyr::n(),
      .groups = "drop"
    )

  if (direction == "best") {
    result |>
      dplyr::arrange(dplyr::desc(.data$total_sales)) |>
      dplyr::slice_head(n = 1)
  } else {
    result |>
      dplyr::arrange(.data$total_sales) |>
      dplyr::slice_head(n = 1)
  }
}

prepare_management_period_sales <- function(train,
                                            date_col,
                                            family_col,
                                            sales_col,
                                            period = c("month", "week")) {
  period <- match.arg(period)

  train |>
    dplyr::mutate(
      period_date = lubridate::floor_date(.data[[date_col]], unit = period)
    ) |>
    dplyr::group_by(
      .data[[family_col]],
      .data$period_date
    ) |>
    dplyr::summarise(
      total_sales = sum(.data[[sales_col]], na.rm = TRUE),
      average_sales = mean(.data[[sales_col]], na.rm = TRUE),
      observations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data[[family_col]],
      .data$period_date
    )
}


get_fastest_growing_category <- function(period_sales, family_col) {
  growth <- period_sales |>
    dplyr::group_by(.data[[family_col]]) |>
    dplyr::arrange(.data$period_date, .by_group = TRUE) |>
    dplyr::summarise(
      first_period = dplyr::first(.data$period_date),
      last_period = dplyr::last(.data$period_date),
      first_sales = dplyr::first(.data$total_sales),
      last_sales = dplyr::last(.data$total_sales),
      absolute_growth = .data$last_sales - .data$first_sales,
      percent_growth = dplyr::if_else(
        .data$first_sales == 0,
        NA_real_,
        .data$absolute_growth / .data$first_sales
      ),
      .groups = "drop"
    )

  growth |>
    dplyr::arrange(dplyr::desc(.data$percent_growth), dplyr::desc(.data$absolute_growth)) |>
    dplyr::slice_head(n = 1)
}
get_largest_declining_category <- function(period_sales, family_col) {
  decline <- period_sales |>
    dplyr::group_by(.data[[family_col]]) |>
    dplyr::arrange(.data$period_date, .by_group = TRUE) |>
    dplyr::mutate(
      previous_sales = dplyr::lag(.data$total_sales),
      absolute_change = .data$total_sales - .data$previous_sales,
      percent_change = dplyr::if_else(
        .data$previous_sales == 0,
        NA_real_,
        .data$absolute_change / .data$previous_sales
      )
    ) |>
    dplyr::filter(
      .data$period_date == max(.data$period_date, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  decline |>
    dplyr::arrange(.data$percent_change, .data$absolute_change) |>
    dplyr::slice_head(n = 1)
}

get_last_period_summary <- function(period_sales) {
  last_period <- max(period_sales$period_date, na.rm = TRUE)

  period_sales |>
    dplyr::filter(.data$period_date == last_period) |>
    dplyr::summarise(
      period_date = last_period,
      total_sales = sum(.data$total_sales, na.rm = TRUE),
      average_sales = mean(.data$total_sales, na.rm = TRUE),
      categories = dplyr::n(),
      best_category_sales = max(.data$total_sales, na.rm = TRUE),
      worst_category_sales = min(.data$total_sales, na.rm = TRUE),
      .groups = "drop"
    )
}


get_metadata_performance <- function(sales_with_stores,
                                     metadata_col,
                                     sales_col,
                                     direction = c("best", "worst")) {
  direction <- match.arg(direction)

  result <- sales_with_stores |>
    dplyr::group_by(.data[[metadata_col]]) |>
    dplyr::summarise(
      total_sales = sum(.data[[sales_col]], na.rm = TRUE),
      average_sales = mean(.data[[sales_col]], na.rm = TRUE),
      observations = dplyr::n(),
      .groups = "drop"
    )

  if (direction == "best") {
    result |>
      dplyr::arrange(dplyr::desc(.data$total_sales)) |>
      dplyr::slice_head(n = 1)
  } else {
    result |>
      dplyr::arrange(.data$total_sales) |>
      dplyr::slice_head(n = 1)
  }
}


get_promotion_summary <- function(train, sales_col, promo_col) {
  promoted <- train[[promo_col]] > 0

  tibble::tibble(
    promoted_observations = sum(promoted, na.rm = TRUE),
    promotion_share = mean(promoted, na.rm = TRUE),
    total_onpromotion = sum(train[[promo_col]], na.rm = TRUE),
    promoted_sales = sum(train[[sales_col]][promoted], na.rm = TRUE),
    promoted_sales_share = dplyr::if_else(
      sum(train[[sales_col]], na.rm = TRUE) == 0,
      NA_real_,
      promoted_sales / sum(train[[sales_col]], na.rm = TRUE)
    )
  )
}
