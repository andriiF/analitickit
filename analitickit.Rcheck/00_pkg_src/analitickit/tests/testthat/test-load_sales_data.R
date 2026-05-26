test_that("load_sales_data loads all required CSV files", {
  temp_dir <- tempfile("sales_data_")
  dir.create(temp_dir)

  writeLines(
    c(
      "id,date,store_nbr,family,sales,onpromotion",
      "0,2013-01-01,1,AUTOMOTIVE,0.0,0",
      "1,2013-01-01,1,BABY CARE,0.0,0"
    ),
    file.path(temp_dir, "train.csv")
  )

  writeLines(
    c(
      "store_nbr,city,state,type,cluster",
      "1,Quito,Pichincha,D,13",
      "2,Quito,Pichincha,D,13"
    ),
    file.path(temp_dir, "stores.csv")
  )

  writeLines(
    c(
      "date,type,locale,locale_name,description,transferred",
      "2012-03-02,Holiday,Local,Manta,Fundacion de Manta,FALSE"
    ),
    file.path(temp_dir, "holidays_events.csv")
  )

  result <- load_sales_data(temp_dir)

  expect_type(result, "list")
  expect_named(result, c("train", "stores", "holidays_events"))

  expect_s3_class(result$train, "tbl_df")
  expect_s3_class(result$stores, "tbl_df")
  expect_s3_class(result$holidays_events, "tbl_df")

  expect_equal(nrow(result$train), 2)
  expect_equal(nrow(result$stores), 2)
  expect_equal(nrow(result$holidays_events), 1)
})

test_that("load_sales_data assigns correct column types", {
  temp_dir <- tempfile("sales_data_")
  dir.create(temp_dir)

  writeLines(
    c(
      "id,date,store_nbr,family,sales,onpromotion",
      "0,2013-01-01,1,AUTOMOTIVE,0.0,0"
    ),
    file.path(temp_dir, "train.csv")
  )

  writeLines(
    c(
      "store_nbr,city,state,type,cluster",
      "1,Quito,Pichincha,D,13"
    ),
    file.path(temp_dir, "stores.csv")
  )

  writeLines(
    c(
      "date,type,locale,locale_name,description,transferred",
      "2012-03-02,Holiday,Local,Manta,Fundacion de Manta,FALSE"
    ),
    file.path(temp_dir, "holidays_events.csv")
  )

  result <- load_sales_data(temp_dir)

  expect_s3_class(result$train$date, "Date")
  expect_type(result$train$id, "integer")
  expect_type(result$train$store_nbr, "integer")
  expect_type(result$train$family, "character")
  expect_type(result$train$sales, "double")
  expect_type(result$train$onpromotion, "integer")

  expect_type(result$stores$store_nbr, "integer")
  expect_type(result$stores$city, "character")
  expect_type(result$stores$state, "character")
  expect_type(result$stores$type, "character")
  expect_type(result$stores$cluster, "integer")

  expect_s3_class(result$holidays_events$date, "Date")
  expect_type(result$holidays_events$type, "character")
  expect_type(result$holidays_events$locale, "character")
  expect_type(result$holidays_events$locale_name, "character")
  expect_type(result$holidays_events$description, "character")
  expect_type(result$holidays_events$transferred, "logical")
})

test_that("load_sales_data errors when directory does not exist", {
  expect_error(
    load_sales_data("folder_ktory_nie_istnieje"),
    "Directory does not exist"
  )
})


test_that("load_sales_data errors when required files are missing", {
  temp_dir <- tempfile("sales_data_")
  dir.create(temp_dir)

  writeLines(
    c(
      "id,date,store_nbr,family,sales,onpromotion",
      "0,2013-01-01,1,AUTOMOTIVE,0.0,0"
    ),
    file.path(temp_dir, "train.csv")
  )

  expect_error(
    load_sales_data(temp_dir),
    "Missing required file:"
  )
})


test_that("load_sales_data errors when train.csv has missing columns", {
  temp_dir <- tempfile("sales_data_")
  dir.create(temp_dir)

  writeLines(
    c(
      "id,date,store_nbr,family,sales",
      "0,2013-01-01,1,AUTOMOTIVE,0.0"
    ),
    file.path(temp_dir, "train.csv")
  )

  writeLines(
    c(
      "store_nbr,city,state,type,cluster",
      "1,Quito,Pichincha,D,13"
    ),
    file.path(temp_dir, "stores.csv")
  )

  writeLines(
    c(
      "date,type,locale,locale_name,description,transferred",
      "2012-03-02,Holiday,Local,Manta,Fundacion de Manta,FALSE"
    ),
    file.path(temp_dir, "holidays_events.csv")
  )
  expect_error(
    load_sales_data(temp_dir),
    "Missing required column\\(s\\) in train\\.csv: onpromotion"
  )
})
