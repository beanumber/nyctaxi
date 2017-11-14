context("nyctaxi")

## TODO: Rename context
## TODO: Add more tests

test_that("etl nyctaxi", {
  mainDir <- "~/Desktop/nyctaxi/"
  if (dir.exists(file.path(mainDir)) == TRUE) {
    taxi <- etl("nyctaxi")
    expect_s3_class(taxi, c("etl_nyctaxi", "etl", "src_sqlite", "src_sql", "src"))
    expect_message(taxi %>% etl_extract(years = 2016, months = 1, types = "green"), "Extracting")
  }
})

test_that("mysql works", {
  if (require(RMySQL) && mysqlHasDefault()) {
    db <- src_mysql_cnf()
    expect_s3_class(db, "src_dbi")
    taxi <- etl("nyctaxi", db)
    expect_s3_class(taxi,c("etl_nyctaxi", "etl", "src_mysql", "src_sql", "src"))
  }
})

test_that("get_file_path works", {
  expect_equal(nrow(get_file_path(2017, 1:6, "yellow", "~/")), 6)
})

test_that("download_nyc_data works", {
  taxi <- etl("nyctaxi")
  num_rows <- sample.int(100, size = 1)
  lyft <- download_nyc_data(taxi, n = num_rows)
  expect_equal(nrow(readr::read_csv(lyft)), num_rows)
})

