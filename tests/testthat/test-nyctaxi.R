context("nyctaxi")

## TODO: Rename context
## TODO: Add more tests

test_that("etl nyctaxi", {
  mainDir <- "~/Desktop/nyctaxi/"
  if (dir.exists(file.path(mainDir)) == TRUE) {
    taxi <- etl("nyctaxi")
    expect_s3_class(taxi,c("etl_nyctaxi","etl","src_sqlite","src_sql","src"))
    expect_message(taxi %>% etl_extract(years = 2016, months = 1, types = "green"), "Extracting")
  }
})

test_that("mysql works", {
  if (require(RMySQL) && mysqlHasDefault()) {
    db <- src_mysql_cnf()
    expect_s3_class(db, "src_dbi")
    taxi <- etl("nyctaxi", db)
    expect_s3_class(taxi,c("etl_nyctaxi","etl","src_mysql","src_sql","src"))
  }
})


