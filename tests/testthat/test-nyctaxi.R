context("nyctaxi")

## TODO: Rename context
## TODO: Add more tests

test_that("etl nyctaxi", {
  #expect_equal(2 * 2, 4)
  taxi <- etl("nyctaxi")
  expect_s3_class(taxi,c("etl_nyctaxi","etl","src_sqlite","src_sql","src"))
  #taxi %>% etl_extract()
  
})

test_that("mysql works", {
  if (require(RMySQL) && mysqlHasDefault()) {
    db <- src_mysql_cnf()
    expect_s3_class(db, "src_dbi")
    taxi <- etl("nyctaxi", db)
    expect_s3_class(taxi,c("etl_nyctaxi","etl","src_mysql","src_sql","src"))
  }
})


