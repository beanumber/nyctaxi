context("nyctaxi")

## TODO: Rename context
## TODO: Add more tests

test_that("etl nyctaxi", {
  #expect_equal(2 * 2, 4)
  taxi <- etl("nyctaxi")
  expect_s3_class(taxi,c("etl_nyctaxi","etl","src_sqlite","src_sql","src"))
  #taxi %>% etl_extract()
  
})
