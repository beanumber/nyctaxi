# Load data
# 

library(nyctaxi)

db <- src_mysql_cnf(dbname = "nyctaxi")
rides <- etl("nyctaxi", db = db, dir = "~/dumps/nyctaxi")

rides %>%
  etl_init()

rides %>%
  etl_extract(year = 2017, months = 1:12, type = "yellow") %>%
  etl_transform(year = 2017, months = 1:12, type = "yellow") %>%
  etl_load(years = 2017, months = 1:3, type = "yellow")

