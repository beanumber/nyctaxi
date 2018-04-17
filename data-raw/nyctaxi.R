library(tidyverse)
library(nyctaxi)

my_dir <- "~/dumps/nyctaxi/"

taxi <- etl("nyctaxi", dir = my_dir)
taxi %>%
  etl_extract(years = 2016, months = 1, type = "yellow") %>%
  etl_extract(years = 2016, months = 1, type = "green")

yellow_tripdata_2016_01 <- read_csv(file.path(my_dir, "raw/yellow_tripdata_2016-01.csv"))
green_tripdata_2016_01 <- read_csv(file.path(my_dir, "raw/green_tripdata_2016-01.csv"))

set.seed(5)

# take a random sample of size 1000 from a dataset  
# sample without replacement

n <- 50000

yellow_2016_01_sample <- yellow_tripdata_2016_01 %>%
  sample_n(n)
green_2016_01_sample <- green_tripdata_2016_01 %>%
  sample_n(n)

names(yellow_2016_01_sample) <- tolower(names(yellow_2016_01_sample))
names(green_2016_01_sample) <- tolower(names(green_2016_01_sample))

save(yellow_2016_01_sample, file = "data/yellow_2016_01_sample.rda", compress = "xz")
save(green_2016_01_sample, file = "data/green_2016_01_sample.rda", compress = "xz")
