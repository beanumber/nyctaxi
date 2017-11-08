taxi <- etl("nyctaxi", dir = "~/Dropbox/nyctaxi/")
taxi %>%
  etl_extract(years = 2016, months = 1, types = c("yellow","green"), transportation = "taxi") %>%
  etl_transform(years = 2016, months = 1, types = c("yellow","green"), transportation = "taxi")

yellow_tripdata_2016.01 <- read.csv("~/Dropbox/nyctaxi/raw/yellow_tripdata_2016-01.csv")
green_tripdata_2016.01 <- read.csv("~/Dropbox/nyctaxi/raw/green_tripdata_2016-01.csv")

set.seed(5)

# take a random sample of size 1000 from a dataset  
# sample without replacement
yellow_2016_01_sample <- yellow_tripdata_2016.01[sample(1:nrow(yellow_tripdata_2016.01), 100, replace=FALSE),]
green_2016_01_sample <- green_tripdata_2016.01[sample(1:nrow(green_tripdata_2016.01), 100, replace=FALSE),]

names(yellow_2016_01_sample) <- tolower(names(yellow_2016_01_sample))
names(green_2016_01_sample) <- tolower(names(green_2016_01_sample))

save(yellow_2016_01_sample, file ="yellow_2016_01_sample.rda")
save(green_2016_01_sample, file ="green_2016_01_sample.rda")
