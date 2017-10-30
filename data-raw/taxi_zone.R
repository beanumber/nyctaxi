#get taxi zone info from Github Repository 
download.file("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/taxi-zone-lookup.csv",
              "~/Desktop/taxi_zone_lookup.csv")
taxi_zone_lookup <- read.csv("~/Desktop/taxi_zone_lookup.csv")
save(taxi_zone_lookup, file = "taxi_zone_lookup.rda")
