# https://s3.amazonaws.com/nyc-tlc/misc/taxi_zones.zip

library(rgdal)
dsn <- path.expand("data-raw/taxi_zones/")
list.files(dsn)
ogrListLayers(dsn)
ogrInfo(dsn, layer = "trails")


taxi_zones_lcc <- readOGR(dsn = dsn, layer = "taxi_zones")

# check to make sure they are all projected the same
proj4string(taxi_zones_lcc)

# fix the projection string
# proj4string(macleish_layers[[10]]) <- proj4string(macleish_layers[[1]])

taxi_zones <- spTransform(taxi_zones_lcc, CRSobj = CRS("+init=epsg:4326"))

save(taxi_zones, file = "data/taxi_zones.rda", compress = "xz")
