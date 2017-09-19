require(etl)
year <- 2016
month <- 1
month <- 2

#extract
obj <- etl("nyctaxi", dir = "~/Desktop/nyctaxi/")
raw_dir <- paste0(attr(obj, "dir"), "/raw")
remote <- paste0("https://s3.amazonaws.com/nyc-tlc/trip+data/", 
                 type, "_tripdata_", year, "-0", month, ".csv")
remote

etl::smart_download(obj, remote)
#is there a way to alarm users once the download is compeleted


#transform.default
src <- list.files(attr(obj, "raw_dir"), "\\.csv", full.names = TRUE)
src
lcl <- file.path(attr(obj, "load_dir"), basename(src))
lcl
file.copy(from = src, to = lcl)

etl::etl_transform(obj)

#problem
#when i run them saperatly it works, but together it did not work???




