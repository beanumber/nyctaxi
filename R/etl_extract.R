#' Extract data from NYC Taxi Trips
#' 
#' @import etl
#' @export 
#' @details extract data from NYC taxi trips 
#' @inheritParams etl::etl_extract
#' @seealso \code{\link[etl]{etl_extract}}
#' @examples 
#' 
#' taxi <- etl("nyctaxi", dir = "~/dumps/nyctaxi/")
#' taxi %>% 
#'    etl_extract() %>% 
#'    etl_transform() %>% 
#'    etl_load() 

etl_extract.etl_nyctaxi <- function(obj, month = 1, color = "yellow",fare=FALSE,...) {
  message("Extracting raw data...")
  #message("Choose color yellow or green for trip data...")
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  if (!dir.exists(raw_dir)) {
    dir.create(raw_dir)
  }
  month <- as.character(month)
  local <- paste0(raw_dir, "/trip/",color,"_tripdata_",year,"-0",month,".csv")
  remote <- paste0("https://s3.amazonaws.com/nyc-tlc/trip+data/",color,"_tripdata_",year,"-0",month,".csv")
  smart_download(remote, local,method="curl")
  #if(fare) {
  # local <- paste0(raw_dir, "/fare/trip_fare_",month,".csv.zip")
  #  remote <- paste0("http://nyctaxitrips.blob.core.windows.net/data/trip_fare_",month,".csv.zip")
  #  download.file(remote, local,method="curl")
  #}
  invisible(obj)
}