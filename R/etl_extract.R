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

etl_extract.etl_nyctaxi <- function(obj, month = 1, trip=TRUE,fare=FALSE,...) {
  message("Extracting raw data...")
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  if (!dir.exists(raw_dir)) {
    dir.create(raw_dir)
  }
  month <- as.character(month)
  if(trip) {
    local <- paste0(raw_dir, "/trip/trip_data_",month,".csv.zip")
    remote <- paste0("http://nyctaxitrips.blob.core.windows.net/data/trip_data_",month,".csv.zip")
    etl::smart_download(remote, local)
    invisible(obj)
  }
  if(fare) {
    local <- paste0(raw_dir, "/fare/trip_fare_",month,".csv.zip")
    remote <- paste0("http://nyctaxitrips.blob.core.windows.net/data/trip_fare_",month,".csv.zip")
    etl::smart_download(remote, local)
    invisible(obj)
  }
}