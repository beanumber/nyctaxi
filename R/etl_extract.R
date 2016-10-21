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

etl_extract.etl_nyctaxi <- function(obj, ...) {
  message("Extracting raw data...")
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  if (!dir.exists(raw_dir)) {
    dir.create(raw_dir)
  }
  
  local <- paste0(raw_dir, "/trip_data_1.csv.zip")
  remote <- "https://nyctaxitrips.blob.core.windows.net/data/trip_data_1.csv.zip"
  etl::smart_download(remote, local)
  invisible(obj)
}