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

etl_extract.etl_nyctaxi <- function(obj, year = 2016, month = 1, type  = "yellow", ...) {
  message("Extracting raw data...")
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  
  remote <- paste0("https://s3.amazonaws.com/nyc-tlc/trip+data/", 
                   type, "_tripdata_", year, "-0", month, ".csv")
  etl::smart_download(obj, remote)

  invisible(obj)
}