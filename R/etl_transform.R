#' Extract data from NYC Taxi Trips
#' 
#' @import etl
#' @importFrom RCurl getURL
#' @export
#' 
#' @examples 
#' 
#' if (require(RMySQL)) {
#'  db <- src_mysql(dbname = "nyctaxi", user = "r-user", password = "mypass")
#' }
#' 
#' taxi <- etl("nyctaxi", db, dir = "~/dumps/nyctaxi/")
#' taxi %>%
#'   etl_load()
#' taxi %>%
#'   

etl_transform.etl_nyctaxi <- function(obj, ...) {
  message("Transforming raw data...")
  unzip(paste0(dir, "trip_data_1.csv.zip"))
}

#' @export

etl_load.etl_nyctaxi <- function(obj, schema = FALSE, ...) {
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  
  db <- verify_con(obj)
  if (is(db$con, "DBIConnection")) {
      message(dbRunScript(db$con, system.file("sql/nyctaxi.mysql", package = "nyctaxi"), ...))
  } else {
    stop("Invalid connection to database.")
  }
  invisible(db)
}