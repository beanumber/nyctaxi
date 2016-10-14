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

etl_transform.etl_nyctaxi <- function(obj, month = 1, ...) {
  message("Transforming raw data...")
  
  #directory for src and dest file
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  load_dir <- paste0(attr(obj, "load_dir"), "/month_", month)
  
  #unzip file
  filename <- paste0("trip_data_", month, ".csv.zip")
  path <- paste0(raw,"/", filename)
  unzip(path, exdir = load_dir)
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