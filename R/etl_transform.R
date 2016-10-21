#' @rdname etl_extract.etl_nyctaxi
#' @export  

etl_transform.etl_nyctaxi <- function(obj, month = 1, ...) {
  message("Transforming raw data...")
  
  #directory for src and dest file
  raw_dir <- attr(obj, "raw_dir")
  load_dir <- attr(obj, "load_dir")
  
  #unzip file
  filename <- paste0("trip_data_", month, ".csv.zip")
  path <- file.path(raw_dir, filename)
  unzip(path, exdir = load_dir)
}

#' @export
#' @rdname etl_extract.etl_nyctaxi
#' 
#' 
etl_load.etl_nyctaxi <- function(obj, schema = FALSE, ...) {
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  
  db <- verify_con(obj)
  if (is(db$con, "DBIConnection")) {
      message(dbRunScript(db$con, system.file("sql/nyctaxi.mysql", package = "nyctaxi"), ...))
  } else {
    stop("Invalid connection to database.")
  }
  invisible(db)
  
  #DBI::dbWriteTable()
}