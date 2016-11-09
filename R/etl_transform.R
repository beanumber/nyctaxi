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
#' @importFrom DBI dbWriteTable
#' @import etl
#' 
etl_load.etl_nyctaxi <- function(obj, schema = FALSE, type="yellow",year="2016", month=1,...) {
  #raw_dir <- paste0(attr(obj, "dir"), "/raw")
  
  #extract only the data of the month we want
  #load_dir <- attr(obj, "load_dir")
  #filename <- paste0("trip_data_", month, ".csv")
  #path <- file.path(load_dir,filename)
  #trip <- read.csv(path)
  path <- paste0(raw_dir, "/",color,"/",type,"_tripdata_",year,"-0",month,".csv")
  #db <- etl:::verify_con(obj) --> encountered error as this is an S3 object
  #db <- dbConnect(RSQLite::RSQLite(),":memory:") alternative 

  #DBI::dbWriteTable()
  DBI:::dbWriteTable(conn=obj$con, "trips",path)
  invisible(db)
}