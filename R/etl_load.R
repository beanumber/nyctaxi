#' @import etl dplyr
#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @importFrom DBI dbWriteTable
#' @details load NYC Yellow taxi trip data from load directory into a sql database, 
#' the default is a sqlite database
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_load}}

 
etl_load.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                 months = 1:12, 
                                 type  = "yellow", ...) {
  #TAXI YELLOW----------------------------------------------------------------
  taxi_yellow <- function(obj, years, months) {
    #create a list of file that the user wants to load
    remote <- etl::valid_year_month(years, months, begin = "2009-01-01") %>%
      mutate_(src = ~file.path(attr(obj, "load_dir"), 
                               paste0("yellow", "_tripdata_", year, "-",
                                      stringr::str_pad(month, 2, "left", "0"), ".csv"))) 
    #create a df of file path of the files that are in the load directory
    src <- list.files(attr(obj, "load_dir"), "tripdata", full.names = TRUE)
    src <- data.frame(src)
    #only keep the files thst the user wants to transform
    src_small <- inner_join(remote, src, by = "src")
    if(nrow(src_small) == 0) {
      message("The taxi files you requested are not available in the load directory...")
    } else {
      message("Loading taxi data from load directory to a sql database...")
      mapply(DBI::dbWriteTable, 
             name = src_small$type, value = src_small$src, 
             MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))}}
  #TAXI GREEN----------------------------------------------------------------
  taxi_green <- function(obj, years, months) {
    #create a list of file that the user wants to load
    remote <- etl::valid_year_month(years, months, begin = "2013-08-01") %>%
      mutate_(src = ~file.path(attr(obj, "load_dir"), 
                               paste0("green", "_tripdata_", year, "-",
                                      stringr::str_pad(month, 2, "left", "0"), ".csv")))
    #create a df of file path of the files that are in the load directory
    src <- list.files(attr(obj, "load_dir"), "tripdata", full.names = TRUE)
    src <- data.frame(src)
    #only keep the files thst the user wants to transform
    src_small <- inner_join(remote, src, by = "src")
    if(nrow(src_small) == 0) {
      message("The taxi files you requested are not available in the load directory...")
    } else {
      message("Loading taxi data from load directory to a sql database...")
      mapply(DBI::dbWriteTable, 
             name = src_small$type, value = src_small$src, 
             MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))}}
  #UBER----------------------------------------------------------------
  uber <- function(obj) {
    uberfileURL <- file.path(attr(obj, "load_dir"), "uber.csv")
    if(file.exists(uberfileURL)) {
      message("Loading uber data from load directory to a sql database...")
      mapply(DBI::dbWriteTable, name = remote_small_2014$table_name, 
             value = remote_small_2014$src, 
             MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))
    } else {
      message("There is no uber data in the load directory...")}}
  #LYFT----------------------------------------------------------------
  lyft <- function(obj, years, months){
    message("Loading lyft data from load directory to a sql database...")
    #create a list of file that the user wants to load
    valid_months <- etl::valid_year_month(years, months, begin = "2015-01-01")
    src <- list.files(attr(obj, "load_dir"), "lyft", full.names = TRUE)
    src_year <- valid_months %>% distinct_(~year)
    remote <- data_frame(src)
    remote <- remote %>% mutate_(tablename = ~"lyft", year = ~substr(basename(src),6,9))
    class(remote$year) <- "numeric"
    remote <- inner_join(remote,src_year, by = "year" )
    if(nrow(remote) != 0) {
      write_data <- function(...) {
        lapply(remote$src, FUN = DBI::dbWriteTable, conn = obj$con, 
               name = "lyft", append = TRUE, sep = "|", ... = ...)}
      write_data(...)
    } else {
      message("The lyft files you requested are not available in the load directory...")}}
  
  if (type == "yellow"){taxi_yellow(obj, years, months)} 
  else if (type == "green"){taxi_green(obj, years, months)}
  else if (type == "uber"){uber(obj)}
  else if (type == "lyft"){lyft(obj, years, months)}
  else {message("The type you chose does not exit...")}
  invisible(obj)
}
