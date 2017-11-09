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
                                 types  = "yellow",
                                 transportation = "taxi", ...) {
  
  #TAXI----------------------------------------------------------------
  if (transportation == "taxi") {
    message("Loading taxi data from load directory to a sql database...")
    
    #create a list of file that the user wants to load
    remote <- get_file_path(years, months, types, path = attr(obj, "load_dir"))
    
    #create a df of file path of the files that are in the load directory
    src <- list.files(attr(obj, "load_dir"), "tripdata", full.names = TRUE)
    src <- data.frame(src)
    
    #only keep the files thst the user wants to transform
    src_small <- inner_join(remote, src, by = "src")
    
    if(nrow(src_small) == 0) {
      message("The taxi files you requested are not available in the load directory...")
    } else {
      mapply(DBI::dbWriteTable, 
             name = src_small$type, value = src_small$src, 
             MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))
    }
  }
  
  #UBER----------------------------------------------------------------
  else if (transportation == "uber") {
    
    #create a list of file that the user wants to load
    remote <- etl::valid_year_month(years, months) %>%
      mutate_(src = ~file.path(attr(obj, "load_dir"), 
                                    paste0("uber-raw-data-", tolower(month.abb[month]), substr(year,3,4), ".csv")),
              year_month = ~paste0(year, "-", month))
    
    #2015
    zipped_uberfileURL <- file.path(attr(obj, "load_dir"), "uber-raw-data-janjune-15.csv")
    raw_month_2015 <- etl::valid_year_month(years = 2015, months = 1:6) %>%
      mutate_(year_month = ~paste0(year, "-", month))
    
    #only keep the files thst the user wants to transform
    remote_small_2015 <- inner_join(raw_month_2015, remote, by = "month_begin")
    
    if(file.exists(zipped_uberfileURL) && nrow(remote_small_2015) != 0){
      message("Loading uber 2015 data from load directory to a sql database...")
      mapply(DBI::dbWriteTable, 
             name = "uber", 
             value = zipped_uberfileURL, 
             MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))
    } else if (file.exists(zipped_uberfileURL) && nrow(remote_small_2015) == 0) {
      message("The 2015 uber data you requested are not available in load directory...")
    }
    
    #2014
    #create a list of file paths of the files that are in the load directory
    src <- list.files(attr(obj, "load_dir"), "uber", full.names = TRUE)
    src <- data.frame(src)
    
    #only keep the files thst the user wants to transform
    remote_small_2014 <- inner_join(src, remote, by = "src") %>%
      mutate_(table_name = ~"uber")
    
    if(nrow(remote_small_2014) == 0) {
      message("The uber 2014 files you requested are not available in the load directory...")
    } else {
      message("Loading uber 2014 data from load directory to a sql database...")
      mapply(DBI::dbWriteTable, 
             name = remote_small_2014$table_name, 
             value = remote_small_2014$src, 
             MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))
    }
  }
  
  #LYFT----------------------------------------------------------------
  else if (transportation == "lyft") {
    message("Loading lyft data from load directory to a sql database...")
    
    #create a list of file that the user wants to load
    valid_months <- etl::valid_year_month(years, months, begin = "2015-01-01")
    
    #create a df of file path of the files that are in the load directory
    fileURL <- file.path(attr(obj, "load_dir"), "lyft.csv")
    
    if(file.exists(fileURL) && nrow(valid_months) != 0) {
      mapply(DBI::dbWriteTable, 
             name = "lyft", value = fileURL, 
             MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))
    } else {
      message("The lyft files you requested are not available in the load directory...")
    }
  }
  else{
    warning("The transportation you specified does not exist...")
  }
  
  invisible(obj)
}
