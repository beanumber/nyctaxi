#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @importFrom DBI dbWriteTable
#' @import etl
#' 
etl_load.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                 months = 1:12, 
                                 types  = "yellow", ...) {
  
  #get a list of all possible combinations of years, months, and types variables
  get_dates <- function(x, years, months) {
    valid_year_month(years, months) %>%
      mutate(type = x)
  }
  
  #create a list of file names
  remote <- lapply(types, get_dates, years, months) %>%
    bind_rows() %>%
    mutate(file = paste0(type, "_tripdata_", year, "-", 
                        stringr::str_pad(month, 2, "left", "0"), ".csv"), 
           src = file.path(attr(obj, "load_dir"), file))
  
  smart_upload(obj, src = remote$src, tablenames = remote$type)
  invisible(obj)
}
