#' @import etl
#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @importFrom DBI dbWriteTable
#' @details load NYC Yellow taxi trip data from load directory into a sql database, 
#' the default is a sqlite database
#' @param obj an etl object 
#' @param years a numeric vector giving the years. The default is the most recent year.
#' @param months a numeric vector giving the months. The default is January to December.
#' @param types a character vector giving the type of taxi trip data of \code{yellow} 
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_load}}

 
etl_load.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                 months = 1:12, 
                                 types  = "yellow", ...) {
  
  remote <- get_file_path(years, months, types, path = attr(obj, "load_dir")) 
  #get a list of all possible combinations of years, months, and types variables
  # get_dates <- function(x, years, months) {
  #   valid_year_month(years, months) %>%
  #     mutate(type = x)
  # }
  # 
  # #create a list of file names
  # remote <- lapply(types, get_dates, years, months) %>%
  #   bind_rows() %>%
  #   mutate(file = paste0(type, "_tripdata_", year, "-", 
  #                       stringr::str_pad(month, 2, "left", "0"), ".csv"), 
  #          src = file.path(attr(obj, "load_dir"), file))
  
  smart_upload(obj, src = remote$src, tablenames = remote$type)
  invisible(obj)
}
