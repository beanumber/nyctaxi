#' @import etl
#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @importFrom 
#' @details transform NYC Yellow taxi trip data from raw directory to load directory
#' @param etl an etl object 
#' @param years a numeric vector giving the years. The default is the most recent year.
#' @param months a numeric vector giving the months. The default is January to December.
#' @param types a character vector giving the type of taxi trip data of \code{yellow} 
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_transform}}


etl_extract.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    types  = "yellow", ...) {
  message("Extracting raw data...")
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  
  remote <- get_file_path(years, months, types, path = "https://s3.amazonaws.com/nyc-tlc/trip+data/") 
  
  # get_dates <- function(x, years, months) {
  #   valid_year_month(years, months) %>%
  #     mutate(type = x)
  # }
  # 
  # remote <- lapply(types, get_dates, years, months) %>%
  #   bind_rows() %>%
  #   mutate(url = paste0("https://s3.amazonaws.com/nyc-tlc/trip+data/", 
  #                 type, "_tripdata_", year, "-", 
  #                 stringr::str_pad(month, 2, "left", "0"), ".csv"))
  
  etl::smart_download(obj, remote$url)
  
  invisible(obj)
}

