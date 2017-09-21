#' Extract NYC Taxi Trip Data from data from NYC Taxi & Limousine Commission
#' 
#' @import etl
#' @importFrom stringr str_pad
#' @export 
#' @details extract NYC Yellow taxi trip data from Jan 2009 and 
#' Green taxi trip data from Aug 2013 data from NYC Taxi & Limousine Commission 
#' @param years a numeric vector giving the years. The default is the most recent year.
#' @param months a numeric vector giving the months. The default is January to December.
#' @param types a character vector giving the type of taxi trip data of \code{yellow} 
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams etl::etl_extract
#' @seealso \code{\link[etl]{etl_extract}}
#' @examples 
#' 
#' taxi <- etl("nyctaxi", dir = "~/Desktop/nyctaxi/")
#' \dontrun{
#' taxi %>% 
#'    etl_extract(years = 2016, months = 1:2, types = "yellow") %>% 
#'    etl_transform(years = 2016, months = 1:2, types = "yellow") %>% 
#'    etl_load(years = 2016, months = 1:2, types = "yellow") 
#' }

etl_extract.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    types  = "yellow", ...) {
  message("Extracting raw data...")
  raw_dir <- paste0(attr(obj, "dir"), "/raw")
  
  get_dates <- function(x, years, months) {
    valid_year_month(years, months) %>%
      mutate(type = x)
  }
  
  remote <- lapply(types, get_dates, years, months) %>%
    bind_rows() %>%
    mutate(url = paste0("https://s3.amazonaws.com/nyc-tlc/trip+data/", 
                  type, "_tripdata_", year, "-", 
                  stringr::str_pad(month, 2, "left", "0"), ".csv"))
  
  etl::smart_download(obj, remote$url)

  invisible(obj)
}

