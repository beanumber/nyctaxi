#' Extract data from NYC Taxi Trips
#' 
#' @import etl
#' @importFrom stringr str_pad
#' @export 
#' @details extract data from NYC taxi trips 
#' @param year a numeric vector giving the years
#' @param month a numeric vector giving the months
#' @param types a character vector of \code{yellow} and/or \code{green}. The default 
#' is \code{yellow}.
#' @inheritParams etl::etl_extract
#' @seealso \code{\link[etl]{etl_extract}}
#' @examples 
#' 
#' taxi <- etl("nyctaxi", dir = "~/Desktop/nyctaxi/")
#' \dontrun{
#' taxi %>% 
#'    etl_extract() %>% 
#'    etl_transform() %>% 
#'    etl_load() 
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
                  type, "_tripdata_", year, "-", stringr::str_pad(month, 2, "left", "0"), ".csv"))
  
  etl::smart_download(obj, remote$url)

  invisible(obj)
}

#do we want to change type into types? 
#to download both yellow and green cabs data by one commend?

