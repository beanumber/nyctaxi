#' Extract NYC Taxi Trip Data from data from NYC Taxi & Limousine Commission
#' 
#' @import etl
#' @importFrom stringr str_pad
#' @export 
#' @details extract NYC Yellow taxi trip data from Jan 2009 and 
#' Green taxi trip data from Aug 2013 data from NYC Taxi & Limousine Commission 
#' @param obj an etl object 
#' @param years a numeric vector giving the years. The default is the most recent year.
#' @param months a numeric vector giving the months. The default is January to December.
#' @param types a character vector giving the type of taxi trip data of \code{yellow} 
#' @param ... arguments passed to \code{\link[etl]{smart_download}}
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_extract}}
#' @examples 
#' 
#' 
#' \dontrun{
#' taxi <- etl("nyctaxi", dir = "~/Desktop/nyctaxi/")
#' taxi %>% 
#'    etl_extract(years = 2016, months = 1:2, types = c("yellow","green")) %>% 
#'    etl_transform(years = 2016, months = 1:2, types = c("yellow","green")) %>% 
#'    etl_load(years = 2016, months = 1:2, types = c("yellow","green")) 
#' }

etl_extract.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    types  = "yellow", ...) {
  message("Extracting raw data...")
  
  remote <- get_file_path(years, months, types, path = "https://s3.amazonaws.com/nyc-tlc/trip+data") 
    
  etl::smart_download(obj, remote$src, ...)

  invisible(obj)
}

