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
#' @param types a character vector giving the type of taxi trip data of \code{yellow}.
#' @param transportation a character variable giving the type of data the user wants to download. 
#' There are three options: taxi, uber, and lyft.
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
#'    etl_extract(years = 2015, months = 1, types = c("green"), transportation = "lyft") %>% 
#'    etl_transform(years = 2015, months = 1, types = c("green")) %>% 
#'    etl_load(years = 2015, months = 1, types = c("green")) 
#' }

etl_extract.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    types  = "yellow", transportation = "taxi",...) {
  message("Extracting raw data...")
  
  #choose transportation type
  #TAXI-----------------------------------------------------------------------
  if (transportation == "taxi") {
    remote <- get_file_path(years, months, types, path = "https://s3.amazonaws.com/nyc-tlc/trip+data") 
    etl::smart_download(obj, remote$src, ...)} 
  
  #UBER-----------------------------------------------------------------------
  else if (transportation == "uber") {
    raw_month <- etl::valid_year_month(years = 2014, months = 4:9)
    path = "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data"
    remote <- etl::valid_year_month(years, months)
    
    if (2015 %in% years) {
      #download 2015 data
      etl::smart_download(obj, "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-janjune-15.csv.zip")
      
      #download other data
      small <- inner_join(remote, raw_month)
      small <- small %>%
        filter_(year != 2015) %>%
        mutate_(month_abb = ~tolower(month.abb[month]),
                src = ~file.path(path, paste0("uber-raw-data-",month_abb,
                                              substr(year,3,4),
                                              ".csv")))
      etl::smart_download(obj, small$src) 
    } else {
      #file paths
      small <- inner_join(remote, raw_month)
      small <- small %>%
        mutate_(month_abb = ~tolower(month.abb[month]),
                src = ~file.path(path, paste0("uber-raw-data-",month_abb,
                                              substr(year,3,4),
                                              ".csv")))
      #download uber data
      etl::smart_download(obj, small$src) }
    } 
  
  #LYFT-----------------------------------------------------------------------
  else if (transportation == "lyft") {
    
    #check if the week is valid
    valid_months <- etl::valid_year_month(years, months, begin = "2015-01-01")
    
    #file path
    base_url = "https://data.cityofnewyork.us/resource/juxc-sutg.csv"
    etl::smart_download(obj,base_url, ...)
  } else {
    warning("The transportation you specified does not exist...")
  }
  

  invisible(obj)
}

