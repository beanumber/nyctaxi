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
#' @param transportation a character variable of giving the type of data the user wants to download. 
#' There are three options: taxi, uber, and lyft, and users can only choose one transportation at a time.
#' @param ... arguments passed to \code{\link[etl]{smart_download}}
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_extract}}
#' @examples 
#' 
#' 
#' \dontrun{
#' taxi <- etl("nyctaxi", dir = "~/Desktop/nyctaxi")
#' taxi %>% 
#'    etl_extract(years = 2014:2016, months = 1, types = c("green"), transportation = "taxi") %>% 
#'    etl_transform(years = 2015, months = 1:12, types = c("green"), transportation = "taxi") %>% 
#'    etl_load(years = 2015, months = 1:12, types = c("green"), transportation = "taxi") 
#' }

etl_extract.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    types  = "yellow", transportation = "taxi",...) {
  
  
  #choose transportation type
  #TAXI-----------------------------------------------------------------------
  if (transportation == "taxi") {
    message("Extracting raw taxi data...")
    remote <- get_file_path(years, months, types, 
                            path = "https://s3.amazonaws.com/nyc-tlc/trip+data") 
    tryCatch(expr = etl::smart_download(obj, remote$src, ...),
             error = function(e){warning(e)}, 
             finally = warning("Only the following data are availabel on TLC:
                               Green taxi data: 2013 Aug - last month
                               Yellow taxi data: 2009 Jan - last month")
             )
    
    } 
  
  #UBER-----------------------------------------------------------------------
  else if (transportation == "uber") {
    message("Extracting raw uber data...")
    raw_month_2014 <- etl::valid_year_month(years = 2014, months = 4:9)
    raw_month_2015 <- etl::valid_year_month(years = 2015, months = 1:6)
    raw_month <- bind_rows(raw_month_2014, raw_month_2015)
    path = "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data"
    remote <- etl::valid_year_month(years, months)
    remote_small <- intersect(raw_month, remote)
    
    if (2015 %in% remote_small$year && !(2014 %in% remote_small$year)){
      #download 2015 data
      message("Downloading Uber 2015 data...")
      etl::smart_download(obj, "https://github.com/fivethirtyeight/uber-tlc-foil-response/raw/master/uber-trip-data/uber-raw-data-janjune-15.csv.zip")
      }
    else if (2015 %in% remote_small$year && 2014 %in% remote_small$year) {
      #download 2015 data
      message("Downloading Uber 2015 data...")
      etl::smart_download(obj, "https://github.com/fivethirtyeight/uber-tlc-foil-response/raw/master/uber-trip-data/uber-raw-data-janjune-15.csv.zip")
      
      #download 2014 data
      small <- remote_small %>%
        filter_(~year == 2014) %>%
        mutate_(month_abb = ~tolower(month.abb[month]),
                src = ~file.path(path, paste0("uber-raw-data-",month_abb,
                                              substr(year,3,4),
                                              ".csv")))
      message("Downloading Uber 2014 data...")
      etl::smart_download(obj, small$src) 
    } else if (2014 %in% remote_small$year && !(2015 %in% remote_small$year)) {
      message("Downloading Uber 2014 data...")
      #file paths
      small <- remote_small %>%
        mutate_(month_abb = ~tolower(month.abb[month]),
                src = ~file.path(path, paste0("uber-raw-data-",month_abb,
                                              substr(year,3,4),
                                              ".csv")))
      etl::smart_download(obj, small$src) }
    else {
      warning("The Uber data you requested are not currently available. Only data from 2014/04-2014/09 and 2015/01-2015/06 are available...")
    }
    } 
  
  #LYFT-----------------------------------------------------------------------
  else if (transportation == "lyft") {
    message("Extracting raw lyft data...")
    #check if the week is valid
    valid_months <- etl::valid_year_month(years, months, begin = "2015-01-01")
    base_url = "https://data.cityofnewyork.us/resource/edp9-qgv4.csv"
    valid_months <- valid_months %>%
      mutate_(lcl = ~paste0(attr(obj, "raw_dir"), 
                            "/lyft_", valid_months$year, ".csv"))%>%
      mutate_(src = ~paste0(base_url,
                                          "?years=", 
                                          valid_months$year,
                                          "&$limit=50000")) %>%
      mutate_(new_filenames = ~paste0("lyft-", year, ".csv")) %>%
      mutate_(drop = TRUE)
    #only keep one data set per year
    year <- valid_months[1,1]
    n <- nrow(valid_months)
    for (i in 2:n) {
      if(year == valid_months[i-1,1]) {
        valid_months[i,8] <- FALSE
        year <- valid_months[i+1,1]
      } else {
        valid_months[i,8] <- TRUE
        year <- valid_months[i+1,1]
      }
    }
    row_to_keep = valid_months$drop
    valid_months <- valid_months[row_to_keep,]
    #download lyft files, try two different methods
    download_nyc_data(obj, valid_months$year, n = 50000, 
                      names = valid_months$new_filenames)
    # first_try<-tryCatch(
    #   download_nyc_data(obj, valid_months$year, n = 50000, 
    #                     names = valid_months$new_filenames, method = "curl"),
    #   error = function(e){warning(e)},finally = 'method = "curl" fails')
    # 
    # ifelse(first_try[[1]] == 0, print("Download succeeded."), 
    #        tryCatch(download_nyc_data(obj, valid_months$year, n = 50000, 
    #                                   names = valid_months$new_filenames, method = "auto"),
    #                 error = function(e){warning(e)},finally = 'method = "auto" fails'))
  }
    invisible(obj)
}

