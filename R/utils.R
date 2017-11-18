#' Utility function that generates file paths
#' @description Take in arguments common to all three functions 
#' (e.g. years, months, types) as well as the stem of the URL and 
#' return the matched file paths.
#' @inheritParams etl_extract.etl_nyctaxi
#' @param path path to desired file
#' @export
#' @examples 
#' get_file_path(2017, 1:6, "yellow", "~/")
#' 

get_file_path <- function(years, months, types, path) {
  
  get_dates <- function(types, years, months) {
    valid_year_month(years, months) %>%
      mutate(type = types)
  }
  
  lapply(types, get_dates, years, months) %>%
    bind_rows() %>%
    mutate_(src = ~file.path(path, paste0(type, "_tripdata_", year, "-", 
                        stringr::str_pad(month, 2, "left", "0"), ".csv")))
}

download_nyc_data <- function(obj, year, n, names, ...) {
  url <- paste0("https://data.cityofnewyork.us/resource/edp9-qgv4.csv?years=",
                year,"&$limit=", n)
  lcl <- file.path(attr(obj, "raw"), names)
  downloader::download(url, destfile = lcl, ...)
  lcl
}
