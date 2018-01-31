#' Utility function that download NYC Open Data Open Portal data
#' @description Takes in arguments, sych as obj, year, names, 
#' as well as the stem of the URL and returns the matched file paths.
#' @inheritParams etl_extract.etl_nyctaxi
#' @param url   base url to desired file
#' @param n number of observations
#' @param names names of the destination files
#' @importFrom downloader download
#' @export
#' @examples 
#' \dontrun{
#' taxi <- etl("nyctaxi", dir = "~/Desktop/nyctaxi")
#' base_url <- "https://data.cityofnewyork.us/resource/edp9-qgv4.csv"
#' download_nyc_data(taxi, base_url, 2015, 100, "2015") 
#' }
#' 

download_nyc_data <- function(obj, url, years, n, names, ...) {
  url <- paste0(url,"?years=",
                years,"&$limit=", n)
  lcl <- file.path(attr(obj, "raw"), names)
  downloader::download(url, destfile = lcl, ...)
  lcl
}
