#' Utility function that generates file paths
#' @description Take in arguments common to all three functions 
#' (e.g. years, months, types) as well as the stem of the URL and 
#' return the matched file paths.
#' @param years 
#' @param months 
#' @param types 
#' @param path 
#' @export

get_file_path <- function(years, months, types, path) {
  
  get_dates <- function(types, years, months) {
    valid_year_month(years, months) %>%
      mutate(type = types)
  }
  
  lapply(types, get_dates, years, months) %>%
    bind_rows() %>%
    mutate(src = file.path(path, paste0(type, "_tripdata_", year, "-", 
                        stringr::str_pad(month, 2, "left", "0"), ".csv")))
}


