#' @rdname etl_extract.etl_nyctaxi
#' @export  


etl_transform.etl_nyctaxi <- function(obj, year = 2016, month = 1, type = "yellow", ...) {
  message("Transforming raw data...")
  
  #directory for src and dest file
  raw_dir <- attr(obj, "raw_dir")
  load_dir <- attr(obj, "load_dir")
 
  #copy file from raw to load
  local <- paste0(raw_dir, "/", type, "_tripdata_", year, "-0", month, ".csv")
  file.copy(from = local, to = load_dir)
  
  #unzip(path, exdir = load_dir)
  invisible(obj)
}

#---------------------------------------------------------------------------
#priscilla
# try to use match_files_by_year_months
etl::match_files_by_year_months(src,"\\.csv", years = 2016, months = 1)
src

require(dplyr)
year <- 2016
month <- 1

files <- src
basename(src)
pattern <- "2.*-.*"

#regular expressions are hard to write
file_date = etl::extract_date_from_filename(files, pattern)
file_date

file_df <- data.frame(filename = files,
                      file_date = etl::extract_date_from_filename(files,
                                                             pattern)) %>%
  mutate_(file_year = ~lubridate::year(file_date),
          file_month = ~lubridate::month(file_date))

valid <- valid_year_month(years, months)

good <- file_df %>%
  left_join(valid, by = c("file_year" = "year", "file_month" = "month")) %>%
  filter_(~!is.na(month_begin))

return(as.character(good$filename))

