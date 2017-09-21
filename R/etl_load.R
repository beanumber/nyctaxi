#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @importFrom DBI dbWriteTable
#' @import etl
#' 
etl_load.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                 months = 1:12, 
                                 types  = "yellow", ...) {
  
  #get a list of all possible combinations of years, months, and types variables
  get_dates <- function(x, years, months) {
    valid_year_month(years, months) %>%
      mutate(type = x)
  }
  
  #create a list of file names
  remote <- lapply(types, get_dates, years, months) %>%
    bind_rows() %>%
    mutate(file = paste0(type, "_tripdata_", year, "-", 
                        stringr::str_pad(month, 2, "left", "0"), ".csv")) %>%
    mutate(src = paste0(attr(obj, "load_dir"),file))
  
  #create a dataset for each color
  file_by_color <- function(data,type) {
    df <- data %>%
      filter(file == paste0(type, "_tripdata_", year, "-", 
                            stringr::str_pad(month, 2, "left", "0"), ".csv"))
    assign(paste(type,"tripdata",sep = "-"), df )
    invisible(df)
  }
  
  #check whether the file has already been loaded or not
  match_files_by_year_months(list.files(attr(obj, "load_dir")),
                             pattern = "yellow_tripdata_%Y_%m.+\\.csv", 
                             years = years, months = months)
  
  #load the files in the color df
  load_color_file <- function(data,type) {
    #src <- list.files(attr(obj, "load_dir"), pattern = "yellow.+\\.csv", full.names = TRUE)
    smart_upload(obj, src, tablenames = paste(type,"tripdata",sep = "-"))
  }
    
  invisible(obj)
}
