#' @import etl
#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @details transform NYC Yellow taxi trip data from raw directory to load directory
#' @param etl an etl object 
#' @param years a numeric vector giving the years. The default is the most recent year.
#' @param months a numeric vector giving the months. The default is January to December.
#' @param types a character vector giving the type of taxi trip data of \code{yellow} 
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_transform}}


etl_transform.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    types  = "yellow", ...) {
  
  message("Transforming data in raw directory into load directory...")
  
  #create a df of file path of the files that the user wants to transform
  remote <- get_file_path(years, months, types, path = paste0(attr(obj, "dir"), "/raw/")) 
  
  #create a df of file path of the files that are in the raw directory
  src <- list.files(attr(obj, "raw_dir"), "\\.csv", full.names = TRUE)
  source <- data.frame(src)
  
  #only keep the files thst the user wants to transform
  remote_small <- inner_join(remote, source, by = "src")
  
  lcl <- file.path(attr(obj, "load_dir"), basename(remote_small$src))
  
  file.copy(from = remote_small$src, to = lcl)
  
  invisible(obj)
}

