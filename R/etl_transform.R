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
  
  message("Transforming data from raw directory to load directory...")
  
  #create a df of file path of the files that the user wants to transform
  remote <- get_file_path(years, months, types, path = attr(obj, "raw_dir")) 
  
  #create a df of file path of the files that are in the raw directory
  src <- list.files(attr(obj, "raw_dir"), "\\.csv", full.names = TRUE)
  
  #only keep the files thst the user wants to transform
  src_small <- intersect(src, remote$src)
  
  #find the load directory
  lcl <- file.path(attr(obj, "load_dir"), basename(src_small))
  
  file.copy(from = src_small, to = lcl)
  # check that second line IS blank, and the do this
  cmds <- paste("sed -i -e '2d'", lcl)
  lapply(cmds, system)
  
  invisible(obj)
}

