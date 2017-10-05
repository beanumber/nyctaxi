#' @import etl
#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @importFrom DBI dbWriteTable
#' @details load NYC Yellow taxi trip data from load directory into a sql database, 
#' the default is a sqlite database
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_load}}

 
etl_load.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                 months = 1:12, 
                                 types  = "yellow", ...) {
  #deal with the blank row in green taxi cab data
  remote <- get_file_path(years, months, types, path = attr(obj, "load_dir")) %>%
    mutate(skip = ifelse(type == "green", 1, 0))
  
  #create a df of file path of the files that are in the load directory
  src <- list.files(attr(obj, "load_dir"), "\\.csv", full.names = TRUE)
  src <- data.frame(src)
  
  #only keep the files thst the user wants to transform
  src_small <- inner_join(remote, src, by = "src")
  
  mapply(DBI::dbWriteTable, 
         name = src_small$type, value = src_small$src, 
         MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))
  
  #smart_upload(obj, src = remote$src, tablenames = remote$type)
  
  invisible(obj)
}
