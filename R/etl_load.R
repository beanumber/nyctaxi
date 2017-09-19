#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @importFrom DBI dbWriteTable
#' @import etl
#' 
etl_load.etl_nyctaxi <- function(obj, years = "2016", months = 1, types = "yellow", ...) {
  match_files_by_year_months(list.files(attr(obj, "load_dir")),
                             pattern = "yellow_tripdata_%Y_%m.+\\.csv", years = years, months = months)
  src <- list.files(attr(obj, "load_dir"), pattern = "yellow.+\\.csv", full.names = TRUE)
  smart_upload(obj, src, tablenames = "yellow")
  invisible(obj)
}
