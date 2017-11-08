#' @import etl
#' @importFrom rlang .data
#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @details transform NYC Yellow taxi trip data from raw directory to load directory
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_transform}}


etl_transform.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    types  = "yellow", 
                                    transportation = "taxi",...) {
  
  
  
  #TAXI----------------------------------------------------------------
  if (transportation == "taxi") {
    message("Transforming taxi data from raw to load directory...")
    #create a df of file path of the files that the user wants to transform
    remote <- get_file_path(years, months, types, path = attr(obj, "raw_dir")) 
    remote_green <- remote %>% 
      filter_(~type == "green")
    
    #create a df of file path of the files that are in the raw directory
    src <- list.files(attr(obj, "raw_dir"), "\\.csv", full.names = TRUE)
    
    #only keep the files thst the user wants to transform
    src_small <- intersect(src, remote$src)
    
    src_small_green <- intersect(src, remote_green$src)
    
    if (length(src_small) == 0) {
      message("The files you requested are not available in the raw directory.")
    } else {
      #find the load directory
      lcl <- file.path(attr(obj, "load_dir"), basename(src_small))
      lcl_green <- file.path(attr(obj, "raw_dir"), basename(src_small_green))
      
      # check that the type is green, and then do this
      if (.Platform$OS.type == "unix"){
        cmds <- paste("sed -i -e '2d'", lcl_green)
        lapply(cmds, system)
      } else {
        message("Windows system does not currently support this function...")
      }
      
      #copy the files in the raw directory and paste them to the load directory
      file.copy(from = src_small, to = lcl)
    }
  }
  
  
  #UBER----------------------------------------------------------------
  else if (transportation == "uber") {
    message("Transforming uber data from raw to load directory...")
    zipped_uberfileURL <- file.path(attr(obj, "raw_dir"), "uber-raw-data-janjune-15.csv.zip")
    #creat a list of 2014 uber data file directory
    uber14_list <- list.files(path = attr(obj, "raw_dir"), pattern = "14.csv")
    
    if(file.exists(zipped_uberfileURL)){
      unzip(zipped_uberfileURL, exdir = file.path(attr(obj, "load_dir"),"uber-raw-data-2015_01-06.csv"))
    } 
    else if( length(uber14_list) != 0){
      raw_file_path <- data.frame(uber14_list) %>%
        mutate_(basename = ~attr(obj, "raw_dir")) %>%
        mutate_(raw_file_dir = ~paste0(basename, "/",uber14_list))
      
      load_file_path <- data.frame(uber14_list) %>%
        mutate_(basename = ~attr(obj, "load_dir")) %>%
        mutate_(raw_file_dir = ~paste0(basename, "/",uber14_list))
      
      #copy the files in the raw directory and paste them to the load directory
      file.copy(from = raw_file_path$raw_file_dir, to = load_file_path$raw_file_dir)
    }
    else {
      message("There is no 2015 Uber data in the raw directory...")
    }
  }
  
  
  #LYFT----------------------------------------------------------------
  else if (transportation == "lyft") {
    message("Transforming lyft data from raw to load directory...")
    fileURL <- file.path(attr(obj, "raw_dir"), "juxc-sutg.csv")
    if(file.exists(fileURL)){
      file.rename(fileURL, file.path(attr(obj, "load_dir"), "lyft.csv"))
    } else {
      message("There is no Lyft data in the raw directory...")
    }
  }
  
  else{
    warning("The transportation you specified does not exist...")
  }
  
  #is it caused by invisible obj?
  invisible(obj)
}

