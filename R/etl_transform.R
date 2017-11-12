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
      filter_(~type == "green")%>%
      filter_(~year != 2015)
    
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
    #creat a list of 2014 uber data file directory
    uber14_list <- list.files(path = attr(obj, "raw_dir"), pattern = "14.csv")
    remote <- etl::valid_year_month(years, months) %>%
      mutate_(month_abb = ~tolower(month.abb[month]),
              src = ~file.path(attr(obj, "raw_dir"), paste0("uber-raw-data-",month_abb,
                                            substr(year,3,4),
                                            ".csv")))
    remote_small_2014 <- intersect(uber14_list, basename(remote$src))
    
    #2015
    zipped_uberfileURL <- file.path(attr(obj, "raw_dir"), "uber-raw-data-janjune-15.csv.zip")
    raw_month_2015 <- etl::valid_year_month(years = 2015, months = 1:6)
    remote_2015 <- etl::valid_year_month(years, months)
    remote_small_2015 <- inner_join(raw_month_2015, remote_2015)
    
    #both 2014 and 2015
    if(file.exists(zipped_uberfileURL) && nrow(remote_small_2015) != 0 && length(remote_small_2014) != 0){
      message("Transforming uber 2015 data from raw to load directory...")
      utils::unzip(zipfile = zipped_uberfileURL, 
                   unzip = "internal",
                   exdir = file.path(tempdir(), "uber-raw-data-janjune-15.csv.zip"))
      file.rename(from = file.path(tempdir(), "uber-raw-data-janjune-15.csv.zip","uber-raw-data-janjune-15.csv"),
                  to = file.path(attr(obj, "load_dir"), "uber-raw-data-janjune-15.csv"))

      message("Transforming uber 2014 data from raw to load directory...")
      raw_file_path <- data.frame(uber14_list) %>%
        mutate_(basename = ~attr(obj, "raw_dir")) %>%
        mutate_(raw_file_dir = ~paste0(basename, "/",uber14_list))
      
      load_file_path <- data.frame(uber14_list) %>%
        mutate_(basename = ~attr(obj, "load_dir")) %>%
        mutate_(raw_file_dir = ~paste0(basename, "/",uber14_list))
      
      #copy the files in the raw directory and paste them to the load directory
      file.copy(from = raw_file_path$raw_file_dir, to = load_file_path$raw_file_dir)
    }
    #2015 only
    else if(file.exists(zipped_uberfileURL) && nrow(remote_small_2015) != 0 && length(remote_small_2014) == 0){
      message("Transforming uber 2015 data from raw to load directory...")
      utils::unzip(zipfile = zipped_uberfileURL, 
                   unzip = "internal",
                   exdir = file.path(tempdir(), "uber-raw-data-janjune-15.csv.zip"))
      file.rename(from = file.path(tempdir(), "uber-raw-data-janjune-15.csv.zip","uber-raw-data-janjune-15.csv"),
                  to = file.path(attr(obj, "load_dir"), "uber-raw-data-janjune-15.csv"))
      
    }
    #2014 only
    else if(nrow(remote_small_2015) == 0 && length(remote_small_2014) != 0){
      message("Transforming uber 2014 data from raw to load directory...")
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
      message("The 2014 Uber data you requested is not in the raw directory...")
    }
  }
  
  
  #LYFT----------------------------------------------------------------
  else if (transportation == "lyft") {
    valid_months <- etl::valid_year_month(years, months = 1, begin = "2015-01-01")
    message("Transforming lyft data from raw to load directory...")
    src <- list.files(attr(obj, "raw_dir"), "lyft", full.names = TRUE)
    src_year <- valid_months %>%
      distinct_(~year)
    
    remote <- data_frame(src)
    remote <- remote %>%
      mutate_(lcl = ~file.path(attr(obj, "load_dir"),basename(src)),
              basename = ~basename(src),
              year = ~substr(basename,6,9))
    class(remote$year) <- "numeric"
    remote <- inner_join(remote,src_year, by = "year" )
    
    for(i in 1:nrow(remote)) {
        datafile <- readr::read_csv(remote$src[i])
        readr::write_delim(datafile, path = remote$lcl[i], delim = "|", na = "")
    } 
  }

  else{
    warning("The transportation you specified does not exist...")
  }
  
  invisible(obj)
}

