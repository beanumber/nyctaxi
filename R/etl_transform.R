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
  taxi <- function(obj, years, months, types) {
    message("Transforming taxi data from raw to load directory...")
    #create a df of file path of the files that the user wants to transform
    remote <- get_file_path(years, months, types, path = attr(obj, "raw_dir")) 
    #create a df of file path of the files that are in the raw directory
    src <- list.files(attr(obj, "raw_dir"), "tripdata", full.names = TRUE)
    src_small <- intersect(src, remote$src)
    
    #Clean the green taxi data files
    #get rid of 2nd blank row----------------------------------------------------------
    if (length(src_small) == 0){
      message("The files you requested are not available in the raw directory.")
    } else{
      #a list of the ones that have a 2nd blank row
      remote_green_1 <- remote %>% 
        filter_(~type == "green")%>%
        filter_(~year != 2015)
      src_small_green_1 <- intersect(src, remote_green_1$src)
      
      # check that the sys support command line, and then remove the blank 2nd row
      if(length(src_small_green_1) != 0) {
        if (.Platform$OS.type == "unix"){
          cmds_1 <- paste("sed -i -e '2d'", src_small_green_1)
          lapply(cmds_1, system)
        } else {
          message("Windows system does not currently support removing the 2nd blank row 
                  in the green taxi datasets. This might affect loading data into SQL...")
        }
      }else {
        "You did not request for any green taxi data, or all the green taxi data you requested are cleaned."
      }
      #fix column number---------------------------------------------------------------
      remote_green_2 <- remote %>% 
        filter_(~type == "green") %>%
        filter_(~year %in% c(2013, 2014, 2015)) %>%
        mutate_(keep = ~ifelse(year %in% c(2013,2014), 20,21),
                new_file = ~paste0("green_tripdata_", year, "_", 
                                   stringr::str_pad(month, 2, "left", "0"),
                                   ".csv"))
      src_small_green_2 <- intersect(src, remote_green_2$src)
      src_small_green_2_df <- data.frame(src_small_green_2) 
      names(src_small_green_2_df) <- "src"
      src_small_green_2_df <- inner_join(src_small_green_2_df, remote_green_2, by = "src")
      
      src_small_green_2_df <- src_small_green_2_df %>%
        mutate(cmds_2 = paste("cut -d, -f1-", keep," ",src, " > ",attr(obj, "raw_dir"),
                              "/green_tripdata_", year, "_", stringr::str_pad(month, 2, "left", "0"),".csv",
                              sep = ""))
      #remove the extra column
      if(length(src_small_green_2) != 0) {
        if (.Platform$OS.type == "unix"){
          lapply(src_small_green_2_df$cmds_2, system)} 
        else {
          message("Windows system does not currently support removing the 2nd blank row 
                  in the green taxi datasets. This might affect loading data into SQL...")
        }
      }else {
        "All the green taxi data you requested are in cleaned formats."
      }
      #Find the files paths of the files that need to be transformed----------------------
      file.rename(file.path(substr(src_small_green_2_df$src,1,36),
                            src_small_green_2_df$new_file), 
                  file.path(attr(obj, "load_dir"), basename(src_small_green_2_df$src)))
     
      #Move the files
      in_raw <- basename(src_small)
      in_load <- basename(list.files(attr(obj, "load_dir"), "tripdata", full.names = TRUE))
      file_remian <- setdiff(in_raw,in_load)
      file.copy(file.path(attr(obj, "raw_dir"),file_remian),
                file.path(attr(obj, "load_dir"),file_remian) )
    }
  }
  #UBER----------------------------------------------------------------
  uber <- function(obj, years, months) {
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
  }
  #LYFT----------------------------------------------------------------
  lyft <- function(obj, years, months){
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
        readr::write_delim(datafile, path = remote$lcl[i], delim = "|", na = "")} 
  }
  
  #transform the data from raw to load
  if (transportation == "taxi"){taxi(obj, years,months,types)} 
  else if (transportation == "uber"){uber(obj, years,months)}
  else if (transportation == "lyft"){lyft(obj, years,months)}
  else {message("The transportation you chose does not exit...")}
  invisible(obj)
}

