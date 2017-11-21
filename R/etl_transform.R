#' @import etl
#' @importFrom rlang .data
#' @importFrom readr read_csv
#' @importFrom lubridate as_datetime
#' @export
#' @rdname etl_extract.etl_nyctaxi
#' @details transform NYC Yellow taxi trip data from raw directory to load directory
#' and/or \code{green}. The default is \code{yellow}.
#' @inheritParams get_file_path
#' @seealso \code{\link[etl]{etl_transform}}

etl_transform.etl_nyctaxi <- function(obj, years = as.numeric(format(Sys.Date(),'%Y')), 
                                    months = 1:12, 
                                    type  = "yellow",...) {
  #TAXI YELLOW----------------------------------------------------------------
  taxi_yellow <- function(obj, years, months) {
    message("Transforming yellow taxi data from raw to load directory...")
    #create a df of file path of the files that the user wants to transform
    remote <- etl::valid_year_month(years, months, begin = "2009-01-01") %>%
      mutate_(src = ~file.path(attr(obj, "raw_dir"), 
                               paste0("yellow", "_tripdata_", year, "-",
                                      stringr::str_pad(month, 2, "left", "0"), ".csv"))) 
    #create a df of file path of the files that are in the raw directory
    src <- list.files(attr(obj, "raw_dir"), "yellow", full.names = TRUE)
    src_small <- intersect(src, remote$src)
    #Move the files
    in_raw <- basename(src_small)
    in_load <- basename(list.files(attr(obj, "load_dir"), "yellow", full.names = TRUE))
    file_remian <- setdiff(in_raw,in_load)
    file.copy(file.path(attr(obj, "raw_dir"),file_remian),
              file.path(attr(obj, "load_dir"),file_remian) )}
  #TAXI GREEN----------------------------------------------------------------
  taxi_green <- function(obj, years, months) {
    message("Transforming green taxi data from raw to load directory...")
    #create a df of file path of the files that the user wants to transform
    remote <- etl::valid_year_month(years, months, begin = "2013-08-01") %>%
      mutate_(src = ~file.path(attr(obj, "raw_dir"), paste0("green", "_tripdata_", year, "-",
                                      stringr::str_pad(month, 2, "left", "0"), ".csv"))) 
    #create a df of file path of the files that are in the raw directory
    src <- list.files(attr(obj, "raw_dir"), "green", full.names = TRUE)
    src_small <- intersect(src, remote$src)
    #Clean the green taxi data files
    #get rid of 2nd blank row----------------------------------------------------------
    if (length(src_small) == 0){
      message("The files you requested are not available in the raw directory.")
    } else{
      #a list of the ones that have a 2nd blank row
      remote_green_1 <- remote %>% filter_(~year != 2015)
      src_small_green_1 <- intersect(src, remote_green_1$src)
      # check that the sys support command line, and then remove the blank 2nd row
      if(length(src_small_green_1) != 0) {
        if (.Platform$OS.type == "unix"){
          cmds_1 <- paste("sed -i -e '2d'", src_small_green_1)
          lapply(cmds_1, system)
        } else {
          message("Windows system does not currently support removing the 2nd blank row 
                  in the green taxi datasets. This might affect loading data into SQL...")}
        }else {
          "You did not request for any green taxi data, or all the green taxi data you requested are cleaned."}
      #fix column number---------------------------------------------------------------
      remote_green_2 <- remote %>%
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
        mutate(cmds_2 = paste("cut -d, -f1-", keep," ",src, " > ",attr(obj, "raw_dir"),"/green_tripdata_", 
                              year, "_", stringr::str_pad(month, 2, "left", "0"),".csv", sep = ""))
      #remove the extra column
      if(length(src_small_green_2) != 0) {
        if (.Platform$OS.type == "unix"){
          lapply(src_small_green_2_df$cmds_2, system)} 
        else {
          message("Windows system does not currently support removing the 2nd blank row 
                  in the green taxi datasets. This might affect loading data into SQL...")}
        }else {
          "All the green taxi data you requested are in cleaned formats."}
      #Find the files paths of the files that need to be transformed----------------------
      file.rename(file.path(dirname(src_small_green_2_df$src),
                            src_small_green_2_df$new_file), 
                  file.path(attr(obj, "load_dir"), basename(src_small_green_2_df$src)))
      #Move the files
      in_raw <- basename(src_small)
      in_load <- basename(list.files(attr(obj, "load_dir"), "green", full.names = TRUE))
      file_remian <- setdiff(in_raw,in_load)
      file.copy(file.path(attr(obj, "raw_dir"),file_remian), file.path(attr(obj, "load_dir"),file_remian) )}}
  #UBER----------------------------------------------------------------
  uber <- function(obj, years, months) {
    message("Transforming uber data from raw to load directory...")
    #creat a list of 2014 uber data file directory
    uber14_list <- list.files(path = attr(obj, "raw_dir"), pattern = "14.csv")
    uber14_list <- data.frame(uber14_list)
    uber14_list <- uber14_list %>% mutate_(file_path = ~file.path(attr(obj, "raw_dir"), uber14_list))
    uber14file <- lapply(uber14_list$file_path, readr::read_csv)
    n <- length(uber14file)
    if (n == 1) {
      uber14 <- data.frame(uber14file[1])
    } else if (n == 2) {
      uber14 <- bind_rows(uber14file[1], uber14file[2])
    } else if (n > 2) {
      uber14 <- bind_rows(uber14file[1], uber14file[2])
      for (i in 3:n){uber14 <- bind_rows(uber14, uber14file[i])}
    }
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    uber14_datetime <- uber14 %>%
      mutate(date = gsub( " .*$", "", `Date/Time`), len_date = nchar(date), 
             time = sub('.*\\ ', '', `Date/Time`))
    uber14_datetime <- uber14_datetime %>%
      mutate(month = substr(`Date/Time`, 1, 1),
             day = ifelse(len_date == 8, substr(`Date/Time`, 3,3),substr(`Date/Time`, 3,4)),
             pickup_date = lubridate::ymd_hms(paste0("2014-", month, "-", day, " ", time)))
    uber14_df <- uber14_datetime[-c(1,5:9)]
    
    #2015
    zipped_uberfileURL <- file.path(attr(obj, "raw_dir"), "uber-raw-data-janjune-15.csv.zip")
    raw_month_2015 <- etl::valid_year_month(years = 2015, months = 1:6)
    remote_2015 <- etl::valid_year_month(years, months)
    remote_small_2015 <- inner_join(raw_month_2015, remote_2015)
    if(file.exists(zipped_uberfileURL) && nrow(remote_small_2015) != 0){
      utils::unzip(zipfile = zipped_uberfileURL, 
                   unzip = "internal",
                   exdir = file.path(tempdir(), "uber-raw-data-janjune-15.csv.zip"))
      uber15 <- readr::read_csv(file.path(tempdir(), "uber-raw-data-janjune-15.csv.zip","uber-raw-data-janjune-15.csv"))}
    
    names(uber14_df) <- c("lat", "lon", "dispatching_base_num", "pickup_date")
    names(uber15) <- tolower(names(uber15))
    uber <- bind_rows(uber14_df, uber15)
    uber <- uber[-c(1)]
    write.csv(uber, file.path(attr(obj, "load_dir"),"uber.csv"))}
  #LYFT----------------------------------------------------------------
  lyft <- function(obj, years, months){
    valid_months <- etl::valid_year_month(years, months = 1, begin = "2015-01-01")
    message("Transforming lyft data from raw to load directory...")
    src <- list.files(attr(obj, "raw_dir"), "lyft", full.names = TRUE)
    src_year <- valid_months %>% distinct_(~year)
    remote <- data_frame(src)
    remote <- remote %>%
      mutate_(lcl = ~file.path(attr(obj, "load_dir"),basename(src)),
              basename = ~basename(src), year = ~substr(basename,6,9))
    class(remote$year) <- "numeric"
    remote <- inner_join(remote,src_year, by = "year" )
    for(i in 1:nrow(remote)) {
        datafile <- readr::read_csv(remote$src[i])
        readr::write_delim(datafile, path = remote$lcl[i], delim = "|", na = "")}}
  
  #transform the data from raw to load
  if (type == "yellow"){taxi_yellow(obj, years, months)} 
  else if (type == "green"){taxi_green(obj, years, months)}
  else if (type == "uber"){uber(obj, years, months)}
  else if (type == "lyft"){lyft(obj, years, months)}
  else {message("The type you chose does not exit...")}
  
  invisible(obj)
}

