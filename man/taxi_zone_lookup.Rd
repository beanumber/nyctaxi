% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{taxi_zone_lookup}
\alias{taxi_zone_lookup}
\title{TLC taxi zone location IDs and corresponding boroughs and zone}
\format{TLC taxi zone information.
\describe{
  \item{LocationID}{TLC taxi zone LocationID.}
  \item{Borough}{Borough corresponding to a TLC taxi zone LocationID.}
  \item{Zone}{Zone corresponding to a TLC taxi zone LocationID.}
}}
\source{
The data is available at \url{http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml}
}
\usage{
taxi_zone_lookup
}
\description{
#' The third table `taxi-zone-lookup` shows the taxi `Zone` and `Borough` 
for each `locationID`. For trip record data including TLC taxi zone location IDs, 
location names and corresponding boroughs for each ID can be found in this data table.
}
\keyword{datasets}
