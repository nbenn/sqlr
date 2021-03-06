% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqlr.R
\docType{data}
\name{airports}
\alias{airports}
\title{Airport metadata}
\format{A data frame with columns:
\describe{
 \item{faa}{FAA airport code}
 \item{name}{Usual name of the aiport}
 \item{lat,lon}{Location of airport}
 \item{alt}{Altitude, in feet}
 \item{tz}{Timezone offset from GMT}
 \item{dst}{Daylight savings time zone. A = Standard US DST: starts on the
    second Sunday of March, ends on the first Sunday of November.
    U = unknown. N = no dst.}
 \item{tzone}{IANA time zone, as determined by GeoNames webservice}
}}
\source{
https://cran.r-project.org/package=nycflights13, v0.2.2
}
\usage{
airports
}
\description{
Useful metadata about airports.
}
\examples{
\dontrun{
  airports <- nycflights13::airports
  attr(airports, "spec") <- NULL
  airports$tz <- as.integer(airports$tz)
  airports$tzone[airports$tzone == "\\\\N"] <- NA
  airports$tzone <- as.factor(airports$tzone)
  save(airports, file = "data/airports.rda", compress = "xz")
}

}
\keyword{datasets}
