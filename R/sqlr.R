#' Airline names.
#'
#' Look up airline names from their carrier codes.
#'
#' @source https://cran.r-project.org/package=nycflights13, v0.2.2
#'
#' @format Data frame with columns
#' \describe{
#' \item{carrier}{Two letter abbreviation}
#' \item{name}{Full name}
#' }
#'
#' @examples
#' \dontrun{
#'   airlines <- nycflights13::airlines
#'   save(airlines, file = "data/airlines.rda", compress = "xz")
#' }
#'
"airlines"

#' Airport metadata
#'
#' Useful metadata about airports.
#'
#' @source https://cran.r-project.org/package=nycflights13, v0.2.2
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{faa}{FAA airport code}
#'  \item{name}{Usual name of the aiport}
#'  \item{lat,lon}{Location of airport}
#'  \item{alt}{Altitude, in feet}
#'  \item{tz}{Timezone offset from GMT}
#'  \item{dst}{Daylight savings time zone. A = Standard US DST: starts on the
#'     second Sunday of March, ends on the first Sunday of November.
#'     U = unknown. N = no dst.}
#'  \item{tzone}{IANA time zone, as determined by GeoNames webservice}
#' }
#'
#' @examples
#' \dontrun{
#' airports <- nycflights13::airports
#' attr(airports, "spec") <- NULL
#' airports$tz <- as.integer(airports$tz)
#' airports$tzone[airports$tzone == "\\N"] <- NA
#' airports$tzone <- as.factor(airports$tzone)
#' save(airports, file = "data/airports.rda", compress = "xz")
#' }
#'
"airports"

#' Flights data
#'
#' On-time data for all flights that departed NYC (i.e. JFK, LGA or EWR) in
#' 2013.
#'
#' @source https://cran.r-project.org/package=nycflights13, v0.2.2
#'
#' @format Data frame with columns
#' \describe{
#' \item{departure}{Date and time of departure as a \code{POSIXct} date}
#' \item{dep_delay,arr_delay}{Departure and arrival delays, as a
#'   \code{difftime} object. Negative times represent early
#'   departures/arrivals.}
#' \item{air_time}{Amount of time spent in the air, in minutes}
#' \item{carrier}{Two letter carrier abbreviation. See \code{\link{airlines}}
#'   to get name}
#' \item{flight}{Flight number}
#' \item{tailnum}{Plane tail number}
#' \item{origin,dest}{Origin and destination. See \code{\link{airports}} for
#'   additional metadata.}
#' \item{distance}{Distance between airports, in miles}
#' }
#'
#' @examples
#' \dontrun{
#'   flights <- nycflights13::flights
#'   flights <- flights[flights$month %in% 1:3, ]
#'   tmp <- sprintf("%04d", flights$dep_time)
#'   tmp[is.na(flights$dep_time)] <- NA
#'   flights$departure <- as.POSIXct(paste(paste(flights$year, flights$month,
#'                                               flights$day, sep = "-"),
#'                                         paste(substr(tmp, 1, 2),
#'                                               substr(tmp, 3, 4),
#'                                               sep = ":")),
#'                                   format = "%Y-%m-%d %H:%M",
#'                                   tz = "America/New_York")
#'   flights$dep_delay <- hms::hms(minutes = flights$dep_delay)
#'   flights$arr_delay <- hms::hms(minutes = flights$arr_delay)
#'   flights$air_time <- hms::hms(minutes = flights$air_time)
#'   flights <- flights[, c("departure", "dep_delay", "arr_delay", "air_time",
#'                          "carrier", "flight", "tailnum", "origin", "dest",
#'                          "distance")]
#'   save(flights, file = "data/flights.rda", compress = "xz")
#' }
#'
"flights"

#' Plane metadata.
#'
#' Plane metadata for all plane tailnumbers found in the FAA aircraft
#' registry. American Airways (AA) and Envoy Air (MQ) report fleet numbers
#' rather than tail numbers (e.g.
#' \url{http://www.flyerguide.com/Tail_Numbers_(AA)})
#' so can't be matched.
#'
#' @source https://cran.r-project.org/package=nycflights13, v0.2.2
#'
#' @format A data frame with columns:
#' \describe{
#' \item{tailnum}{Tail number}
#' \item{year}{Year manufactured}
#' \item{type}{Type of plane}
#' \item{manufacturer,model}{Manufacturer and model}
#' \item{engines,seats}{Number of engines and seats}
#' \item{speed}{Average cruising speed in mph}
#' \item{engine}{Type of engine}
#' }
#'
#' @examples
#' \dontrun{
#'   planes <- nycflights13::planes
#'   planes$type <- factor(planes$type)
#'   planes$manufacturer <- factor(planes$manufacturer)
#'   planes$engine <- factor(planes$engine)
#'   save(planes, file = "data/planes.rda", compress = "xz")
#' }
#'
"planes"

#' Hourly weather data
#'
#' Hourly meterological data for LGA, JFK and EWR.
#'
#' @source ASOS download from Iowa Environmental Mesonet,
#'   https://mesonet.agron.iastate.edu/request/download.phtml.
#' @format A data frame with columns
#' \describe{
#' \item{origin}{Weather station. Named origin to faciliate merging with
#'   \code{\link{flights}} data}
#' \item{ts}{Date/time of recording as a \code{POSIXct} date}
#' \item{temp,dewp}{Temperature and dewpoint in F}
#' \item{humid}{Relative humidity}
#' \item{wind_dir,wind_speed,wind_gust}{Wind direction (in degrees), speed
#'   and gust speed (in mph)}
#' \item{precip}{Preciptation, in inches}
#' \item{pressure}{Sea level pressure in millibars}
#' \item{visib}{Visibility rating, corresponds to rounded up miles (0-10)}
#' }
#'
#' @examples
#' \dontrun{
#'   weather <- nycflights13::weather
#'   weather <- weather[weather$month %in% 1:3, ]
#'   weather$ts <- as.POSIXct(paste(paste(weather$year, weather$month,
#'                                        weather$day, sep = "-"),
#'                                  paste0(sprintf("%02d", weather$hour),
#'                                         ":00")),
#'                                  format = "%Y-%m-%d %H:%M",
#'                                  tz = "America/New_York")
#'   weather <- weather[!is.na(weather$ts), ]
#'   weather[, c("temp", "dewp", "humid", "precip", "pressure")] <- lapply(
#'     weather[, c("temp", "dewp", "humid", "precip", "pressure")], as.single)
#'   weather$wind_dir <- as.integer(weather$wind_dir)
#'   weather$visib <- factor(ceiling(weather$visib), levels = 0:10,
#'                           ordered = TRUE)
#'   weather <- weather[, c("origin", "ts", "temp", "dewp", "humid",
#'                          "wind_dir", "wind_gust", "wind_speed", "precip",
#'                          "pressure", "visib")]
#'   save(weather, file = "data/weather.rda", compress = "xz")
#' }
#'
"weather"
