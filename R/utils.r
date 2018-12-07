#' Converts a Date to msf formatted date
#'
#' MSF dates have a specific format. This will parse R dates to MSF format.
#' @param date an R date
#' @keywords internal
msf_date <- function(date) {
  stopifnot(methods::is(date, "Date"))
  format(date, "%Y%m%d")
}

#' Converts msf formatted time to POSIXct
#'
#' MSF datetimes have a specific format. This will parse them to R datetimes.
#' @param x character string
#' @keywords internal
msf_time <- function(x) {
  new_time <- gsub("T||\\..*$", "", x)
  as.POSIXct(new_time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Adds msf_* type to object class
#'
#' @param obj object to modify
#' @param newclass new class will be msf_newclass
#' @export
msf_class <- function(obj, newclass) {
  class(obj) <- c(paste0("msf_", newclass), class(obj))
  obj
}

#' Delays function result
#'
#' @param delay number of seconds to delay function by
#' @param f function
#'
#' @keywords internal
delay_by <- function(delay, f) {
  function(...) {
    res <- f(...)
    Sys.sleep(delay)
    res
  }
}

#' @keywords internal
date_to_season <- function(dates, sport = c("nba", "nfl", "nhl", "mlb"),
                           type = c("regular", "playoff")) {

  sport <- match.arg(sport)
  type <- match.arg(type)
  years <- as.integer(format(dates, "%Y"))

  if (sport %in% c("nba", "nhl")) {
    months <- as.integer(format(dates, "%m"))
    ifelse(months >= 9,
           paste(years, years + 1, type, sep = "-"),
           paste(years - 1, years, type, sep = "-")
    )
  } else {
    paste(years, type, sep = "-")
  }

}

