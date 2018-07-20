#' Converts a Date to msf formatted date
msf_date <- function(date) {
  stopifnot(methods::is(date, "Date"))
  format(date, "%Y%m%d")
}

#' Converts msf formatted time to POSIXct
msf_time <- function(x) {
  new_time <- gsub("T||\\..*$", "", x)
  as.POSIXct(new_time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Delays function result
delay_by <- function(delay, f) {
  function(...) {
    res <- f(...)
    Sys.sleep(delay)
    res
  }
}
