#' MySportsFeeds API calls by date
#'
#' If you provide multiple dates it will execute with a delay between each query.
#' This is to ensure you obey limit rates but you are free to lower the parameter if you don't
#' think your queries will reach the throttle limits.
#'
#' @param feed feed to request
#' @param sport mlb | nfl | nba | etc..
#' @param date date, ex. as.Date("2017-05-08")
#' @param delay number of seconds between queries
#' @param ... additional query parameters
#'
#' @examples
#' \dontrun{
#' j <- daily_dfs("mlb", date = as.Date("2018-07-15"))
#' }
msf_by_date <- function(feed, sport, date = Sys.Date(), delay = 1, ...) {
  season <- unique(date_to_season(date))
  stopifnot(length(feed) == 1L, length(sport) == 1L, length(season) == 1L)

  date <- msf_date(date)
  path <- sprintf("%s/%s/date/%s/%s.json", sport, season, date, feed)

  if (!missing(...)) {
    query <- list(...)
    query <- lapply(query, paste, collapse = ",")
  } else {
    query <- NULL
  }

  if (length(date) > 1) {
    result <- purrr::map(path, ~ delay_by(delay, msf_api)(.x, query))
    result <- purrr::map2(result, path, function(r, p) {attr(r, "local_path") <- path; r})
    names(result) <- date
    result <- lapply(result, msf_class, feed)
  } else {
    result <- msf_api(path, query)
    attr(result, "local_path") <- path
    result <- msf_class(result, feed)
  }

  result
}

#' @describeIn msf_by_date A list of players, along with their DFS salaries and actual fantasy points.
#' @export
daily_dfs <- function(sport, date = Sys.Date(), ...) {
  msf_by_date("dfs", sport, date, ...)
}

#' @describeIn msf_by_date All games on a given date including schedule, status and scores.
#' @export
daily_games <- function(sport, date = Sys.Date(), ...) {
  msf_by_date("games", sport, date, ...)
}
