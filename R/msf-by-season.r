#' MySportsFeeds API calls by season
#'
#' If you provide multiple seasons it will execute with a delay between each query.
#' This is to ensure you obey limit rates but you are free to lower the parameter if you don't
#' think your queries will reach the throttle limits.
#'
#' @param feed feed to request
#' @param sport mlb | nfl | nba | etc..
#' @param season ex. 2017-regular | current
#' @param delay number of seconds between queries
#' @param ... additional query parameters
#'
#' @examples
#' \dontrun{
#' j <- seasonal_dfs("nba", "2017-2018-regular", team = "SAS")
#' }
msf_by_season <- function(feed, sport, season = "current", delay = 1,  ...) {
  stopifnot(length(feed) == 1L, length(sport) == 1L)

  path <- sprintf("%s/%s/%s.json", sport, season, feed)

  if (!missing(...)) {
    query <- list(...)
    query <- lapply(query, paste, collapse = ",")
  } else {
    query <- NULL
  }

  if (length(season) > 1) {
    result <- purrr::map(path, ~ delay_by(delay, msf_api)(.x, query))
    result <- purrr::map2(result, path, function(r, p) {attr(r, "local_path") <- path; r})
    names(result) <- season
    result <- lapply(result, msf_class, feed)
  } else {
    result <- msf_api(path, query)
    attr(result, "local_path") <- path
    result <- msf_class(result, feed)
  }

  result
}

#' @describeIn msf_by_season All DFS entries for a season including salaries and fantasy points.
#' @export
seasonal_dfs <- function(sport, season, ...) {
  msf_by_season("dfs", sport, season, ...)
}

