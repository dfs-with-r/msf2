#' MySportsFeeds API calls by game
#'
#' If you provide multiple gameids it will execute with a delay between each query.
#' This is to ensure you obey limit rates but you are free to lower the parameter if you don't
#' think your queries will reach the throttle limits.
#'
#' @param feed feed to request
#' @param sport mlb | nfl | nba | etc..
#' @param gameid strings, "40265" or c("40265", "11111") or "20171015-SF-WAS"
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param delay number of seconds between queries
#' @param ... additional parameters passed to \code{msf_by_game} such as season or delay
#'
#' @examples
#' \dontrun{
#' j <- game_lineup("mlb", "43333", season = "2017-regular")
#' }
msf_by_game <- function(feed, sport, gameid, season = "current", delay = 1) {
  stopifnot(length(feed) == 1L, length(sport) == 1L, length(season) == 1L)
  path <- sprintf("%s/%s/games/%s/%s.json", sport, season, gameid, feed)

  if (length(gameid) > 1) {
    result <- purrr::map(path, ~ delay_by(delay, msf_api)(.x))
    result <- purrr::map2(result, path, function(r, p) {attr(r, "local_path") <- p; r})
    names(result) <- gameid
  } else {
    result <- msf_api(path)
    attr(result, "local_path") <- path
  }

  result
}

#' @describeIn msf_by_game Expected and Actual starting lineup for both teams of a game.
#' @export
game_lineup <- function(sport, gameid, ...) {
  msf_by_game("lineup", sport, gameid, ...)
}
