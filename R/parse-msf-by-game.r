#' Tidy Starting Lineups
#'
#' @param j msf object
#' @param ... additional arguments. currently unused
#' @export
tidy.msf_lineup <- function(j, ...) {
  # game
  game_id <- j[["game"]][["id"]]
  game_time <- msf_time(j[["game"]][["startTime"]])

  # lineups
  team_lineups <- j[["teamLineups"]]

  team1 <- parse_game_lineups_team(team_lineups[[1]])
  team2 <- parse_game_lineups_team(team_lineups[[2]])

  lineups <- rbind(team1, team2)
  lineups[["game_id"]] <- as.character(game_id)
  lineups[["game_time"]] <- game_time

  lineups[c("game_id", "game_time", setdiff(colnames(lineups), c("game_id", "game_time")))]
}

#' @keywords internal
parse_game_lineups_team <- function(j) {
  # team
  team_id <- j[["team"]][["id"]]
  team <- j[["team"]][["abbreviation"]]

  # expected
  expected <- j[["expected"]][["lineupPositions"]]
  expected_position <- purrr::map_chr(expected, "position")
  expected_id <- purrr::map_int(expected, c("player", "id"), .default = NA_integer_)
  expected_fname <- purrr::map_chr(expected, c("player", "firstName"), .default = NA_character_)
  expected_lname <- purrr::map_chr(expected, c("player", "lastName"), .default = NA_character_)

  # actual
  actual <- j[["actual"]][["lineupPositions"]]
  actual_position <- purrr::map_chr(actual, "position")
  actual_id <- purrr::map_int(actual, c("player", "id"), .default = NA_integer_)
  actual_fname <- purrr::map_chr(actual, c("player", "firstName"), .default = NA_character_)
  actual_lname <- purrr::map_chr(actual, c("player", "lastName"), .default = NA_character_)

  tibble::tibble(
    team_id = as.character(team_id),
    team = team,
    expected_position = expected_position,
    expected_id = as.character(expected_id),
    expected_name = paste(expected_fname, expected_lname),
    actual_position = actual_position,
    actual_id = as.character(actual_id),
    actual_name = paste(actual_fname, actual_lname)
  )
}

#' Tidy Game Boxscores
#'
#' @param j msf object
#' @param ... additional arguments. currently unused
#' @export
tidy.msf_boxscore <- function(j, ...) {
  home_stats <- parse_boxscore_stats(j[[c("stats", "home", "players")]])
  away_stats <- parse_boxscore_stats(j[[c("stats", "away", "players")]])

  # add team_id
  home_stats[["team_id"]] <- as.character(j[[c("game", "homeTeam", "id")]])
  home_stats[["team"]] <- j[[c("game", "homeTeam", "abbreviation")]]

  away_stats[["team_id"]] <- as.character(j[[c("game", "awayTeam", "id")]])
  away_stats[["team"]] <- j[[c("game", "awayTeam", "abbreviation")]]

  # combine home and away
  stats <- do.call(rbind, list(home_stats, away_stats))

  # add game_id
  stats[["game_id"]] <- as.character(j[[c("game", "id")]])
  to_front <- c("game_id", "team_id", "team")
  stats <- stats[c(to_front, setdiff(colnames(stats), to_front))]

  stats
}

#' @keywords internal
parse_boxscore_stats <- function(j) {
  # extract player info
  player_info <- purrr::map_dfr(j, ~ purrr::compact(.x[["player"]]))
  player_info[["player_id"]] <- as.character(player_info[["id"]])
  player_info[["player_name"]] <- paste(player_info[["firstName"]], player_info[["lastName"]])
  player_info <- player_info[c("player_id", "player_name", "position")]

  # extract player stats
  player_stats <- purrr::map(j, ~ purrr::flatten(.x[["playerStats"]][[1]]))

  # remove PerGame columns (this is only one game)
  player_stats <- purrr::map(player_stats, ~ .x[!grepl("PerGame", names(.x))])

  # combine into table
  player_stats <- purrr::map_dfr(player_stats, tibble::as_tibble)

  tibble::as_tibble(cbind(player_info, player_stats))
}
