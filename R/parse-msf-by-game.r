#' @export
parse_game_lineups <- function(j) {
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

  data.frame(
    team_id = as.character(team_id),
    team = team,
    expected_position = expected_position,
    expected_id = as.character(expected_id),
    expected_name = paste(expected_fname, expected_lname),
    actual_position = actual_position,
    actual_id = as.character(actual_id),
    actual_name = paste(actual_fname, actual_lname),
    stringsAsFactors = FALSE
  )
}
