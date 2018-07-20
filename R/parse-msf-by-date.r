#' @export
parse_daily_dfs <- function(j, site = c("draftkings", "fanduel")) {
  site <- match.arg(site)
  site_names<- tolower(purrr::map_chr(j$dfsEntries, "dfsSource"))
  index <- which(site == site_names)
  dfsRows <- j$dfsEntries[[index]]$dfsRows

  # players
  players <- purrr::map(dfsRows, "player")
  player_id <- purrr::map_int(players, "id")
  player_position <- purrr::map_chr(players, "position")
  player_fname <- purrr::map_chr(players, "firstName")
  player_lname <- purrr::map_chr(players, "lastName")
  player_name <- paste(player_fname, player_lname)

  # teams
  teams <- purrr::map(dfsRows, "team")
  team_id <- purrr::map_int(teams, "id")
  team_name <- purrr::map_chr(teams, "abbreviation")

  # game
  games <- purrr::map(dfsRows, "game")
  game_id <- purrr::map_int(games, "id", .default = NA_integer_)
  game_time <- purrr::map_chr(games, "startTime", .default = NA_character_)
  game_time <- msf_time(game_time)

  # dfs
  dfs_id <- purrr::map_int(dfsRows, "dfsSourceId")
  dfs_salary <- purrr::map_int(dfsRows, "salary")
  dfs_fpts <- purrr::map_dbl(dfsRows, "fantasyPoints", .default = NA_real_)

  data.frame(
    game_id = as.character(game_id),
    game_time = game_time,
    player_id = as.character(player_id),
    player_name = player_name,
    position = player_position,
    team_id = as.character(team_id),
    team = team_name,
    dfs_id = as.character(dfs_id),
    dfs_salary = dfs_salary,
    dfs_fpts = dfs_fpts,
    stringsAsFactors = FALSE)
}

#' @export
parse_daily_games <- function(j) {
  games <- j[["games"]]
  schedules <- purrr::map(games, "schedule")
  scores <- purrr::map(games, "score")

  # schedule
  game_id <- purrr::map_int(schedules, "id")
  game_time <- purrr::map_chr(schedules, "startTime")
  game_time <- msf_time(game_time)
  game_status <- purrr::map_chr(schedules, "playedStatus")
  away_id <- purrr::map_int(schedules, c("awayTeam", "id"))
  away_team <- purrr::map_chr(schedules, c("awayTeam", "abbreviation"))
  home_id <- purrr::map_int(schedules, c("homeTeam", "id"))
  home_team <- purrr::map_chr(schedules, c("homeTeam", "abbreviation"))

  # scores
  away_score <- purrr::map_int(scores, "awayScoreTotal")
  home_score <- purrr::map_int(scores, "homeScoreTotal")

  data.frame(
    game_id = as.character(game_id),
    game_time = game_time,
    game_status = game_status,
    away_id = as.character(away_id),
    away_team = away_team,
    away_score = away_score,
    home_id = as.character(home_id),
    home_team = home_team,
    home_score = home_score,
    stringsAsFactors = FALSE
  )
}
