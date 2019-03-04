#' Tidy DFS
#' Converts daily and seasonal dfs data to a dataframe
#' @param j msf object
#' @param site DFS site to get data for
#' @param ... additional arguments. currently unused
#' @export
tidy.msf_dfs <- function(j, site = c("draftkings", "fanduel"), ...) {
  site <- match.arg(site)
  site_names<- tolower(purrr::map_chr(j$dfsEntries, "dfsSource"))
  index <- which(site == site_names)

  if (length(index) == 0) return(NULL)

  dfsRows <- j$dfsEntries[[index]]$dfsRows

  # players
  players <- purrr::map(dfsRows, "player")
  player_id <- purrr::map_int(players, "id", .default = NA_integer_)
  player_position <- purrr::map_chr(players, "position", .default = NA_character_)
  player_fname <- purrr::map_chr(players, "firstName", .default = NA_character_)
  player_lname <- purrr::map_chr(players, "lastName", .default = NA_character_)
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
  dfs_id <- purrr::map_chr(dfsRows, "dfsSourceId", .default = NA_integer_)
  dfs_salary <- purrr::map_int(dfsRows, "salary")
  dfs_fpts <- purrr::map_dbl(dfsRows, "fantasyPoints", .default = NA_real_)

  tibble::tibble(
    game_id = as.character(game_id),
    game_time = game_time,
    player_id = as.character(player_id),
    player_name = player_name,
    position = player_position,
    team_id = as.character(team_id),
    team = team_name,
    dfs_id = as.character(dfs_id),
    dfs_salary = dfs_salary,
    dfs_fpts = dfs_fpts)
}

#' Tidy Daily Games
#' @param j msf object
#' @param ... additional arguments. currently unused
#' @export
tidy.msf_games <- function(j, ...) {
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
  away_score <- purrr::map_int(scores, "awayScoreTotal", .default = NA_integer_)
  home_score <- purrr::map_int(scores, "homeScoreTotal", .default = NA_integer_)

  tibble::tibble(
    game_id = as.character(game_id),
    game_time = game_time,
    game_status = game_status,
    away_id = as.character(away_id),
    away_team = away_team,
    away_score = away_score,
    home_id = as.character(home_id),
    home_team = home_team,
    home_score = home_score
  )
}

#' Tidy Daily Odds
#' @param j msf object
#' @param ... additional arguments. currently unused
#' @export
tidy.msf_odds_gamelines  <- function(j, ...) {
  gamelines <- j$gameLines

  # game
  games <- purrr::map(gamelines, "game")
  game_id <- purrr::map_int(games, "id", .default = NA_integer_)
  game_time <- purrr::map_chr(games, "startTime", .default = NA_character_)
  game_time <- msf_time(game_time)

  # lines
  lines <- purrr::map(gamelines, "lines")
  lines <- purrr::map(lines, ~purrr::map(.x, tidy_lines))

  # results
  tibble::tibble(
    game_id = as.character(game_id),
    game_time = game_time,
    lines = lines
  )
}

#' Tidy MoneyLines
#'
#' Currently only exracts moneylines, not point spreads or over/unders.
#' @param j json data
#' @keywords internal
tidy_lines <- function(j) {
  # source
  source_name <- j$source$name

  # moneyline
  to_keep <- purrr::map_lgl(j$moneyLines, ~ .x$moneyLine$gameSegment == "FULL", .default = FALSE)
  ml <- j$moneyLines[to_keep]

  # time odds were issued
  time_line <- purrr::map_chr(ml, "asOfTime", .default = NA_character_)
  time_line <- msf_time(time_line)

  # home/away odds
  moneylines <- purrr::map(ml, "moneyLine")
  away_line <- purrr::map_int(moneylines, c("awayLine", "american"), .default = NA_integer_)
  home_line <- purrr::map_int(moneylines, c("homeLine", "american"), .default = NA_integer_)


  tibble::tibble(
    source = source_name,
    time = time_line,
    away_line = away_line,
    home_line = home_line
  )
}
