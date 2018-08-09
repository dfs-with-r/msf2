#' @export
parse_players <- function(j) {
  players <- purrr::map(j[["players"]], "player")

  # player features
  player_id <- purrr::map_int(players, "id")
  player_fname <- purrr::map_chr(players, "firstName")
  player_lname <- purrr::map_chr(players, "lastName")
  player_position <- purrr::map_chr(players, "position")
  player_birthdate <- purrr::map_chr(players, "birthDate", .default = NA_character_)
  player_height <- purrr::map_chr(players, "height", .default = NA_character_)
  player_weight <- purrr::map_int(players, "weight", .default = NA_integer_)
  player_bhand <- purrr::map_chr(players, c("handedness", "bats"), .default = NA_character_)
  player_phand <- purrr::map_chr(players, c("handedness", "throws"), .default = NA_character_)
  official_id <- purrr::map_int(players, c("externalMapping", "id"), .default = NA_integer_)
  official_source <- purrr::map_chr(players, c("externalMapping", "source"), .default = NA_character_)

  data.frame(
    player_id = as.character(player_id),
    player_name = paste(player_fname, player_lname),
    position = player_position,
    birthdate = as.Date(player_birthdate),
    height = gsub("\\\"","", player_height),
    weight = player_weight,
    b_hand = player_bhand,
    p_hand = player_phand,
    official_id = as.character(official_id),
    official_source = official_source,
    stringsAsFactors = FALSE
  )
}
