#' Tidy Players
#' @param j msf object
#' @param ... additional arguments. currently unused
#' @export
tidy.msf_players <- function(j, ...) {
  players <- purrr::map(j[["players"]], "player")

  # player features
  player_id <- purrr::map_int(players, "id")
  player_fname <- purrr::map_chr(players, "firstName")
  player_lname <- purrr::map_chr(players, "lastName")
  player_position <- purrr::map_chr(players, "primaryPosition", .default = NA_character_)
  player_birthdate <- purrr::map_chr(players, "birthDate", .default = NA_character_)
  player_height <- purrr::map_chr(players, "height", .default = NA_character_)
  player_weight <- purrr::map_int(players, "weight", .default = NA_integer_)
  player_handedness <- purrr::map(players, "handedness", .default = list())
  official_id <- purrr::map_int(players, ~.x[["externalMappings"]][[1]][["id"]], .default = NA_integer_)
  official_source <- purrr::map_chr(players, ~.x[["externalMappings"]][[1]][["source"]], .default = NA_character_)

  tibble::tibble(
    player_id = as.character(player_id),
    player_name = paste(player_fname, player_lname),
    position = player_position,
    birthdate = as.Date(player_birthdate),
    height = gsub("\\\"","", player_height),
    weight = player_weight,
    handedness = player_handedness,
    official_id = as.character(official_id),
    official_source = official_source
  )
}
