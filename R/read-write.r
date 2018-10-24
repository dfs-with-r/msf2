#' Write msf api objects to disk
#'
#' @param j a list representing a json object
#' @param datadir base directory to write files into
#' @export
write_msf <- function(j, datadir = ".") {
  path <- file.path(datadir, attr(j, "local_path"))

  # create directory if it doesn't exist
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }

  jsonlite::write_json(j, path)
  path
}

#' Read msf api objects from disk
#'
#' @param path filepath where data is located
#' @export
read_msf <- function(path) {
  j <- jsonlite::read_json(path, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                      simplifyMatrix = FALSE)

  obj <- new_api(j, path)
  attr(obj, "local_path") <- path

  obj
}
