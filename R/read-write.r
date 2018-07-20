#' Write msf api objects to disk
#' @export
write_msf <- function(j, datadir = ".") {
  path <- file.path(datadir, attr(j, "local_path"))

  # create directory if it doesn't exist
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }

  jsonlite::write_json(j, path)
}

#' Read msf api objects from disk
#' @export
read_msf <- function(path) {
  j <- jsonlite::read_json(path, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                      simplifyMatrix = FALSE)

  obj <- new_api(j, path)
  attr(j, "local_path") <- path

  j
}
