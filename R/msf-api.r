#' MySportsFeeds API v2
#'
#' @param path path to pull
#' @param query query parameters
#' @export
msf_api <- function(path, query = NULL) {
  # url
  baseurl <- "https://api.mysportsfeeds.com"
  path <- paste0("v2.0/pull/", path)
  url <- httr::modify_url(baseurl, path = path, query = query)

  # authentication
  api_key <- Sys.getenv("MSF_API")

  # get data
  resp <- httr::GET(url, httr::authenticate(api_key, "MYSPORTSFEEDS"))

  # get content
  page <- httr::content(resp, "text", encoding = "utf-8")

  # check errors
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "MySportsFeeds API request failed [%s]\n[%s]",
        httr::status_code(resp), url
      ),
      call. = FALSE
    )
  }

  # check data type
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # parse content
  json <- jsonlite::fromJSON(page, simplifyVector = TRUE,
                             simplifyDataFrame = FALSE, simplifyMatrix = FALSE)

  # return S3 object
  new_api(json, path)
}

new_api <- function(json, path) {
  json <- msf_class(json, "api")
  attr(json, "path") <- path

  json
}

#' @export
print.msf_api <- function(x, ...) {
  cat("<MySportsFeeds ", attr(x, "path"), ">\n", sep = "")
  utils::str(x, 2, give.attr = FALSE)
  invisible(x)
}

#' Add MySportsFeeds API key to R environment file
#'
#' The functions in this package will look for your MySportsFeeds API key
#' in a special environment variable \code{MSF_API}. This function opens the .Renviron
#' file and tells what line you should add. Just copy-and-paste the line into the file,
#' save it, and restart R.
#'
#' @param key API key provided by MySportsFeeds
#' @export
add_key <- function(key) {
  msg <- sprintf("MSF_API=%s", key)

  usethis:::todo("Add this line to your .Renviron file:")
  usethis:::code_block(msg, copy = FALSE)
  usethis::edit_r_environ()
}
