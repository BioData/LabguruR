
#' Labguru get by id
#'
#' Takes a type name and id and gets the information from the API.
#'
#' @param type character(1) type name of what you want to request
#' @param id numeric(1) id indicating a experiment on labguru server
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return parsed object
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_get_by_id(type = "experiments",
#'                   id   = 1)
#' }
labguru_get_by_id <- function(type,
                              id,
                              server = Sys.getenv("LABGURU_SERVER"),
                              token  = Sys.getenv("LABGURU_TOKEN")) {

  # URL
  base_url <- server
  path     <- paste0("/api/v1/", type, "/", id)
  query    <- paste0("token=", token)

  url <- httr::modify_url(url   = base_url,
                          path  = path,
                          query = query)

  # Encoding certain URL characters (like space)
  url <- URLencode(url)

  resp <- httr::GET(url)

  # Expect resp to be JSON
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }

  # Parse without simplifaction for consistency
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"),
                               simplifyVector    = FALSE,
                               simplifyDataFrame = TRUE,
                               flatten           = TRUE)

  # check for request error
  if (httr::http_error(resp)) {
    stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$error), call. = FALSE)
  }

  parsed
}


#' Labguru post item
#'
#' POST a body to a url with optional arguments. Function includes standard error handling.
#'
#' @param url url to the labGuru instance API endpoint
#' @param body content to be included in POST call
#' @param encode encoding of the request (defaults to NULL, should be JSON)
#' @param config other configuration options passed on to httr POST
#' @param handle handling arguments passed on to httr POST
#'
#' @return parsed object
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' # helper function for POST requests
labguru_post_item <- function(url,
                              body,
                              encode = NULL,
                              config = NULL,
                              handle = NULL) {

  # Encoding certain URL characters (like space)
  url <- URLencode(url)

  # Build the list such that NULLS are completely left out.
  args <- list()
  args$url    <- url
  args$body   <- body
  args$encode <- encode
  args$config <- config
  args$handle <- handle

  resp <- do.call(httr::POST, args)

  # Expect resp to be JSON
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }

  # Parse without simplifaction for consistency
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)

  # check for request error
  if (httr::http_error(resp)) {
    message(parsed$errors)
    stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$errors), call. = FALSE)
  }

  parsed
}



#' Labguru list items
#'
#' List
#'
#' @param url url to the labGuru instance API endpoint
#' @param config other configuration options passed on to httr GET
#' @param handle handling arguments passed on to httr POST
#'
#' @return parsed object
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' # helper function for GET requests
labguru_list_items <- function(url,
                               config = NULL,
                               handle = NULL) {

  # Encoding certain URL characters (like space)
  url <- URLencode(url)

  # Post
  resp <- httr::GET(url = url,
                    if (!is.null(config)) config = config,
                    if (!is.null(handle)) handle = handle
  )

  # Expect resp to be JSON
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }

  # Parse with simplifaction to dataframe
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"),
                               simplifyVector    = FALSE,
                               simplifyDataFrame = TRUE,
                               flatten           = TRUE)

  # check for request error
  if (httr::http_error(resp)) {
    stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$error), call. = FALSE)
  }

  parsed
}
