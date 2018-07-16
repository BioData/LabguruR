
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
#' @param url 
#' @param body 
#' @param encode 
#' @param config 
#' @param handle 
#'
#' @return parsed object
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
labguru_post_item <- function(url,
                              body,
                              encode = NULL,
                              config = NULL,
                              handle = NULL) {
  
  # Post
  resp <- httr::POST(url    = url, 
                     body   = body,
                     if (!is.null(encode)) encode = encode,
                     if (!is.null(config)) config = config,
                     if (!is.null(handle)) handle = handle
  )
  
  # Expect resp to be JSON 
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }
  
  # Parse without simplifaction for consistency
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)
  
  # check for request error
  if (httr::http_error(resp)) {
    stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$error), call. = FALSE)
  }
  
  parsed
}



#' Labguru list items
#' 
#' List 
#'
#' @param url 
#' @param body 
#' @param encode 
#' @param config 
#' @param handle 
#'
#' @return parsed object
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
labguru_list_items <- function(url,
                               config = NULL,
                               handle = NULL) {
  
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
