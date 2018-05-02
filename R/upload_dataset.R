


#' Labguru Upload Dataset
#'
#' Upload a dataset to labguru
#'
#' @param dataset An R data frame
#' @param name A character string
#' @param description A character string or NULL (default) to ignore
#' @param server A character string indicating the server URL
#' @param token An access token for API authentication
#'
#' @return NULL
#' @export
#'
#' @examples
#' labguru_upload_dataset(dataset    = mtcars,
#'                        name       = "mtcars",
#'                        desription = "Motor Trend Car Road Tests data")
labguru_upload_dataset <- function(dataset, 
                                   name, 
                                   description = NULL, 
                                   server = Sys.getenv("LABGURU_SERVER"), 
                                   token  = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  if (server == "") stop("Argument server not set")
  if (token == "") stop("Argument token not set")
  if (!is.data.frame(dataset)) stop("Argument dataset is not a data frame")
  if (!is.character(name)) stop("Argument name is not a character string")
  if (!is.null(description)) {
    if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
  }

  # URL
  base_url <- server
  path     <- "/api/v1/datasets"
  query    <- paste0("token=", token, 
                     "&name=", name, 
                     if (!is.null(description)) {paste0("&description=", description)})
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path, 
                          query = query)
  
  # Post
  resp <- httr::POST(url    = url, 
                     body   = as.data.frame(dataset), 
                     encode = "json")
  
  # # Preferred way
  # # BODY
  # body <- list("data"  = dataset,
  #              "item"  = list(name = name),
  #              "token" = token)
  # 
  # # POST
  # resp <- httr::POST(url    = url,
  #                    body   = body,
  #                    encode = "json")
  
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
  
  invisible(TRUE)
}




