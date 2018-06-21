
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
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_upload_dataset(dataset    = mtcars,
#'                        name       = "mtcars",
#'                        desription = "Motor Trend Car Road Tests data")
#' }
labguru_upload_dataset <- function(dataset, 
                                   name, 
                                   # description = NULL, 
                                   server = Sys.getenv("LABGURU_SERVER"), 
                                   token  = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  check_arg_dataset(dataset)
  check_arg_name(name)
  # check_arg_description(description)
  check_arg_server(server)
  check_arg_token(token)

  # URL
  base_url <- server
  path     <- "/api/v1/datasets"
  query    <- paste0("token=", token, 
                     "&name=", name)#, 
                     # if (!is.null(description)) {paste0("&description=", description)})
  
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
  
  # invisible(TRUE)
  # Return ID
  list(id = parsed$dataset_id)
}




#' Labguru list datasets
#' 
#' This function returns information of the available datasets in a data frame.
#'
#' @param page Single numeric representing the page number of data to request. Limited data can be return in 1 request, incrementally try higher page numbers for more datasets
#' @param get_cols Single character either 'limited' or 'all' to return a subset or all of the information regarding the datasets
#' @param server Single character indicating the server URL
#' @param token Single character access token for API authentication
#'
#' @return dataframe with information of datasets, NULL if no datasets were available for the request
#' @export
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_list_datasets()
#' }
labguru_list_datasets <- function(page     = 1,
                                  get_cols = "limited",
                                  server   = Sys.getenv("LABGURU_SERVER"), 
                                  token    = Sys.getenv("LABGURU_TOKEN")) {
  
  check_arg_page(page)
  check_arg_server(server)
  check_arg_token(token)
  check_arg_get_cols(get_cols, c("limited", "all"))
  
  # URL
  base_url <- server
  path     <- "/api/v1/datasets"
  query    <- paste0("token=", token, 
                     "&page=", page)
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path,
                          query = query)
  
  resp <- httr::GET(url)
  
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
  
  if (length(parsed) == 0) {
    message("No data was available for this request")
    return(NULL)
  }
  
  # Subset primary elements that can't be NULL
  if (get_cols == "limited") {
    parsed[c("id", "name", "description", "api_url")]
  } else {
    parsed
  }
}






#' Labguru download dataset
#' 
#' Takes a dataset id and donwloads the dataset.
#'
#' @param dataset_id Single numeric id indicating a dataset on labguru server
#' @param server Single character indicating the server URL
#' @param token Single character access token for API authentication
#'
#' @return data frame of labguru dataset
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_downlaod_datasets(dataset_id = 1)
#' }
labguru_download_dataset <- function(dataset_id,
                                     server = Sys.getenv("LABGURU_SERVER"), 
                                     token  = Sys.getenv("LABGURU_TOKEN")) {
  # GET /api/v1/datasets/1.json?token=YOUR_TOKEN_HERE
  # URL
  base_url <- server
  path     <- paste0("/api/v1/datasets/", dataset_id)
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
  
  parsed$vectors
}

