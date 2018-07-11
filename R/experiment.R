
#' Labguru add experiment
#'
#' Add a new experiment to labguru
#'
#' @param title character(1) The title of the experiment
#' @param project_id numeric(1) The project id for which to add a new experiment
#' @param folder_id numeric(1) The folder id for which to add a new experiment
#' @param description character(1) The description of the experiment
#' @param return character(1) whether the function returns either 'id' (default) or 'all' experiment information
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list with either experiment id only or all experiment information
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_add_experiment(project_id  = 1,
#'                        title       = "My new experiment",
#'                        description = "This experiment contains ...")
#' }
labguru_add_experiment <- function(title, 
                                   project_id,
                                   folder_id,
                                   description = NULL, 
                                   return      = "id",
                                   server      = Sys.getenv("LABGURU_SERVER"), 
                                   token       = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  check_arg_title(title)
  check_arg_description(description)
  check_arg_server(server)
  check_arg_token(token)
  
  # CHECK ARG PROJECT ID
  # CHECK ARG FOLDER ID
  
  stopifnot(is.character(return))
  stopifnot(length(return) == 1)
  stopifnot(return %in% c('id', 'all'))
  
  # URL
  url <- httr::modify_url(url   = server, 
                          path  = "/api/v1/experiments")
  
  # Body
  body <- list("token"              = token,
               "item[project_id]"   = project_id,
               "item[milestone_id]" = folder_id,
               "item[title]"        = title, 
               "item[description]"  = description) 
  
  # POST
  parsed <- labguru_post_item(url  = url,
                              body = body)
  
  # # Post
  # resp <- httr::POST(url    = url, 
  #                    body   = body)
  # 
  # # Expect resp to be JSON 
  # if (httr::http_type(resp) != "application/json") {
  #   stop("API did not return JSON", call. = FALSE)
  # }
  # 
  # # Parse without simplifaction for consistency
  # parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)
  # 
  # # check for request error
  # if (httr::http_error(resp)) {
  #   stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$error), call. = FALSE)
  # }
  
  # return information
  if (return == "id") {
    list(id = parsed$id)
  } else {
    parsed
  }
}

#' Labguru list experiments
#' 
#' This function returns information of the available experiments in a data frame.
#'
#' @param folder_id numeric(1) The folder id for which to list experiments. 
#' @param page numeric(1) representing the page number of data to request. Limited data can be return in 1 request, incrementally try higher page numbers for more experiments
#' @param get_cols character(1) either 'limited' or 'all' to return a subset or all of the information regarding the experiments
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return dataframe with information of experiments, NULL if no projects were available for the request
#' @export
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' # The following example shows limited information for experiments in all folders (default)
#' labguru_list_experiments(folder_id = NULL, 
#'                          page      = 1, 
#'                          get_cols  = "limited")
#' }
labguru_list_experiments <- function(folder_id = NULL,
                                     page      = 1,
                                     get_cols  = "limited",
                                     server    = Sys.getenv("LABGURU_SERVER"), 
                                     token     = Sys.getenv("LABGURU_TOKEN")) {
  
  check_arg_page(page)
  check_arg_server(server)
  check_arg_token(token)
  check_arg_get_cols(get_cols, c("limited", "all"))
  
  # CHECK ARG FOLDER_ID (can be null)
  
  # URL
  url <- httr::modify_url(url   = server, 
                          path  = "/api/v1/experiments",
                          query = paste0("token=", token, 
                                         "&page=", page))
  
  parsed <- labguru_list_items(url)
  
  # Empty pages return and empty list 
  if (length(parsed) == 0) {
    message("No experiments were available for this request")
    return(NULL)
  }
  
  # folder_id not available in parsed result so find experiment_ids from a get_folder request
  if (!is.null(folder_id)) {
    exp_id <- labguru_get_folder(folder_id = folder_id, server = server, token = token)
    exp_id <- exp_id$experiments$id
      
    parsed <- parsed[parsed$id %in% exp_id, ]
    
    if (nrow(parsed) == 0) {
      message("No experiments were available for this request")
      return(NULL)
    }
  }
  
  # Subset primary elements that can't be NULL
  if (get_cols == "limited") {
    parsed[c("id", "title", "api_url")]
  } else { 
    parsed
  } 
}


#' Labguru get experiment
#' 
#' Takes a experiment id and gets the experiment information.
#'
#' @param experiment_id numeric(1) id indicating a experiment on labguru server
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list object of labguru experiment
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_get_experiment(experiment_id = 1)
#' }
labguru_get_experiment <- function(experiment_id,
                                   server = Sys.getenv("LABGURU_SERVER"), 
                                   token  = Sys.getenv("LABGURU_TOKEN")) {
  
  # check arg experiment_id
  
  parsed <- labguru_get_by_id(type   = "experiments",
                              id     = experiment_id,
                              server = server,
                              token  = token)
  
  parsed
}




labguru_add_attachment_to_experiment <- function() {
  
  
  
}




