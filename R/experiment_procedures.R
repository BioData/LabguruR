
#' Labguru add experiment procedure
#'
#' Add a new experiment procedure to labguru
#'
#' @param name character(1) The name of the experiment procedure
#' @param experiment_id numeric(1) The experiment id for which to add a new procedure
#' @param return character(1) whether the function returns either 'id' (default) or 'all' experiment procedure information
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list with either experiment procedure id only or all experiment procedure information
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_add_experiment_procedure(project_id  = 1,
#'                                  name        = "My new experiment procedure",
#'                                  description = "This experiment procedure contains ...")
#' }
labguru_add_experiment_procedure <- function(name,
                                             experiment_id,
                                             return      = "id",
                                             server      = Sys.getenv("LABGURU_SERVER"),
                                             token       = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  check_arg_single_character(name, null = FALSE)
  check_arg_single_integer(experiment_id, null = FALSE)
  check_arg_char_opts(return, opts = c("id", "all"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)

  # URL
  url <- httr::modify_url(url   = server,
                          path  = "/api/v1/element_containers")

  # Body
  body <- list("token"                = token,
               "item[name]"           = name,
               "item[container_id]"   = experiment_id,
               "item[container_type]" = "Projects::Experiment")
  
  # POST
  parsed <- labguru_post_item(url  = url,
                              body = body)

  # return information
  if (return == "id") {
    list(id = parsed$id)
  } else {
    parsed
  }
}

#' Labguru list experiment_procedures
#' 
#' This function returns information of the available experiment procedures in a data frame.
#'
#' @param experiment_id numeric(1) The experiment id for which to list experiment procedures. 
#' @param page numeric(1) representing the page number of data to request. Limited data can be return in 1 request, incrementally try higher page numbers for more experiments
#' @param get_cols character(1) either 'limited' or 'all' to return a subset or all of the information regarding the experiment procedures
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return dataframe with information of experiment procedures, NULL if no experiment procedures were available for the request
#' @export
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' # The following example shows limited information for experiments in all experiments (default)
#' labguru_list_experiment_procedures(experiment_id = NULL, 
#'                                    page          = 1, 
#'                                    get_cols      = "limited")
#' }
labguru_list_experiment_procedures <- function(experiment_id = NULL,
                                               page          = 1,
                                               get_cols      = "limited",
                                               server        = Sys.getenv("LABGURU_SERVER"), 
                                               token         = Sys.getenv("LABGURU_TOKEN")) {
  
  check_arg_single_integer(experiment_id, null = TRUE)
  check_arg_single_integer(page, null = FALSE)
  check_arg_char_opts(get_cols, c("limited", "all"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)
  
  # CHECK ARG experiment_ID (can be null)
  
  # URL
  url <- httr::modify_url(url   = server, 
                          path  = "/api/v1/element_containers",
                          query = paste0("token=", token, 
                                         "&page=", page))
  
  parsed <- labguru_list_items(url)
  
  # Empty pages return and empty list 
  if (length(parsed) == 0) {
    message("No experiments were available for this request")
    return(NULL)
  }
  
  # experiment_id not available in parsed result so find experiment_procedure_ids from a get_experiment request
  if (!is.null(experiment_id)) {
    ex_pr_id <- labguru_get_experiment(experiment_id = 1)
    ex_pr_id <- ex_pr_id$experiment_procedures$experiment_procedure.id
      
    parsed <- parsed[parsed$id %in% ex_pr_id, ]
    
    if (nrow(parsed) == 0) {
      message("No experiments were available for this request")
      return(NULL)
    }
  }
  
  # Subset primary elements that can't be NULL
  if (get_cols == "limited") {
    parsed[c("id", "name")]
  } else { 
    parsed
  } 
}


#' Labguru get experiment procedure
#'
#' Takes a experiment procedure id and gets the experiment procedure information.
#'
#' @param experiment_procedure_id numeric(1) id indicating a experiment procedure on labguru server
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list object of labguru experiment procedure
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_get_experiment_procedure(experiment_procedure_id = 1)
#' }
labguru_get_experiment_procedure <- function(experiment_procedure_id,
                                             server = Sys.getenv("LABGURU_SERVER"),
                                             token  = Sys.getenv("LABGURU_TOKEN")) {

  check_arg_single_integer(experiment_procedure_id, null = FALSE)
  check_arg_server(server)
  check_arg_token(token)

  parsed <- labguru_get_by_id(type   = "element_containers",
                              id     = experiment_procedure_id,
                              server = server,
                              token  = token)

  parsed
}






