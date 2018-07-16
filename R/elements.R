#' 
#' #' Labguru add element
#' #'
#' #' Add a new element to labguru
#' #'
#' #' @param name character(1) The name of the element
#' #' @param experiment_id numeric(1) The experiment id for which to add a new procedure
#' #' @param return character(1) whether the function returns either 'id' (default) or 'all' element information
#' #' @param server character(1) indicating the server URL
#' #' @param token character(1) access token for API authentication
#' #'
#' #' @return list with either element id only or all element information
#' #' @export
#' #'
#' #' @import httr
#' #' @importFrom jsonlite fromJSON
#' #'
#' #' @examples
#' #' \dontrun{
#' #' labguru_add_element(project_id  = 1,
#' #'                     name        = "My new element",
#' #'                     description = "This element contains ...")
#' #' }
#' labguru_add_element <- function(name,
#'                                 experiment_id,
#'                                 return      = "id",
#'                                 server      = Sys.getenv("LABGURU_SERVER"),
#'                                 token       = Sys.getenv("LABGURU_TOKEN")) {
#'   
#'   # Test arguments
#'   
#'   # CHECK ARG experiment ID
#'   
#'   stopifnot(is.character(return))
#'   stopifnot(length(return) == 1)
#'   stopifnot(return %in% c('id', 'all'))
#'   
#'   # URL
#'   url <- httr::modify_url(url   = server,
#'                           path  = "/api/v1/element_containers")
#'   
#'   # Body
#'   body <- list("token"                = token,
#'                "item[name]"           = name,
#'                "item[container_id]"   = experiment_id,
#'                "item[container_type]" = "Projects::Experiment")
#'   
#'   # POST
#'   parsed <- labguru_post_item(url  = url,
#'                               body = body)
#'   
#'   # return information
#'   if (return == "id") {
#'     list(id = parsed$id)
#'   } else {
#'     parsed
#'   }
#' }

#' Labguru list elements
#' 
#' This function returns information of the available elements in a data frame.
#'
#' @param experiment_id numeric(1) The experiment id for which to list elements. 
#' @param page numeric(1) representing the page number of data to request. Limited data can be return in 1 request, incrementally try higher page numbers for more elements
#' @param get_cols character(1) either 'limited' or 'all' to return a subset or all of the information regarding the elements
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return dataframe with information of elements, NULL if no elements were available for the request
#' @export
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' # The following example shows limited information for elements in all elements (default)
#' labguru_list_elements(experiment_id = NULL, 
#'                       page          = 1, 
#'                       get_cols      = "limited")
#' }
labguru_list_elements <- function(experiment_id = NULL,
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
                          path  = "/api/v1/elements",
                          query = paste0("token=", token, 
                                         "&page=", page))
  
  parsed <- labguru_list_items(url)
  
  # Empty pages return and empty list 
  if (length(parsed) == 0) {
    message("No experiments were available for this request")
    return(NULL)
  }
  
  # experiment_id not available in parsed result so find element_ids from a get_experiment request
  if (!is.null(experiment_id)) {
    ex_pr_id <- labguru_get_experiment(experiment_id = 1)
    ex_pr_id <- ex_pr_id$elements$element.id
    
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


#' #' Labguru get element
#' #'
#' #' Takes a element id and gets the element information.
#' #'
#' #' @param element_id numeric(1) id indicating a element on labguru server
#' #' @param server character(1) indicating the server URL
#' #' @param token character(1) access token for API authentication
#' #'
#' #' @return list object of labguru element
#' #' @export
#' #'
#' #' @import httr
#' #' @importFrom jsonlite fromJSON
#' #'
#' #' @examples
#' #' \dontrun{
#' #' labguru_get_element(element_id = 1)
#' #' }
#' labguru_get_element <- function(element_id,
#'                                 server = Sys.getenv("LABGURU_SERVER"),
#'                                 token  = Sys.getenv("LABGURU_TOKEN")) {
#'   
#'   # check arg element_id
#'   
#'   parsed <- labguru_get_by_id(type   = "element_containers",
#'                               id     = element_id,
#'                               server = server,
#'                               token  = token)
#'   
#'   parsed
#' }
#' 
#' 




