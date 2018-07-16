
#' Labguru add project
#'
#' Add a new project to labguru
#'
#' @param title character(1) The title of the project
#' @param description character(1) The description of the project
#' @param return character(1) whether the function returns either 'id' (default) or 'all' project information
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list with either project id only or all project information
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_upload_project(title       = "My new project",
#'                        description = "This project is an analysis of ...")
#' }
labguru_add_project <- function(title, 
                                description = NULL, 
                                return      = "id",
                                server      = Sys.getenv("LABGURU_SERVER"), 
                                token       = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  check_arg_single_character(title, null = FALSE)
  check_arg_single_character(description, null = TRUE)
  check_arg_char_opts(return, opts = c("id", "all"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)
  
  # URL
  url <- httr::modify_url(url   = server, 
                          path  = "/api/v1/projects")
  
  # Body
  body <- list("token"             = token,
               "item[title]"       = title, 
               "item[description]" = description) 
  
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

#' Labguru list projects
#' 
#' This function returns information of the available projects in a data frame.
#'
#' @param page numeric(1) representing the page number of data to request. Limited data can be return in 1 request, incrementally try higher page numbers for more datasets
#' @param get_cols character(1) either 'limited' or 'all' to return a subset or all of the information regarding the projects
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return dataframe with information of projects, NULL if no projects were available for the request
#' @export
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_list_projects()
#' }
labguru_list_projects <- function(page     = 1,
                                  get_cols = "limited",
                                  server   = Sys.getenv("LABGURU_SERVER"), 
                                  token    = Sys.getenv("LABGURU_TOKEN")) {
  
  check_arg_single_integer(page, null = FALSE)
  check_arg_char_opts(get_cols, c("limited", "all"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)
  
  # URL
  url <- httr::modify_url(url   = server, 
                          path  = "/api/v1/projects",
                          query = paste0("token=", token, 
                                         "&page=", page))
  
  parsed <- labguru_list_items(url)
  
  # Empty pages return and empty list 
  if (length(parsed) == 0) {
    message("No experiments were available for this request")
    return(NULL)
  }
  
  # Subset primary elements that can't be NULL
  if (get_cols == "limited") {
    parsed[c("id", "title", "description", "api_url")]
  } else {
    parsed
  }
}


#' Labguru get project
#' 
#' Takes a project id and gets the project with milestones.
#'
#' @param project_id numeric(1) id indicating a project on labguru server
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list of labguru project
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#' labguru_get_project(project_id = 1)
#' }
labguru_get_project <- function(project_id,
                                server = Sys.getenv("LABGURU_SERVER"), 
                                token  = Sys.getenv("LABGURU_TOKEN")) {
  
  check_arg_single_integer(project_id, null = FALSE)
  check_arg_server(server)
  check_arg_token(token)
  
  parsed <- labguru_get_by_id(type   = "projects",
                              id     = project_id,
                              server = server,
                              token  = token)
  
  parsed
    
  # # URL
  # base_url <- server
  # path     <- paste0("/api/v1/projects/", project_id)
  # query    <- paste0("token=", token)
  # 
  # url <- httr::modify_url(url   = base_url, 
  #                         path  = path,
  #                         query = query)
  # 
  # resp <- httr::GET(url)
  # 
  # # Expect resp to be JSON 
  # if (httr::http_type(resp) != "application/json") {
  #   stop("API did not return JSON", call. = FALSE)
  # }
  # 
  # # Parse without simplifaction for consistency
  # parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"), 
  #                              simplifyVector    = FALSE, 
  #                              simplifyDataFrame = TRUE, 
  #                              flatten           = TRUE)
  # 
  # # check for request error
  # if (httr::http_error(resp)) {
  #   stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$error), call. = FALSE)
  # }
  # 
  # parsed
}
