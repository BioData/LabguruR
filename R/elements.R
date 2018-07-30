
#' Labguru add element
#'
#' Add a new element to labguru
#'
#' @param data character(1) 
#' @param experiment_procedure_id numeric(1) The experiment procedure id for which to add a new element
#' @param return character(1) whether the function returns either 'id' (default) or 'all' element information
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list with either element id only or all element information
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_add_element(data                    = "<p>Hello World</p>",
#'                     experiment_procedure_id = 1,
#'                     return                  = "id")
#' }
labguru_add_element <- function(data    = NULL,
                                img_id  = NULL,
                                rscript = NULL,
                                experiment_procedure_id,
                                return  = "id",
                                server  = Sys.getenv("LABGURU_SERVER"),
                                token   = Sys.getenv("LABGURU_TOKEN")) {
  
  check_arg_single_character(data, null = TRUE)
  check_arg_single_integer(img_id, null = TRUE)
  check_arg_single_character(rscript, null = TRUE)
  check_arg_single_integer(experiment_procedure_id, null = FALSE)
  check_arg_char_opts(return, opts = c("id", "all"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)
  
  if (sum(is.null(data), is.null(img_id), is.null(rscript)) != 2) {
    stop("One of 'data', 'img' or 'rscript' must be a character, the other two must be NULL.")
  } 
  if (!is.null(img_id)) {
    data <- paste0('<img src="',
                   server,
                   labguru_get_by_id("attachments", img_id)$meddium_url,
                   '" >')
  } else if (!is.null(rscript)) {
    data <- labguru_read_rscript_to_html(rscript)
  }
  
  # URL
  url <- httr::modify_url(url   = server,
                          path  = "/api/v1/elements")

  # Body
  body <- list("token"                = token,
               "item[data]"           = data,
               "item[container_id]"   = experiment_procedure_id,
               "item[container_type]" = "ExperimentProcedure",
               "item[element_type]"   = "text")

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
    message("No elements were available for this request")
    return(NULL)
  }
  
  # experiment_id not available in parsed result so find element_ids from a get_experiment request
  if (!is.null(experiment_id)) {
    ex_pr_id <- labguru_get_experiment(experiment_id = experiment_id)
    ex_pr_id <- ex_pr_id$elements$element.id
    
    parsed <- parsed[parsed$id %in% ex_pr_id, ]
    
    if (nrow(parsed) == 0) {
      message("No elements were available for this request")
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


#' Labguru get element
#'
#' Takes a element id and gets the element information.
#'
#' @param element_id numeric(1) id indicating a element on labguru server
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list object of labguru element
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_get_element(element_id = 1)
#' }
labguru_get_element <- function(element_id,
                                server = Sys.getenv("LABGURU_SERVER"),
                                token  = Sys.getenv("LABGURU_TOKEN")) {

  check_arg_single_integer(element_id, null = FALSE)

  parsed <- labguru_get_by_id(type   = "elements",
                              id     = element_id,
                              server = server,
                              token  = token)

  parsed
}



#' Labguru read rscript to html
#' 
#' Read an R script and place each line into paragraph tags (<p>)
#' 
#' @param file Path to a .R file, or .txt file
#'
#' @return character(1) paragraphed R script
#'
#' @examples
labguru_read_rscript_to_html <- function(file) {
  
  check_arg_file(file)
  
  stopifnot()
  
  rscript <- readLines(file)
  rscript <- paste("<p>", 
                   paste(rscript, collapse = "</p> <p>"), 
                   "</p>")
  
  rscript
}
