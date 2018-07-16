
#' Labguru upload file
#' 
#' Upload a file to Labguru
#'
#' @param file character(1) that is the path to a file
#' @param title character(1) as visualization title on Labguru
#' @param description character(1) to describe visualization on Labguru
#' @param attach_to_uuid (optional) character(1) that is the LG_UUID of the object to which this file should be attached
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list containing uploaded id and url
#' @export
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' data("compensation")
#' 
#' library(ggplot2)
#' plot <- ggplot(compensation,aes(x = Root, y = Fruit, colour = Grazing)) +
#'   geom_point(size = 5) +
#'   xlab("Root Biomass") +
#'   ylab("Fruit Production") +
#'   theme_bw()
#'   
#' ggsave("grazing.png", plot)
#' 
#' labguru_upload_image(file_path   = "grazing.png",
#'                      name        = "Grazing",
#'                      description = "Fruit production versus Root biomass for grazed and ungrazed")
#' }
labguru_upload_file <- function(file, 
                                title,
                                description    = NULL,
                                # attach_to_uuid = NULL,
                                server         = Sys.getenv("LABGURU_SERVER"), 
                                token          = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  check_arg_file(file)  
  check_arg_single_character(title, null = FALSE)
  check_arg_single_character(description, null = TRUE)
  check_arg_server(server)
  check_arg_token(token)
  
  # URL
  base_url <- server
  path     <- "/api/v1/attachments"
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path)
  
  # Body
  body <- list("token"                = token,
               "item[title]"          = title, 
               "item[description]"    = description,
               "item[attachment]"     = httr::upload_file(file))#,
               # "item[attach_to_uuid]" = attach_to_uuid) 
  
  # Post
  resp <- httr::POST(url    = url, 
                     body   = body)
  
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
  # RETURN ID AND URL
  list(id  = parsed$id,
       url = parsed$api_url)
}





#' Labguru upload visualization
#' 
#' 
#' 
#' @param file character(1) that is a path to a file
#' @param title character(1) that is the title for the uploaded file on Labguru
#' @param dataset_id Numeric(1) to link the visualization to dataset(s) on Labguru
#' @param name character(1) that is the name for visualisation link with database 
#' @param description character(1) to describe visualization on Labguru
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return
#' @export
#'
#' @examples
labguru_upload_visualization <- function(file, 
                                         title,
                                         dataset_id     = NULL,
                                         name           = NULL,
                                         description    = NULL,
                                         # attach_to_uuid = NULL,
                                         server         = Sys.getenv("LABGURU_SERVER"), 
                                         token          = Sys.getenv("LABGURU_TOKEN")) {
  
  # Check arguments
  check_arg_file(file)  
  check_arg_single_character(title, null = FALSE)
  check_arg_single_integer(dataset_id, null = FALSE)
  check_arg_single_character(name, null = TRUE)
  check_arg_single_character(description, null = TRUE)
  check_arg_server(server)
  check_arg_token(token)
  
  # Upload file
  uploaded_file <- labguru_upload_file(file           = file,
                                       title          = title,
                                       description    = description,
                                       # attach_to_uuid = attach_to_uuid,
                                       server         = server,
                                       token          = token)
  
  # Link file to dataset if dataset_id is set
  if (!is.null(dataset_id)) {
    # Check arguments
    check_arg_dataset_id(dataset_id)
    check_arg_name(name)
    
    link <- labguru_link_visualization(dataset_id    = dataset_id,
                                       attachment_id = uploaded_file$id,
                                       name          = name,
                                       description   = description,
                                       server        = server,
                                       token         = token)
    list(file = uploaded_file,
         link = link)
  } else {
    uploaded_file
  }
}



#' Labguru link visualization
#'
#' @param dataset_id numeric(1) The dataset id for which to link visualization
#' @param attachment_id numeric(1) The attachment id for which to link visualization
#' @param name character(1) the name
#' @param description character(1) The description of the folder
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
labguru_link_visualization <- function(dataset_id, 
                                       attachment_id,
                                       name,
                                       description    = NULL,
                                       server         = Sys.getenv("LABGURU_SERVER"), 
                                       token          = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  check_arg_single_integer(dataset_id, null = FALSE)
  check_arg_single_integer(attachment_id, null = FALSE)
  check_arg_single_character(name, null = FALSE)
  check_arg_single_character(description, null = TRUE)
  check_arg_server(server)
  check_arg_token(token)

  # URL
  base_url <- server
  path     <- "/api/v1/visualizations"
  
  # # Preferred
  # url <- httr::modify_url(url   = base_url, 
  #                         path  = path)
  # 
  # # Body
  # body <- list("token"               = token,
  #              "item[dataset_id]"    = dataset_id,
  #              "item[attachment_id]" = attachment_id,
  #              "item[name]"          = name,
  #              "item[description]"   = description)
  # # Post
  # resp <- httr::POST(url    = url, 
  #                    body   = body,
  #                    encode = "json")
  
  url <- paste(base_url, "/api/v1/visualizations", "?token=", token, sep="")
  
  data <- list(dataset_id    = dataset_id, 
               attachment_id = attachment_id, 
               name          = name, 
               description   = description)
  
  item_to_post <- list(item = data)
  
  resp <- httr::POST(url, 
                     body = item_to_post,
                     encode = "json")
  
  
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
  # RETURN ID AND URL
  list(id  = parsed$id,
       url = parsed$api_url)
}