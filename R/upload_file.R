
#' Labguru upload file
#' 
#' Upload a file to Labguru
#'
#' @param file Single character that is the path to a file
#' @param title Single character as visualization title on Labguru
#' @param desciption Single character to describe visualization on Labguru
#' @param attach_to_uuid (optional) single character that is the LG_UUID of the object to which this file should be attached
#' @param server Single character indicating the server URL
#' @param token Single character access token for API authentication
#'
#' @return list containing uploaded id and url
#' @export
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
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
labguru_upload_file <- function(file, 
                                title,
                                description    = NULL,
                                attach_to_uuid = NULL,
                                server         = Sys.getenv("LABGURU_SERVER"), 
                                token          = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  check_arg_file(file)
  check_arg_title(title)
  check_arg_description(description)
  check_arg_attach_to_uuid(attach_to_uuid)
  check_arg_server(server)
  check_arg_token(token)
  
  # if (server == "") stop("Argument server not set")
  # if (token == "") stop("Argument token not set")
  # if (!is.character(file)) stop("Argument file_path is not a character string")
  # if (!file.exists(file)) stop(paste("Can't find file", file))
  # if (!is.character(title)) stop("Argument name is not a character string")
  # if (!is.null(description)) {
  #   if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
  # }
  # 
  # if (!is.null(attach_to_uuid)) stop("Sorry, attach_to_uuid can't be used yet")
  
  # URL
  base_url <- server
  path     <- "/api/v1/attachments"
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path)
  
  # Body
  body <- list("token"                = token,
               "item[title]"          = title, 
               "item[description]"    = description,
               "item[attachment]"     = httr::upload_file(file),
               "item[attach_to_uuid]" = attach_to_uuid) 
  
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





# base_url <- "https://jonathan.labguru.com"
# url <- paste(base_url, "/api/v1/attachments",sep="")
# fd <-  list ("item[title]"  = 'comps graph', "item[attachment]" = upload_file("grazing.png"), "token" = token) 
# attachment <- POST(url, body = fd)
# dat <- jsonlite::fromJSON(content(attachment, as="text"))
# 
# url <- paste(base_url, "/api/v1/visualizations","?token=",token,sep="")
# dataset_id <- 1 
# attachment_id <- dat$id
# data <- list( "dataset_id" = dataset_id, "attachment_id" = attachment_id, "name" = "Root VS BioMass", description = "")
# item_to_post <- list("item" = data)
# element <- httr::POST(url, body = item_to_post,encode ="json")


#' Labguru upload visualization
#' 
#' 
#' 
#' @param file Single character that is a path to a file
#' @param title Single character as visualization title on Labguru
#' @param desciption Single character to describe visualization on Labguru
#' @param dataset_id Numeric to link the visualization to dataset(s) on Labguru
#' @param server 
#' @param token 
#'
#' @return
#' @export
#'
#' @examples
labguru_upload_visualization <- function(file, 
                                         title,
                                         description = NULL,
                                         dataset_id  = NULL,
                                         server      = Sys.getenv("LABGURU_SERVER"), 
                                         token       = Sys.getenv("LABGURU_TOKEN")) {
  
  # Check arguments
  check_arg_file(file)
  check_arg_title(title)
  check_arg_description(description)
  check_arg_dataset_id(dataset_id)
  check_arg_server(server)
  check_arg_token(token)
  
  uploaded_file <- labguru_upload_file(file           = file,
                                       title          = title,
                                       description    = description,
                                       attach_to_uuid = attach_to_uuid,
                                       server         = server,
                                       token          = token)
  
  uploaded_file$id
  
  if (!is.null(dataset_id)) {
    # Link
    labguru_link_visualization(dataset_id    = datset_id,
                               attachment_id = uploaded_file$id,
                               name          = "?",
                               description   = "?",
                               server        = server,
                               token         = token)
  }
  
  
  # # Test arguments
  # if (server == "") stop("Argument server not set")
  # if (token == "") stop("Argument token not set")
  # if (!is.character(file_path)) stop("Argument file_path is not a character string")
  # if (!file.exists(file_path)) stop(paste("Can't find file", file_path))
  # if (!is.character(name)) stop("Argument name is not a character string")
  # # if (!is.null(description)) {
  # #   if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
  # # }
  # # 
  # # if (!is.null(attach_to_uuid)) stop("Sorry, attach_to_uuid can't be used yet")
  
}



# url <- paste(base_url, "/api/v1/visualizations","?token=",token,sep="")
# dataset_id <- 1 
# attachment_id <- dat$id
# data <- list( "dataset_id" = dataset_id, "attachment_id" = attachment_id, "name" = "Root VS BioMass", description = "")
# item_to_post <- list("item" = data)
# element <- httr::POST(url, body = item_to_post,encode = "json")

#' Title
#'
#' @param dataset_id 
#' @param attachment_id 
#' @param name
#' @param description 
#' @param server 
#' @param token 
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
  if (server == "") stop("Argument server not set")
  if (token == "") stop("Argument token not set")
  # if (!is.character(file_path)) stop("Argument file_path is not a character string")
  # if (!file.exists(file_path)) stop(paste("Can't find file", file_path))
  if (!is.character(name)) stop("Argument name is not a character string")
  # if (!is.null(description)) {
  #   if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
  # }
  # 
  # if (!is.null(attach_to_uuid)) stop("Sorry, attach_to_uuid can't be used yet")
  
  # URL
  base_url <- server
  path     <- "/api/v1/visualizations"
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path)
  
  # Body
  # data <- list("dataset_id"    = dataset_id, 
  #              "attachment_id" = attachment_id, 
  #              "name"          = name, 
  #              description     = description)
  # body <- list("token" = token,
  #              "item"  = data)
  
  # OR
  body <- list("token"               = token,
               "item[dataset_id]"    = dataset_id,
               "item[attachment_id]" = attachment_id,
               "item[name]"          = name,
               "item[description]"   = description)
  
  # Post
  resp <- httr::POST(url    = url, 
                     body   = body,
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
  
  invisible(TRUE)
}