
#' Labguru upload file
#' 
#' Upload a file to Labguru
#'
#' @param file_path String - File path of the file
#' @param name String - Name of the file to use on Labguru
#' @param description (optional) String - Text description for this attachment
#' @param attach_to_uuid (optional) String - The LG_UUID of the object to which this file should be attached
#' @param server A character string indicating the server URL
#' @param token An access token for API authentication
#'
#' @return NULL
#' @export
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
labguru_upload_file <- function(file_path, 
                                name,
                                description    = NULL,
                                attach_to_uuid = NULL,
                                server         = Sys.getenv("LABGURU_SERVER"), 
                                token          = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  if (server == "") stop("Argument server not set")
  if (token == "") stop("Argument token not set")
  if (!is.character(file_path)) stop("Argument file_path is not a character string")
  if (!file.exists(file_path)) stop(paste("Can't find file", file_path))
  if (!is.character(name)) stop("Argument name is not a character string")
  if (!is.null(description)) {
    if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
  }
  
  if (!is.null(attach_to_uuid)) stop("Sorry, attach_to_uuid can't be used yet")
  
  # URL
  base_url <- server
  path     <- "/api/v1/attachments"
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path)
  
  # Body
  body <- list("token"                = token,
               "item[title]"          = name, 
               "item[description]"    = description,
               "item[attachment]"     = httr::upload_file(file_path),
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
  
  invisible(TRUE)
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
#' @param file_path 
#' @param name 
#' @param server 
#' @param token 
#'
#' @return
#' @export
#'
#' @examples
labguru_upload_visualization <- function(file_path, 
                                         name,
                                         # description    = NULL,
                                         # attach_to_uuid = NULL,
                                         server         = Sys.getenv("LABGURU_SERVER"), 
                                         token          = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  if (server == "") stop("Argument server not set")
  if (token == "") stop("Argument token not set")
  if (!is.character(file_path)) stop("Argument file_path is not a character string")
  if (!file.exists(file_path)) stop(paste("Can't find file", file_path))
  if (!is.character(name)) stop("Argument name is not a character string")
  # if (!is.null(description)) {
  #   if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
  # }
  # 
  # if (!is.null(attach_to_uuid)) stop("Sorry, attach_to_uuid can't be used yet")
  
  # URL
  base_url <- server
  path     <- "/api/v1/attachments" # OR "/api/v1/visualizations"
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path)
  
  # Body
  body <- list("token"                = token,
               "item[title]"          = name, 
               # "item[description]"    = description,
               "item[attachment]"     = httr::upload_file(file_path)) #,
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
  
  invisible(TRUE)
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
#' @export
#'
#' @examples
labguru_link_visualization <- function(dataset_id, 
                                         attachment_id,
                                         name,
                                         description    = NULL,
                                         # attach_to_uuid = NULL,
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