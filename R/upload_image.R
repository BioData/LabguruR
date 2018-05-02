
#' Labguru upload image
#' 
#' Upload an image file to Labguru
#'
#' @param file_path File path of the image file
#' @param name Name of the image to use on Labguru
#' @param server A character string indicating the server URL
#' @param token An access token for API authentication
#'
#' @return NULL
#' @export
#'
#' @examples
#' library(ggplot2)
#' plot <- ggplot(mtcars, aes(x = mpg, y = cyl)) + geom_point()
#' ggsave("my_plot.png", plot)
#' 
#' labguru_upload_image(file_path = "my_plot.png"
#'                      name      = "My plot.png")
labguru_upload_image <- function(file_path, 
                                 name,
                                 server = Sys.getenv("LABGURU_SERVER"), 
                                 token  = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  if (server == "") stop("Argument server not set")
  if (token == "") stop("Argument token not set")
  if (!is.character(file.path)) stop("Argument file_path is not a character string")
  if (!file.exists(file.path)) stop(paste("Can't find file", file_path))
  if (!is.character(name)) stop("Argument name is not a character string")
  
  # URL
  base_url <- server
  path     <- "/api/v1/attachments"
  query    <- paste0("token=", token)
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path, 
                          query = query)
  
  # Body
  body <- list("item[title]"      = name, 
               "item[attachment]" = httr::upload_file(file_path)) 
  
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
  
  return(NULL)
}
