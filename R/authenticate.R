

#' Labguru Authenticate
#'
#' This function authenticates a user by email and password and returns a token and sets the system 
#' variable LABGURU_TOKEN to that token by default.
#'
#' @param email Character - Email address
#' @param password Character - Pasword
#' @param server Character - Server URL, "https://jonathan.labguru.com" by default
#' @param set_sys Set server and token as system variables LABGURU_SERVER and LABGURU_TOKEN 
#' 
#' @return acces token (string)
#' @export
#'
#' @examples
#' labguru_authenticate(email    = "my@@email.com",
#'                      password = "mypassword",
#'                      server   = "https://jonathan.labguru.com")
labguru_authenticate <- function(email, password, server = "https://jonathan.labguru.com", set_sys = TRUE) {
  
  # EXTRAS:
  # Add user agent? ua <- httr::user_agent("http://github.com/BioData/LabguruR")
  
  
  # Test arguments
  if (!is.character(email)) {
    stop("Email has to be a character string")
  } else if (!is_valid_email(email)) {
    stop("Invalid email address")
  }
  
  if (!is.character(password)) {
    stop("Password has to be a character string")
  } 
  
  if (!is.character(server)) {
    stop("Server has to be a character string")
  } 
  
  if (!is.logical(set_sys)) {
    stop("set_sys has to be a logical")
  } 
  
  # Set server assystem variable
  if (set_sys) {
    Sys.setenv(LABGURU_SERVER = server)
  }
  
  # URL
  base_url <- server
  path     <- "/api/v1/sessions.json"
  
  url      <- httr::modify_url(url  = base_url, 
                               path = path)
  
  # BODY
  body         <- list("login"    = email, 
                       "password" = password)
  
  # POST
  resp <- httr::POST(url    = url, 
                     body   = body, 
                     encode = "json")
  
  # Expect resp to be JSON 
  if (httr::http_type(resp) != "application/json") {
    message("First request unsuccessful. Retrying...")
    # Retry POST
    resp <- httr::POST(url    = url, 
                       body   = body, 
                       encode = "json")
    if (httr::http_type(resp) != "application/json") {
      stop("API did not return JSON. Please try again.", call. = FALSE)
    }
  }
   
  # Parse without simplifaction for consistency
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)
   
  # check for request error
  if (httr::http_error(resp)) {
    stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$error), call. = FALSE)
  }

  # Return token
  token  <- parsed$token
  
  # Set System environment
  if (set_sys) {
    Sys.setenv(LABGURU_TOKEN = token)
    message("Token set in environment variable.")
    
    # Return token invisibly
    invisible(token)
  } else {
    # If system environment is not set return token visibly
    token
  }
}


#' Labguru Valid Token
#'
#' @param token Labguru API access token
#'
#' @return logical
#' @export
#'
#' @examples
#' labguru_valid_token()
labguru_valid_token <- function(token = Sys.getenv("LABGURU_TOKEN")) {
  print("This function is not implemetned yet")
}


#' Labguru system variables
#'
#' @return print Labguru system variables
#' @export
#'
#' @examples
#' labguru_sys_variables()
labguru_sys_variables <- function() {
  print(paste("LABGURU SERVER:", Sys.getenv("LABGURU_SERVER")))
  print(paste("LABGURU TOKEN:", Sys.getenv("LABGURU_TOKEN")))
}

