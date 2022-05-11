

#' Labguru Authenticate
#'
#' This function authenticates a user by email and password and returns a token and sets the system
#' variable LABGURU_TOKEN to that token by default.
#'
#' @param email character(1) email address
#' @param password character(1) password
#' @param server character(1) Server URL, "https://jonathan.labguru.com" by default
#' @param set_sys logical(1) Set server and token as system variables LABGURU_SERVER and LABGURU_TOKEN
#'
#' @return acces token (string)
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#'
#' @examples
#' \dontrun{
#' labguru_authenticate(email    = "my@@email.com",
#'                      password = "mypassword",
#'                      server   = "https://my.labguru.com")
#' }
labguru_authenticate <- function(email, password, server = "https://my.labguru.com", set_sys = TRUE) {

  # EXTRAS:
  # Add user agent? ua <- httr::user_agent("http://github.com/BioData/LabguruR")

  # Check arguments
  check_arg_email(email)
  check_arg_password(password)
  check_arg_server(server)
  check_arg_single_logical(set_sys, null = FALSE)

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
  body <- list("login"    = email,
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

  # Wrong login details
  if (token == "-1") stop("Invalid credentials")

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


#' Labguru set token
#'
#' Set token and server as system variables. Useful when a user has a token but no login credentials.
#'
#' @param token Labguru API access token
#' @param server character(1) Server URL, "https://jonathan.labguru.com" by default
#' @param set_sys  logical(1) Set server and token as system variables LABGURU_SERVER and LABGURU_TOKEN
#'
#' @return Returns the token (visibly or invisibly depending on set_sys)
#' @export
#'
#' @examples
#' # helper function to set labguru token
labguru_set_token <- function(token, server = "https://jonathan.labguru.com", set_sys = TRUE) {

  check_arg_token(token)
  check_arg_server(server)
  check_arg_single_logical(set_sys, null = FALSE)

  if (set_sys) {
    Sys.setenv(LABGURU_SERVER = server)
    Sys.setenv(LABGURU_TOKEN = token)
    message("Token and server set in environment variable.")

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
#' \dontrun{
#' labguru_valid_token()
#' }
labguru_valid_token <- function(token = Sys.getenv("LABGURU_TOKEN")) {
  check_arg_token(token)
  print("This function is not implemented yet")
  NULL
}


#' Labguru system variables
#'
#' @return print Labguru system variables
#' @export
#'
#' @examples
#' \dontrun{
#' labguru_sys_variables()
#' }
labguru_sys_variables <- function() {
  print(paste("LABGURU SERVER:", Sys.getenv("LABGURU_SERVER")))
  print(paste("LABGURU TOKEN:", Sys.getenv("LABGURU_TOKEN")))
}

