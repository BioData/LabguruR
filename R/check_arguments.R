#' Check argument email
#' 
#' Email should be a single character and represent a valid email address
#'
#' @param email argument to check
#'
#' @return TRUE or error
check_arg_email <- function(email) {
  if (!is.character(email)) {
    stop("Email has to be a character string")
  } else if (length(email) != 1) {
    stop("Email is not length 1")
  } else if (!is_valid_email(email)) {
    stop("Invalid email address")
  }
  
  return(TRUE)
}

#' Check argument password
#' 
#' Password should be a single character
#'
#' @param password argument to check
#'
#' @return TRUE or error
check_arg_password <- function(password) {
  if (!is.character(password)) {
    stop("Password has to be a character string")
  } else if (length(password) != 1) {
    stop("Password is not length 1")
  }
  
  return(TRUE)
}

#' Check argument set_sys
#' 
#' Set sys should be a single logical
#'
#' @param set_sys argument to check
#'
#' @return TRUE or error
check_arg_set_sys <- function(set_sys) {
  if (!is.logical(set_sys)) {
    stop("set_sys has to be a logical")
  } else if (length(set_sys) != 1) {
    stop("set_sys is not length 1")
  }
  
  return(TRUE)
}


#' Check argument file
#' 
#' File should be a single character and represent a file that actually exists.
#'
#' @param file argument to check
#'
#' @return TRUE or error
check_arg_file <- function(file) {
  if (!is.character(file)) stop("Argument file is not a character string")
  if (!file.exists(file)) stop(paste("Can't find file", file))
  
  return(TRUE)
}

#' Check argument title
#' 
#' File should be a single character and is used as title for an object on Labguru
#'
#' @param title argument to check
#'
#' @return TRUE or error
check_arg_title <- function(title) {
  if (!is.character(title)) stop("Argument title is not a character string")
  if (length(title) != 1) stop("Argument title should be length 1")
  
  return(TRUE)
}

#' Check argument name
#' 
#' File should be a single character and is used as name for a visualisation on Labguru
#'
#' @param name argument to check
#'
#' @return TRUE or error
check_arg_name <- function(name) {
  if (!is.character(name)) stop("Argument name is not a character string")
  if (length(name) != 1) stop("Argument name should be length 1")
  
  return(TRUE)
}

#' Check argument description
#' 
#' Description should be NULL or a single character and is used as a description for an object on Labguru
#'
#' @param description argument to check
#'
#' @return TRUE or error
check_arg_description <- function(description) {
  if (!is.null(description)) {
    if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
    if (length(description) != 1) stop("Argument description should be length 1")
  }
  
  return(TRUE)
}

#' Check argument dataset
#' 
#' Dataset should be a data frame 
#'
#' @param dataset argument to check
#'
#' @return TRUE or error
check_arg_dataset <- function(dataset) {
  if (!is.data.frame(dataset)) stop("Argument dataset is not a data frame")
  
  # STILL TO WRITE FOR CHECKING IF DATASET(S) EXIST
  return(TRUE)
}


#' Check argument dataset_id
#' 
#' Dataset id should be NULL or a numeric and indicate (an) existing dataset(s) 
#'
#' @param dataset_id argument to check
#'
#' @return TRUE or error
check_arg_dataset_id <- function(dataset_id) {
  if (!is.null(dataset_id)) {
    if (!is.numeric(dataset_id)) stop("Argument dataset_id is not numeric")
  }
  
  # STILL TO WRITE FOR CHECKING IF DATASET(S) EXIST
  return(TRUE)
}

#' Check argument attach_to_uuid
#' 
#' Attach to uuid has not been implemented yet and hence if not null is now considered FALSE 
#'
#' @param attach_to_uuid argument to check
#'
#' @return TRUE or error
check_arg_attach_to_uuid <- function(attach_to_uuid) {
  if (!is.null(attach_to_uuid)) stop("Sorry, attach_to_uuid can't be used yet")
  
  return(TRUE)
}

#' Check argument server
#' 
#' Server is the base URL for the API connection and has to be a character string of length 1.
#'
#' @param server argument to check
#'
#' @return TRUE or error
check_arg_server <- function(server) {
  if (!is.character(server)) stop("Argument server is not a character string")
  if (length(server) != 1) stop("Argument server must be length 1")
  
  return(TRUE)
}

#' Check argument token
#' 
#' Token is the base URL for the API connection and has to be a character string of length 1
#'
#' @param token argument to check
#'
#' @return TRUE or error
check_arg_token <- function(token) {
  if (!is.character(token)) stop("Argument token is not a character string")
  if (length(token) != 1) stop("Argument token must be length 1")
  
  return(TRUE)
}


