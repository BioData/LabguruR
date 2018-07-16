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

# -----------------------------

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

# -----------------------------

#' Check argument single character
#' 
#' Password should be a single character
#'
#' @param object object to check
#' @param null Whether NULL value is allowed, default is FALSE
#'
#' @return TRUE or error
check_arg_single_character <- function(object, null = FALSE) {
  
  if (null) {
    if (is.null(object)) return(TRUE)
  }
  
  if (!is.character(object)) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' has to be a character string"))
  } else if (length(object) != 1) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' is not length 1"))
  }
  
  return(TRUE)
}


# #' Check argument description
# #' 
# #' Description should be NULL or a single character and is used as a description for an object on Labguru
# #'
# #' @param description argument to check
# #'
# #' @return TRUE or error
# check_arg_description <- function(description) {
#   if (!is.null(description)) {
#     if (!is.character(description)) stop("Argument description is not a character string (or NULL)")
#     if (length(description) != 1) stop("Argument description should be length 1")
#   }
#   
#   return(TRUE)
# }

# #' Check argument name
# #' 
# #' File should be a single character and is used as name for a visualisation on Labguru
# #'
# #' @param name argument to check
# #'
# #' @return TRUE or error
# check_arg_name <- function(name) {
#   if (!is.character(name)) stop("Argument name is not a character string")
#   if (length(name) != 1) stop("Argument name should be length 1")
#   
#   return(TRUE)
# }





# #' Check argument title
# #' 
# #' File should be a single character and is used as title for an object on Labguru
# #'
# #' @param title argument to check
# #'
# #' @return TRUE or error
# check_arg_title <- function(title) {
#   if (!is.character(title)) stop("Argument title is not a character string")
#   if (length(title) != 1) stop("Argument title should be length 1")
#   
#   return(TRUE)
# }




# -----------------------------

#' Check argument single logical
#' 
#' Password should be a single logical
#'
#' @param object object to check
#' @param null Whether NULL value is allowed, default is FALSE
#'
#' @return TRUE or error
check_arg_single_logical <- function(object, null = FALSE) {
  
  if (null) {
    if (is.null(object)) return(TRUE)
  }
  
  if (!is.logical(object)) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' has to be a logical"))
  } else if (length(object) != 1) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' is not length 1"))
  }
  
  return(TRUE)
}

# #' Check argument set_sys
# #' 
# #' Set sys should be a single logical
# #'
# #' @param set_sys argument to check
# #'
# #' @return TRUE or error
# check_arg_set_sys <- function(set_sys) {
#   if (!is.logical(set_sys)) {
#     stop("set_sys has to be a logical")
#   } else if (length(set_sys) != 1) {
#     stop("set_sys is not length 1")
#   }
#   
#   return(TRUE)
# }


# -----------------------------

#' Check argument single numeric
#' 
#' Object should be a single numeric
#'
#' @param object object to check
#' @param null Whether NULL value is allowed, default is FALSE
#'
#' @return TRUE or error
check_arg_single_numeric <- function(object, null = FALSE) {
  
  if (null) {
    if (is.null(object)) return(TRUE)
  }
  
  if (!is.numeric(object)) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' has to be a numeric"))
  } else if (length(object) != 1) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' is not length 1"))
  }
  
  return(TRUE)
}

# #' Check argument dataset_id
# #' 
# #' Dataset id should be NULL or a numeric and indicate (an) existing dataset(s) 
# #'
# #' @param dataset_id argument to check
# #'
# #' @return TRUE or error
# check_arg_dataset_id <- function(dataset_id) {
#   if (!is.null(dataset_id)) {
#     if (!is.numeric(dataset_id)) stop("Argument dataset_id is not numeric")
#   }
#   
#   # STILL TO WRITE FOR CHECKING IF DATASET(S) EXIST
#   return(TRUE)
# }


#' Check argument numeric
#' 
#' Object should be a numeric
#'
#' @param object object to check
#' @param null Whether NULL value is allowed, default is FALSE
#'
#' @return TRUE or error
check_arg_numeric <- function(object, null = FALSE) {
  
  if (null) {
    if (is.null(object)) return(TRUE)
  }
  
  if (!is.numeric(object)) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' has to be a numeric"))
  } 
  
  return(TRUE)
}



# -----------------------------



#' Check argument single integer
#' 
#' Password should be a single integer numeric
#'
#' @param object object to check
#' @param null Whether NULL value is allowed, default is FALSE
#'
#' @return TRUE or error
check_arg_single_integer <- function(object, null = FALSE) {
  
  if (null) {
    if (is.null(object)) return(TRUE)
  }
  
  if (!is.numeric(object)) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' has to be a numeric"))
  } else if (length(object) != 1) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' is not length 1"))
  } else if (!object%%1==0) {
    nm <- deparse(substitute(object))
    stop(paste0("Argument '", nm, "' (", object, ") is not an integer value"))
  }
  
  return(TRUE)
}


# #' Check argument page
# #' 
# #' Page number of GET request must be integer
# #'
# #' @param page argument to check
# #'
# #' @return TRUE or error
# check_arg_page <- function(page) {
#   if (length(page) != 1) stop("Argument page must be length 1")
#   if (!is.numeric(page)) stop("Argument page is not numeric")
#   if (!page%%1==0) stop("Argument page is not an integer value")
#   
#   return(TRUE)
# }

# -----------------------------



#' Check argument file
#' 
#' File should be a single character and represent a file that actually exists.
#'
#' @param file argument to check
#'
#' @return TRUE or error
check_arg_file <- function(file) {
  
  if (!is.character(file)) {
    stop("Argument file is not a character string")
  } else if (length(file) != 1) {
    stop("File is not a length 1 character")
  }
  if (!file.exists(file)) stop(paste("Can't find file", file))
  
  return(TRUE)
}


# -----------------------------

#' Check argument data frame
#' 
#' Dataset should be a data frame 
#'
#' @param dataframe argument to check
#'
#' @return TRUE or error
check_arg_dataframe <- function(dataframe) {
  if (!is.data.frame(dataframe)) stop("Argument data frame is not a data frame")
  
  # Check what other conditions must be met for a data frame to be uploaded to labguru. Can a column be a list? Can it be nested data frames?
  
  return(TRUE)
}

# #' Check argument dataset
# #' 
# #' Dataset should be a data frame 
# #'
# #' @param dataset argument to check
# #'
# #' @return TRUE or error
# check_arg_dataset <- function(dataset) {
#   if (!is.data.frame(dataset)) stop("Argument dataset is not a data frame")
#   
#   # STILL TO WRITE FOR CHECKING IF DATASET(S) EXIST
#   return(TRUE)
# }


# -----------------------------






# -----------------------------



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


# -----------------------------



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


# -----------------------------


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


# -----------------------------


#' Check argument in character options
#' 
#' Check if argument is in the given options
#'
#' @param object argument to check
#' @param opts Possible values for object
#' @param null Whether NULL value is allowed, default is FALSE
#'
#' @return TRUE or error
check_arg_char_opts <- function(object, opts, null = FALSE) {
  
  if (null) {
    if (is.null(object)) return(TRUE)
  }
  
  nm <- deparse(substitute(object))
  
  if (!is.character(object)) stop(paste0("Argument '", nm, "' must be character"))
  if (!is.character(opts)) stop("Package issue. Contact the package maintainer.")
  if (!object %in% opts) stop(paste0("Argument '", nm, "' is not a valid option. Must be one of: ",
                                       paste(opts, collapse = ", ")))
  
  return(TRUE)
}


  
  

# #' Check argument get_cols
# #' 
# #' Character with columns of a dataframe to return
# #'
# #' @param get_cols argument to check
# #' @param opts Possible values for get_cols
# #'
# #' @return TRUE or error
# check_arg_get_cols <- function(get_cols, opts) {
#   if (!is.character(get_cols)) stop("Argument get_cols must be character")
#   if (!is.character(opts)) stop("Package issue. Contact the package maintainer.")
#   if (!get_cols %in% opts) stop(paste0(get_cols, " is not a valid option for get_cols argument. Must be one of: ",
#                                        paste(opts, collapse = ", ")))
#   
#   return(TRUE)
# }
