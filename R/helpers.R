
#' Is valid email
#'
#' @param x email address
#'
#' Check if an email address is valid
#'
#' @return BOOLEAN
#'
#' @export
#' @examples
#' is_valid_email("my@@valid_email.com")
#' is_valid_email("myinvalid.email@@com")
is_valid_email <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(x),
        ignore.case = TRUE)
}
