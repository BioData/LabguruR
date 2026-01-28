
#' Labguru add comment
#'
#' Add a text comment to any commentable entity in Labguru (experiments, projects, etc.)
#'
#' @param comment character(1) The text content of the comment
#' @param commentable_id numeric(1) The id of the item to which the comment is being added
#' @param commentable_type character(1) The type of item. Use short names like "experiment",
#'   "project", "plasmid", etc. See Details for all available types.
#' @param return character(1) whether the function returns either 'id' (default) or 'all' comment information
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @details
#' Available commentable_type values (short names):
#' \itemize{
#'   \item experiment, project, milestone, note, figure
#'   \item plasmid, antibody, primer, protein, sequence, gene, compound
#'   \item bacterium, cell_line, fly, seed, tissue, virus, worm, yeast
#'   \item plant, rodent_specimen, rodent_strain
#'   \item document, paper, protocol, recipe
#'   \item company, order, material, instrument, event
#'   \item box, storage, stock, rodent_cage
#' }
#'
#' @return list with either comment id only or all comment information
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' # Add a comment to an experiment
#' labguru_add_comment(comment         = "Analysis complete. See attached results.",
#'                     commentable_id   = 123,
#'                     commentable_type = "experiment")
#'
#' # Add a comment to a plasmid
#' labguru_add_comment(comment         = "Sequence verified",
#'                     commentable_id   = 456,
#'                     commentable_type = "plasmid")
#' }
labguru_add_comment <- function(comment,
                                commentable_id,
                                commentable_type,
                                return = "id",
                                server = Sys.getenv("LABGURU_SERVER"),
                                token  = Sys.getenv("LABGURU_TOKEN")) {

  # Test arguments
  check_arg_single_character(comment, null = FALSE)
  check_arg_single_integer(commentable_id, null = FALSE)
  check_arg_single_character(commentable_type, null = FALSE)
  check_arg_char_opts(return, opts = c("id", "all"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)

  # Convert short type name to API class name
  api_type <- labguru_get_entity_type(commentable_type)

  # URL
  url <- httr::modify_url(url  = server,
                          path = "/api/v1/comments")

  # Body
  body <- list("token"                 = token,
               "item[comment]"         = comment,
               "item[commentable_id]"  = commentable_id,
               "item[commentable_type]" = api_type)

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


#' Labguru list comments
#'
#' List all comments in your account with optional pagination
#'
#' @param page numeric(1) representing the page number of data to request
#' @param get_cols character(1) either 'limited' or 'all' to return a subset or all information
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return dataframe with comment information, NULL if no comments available
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_list_comments(page = 1)
#' }
labguru_list_comments <- function(page     = 1,
                                  get_cols = "limited",
                                  server   = Sys.getenv("LABGURU_SERVER"),
                                  token    = Sys.getenv("LABGURU_TOKEN")) {

  check_arg_single_integer(page, null = FALSE)
  check_arg_char_opts(get_cols, c("limited", "all"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)

  # URL
  url <- httr::modify_url(url   = server,
                          path  = "/api/v1/comments",
                          query = paste0("token=", token,
                                         "&page=", page))

  parsed <- labguru_list_items(url)

  # Empty pages return an empty list
  if (length(parsed) == 0) {
    message("No comments were available for this request")
    return(NULL)
  }

  # Subset primary elements
  if (get_cols == "limited") {
    parsed[c("id", "comment", "commentable_id", "commentable_type")]
  } else {
    parsed
  }
}


#' Labguru get comment
#'
#' Get a comment by its id
#'
#' @param comment_id numeric(1) id indicating a comment on labguru server
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list object of labguru comment
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_get_comment(comment_id = 1)
#' }
labguru_get_comment <- function(comment_id,
                                server = Sys.getenv("LABGURU_SERVER"),
                                token  = Sys.getenv("LABGURU_TOKEN")) {

  check_arg_single_integer(comment_id, null = FALSE)
  check_arg_server(server)
  check_arg_token(token)

  parsed <- labguru_get_by_id(type   = "comments",
                              id     = comment_id,
                              server = server,
                              token  = token)

  parsed
}
