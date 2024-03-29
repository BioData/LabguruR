
#' Labguru ic50 upload
#'
#' Upload labguru ic50 analysis results to the server.
#'
#' @param table data frame containing ic50 results
#' @param name character(1) name of the analysis
#' @param results_dir character(1) directory where ic50 results are stored
#' @param img_pdf character(1) 'img' (default) or 'pdf' indicating whether result plots are uploaded as single images or pdf (as stored in results_dir)
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return list() with upload information
#' @export
#'
#' @examples
#' # upload helper functionality
labguru_ic50_upload <- function(table,
                                name,
                                results_dir,
                                img_pdf,
                                server = Sys.getenv("LABGURU_SERVER"),
                                token  = Sys.getenv("LABGURU_TOKEN")) {

  check_arg_dataframe(table)
  check_arg_single_character(name, null = FALSE)
  check_arg_single_character(results_dir, null = FALSE)
  check_arg_char_opts(img_pdf, opts = c("img", "pdf"), null = FALSE)
  check_arg_server(server)
  check_arg_token(token)

  fls <- list.files(results_dir)

  if (img_pdf == "img") {
    fls <- fls[grepl(pattern = '.png$', x = fls)]
  } else if (img_pdf == "pdf") {
    fls <- fls[grepl(pattern = '.pdf$', x = fls)]
  } else { stop("Results files must be pdf or images.") }

  rtn <- list()

  rtn$upl <- labguru_upload_dataset(dataset = table,
                                    name    = name,
                                    server  = server,
                                    token   = token)

  titles <- substr(fls, 1, nchar(fls) - 4)
  # titles <- gsub('^.*file\\s*|\\s*png.*$', '', fls)
  for (i in seq_along(fls)) {
    rtn[titles[i]] <- labguru_upload_visualization(file       = paste0(results_dir, "/", fls[i]),
                                                   title      = titles[i],
                                                   name       = titles[i],
                                                   dataset_id = rtn$upl$id)
  }

  rtn
}

#' Labguru ic50 Analysis
#'
#' Download plate from labguru server and run ic50 analysis.
#'
#' @param plate_id numeric(1) plate id of the plate from labguru server
#' @param indir character(1) argument of ic50::ic50() function with same the name
#' @param outdir_plate character(1) directory name where to store plate data
#' @param outdir_results character(1) argument of ic50::ic50() function with the name outdir
#' @param plates numeric(1) argument of ic50::ic50() function with same the name
#' @param inhib numeric(1) argument of ic50::ic50() function with same the name
#' @param normalize character(1) argument of ic50::ic50() function with same the name
#' @param graphics character(1) argument of ic50::ic50() function with same the name
#' @param img_png logical(1) TRUE (default) to create single images from the pdf result, magick package is required for TRUE.
#' @param server character(1) indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return
#' @export
#'
#' @importFrom magick image_read_pdf
#' @importFrom magick image_info
#' @importFrom magick image_write
#'
#' @examples
#' if(requireNamespace("ic50", quietly = TRUE)){
#'  ic50lgres <- labguru_ic50_analysis()
#' }
labguru_ic50_analysis <- function(plate_id,
                                  indir          = ".",
                                  outdir_plate   = "./plate",
                                  outdir_results = "./results",
                                  plates         = 2,
                                  inhib          = NULL,
                                  normalize      = "single",
                                  graphics       = "mean",
                                  img_png        = TRUE, # for img the magick package is required
                                  server         = Sys.getenv("LABGURU_SERVER"),
                                  token          = Sys.getenv("LABGURU_TOKEN")) {

  if (!"package:ic50" %in% search()) {
    message("First you must load the ic50 package for this function to run.
            Run library(ic50), then run the function again.")
    return(NULL)
  }

  check_arg_single_integer(plate_id, null = FALSE)
  check_arg_single_character(indir, null = FALSE)
  check_arg_single_character(outdir_plate, null = FALSE)
  check_arg_single_character(outdir_results, null = FALSE)
  check_arg_single_integer(plates, null = FALSE)
  check_arg_numeric(inhib, null = TRUE)
  check_arg_single_character(normalize, null = FALSE)
  check_arg_single_character(graphics, null = FALSE)
  check_arg_single_logical(img_png, null = FALSE)
  check_arg_server(server)
  check_arg_token(token)

  plate <- labguru_download_plate(plate  = plate_id,
                                  dir    = outdir_plate,
                                  server = server,
                                  token  = token)

  if (plate$length == 96) {
    rslt <- ic50::hts.96(indir     = indir,
                         plates    = plates,
                         measure   = paste0(plate$dir, "/measure.txt"),
                         control   = paste0(plate$dir, "/control.txt"),
                         dilution  = paste0(plate$dir, "/dilution.txt"),
                         inhib     = inhib,
                         outdir    = outdir_results,
                         normalize = normalize,
                         graphics  = graphics)
  } else if (plate$length == 384) {
    rslt <- ic50::hts.384(indir     = indir,
                          plates    = plates,
                          measure   = paste0(plate$dir, "/measure.txt"),
                          control   = paste0(plate$dir, "/control.txt"),
                          dilution  = paste0(plate$dir, "/dilution.txt"),
                          inhib     = inhib,
                          outdir    = outdir_results,
                          normalize = normalize,
                          graphics  = graphics)
  } else {
    stop("Plate length is not 384 or 96. LabguruR is unable to process this")
  }

  if (img_png) {
    message("Storing separate image files from pdf. This may take some time...")

    pdf_img  <- magick::image_read_pdf(paste0(outdir_results, "/dose_response_curves.pdf"))
    img_info <- magick::image_info(pdf_img)

    # Test that img_info and rslt have the same length!
    if (nrow(rslt) != nrow(img_info)) {
      warning("Not the same number of images as results from the ic50 analysis. Returning pdf instead of images")
      img_pdf <- "pdf"
    } else {
      img_info$title <- paste0("file ", rslt$first_file, ", compound ", rslt$compound)
      for (i in seq_along(img_info$title)) {
        magick::image_write(pdf_img[i],
                            paste0(outdir_results, "/", img_info$title[i], ".png"))
      }

    }
  } else { img_info$title <- NULL }

  list(result = rslt,
       dir    = outdir_results,
       img    = img_info$title)
}

#' Labguru download plate
#'
#' Download a plate's data and stores dilution, measure and control files in a folder or returns data frames.
#'
#' @param plate numeric(1) indicating the plate id
#' @param dir character(1) indicating the directory to store the created files
#' @param server character(1) string indicating the server URL
#' @param token character(1) access token for API authentication
#'
#' @return invisible, list() with dir name, and dilution, measure nd control data frames
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' labguru_download_plate(plate = 271)
#' }
labguru_download_plate <- function(plate,
                                   dir    = "./plate",
                                   server = Sys.getenv("LABGURU_SERVER"),
                                   token  = Sys.getenv("LABGURU_TOKEN")) {

  # Test arguments
  check_arg_single_integer(plate, null = FALSE)
  check_arg_single_character(dir, null = FALSE)
  check_arg_server(server)
  check_arg_token(token)


  if (dir.exists(dir)) {
    if (length(dir(path = dir, all.files = TRUE)) != 0) {
      stop(paste0("Please remove or empty the '", dir, "' directory."))
    }
  } else {
    dir.create(dir)
  }

  base_url <- server
  path     <- paste0("/api/v1/plates/", plate)
  query    <- paste0("token=", token)

  url <- httr::modify_url(url   = base_url,
                          path  = path,
                          query = query)

  # Encoding certain URL characters (like space)
  url <- URLencode(url)

  resp <- httr::GET(url)

  # Expect resp to be JSON
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }

  # Parse without simplifaction for consistency
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"),
                               simplifyVector    = FALSE,
                               simplifyDataFrame = TRUE,
                               flatten           = TRUE)

  # check for request error
  if (httr::http_error(resp)) {
    stop(sprintf("API request failed [%s]\n%s", parsed$status, parsed$error), call. = FALSE)
  }

  # Further parse data element
  prs_data <- jsonlite::fromJSON(parsed$data)

  # Prep data
  df       <- ic50_extract_data_from_json(prs_data)

  measure  <- ic50_create_measure_data(df)
  dilution <- ic50_create_dilution_data(df)
  control  <- ic50_create_control_data(df, prs_data$wells)

  write.table(x         = measure,
              file      = paste0(dir, "/measure.txt"),
              append    = FALSE,
              sep       = "\t",
              row.names = FALSE,
              col.names = FALSE,
              quote     = FALSE)
  write.table(x         = control,
              file      = paste0(dir, "/control.txt"),
              append    = FALSE,
              sep       = "\t",
              row.names = FALSE,
              col.names = FALSE,
              quote     = FALSE)
  write.table(x         = dilution,
              file      = paste0(dir, "/dilution.txt"),
              append    = FALSE,
              sep       = "\t",
              row.names = FALSE,
              col.names = FALSE,
              quote     = FALSE)

  # Return information
  invisible(list(dir      = dir,
                 length   = nrow(prs_data$wells),
                 measure  = measure,
                 dilution = dilution,
                 control  = control))
}


#' ic50 extract data from json
#'
#' @param prs_data Parsed JSON plate data from Labguru
#'
#' @return data frame
#'
#' @examples
#' # helper functionality to extract data from json
ic50_extract_data_from_json <- function(prs_data) {
  # PREPARE each compound data frame separately
  dfs <- lapply(X   = prs_data$wells$samples_metadata,
                FUN = function(d) {
                  # Add coordinates before filter
                  d$coordinates <- prs_data$wells$coordinates
                  # Serial_factor is a nested dataframe, unnest it
                  if ("serial_factor" %in% names(d)) {
                    d$comp_serial_factor_value  <- d$serial_factor$value
                    d$comp_serial_factor_symbol <- d$serial_factor$symbol
                    d$serial_factor <- NULL
                  }
                  # Filter empty rows
                  d <- d[!is.na(d$concentration), ]
                  # as.numeric stocks now that only numreic data remains
                  d$stocks <- as.numeric(d$stocks)

                  d
                })

  # SUBSET the compound columns
  # ASSUMPTION: The column prs_data$samples$collection_name will be "Compounds" if it is a compound. Can't be missing or spelt differently
  collection_names        <- prs_data$samples$collection_name
  names(collection_names) <- prs_data$samples$id
  nc <- collection_names[names(dfs)] == "Compounds"

  # BIND ROWS
  comp <- Reduce(f = rbind,
                 x = dfs[nc])
  names(comp)[1:2] <- paste("comp", names(comp)[1:2], sep = "_")

  # ASSUMPTION: THERE CAN ONLY BE ONE CELL LINE
  df <- merge(dfs[!nc][[1]], comp, by.x = "coordinates", by.y = "coordinates")

  # TEST: CELL LINE AND COMPOUND SAME LENGTH
  nrow(df) == nrow(dfs[!nc][[1]])
  nrow(df) == nrow(comp)

  # TEST: NO DUPLICATES IN COORDINATES
  length(unique(df$coordinates)) == nrow(df)

  # ADD: compound names
  compound_names        <- prs_data$samples$name
  names(compound_names) <- as.character(prs_data$samples$id)
  df$comp_name          <- unname(compound_names[as.character(df$comp_stocks)])

  # ADD row and col for printing
  df$row <- vapply(substr(df$coordinates, 1, 1), function(c) { which(c == LETTERS) }, FUN.VALUE = numeric(1))
  df$col <- as.numeric(gsub("[^0-9.-]+", "", df$coordinates))

  # ARRANGE: by row and col
  df <- df[order( df[,"row"], df[,"col"] ),]
}




#' Labguru create measure data for ic50
#'
#' @param df extracted plate data
#'
#' @return data frame of measure data
#'
#' @examples
#' # helper function to create measurement data
ic50_create_measure_data <- function(df) {
  # ASSUMPTION: 1 compound per row
  # CHECK: does it require equal columns per row
  measure <- lapply(X   = unique(df$row),
                    FUN = function(r) {
                      d      <- df[df$row == r, ]
                      name   <- paste0('"', unique(d$comp_name)[1], '"')
                      coords <- paste0('"', d$row, ',', d$col, '"')
                      c(name, coords)
                    })
  names(measure) <- paste0("row", seq_along(measure))
  measure        <- as.data.frame(t(as.data.frame(measure)))

  measure
}


#' Labguru create dilution data for ic50
#'
#' @param df extracted plate data
#'
#' @return data frame of dilution data
#'
#' @examples
#' # helper function to create dilution data
ic50_create_dilution_data <- function(df) {
  # ASSUMPTION: 1 compound per row
  # CHECK: does it require equal columns per row
  dilution <- lapply(X   = unique(df$row),
                     FUN = function(r) {
                       d      <- df[df$row == r, ]
                       name   <- paste0('"', unique(d$comp_name)[1], '"')
                       coords <- paste0('"', d$comp_concentration, '"')
                       c(name, coords)
                     })
  names(dilution) <- paste0("row", seq_along(dilution))
  dilution        <- as.data.frame(t(as.data.frame(dilution)))

  dilution
}


#' Labguru create control data for ic50
#'
#' @param df extracted plate data
#' @param wells extracted wells data
#'
#' @return data frame of control data
#'
#' @examples
#' # helper function to create control data
ic50_create_control_data <- function(df, wells) {
  cntrl <- wells[c("control", "coordinates")]
  cntrl <- cntrl[cntrl$control, ]

  # ADD row and col for printing
  cntrl$row <- vapply(substr(cntrl$coordinates, 1, 1), function(c) { which(c == LETTERS) }, FUN.VALUE = numeric(1))
  cntrl$col <- as.numeric(gsub("[^0-9.-]+", "", cntrl$coordinates))

  # ADD compound name
  # ASSUMTION: each row has only 1 compound name
  temp                  <- unique(df[c("comp_name", "row")])
  compound_names        <- temp$comp_name
  names(compound_names) <- as.character(temp$row)
  cntrl$comp_name       <- unname(compound_names[as.character(cntrl$row)])

  # ASSUMPTION: 1 compound per row
  # CHECK: does it require equal columns per row
  control <- lapply(X   = unique(cntrl$row),
                    FUN = function(r) {
                      d      <- cntrl[cntrl$row == r, ]
                      name   <- paste0('"', unique(d$comp_name)[1], '"')
                      coords <- paste0('"', d$row, ',', d$col, '"')
                      c(name, coords)
                    })
  names(control) <- paste0("row", seq_along(control))
  control        <- as.data.frame(t(as.data.frame(control)))

  control
}


