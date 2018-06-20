
labguru_ic50_analysis <- function(plate_id, 
                                  indir     = ".",
                                  outdir    = tempdir(check = FALSE),
                                  plates    = 2,
                                  inhib     = NULL,
                                  normalize = "single",
                                  graphics  = "mean",
                                  img_pdf   = "img", # for img the magick package is required
                                  server    = Sys.getenv("LABGURU_SERVER"), 
                                  token     = Sys.getenv("LABGURU_TOKEN")) {
  
  plate <- labguru_download_plate(plate  = plate_id, 
                                  dir    = "./fls", #outdir,
                                  server = server, 
                                  token  = token)
  
  if (plate$length == 96) {
    rslt <- ic50::hts.96(indir     = indir,
                         plates    = plates,
                         measure   = paste0(plate$dir, "/measure.txt"),
                         control   = paste0(plate$dir, "/control.txt"),
                         dilution  = paste0(plate$dir, "/dilution.txt"),
                         inhib     = inhib,
                         outdir    = outdir,
                         normalize = normalize,
                         graphics  = graphics)
  } else if (plate$length == 384) {
    rslt <- ic50::hts.384(indir     = indir,
                          plates    = plates,
                          measure   = paste0(plate$dir, "/measure.txt"),
                          control   = paste0(plate$dir, "/control.txt"),
                          dilution  = paste0(plate$dir, "/dilution.txt"),
                          inhib     = inhib,
                          outdir    = outdir,
                          normalize = normalize,
                          graphics  = graphics)
  } else {
    stop("Plate length is not 384 or 96. LabguruR is unable to process this")
  }
  
  if (img_pdf == "img") {
    pdf_img  <- magick::image_read_pdf(paste0(outdir, "dose_response_curves.pdf"))
    img_info <- magick::image_info(pdf_img)
    
    # Test that img_info and rslt have the same length!
    if (nrow(rslt) != nrow(img_info)) {
      warning("Not the same number of images as results from the ic50 analysis. Returning pdf instead of images")
      img_pdf <- "pdf"
    } else {
      img_info$title <- paste0("file ", rslt$first_file, ", compound ", rstl$compound)
      for (i in seq_len(img_info$title)) {
        magick::image_write(pdf_img[i], 
                            paste0(plate$dir, img_info$title[i], ".png"))
      }
        
    }
    
  }
  
}

#' Labguru download plate
#'
#' Download a plate's data and stores dilution, measure and control files in a folder or returns data frames.
#'
#' @param plate Single numeric indicating the plate id
#' @param dir Single character indicating the folder to store the created files
#' @param server A character string indicating the server URL
#' @param token An access token for API authentication
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
                                   dir    = tempdir(),
                                   server = Sys.getenv("LABGURU_SERVER"), 
                                   token  = Sys.getenv("LABGURU_TOKEN")) {
  
  # Test arguments
  # check_arg_plate(plate)
  # check_arg_dir(dir)
  check_arg_server(server)
  check_arg_token(token)

  base_url <- server
  path     <- paste0("/api/v1/plates/", plate)
  query    <- paste0("token=", token)
  
  url <- httr::modify_url(url   = base_url, 
                          path  = path,
                          query = query)
  
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
  
  write.table(measure, 
              file      = paste0(dir, "/measure.txt"),
              row.names = FALSE,
              col.names = FALSE,
              sep       = "\t")
  write.table(control, 
              file      = paste0(dir, "/control.txt"),
              row.names = FALSE,
              col.names = FALSE,
              sep       = "\t")
  write.table(dilution, 
              file      = paste0(dir, "/dilution.txt"),
              row.names = FALSE,
              col.names = FALSE,
              sep       = "\t")
  
  # Return information
  invisible(list(dir      = dir,
                 length   = nrow(prs_data$wells),
                 measure  = measure,
                 dilution = dilution,
                 control  = control))
}


#' ic50 extract data from json
#'
#' @param data Parsed JSON plate data from Labguru
#'
#' @return data frame
#'
#' @examples
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


