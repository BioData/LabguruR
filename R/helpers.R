
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


# Labguru entity types mapping
# Maps short names to API class names used for comments, attachments, and linking
LABGURU_ENTITY_TYPES <- c(
 "instrument"       = "System::Instrument",
 "box"              = "System::Storage::Box",
 "storage"          = "System::Storage::Storage",
 "stock"            = "System::Storage::Stock",
 "company"          = "Catalog::Company",
 "order"            = "Catalog::Order",
 "lineitem"         = "Catalog::Lineitem",
 "generic"          = "Biocollections::Generic",
 "material"         = "Catalog::Material",
 "bacterium"        = "Biocollections::Bacterium",
 "cell_line"        = "Biocollections::CellLine",
 "fly"              = "Biocollections::Fly",
 "seed"             = "Biocollections::Seed",
 "tissue"           = "Biocollections::Tissue",
 "virus"            = "Biocollections::Virus",
 "worm"             = "Biocollections::Worm",
 "yeast"            = "Biocollections::Yeast",
 "plasmid"          = "Biocollections::Plasmid",
 "antibody"         = "Biocollections::Antibody",
 "lipid"            = "Biocollections::Lipid",
 "primer"           = "Biocollections::Primer",
 "protein"          = "Biocollections::Protein",
 "sequence"         = "Biocollections::Sequence",
 "gene"             = "Biocollections::Gene",
 "compound"         = "Chemistry::Compound",
 "plant"            = "Biocollections::Plant",
 "rodent_specimen"  = "Biocollections::RodentSpecimen",
 "rodent_strain"    = "Biocollections::RodentStrain",
 "rodent_cage"      = "System::Storage::RodentCage",
 "note"             = "Projects::Note",
 "figure"           = "Projects::Figure",
 "project"          = "Projects::Project",
 "milestone"        = "Projects::Milestone",
 "experiment"       = "Projects::Experiment",
 "event"            = "System::Event",
 "message"          = "System::Message",
 "whiteboard_post"  = "System::WhiteboardPost",
 "link"             = "Link",
 "document"         = "Knowledgebase::Document",
 "paper"            = "Knowledgebase::Paper",
 "protocol"         = "Knowledgebase::Protocol",
 "recipe"           = "Knowledgebase::Recipe",
 "comment"          = "System::Comment",
 "attachment"       = "System::Attachment",
 "member"           = "Admin::Member"
)


#' Get Labguru API class name from short type name
#'
#' Converts a short entity type name (e.g., "experiment") to the corresponding
#' Labguru API class name (e.g., "Projects::Experiment")
#'
#' @param type character(1) short type name
#'
#' @return character(1) API class name
#'
#' @examples
#' # Internal helper - not exported
#' # labguru_get_entity_type("experiment")
#' # Returns: "Projects::Experiment"
labguru_get_entity_type <- function(type) {
  if (!type %in% names(LABGURU_ENTITY_TYPES)) {
    stop(paste0("Invalid entity type '", type, "'. ",
                "Must be one of: ", paste(names(LABGURU_ENTITY_TYPES), collapse = ", ")))
  }
  LABGURU_ENTITY_TYPES[[type]]
}
