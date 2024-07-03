#' @name parents
#' @rdname parents
#'
#' @title Retrieve parents for specific concepts
#'
#' @description
#' Retrieve IDs of parents for selected taxa in a taxonomic list.
#'
#' @param taxlist An object of class [taxlist-class] containing a taxonomic
#'     list.
#' @param level A character value indicating the level at which the parents will
#'     be extracted (upwards in the taxonomic ranks).
#' @param concept A vector containing concept IDs. The taxa for which the
#'     parents will be retrieved.
#' @param ... Further arguments passed among methods.
#'
#' @example examples/parents.R
#'
#' @exportMethod parents
setGeneric(
  "parents",
  function(taxlist, level, ...) {
    standardGeneric("parents")
  }
)

#' @rdname parents
#' @aliases parents,taxlist,character-method
setMethod(
  "parents", signature(taxlist = "taxlist", level = "character"),
  function(taxlist, level, concept, ...) {
    # Check taxonomic information
    if (length(levels(taxlist)) < 2) {
      stop("There is no taxonomic ranks to assess 'taxlist'")
    }
    # Check requested level
    if (!level %in% levels(taxlist)) {
      stop(paste0("The level '", level, "' is not included in 'taxlist'"))
    }
    # Check concepts
    if (missing(concept)) {
      concept <- taxlist@taxonRelations$TaxonConceptID
    }
    missing_concepts <- concept[!concept %in%
      taxlist@taxonRelations$TaxonConceptID]
    if (length(missing_concepts) > 0) {
      stop(paste0(
        "following concepts are not included in 'taxlist':\n    ",
        paste0(missing_concepts, collapse = "\n    ")
      ))
    }
    # Taxonomy as traits
    taxlist <- tax2traits(taxlist)
    # Retrieve parents
    parent_ids <- with(taxlist@taxonTraits, get(level)[match(
      concept,
      TaxonConceptID
    )])
    return(parent_ids)
  }
)
