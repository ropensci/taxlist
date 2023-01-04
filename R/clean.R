#' @name clean
#'
#' @title Delete orphaned records
#'
#' @description
#' Manipulation of slots may generate orphaned entries in
#' [taxlist-class] objects.
#' The function `clean` deletes such entries and restores the consistency
#' of the objects.
#'
#' @param object A [taxlist-class] object.
#' @param times An integer indicating how many times the cleaning should be
#'     repeated.
#' @param ... Further arguments passed from or to other methods.
#'
#' @details
#' Cleaning of objects will follow the deletion of orphaned names, orphaned
#' taxon trait entries, and orphaned parent entries.
#'
#' @return A clean [taxlist-class] object.
#'
#' @author Miguel Alvarez.
#'
#' @example examples/clean.R
#'
#' @rdname clean
#'
#' @exportMethod clean
setGeneric(
  "clean",
  function(object, ...) {
    standardGeneric("clean")
  }
)

#' One run clean function
#' @keywords internal
clean_once_taxlist <- function(object) {
  # clean slot taxonRelations (lost accepted names)
  object@taxonRelations <- object@taxonRelations[
    object@taxonRelations$AcceptedName %in%
      object@taxonNames$TaxonUsageID,
  ]
  # clean parents (deleted parents)
  object@taxonRelations$Parent[!object@taxonRelations$Parent %in%
    object@taxonRelations$TaxonConceptID] <- NA
  # clean slot taxonNames (skip orphaned names)
  object@taxonNames <- object@taxonNames[
    object@taxonNames$TaxonConceptID %in%
      object@taxonRelations$TaxonConceptID,
  ]
  # clean slot taxonTraits
  if (nrow(object@taxonTraits) > 0) {
    object@taxonTraits <- object@taxonTraits[
      object@taxonTraits$TaxonConceptID %in%
        object@taxonRelations$TaxonConceptID, ,
      drop = FALSE
    ]
  }
  return(object)
}

#' @rdname clean
#' @aliases clean,taxlist-method
setMethod(
  "clean", signature(object = "taxlist"),
  function(object, times = 2, ...) {
    count <- 0
    repeat {
      count <- count + 1
      object <- clean_once_taxlist(object)
      if (count == times) break
    }
    return(object)
  }
)
