#' @name tax2traits
#'
#' @title Set taxonomic information as taxon traits
#'
#' @description
#' Taxonomic classification can be included in [taxlist-class]
#' objects within the information provided at slot `taxonRelations`.
#' Nevertheless, for statistical analyses it may be more convenient to insert
#' such information in the slot `taxonTraits`.
#'
#' @param object An object of class [taxlist-class].
#' @param get_names Logical value indicating whether taxon names should be
#'     retrieved instead of taxon IDs.
#' @param ... Further arguments to be passed among methods.
#'
#' @details
#' This function can only be applied to objects containing parent-child
#' relationships and information on taxonomic levels.
#'
#' @return An object of class [taxlist-class] with taxonomy added
#' as traits.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#'
#' @example examples/tax2traits.R
#'
#' @rdname tax2traits
#'
#' @export
tax2traits <- function(object, ...) UseMethod("tax2traits", object)

#' @rdname tax2traits
#' @aliases tax2traits,taxlist-method
#' @method tax2traits taxlist
#' @export
tax2traits.taxlist <- function(object, get_names = FALSE, ...) {
  # taxonomic table
  TAX <- data.frame(
    TaxonConceptID = object@taxonRelations$TaxonConceptID,
    stringsAsFactors = FALSE
  )
  # Arrange parents with internal function
  TAX <- arrange_taxa(object)
  # supress empty columns
  TAX <- TAX[, apply(TAX, 2, function(x) !all(is.na(x)))]
  object <- update_trait(taxlist = object, taxonTraits = TAX)
  if (get_names) {
    Names <- accepted_name(object)
    for (i in taxlist::levels(object)[taxlist::levels(object) %in%
      colnames(TAX)]) {
      object@taxonTraits[, i] <- Names[
        match(object@taxonTraits[, i], Names$TaxonConceptID),
        "TaxonName"
      ]
    }
  }
  return(object)
}
