#' @name df2taxlist
#'
#' @title Convert data frames and strings into taxlist objects
#'
#' @description
#' Function converting template data frame into [taxlist-class] object.
#' Also character vectors including taxonomic names will be converted
#' but without any information on taxonomic ranks and parental taxa.
#'
#' @param x A data frame or a character vector with taxonomic names. If x is
#'     a data frame, the columns **TaxonUsageID** (integer with IDs for each
#'     name), **TaxonConceptID** (integer with IDs for the respective taxon
#'     concepts), and **TaxonName** (character) are mandatory.
#'     Other optional columns are **AuthorName** (character with names'
#'     authorities), **AcceptedName** (logical indicating whether the name is
#'     an accepted name or a synonym and will be set as TRUE by default),
#'     **Level**
#'     (factor sorting taxonomic ranks in the bottom-up direction), **Parent**
#'     (integer, the taxon concept ID of the parental taxon), and **ViewID**
#'     (integer pointing to the ID of taxonomic view, usually a bibliographic
#'     reference, and will be used only if 'taxonViews' is provided.
#'     Any further column not included in the prototype of taxlist will be
#'     considered as names' attributes and inserted in slot **taxonNames**.
#' @param taxonTraits A data frame with attributes of taxonomic concepts
#'     (optional). If provided, the column **TaxonConceptID** is mandatorial.
#' @param taxonViews A data frame or [biblio::lib_df-class] with references of taxonomic
#'     views (optional). If provided, the column **ViewID** is mandatorial and
#'     have to match the homonymous column at 'x'.
#' @param levels A character vector setting the levels or taxonomic ranks from
#'     the bottom to the top. This argument is optional and if missing, the
#'     column **Level** will be preserved (if factor) or coerced to factor,
#'     except in the case that no column **Level** is provided.
#' @param clean_strings Logical value, whether function [clean_strings()] should
#'     be applied to 'x' or not.
#' @param ... Further arguments passed among methods. For the
#'     `'character-method'`, arguments will be passed to the
#'     `'data.frame-method'`.
#'
#' @return A [taxlist-class] object.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#'
#' @example examples/df2taxlist.R
#'
#' @rdname df2taxlist
#'
#' @export
df2taxlist <- function(x, ...) UseMethod("df2taxlist", x)

#' @rdname df2taxlist
#' @aliases df2taxlist,data.frame-method
#' @method df2taxlist data.frame
#' @export
df2taxlist.data.frame <- function(x, taxonTraits, taxonViews, levels,
                                  clean_strings = TRUE, ...) {
  # Clean main table
  if (clean_strings) {
    x <- clean_strings(x)
  }
  # Create empty object
  taxlist <- new("taxlist")
  # Mandatory column
  if (!"TaxonName" %in% names(x)) {
    stop("'TaxonName' is a mandatory column in 'x'.")
  }
  if (!"AuthorName" %in% names(x)) {
    x$AuthorName <- NA
  }
  if (!"TaxonUsageID" %in% names(x)) {
    x$TaxonUsageID <- id_generator(len = nrow(x))
  }
  if (!"TaxonConceptID" %in% names(x)) {
    message(paste(
      "Missing column 'TaxonConceptID' in 'x'.",
      "All names will be considered as accepted names."
    ))
    x$TaxonConceptID <- id_generator(len = nrow(x))
    x$AcceptedName <- TRUE
  }
  if (!"AcceptedName" %in% names(x)) {
    message(paste(
      "No values for 'AcceptedName' in 'x'.",
      "all names will be considered as accepted names."
    ))
    x$AcceptedName <- TRUE
  }
  # Slot taxonRelations
  taxonRelations <- x[
    x$AcceptedName,
    names(x) %in% names(taxlist@taxonRelations)
  ]
  tr_names <- c("Basionym", "Parent", "Level", "ViewID")
  tr_names <- tr_names[!tr_names %in% names(taxonRelations)]
  for (i in tr_names) {
    taxonRelations[, i] <- NA
  }
  if (!missing(levels)) {
    taxonRelations$Level <- factor(x = taxonRelations$Level, levels = levels)
  }
  # Replace Accepted Names
  AcceptedName <- x[x$AcceptedName, c("TaxonUsageID", "TaxonConceptID")]
  taxonRelations$AcceptedName <-
    AcceptedName$TaxonUsageID[match(
      taxonRelations$TaxonConceptID,
      AcceptedName$TaxonConceptID
    )]
  # Slot taxonNames
  tn_names <- names(taxonRelations)[names(taxonRelations) != "TaxonConceptID"]
  # Assembly output object
  taxlist@taxonRelations <- taxonRelations
  taxlist@taxonNames <- x[, !names(x) %in% tn_names]
  if (!missing(taxonTraits)) {
    taxlist@taxonTraits <- taxonTraits
  }
  if (!missing(taxonViews)) {
    taxlist@taxonViews <- taxonViews
  }
  return(taxlist)
}

#' @rdname df2taxlist
#' @aliases df2taxlist,character-method
#' @method df2taxlist character
#' @export
df2taxlist.character <- function(x, ...) {
  return(df2taxlist(data.frame(TaxonName = x), ...))
}

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,data.frame,taxlist-method
setAs("data.frame", "taxlist", function(from) {
  return(df2taxlist(from))
})

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,character,taxlist-method
setAs("character", "taxlist", function(from) {
  return(df2taxlist(from))
})
