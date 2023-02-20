#' @name accepted_name
#'
#' @title Manage accepted names, synonyms and basionyms
#'
#' @description
#' Taxon usage names for a taxon concept can be divided into three categories:
#' accepted names, basionyms and synonyms.
#' Each single taxon concept may at least have an accepted name, while basionym
#' and synonyms are optional.
#'
#' The function `accepted_name()` retrieves the accepted names for the indicated
#' taxon concepts or for the whole [taxlist-class] object.
#' By using `show_traits=TRUE`, the respective taxon traits will be
#' displayed as well, providing an overview of taxa included in the object.
#' The replacement method for this function will set the respective usage name
#' IDs as accepted names for the respective taxon concept, provided that these
#' names are already set as synonyms in the respective concepts.
#'
#' The function `synonyms()` is working in a similar way as `accepted_name()`,
#' but this function does not include taxon traits in the output.
#' Alternatives for inserting new synonyms into a taxon concept are either
#' moving synonyms from other taxa by using [change_concept<-] or
#' inserting new names in the object by using [add_synonym()].
#'
#' The function `basionym()` is retrieving and setting basionyms in the
#' respective taxon concepts similarly to `accepted_name`, but this function
#' does not retrieve any information on taxon traits, either.
#'
#' The fucntion `change_concept<-` replace a taxon usage name (argument
#' `'UsageID'`) to a different taxonomic concept (argument `'value'`).
#'
#' @param taxlist An object of class [taxlist-class].
#' @param ConceptID Integer containing concept IDs where to request or set names
#'     for one category.
#' @param show_traits Logical value, whether traits should be included in the
#'     output of `accepted_name` or not.
#' @param UsageID Numeric vector with taxon usage IDs that will be changed to a
#'     different taxonomic concept.
#' @param value Integer containing usage IDs to be set to the respective
#'     category in the respective taxon concept.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' Most of the methods return information in data frames, while
#' replacement methods do it as [taxlist-class] objects.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [add_synonym()] [change_concept<-]
#'
#' @example examples/accepted_name.R
#'
#' @rdname accepted_name
#'
#' @export
accepted_name <- function(taxlist, ...) UseMethod("accepted_name", taxlist)

#' @rdname accepted_name
#' @aliases accepted_name,taxlist-method
#' @method accepted_name taxlist
#' @export
accepted_name.taxlist <- function(
    taxlist, ConceptID, show_traits = FALSE,
    ...) {
  if (missing(ConceptID)) {
    ConceptID <- taxlist@taxonRelations$TaxonConceptID
  }
  AcceptedName <- taxlist@taxonRelations[
    taxlist@taxonRelations$TaxonConceptID %in%
      ConceptID, c("TaxonConceptID", "AcceptedName")
  ]
  for (i in c("TaxonName", "AuthorName")) {
    AcceptedName[, i] <- taxlist@taxonNames[
      match(
        AcceptedName$AcceptedName,
        taxlist@taxonNames$TaxonUsageID
      ), i
    ]
  }
  colnames(AcceptedName)[2] <- "TaxonUsageID"
  AcceptedName <- merge(AcceptedName, taxlist@taxonRelations[
    ,
    c("TaxonConceptID", "ViewID", "Level")
  ], sort = FALSE)
  if (show_traits) {
    AcceptedName <- merge(AcceptedName, taxlist@taxonTraits,
      sort = FALSE, all.x = TRUE
    )
  }
  return(AcceptedName)
}

#' @rdname accepted_name
#' @aliases accepted_name<-
#' @export
`accepted_name<-` <- function(taxlist, ..., value) {
  UseMethod("accepted_name<-", taxlist)
}

#' @rdname accepted_name
#' @aliases accepted_name<-,taxlist-method
#' @method accepted_name<- taxlist
#' @export
`accepted_name<-.taxlist` <- function(taxlist, ConceptID, ..., value) {
  # first test
  if (length(ConceptID) != length(value)) {
    stop("'ConceptID' and 'value' should be of the same length.")
  }
  if (!all(taxlist@taxonNames[
    match(
      value,
      taxlist@taxonNames$TaxonUsageID
    ),
    "TaxonConceptID"
  ] == ConceptID)) {
    stop(paste(
      "Some concepts in 'value' are not included in the",
      "respective taxon concept."
    ))
  }
  # now replace
  taxlist@taxonRelations[
    match(
      ConceptID,
      taxlist@taxonRelations$TaxonConceptID
    ),
    "AcceptedName"
  ] <- value
  return(taxlist)
}

#' @rdname accepted_name
#' @aliases synonyms
#' @export
synonyms <- function(taxlist, ...) {
  UseMethod("synonyms", taxlist)
}

#' @rdname accepted_name
#' @aliases synonyms,taxlist-method
#' @method synonyms taxlist
#' @export
synonyms.taxlist <- function(taxlist, ConceptID, ...) {
  if (missing(ConceptID)) {
    ConceptID <- taxlist@taxonRelations$TaxonConceptID
  }
  Syn <- taxlist@taxonNames[taxlist@taxonNames$TaxonConceptID %in%
    ConceptID, c(
    "TaxonUsageID", "TaxonConceptID",
    "TaxonName", "AuthorName"
  )]
  Syn$AcceptedName <- taxlist@taxonRelations$AcceptedName[
    match(
      Syn$TaxonConceptID,
      taxlist@taxonRelations$TaxonConceptID
    )
  ]
  Syn$AuthorAcceptedName <- taxlist@taxonNames$AuthorName[
    match(Syn$AcceptedName, taxlist@taxonNames$TaxonUsageID)
  ]
  Syn$AcceptedName <- taxlist@taxonNames$TaxonName[
    match(Syn$AcceptedName, taxlist@taxonNames$TaxonUsageID)
  ]
  Syn <- Syn[!Syn$TaxonUsageID %in%
    taxlist@taxonRelations$AcceptedName[
      taxlist@taxonRelations$TaxonConceptID %in%
        ConceptID
    ], ]
  return(Syn)
}

#' @rdname accepted_name
#' @aliases basionym
#' @export
basionym <- function(taxlist, ...) {
  UseMethod("basionym", taxlist)
}

#' @rdname accepted_name
#' @aliases basionym,taxlist-method
#' @method basionym taxlist
#' @export
basionym.taxlist <- function(taxlist, ConceptID, ...) {
  if (missing(ConceptID)) {
    ConceptID <- taxlist@taxonRelations$TaxonConceptID
  }
  Basionym <- taxlist@taxonRelations[
    match(
      ConceptID,
      taxlist@taxonRelations$TaxonConceptID
    ),
    c("TaxonConceptID", "Basionym")
  ]
  Basionym$BasionymName <- taxlist@taxonNames$TaxonName[
    match(Basionym$Basionym, taxlist@taxonNames$TaxonUsageID)
  ]
  Basionym$BasionymAuthor <- taxlist@taxonNames$AuthorName[
    match(Basionym$Basionym, taxlist@taxonNames$TaxonUsageID)
  ]
  return(Basionym)
}

#' @rdname accepted_name
#' @aliases basionym<-
#' @export
`basionym<-` <- function(taxlist, ..., value) {
  UseMethod("basionym<-", taxlist)
}

#' @rdname accepted_name
#' @aliases basionym<-,taxlist-method
#' @method basionym<- taxlist
#' @export
`basionym<-.taxlist` <- function(taxlist, ConceptID, ..., value) {
  # first test
  if (length(ConceptID) != length(value)) {
    stop("'ConceptID' and 'value' should be of the same length.")
  }
  if (!all(taxlist@taxonNames[
    match(
      value,
      taxlist@taxonNames$TaxonUsageID
    ),
    "TaxonConceptID"
  ] == ConceptID)) {
    stop(paste(
      "Some concepts in 'value' are not included in the",
      "respective taxon concept."
    ))
  }
  # now replace
  taxlist@taxonRelations[
    match(
      ConceptID,
      taxlist@taxonRelations$TaxonConceptID
    ),
    "Basionym"
  ] <- value
  return(taxlist)
}

#' @rdname accepted_name
#' @aliases change_concept<-
#' @export
`change_concept<-` <- function(taxlist, ..., value) {
  UseMethod("change_concept<-", taxlist)
}

#' @rdname accepted_name
#' @aliases change_concept<-,taxlist-method
#' @method change_concept<- taxlist
#' @export
`change_concept<-.taxlist` <- function(taxlist, UsageID, ..., value) {
  # Test
  if (length(UsageID) != length(value)) {
    stop("'UsageID' and 'value' should be of the same length")
  }
  if (any(UsageID %in% taxlist@taxonRelations$AcceptedName)) {
    stop("Changes on concept are not allowed for accepted names")
  }
  # now replace
  taxlist@taxonNames[
    match(UsageID, taxlist@taxonNames$TaxonUsageID),
    "TaxonConceptID"
  ] <- value
  return(taxlist)
}
