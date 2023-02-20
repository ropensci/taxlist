#' @name taxon_names
#'
#' @title Handle information on taxon usage names.
#'
#' @description
#' The slot `taxonNames` in [taxlist-class] objects contains
#' taxon usage names for the respective taxon.
#' These functions assist on the access and modification of entries for names.
#'
#' @param taxlist A [taxlist-class] object to be modified.
#' @param value A data frame used as new slot `taxonNames` in `taxlist`.
#' @param ConceptID Numeric vector indicating the concept ID to which the
#'     synonyms will be added.
#' @param TaxonName,AuthorName Character values used for the new names
#'     (synonyms).
#' @param UsageID Numeric vector indicating the taxon usage IDs to be updated.
#' @param ... Further arguments passed among methods. In `update_name` are
#'     vectors including the variables to be updated for the respective taxon
#'     usage ID.
#'
#' @details
#' The replacement method `taxon_names<-` is a quick alternative to include
#' names in empty [taxlist-class] objects.
#'
#' The function `add_synonym()` works only for adding names to existing
#' taxon concepts.
#' For adding new taxon concepts as well you should use [add_concept()].
#'
#' @return A data frame or, in the case of the replacement method, a
#' [taxlist-class] object with modified slot `taxonNames`.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [taxlist-class]
#'
#' @example examples/taxon_names.R
#'
#' @rdname taxon_names
#'
#' @export
taxon_names <- function(taxlist, ...) UseMethod("taxon_names", taxlist)

#' @rdname taxon_names
#' @aliases taxon_names,taxlist-method
#' @method taxon_names taxlist
#' @export
taxon_names.taxlist <- function(taxlist, ...) taxlist@taxonNames

#' @rdname taxon_names
#' @aliases taxon_names<-
#' @export
`taxon_names<-` <- function(taxlist, ..., value) {
  UseMethod("taxon_names<-", taxlist)
}

#' @rdname taxon_names
#' @aliases taxon_names<-,taxlist-method
#' @method taxon_names<- taxlist
#' @export
`taxon_names<-.taxlist` <- function(taxlist, ..., value) {
  if (!is(value, "data.frame")) {
    stop("Argument 'value' have to be of class 'data.frame'.")
  }
  tab_names <- c("TaxonUsageID", "TaxonConceptID", "TaxonName")
  tab_names <- tab_names[!tab_names %in% names(value)]
  if (length(tab_names) > 0) {
    stop(paste0(
      "Following mandatory names are missing in 'value': '",
      paste0(tab_names, collapse = "', '"), "'."
    ))
  }
  if(!"AuthorName" %in% names(value))
    value$AuthorName <- NA
  taxlist@taxonNames <- value
  return(taxlist)
}

#' @rdname taxon_names
#' @aliases add_synonym
#' @export
add_synonym <- function(taxlist, ...) UseMethod("add_synonym", taxlist)

#' @rdname taxon_names
#' @aliases add_synonym,taxlist-method
#' @method add_synonym taxlist
#' @export
add_synonym.taxlist <- function(taxlist, ConceptID, TaxonName, AuthorName, ...) {
  if (!all(ConceptID %in% taxlist@taxonRelations$TaxonConceptID)) {
    stop(paste(
      "Some values in 'ConceptID' are not included as",
      "concepts in 'taxlist'"
    ))
  }
  # For addition of multiple synonyms to multiple concepts
  if (length(ConceptID) == 1) rep(ConceptID, length(TaxonName))
  TaxonConceptID <- ConceptID
  TaxonUsageID <- max(taxlist@taxonNames$TaxonUsageID) + 1
  TaxonUsageID <- TaxonUsageID:(TaxonUsageID + length(TaxonName) - 1)
  new_name <- list(
    TaxonConceptID = TaxonConceptID,
    TaxonUsageID = TaxonUsageID, TaxonName = TaxonName,
    AuthorName = AuthorName, ...
  )
  for (i in colnames(taxlist@taxonNames)[
    !colnames(taxlist@taxonNames) %in% names(new_name)
  ]) {
    new_name[[i]] <- rep(NA, length(new_name$TaxonConceptID))
  }
  for (i in names(new_name)[!names(new_name) %in%
    colnames(taxlist@taxonNames)]) {
    taxlist@taxonNames[, i] <- NA
  }
  taxlist@taxonNames <- do.call(
    rbind,
    list(taxlist@taxonNames,
      new_name[match(
        colnames(taxlist@taxonNames),
        names(new_name)
      )],
      stringsAsFactors = FALSE
    )
  )
  return(taxlist)
}

#' @rdname taxon_names
#' @aliases update_name
#' @export
update_name <- function(taxlist, ...) UseMethod("update_name", taxlist)

#' @rdname taxon_names
#' @aliases update_name,taxlist-method
#' @method update_name taxlist
#' @export
update_name.taxlist <- function(taxlist, UsageID, ...) {
  new_entries <- as.data.frame(list(...), stringsAsFactors = FALSE)
  if (length(UsageID) != nrow(new_entries)) {
    stop(paste(
      "Length of 'UsageID' is not matching the length",
      "of corrected entries."
    ))
  }
  if (any(!UsageID %in% taxlist@taxonNames$TaxonUsageID)) {
    stop(paste(
      "Some values of 'UsageID' are not included as",
      "taxon usage names in 'taxlist'."
    ))
  }
  if (any(!colnames(new_entries) %in% colnames(taxlist@taxonNames))) {
    stop(paste(
      "Some of the indicated variables are not included",
      "in 'taxlist' (slot TaxonNames)."
    ))
  }
  for (i in colnames(new_entries)) {
    taxlist@taxonNames[
      match(
        UsageID,
        taxlist@taxonNames$TaxonUsageID
      ),
      i
    ] <- new_entries[, i]
  }
  return(taxlist)
}

#' @rdname taxon_names
#' @aliases delete_name
#' @export
delete_name <- function(taxlist, ...) UseMethod("delete_name", taxlist)

#' @rdname taxon_names
#' @aliases delete_name,taxlist-method
#' @method delete_name taxlist
#' @export
delete_name.taxlist <- function(taxlist, UsageID, ...) {
  if (any(UsageID %in% taxlist@taxonRelations$AcceptedName)) {
    stop(paste(
      "Values in 'UsageID' are not allowed to be",
      "accepted names in 'taxlist'."
    ))
  }
  if (any(UsageID %in% taxlist@taxonRelations$Basionym)) {
    stop(paste(
      "Values in 'UsageID' are not allowed to be",
      "basionyms in 'taxlist'."
    ))
  }
  taxlist@taxonNames <- taxlist@taxonNames[
    !taxlist@taxonNames$TaxonUsageID %in% UsageID,
  ]
  return(taxlist)
}
