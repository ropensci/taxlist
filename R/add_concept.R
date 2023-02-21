#' @name add_concept
#'
#' @title Add new taxonomic concepts into taxlist objects
#'
#' @description
#' Alternative methods to add new concepts into existing `taxlist` objects.
#'
#' @param taxlist A [taxlist-class] object.
#' @param TaxonName Character vector with the accepted name for the new taxon
#'     concepts.
#' @param insert_view A numeric (integer) vector, indicating the views to be
#'     inserted in `taxlist` or the value `TRUE` (see details).
#' @param ConceptID Concept IDs to be updated.
#' @param ... Further arguments passed among methods.
#'
#' @rdname add_concept
#'
#' @aliases add_concept
#'
#' @exportMethod add_concept
setGeneric(
  "add_concept",
  function(taxlist, TaxonName, ...) {
    standardGeneric("add_concept")
  }
)

#' @rdname add_concept
#' @aliases add_concept,taxlist,data.frame-method
setMethod(
  "add_concept", signature(taxlist = "taxlist", TaxonName = "data.frame"),
  function(taxlist, TaxonName, ...) {
    # Input has at least one column
    if (!"TaxonName" %in% names(TaxonName)) {
      stop("Column 'TaxonName' is mandatory in 'TaxonName'.")
    }
    if ("Level" %in% names(TaxonName)) {
      tn_levels <- unique(TaxonName$Level[!is.na(TaxonName$Level)])
      tn_levels <- tn_levels[!tn_levels %in% levels(taxlist)]
      if (length(tn_levels) > 0) {
        stop(paste0(
          "Following levels are not included as taxonomic ranks ",
          "in the 'taxlist' object: '",
          paste0(tn_levels, collapse = "', '"), "'."
        ))
      } else {
        TaxonName$Level <- factor(x = TaxonName$Level, levels = levels(taxlist))
      }
    } else {
      TaxonName$Level <- NA
    }
    # Add identifiers, if missing
    if (!"TaxonUsageID" %in% names(TaxonName)) {
      if (is(taxlist@taxonNames$TaxonUsageID, "numeric")) {
        TaxonName$TaxonUsageID <- id_generator(len = nrow(TaxonName))
      } else {
        TaxonName$TaxonUsageID <- id_generator(
          len = nrow(TaxonName),
          mode = "character",
          nchar = max(nchar(taxlist@taxonNames$TaxonUsageID))
        )
      }
    }
    if (!"TaxonConceptID" %in% names(TaxonName)) {
      if (is(taxlist@taxonRelations$TaxonConceptID, "numeric")) {
        TaxonName$TaxonConceptID <- id_generator(len = nrow(TaxonName))
      } else {
        TaxonName$TaxonConceptID <- id_generator(
          len = nrow(TaxonName),
          mode = "character",
          nchar = max(nchar(taxlist@taxonRelations$TaxonConceptID))
        )
      }
    }
    # Solve identifiers
    TaxonName$TaxonUsageID <- id_solver(
      insert = TaxonName$TaxonUsageID,
      to = taxlist@taxonNames$TaxonUsageID, sep = "_"
    )
    TaxonName$TaxonConceptID <- id_solver(
      insert = TaxonName$TaxonConceptID,
      to = taxlist@taxonRelations$TaxonConceptID, sep = "_"
    )
    # Add missing columns
    TaxonName$AcceptedName <- TaxonName$TaxonUsageID
    tx_names <- c("AuthorName", "Basionym", "Parent", "ViewID")
    tx_names <- tx_names[!tx_names %in% names(TaxonName)]
    for (i in tx_names) {
      TaxonName[, i] <- NA
    }
    # Last checks
    tx_names <- paste(TaxonName$TaxonName, TaxonName$AuthorName)
    tx_names <- tx_names[tx_names %in% paste(
      taxlist@taxonNames$TaxonName,
      taxlist@taxonNames$AuthorName
    )]
    if (length(tx_names) > 0) {
      stop(paste0(
        "Following taxon usage names are already in the ",
        "'taxlist' object:\n    ",
        paste0(tx_names, collapse = "\n    ")
      ))
    }
    tx_parents <- TaxonName$Parent[!is.na(TaxonName$Parent)]
    tx_parents <- tx_parents[!tx_parents %in%
      taxlist@taxonRelations$TaxonConceptID]
    if (length(tx_parents) > 0) {
      stop(paste0(
        "Following taxon concepts included as parents in ",
        "'TaxonName' are not occurring in the ",
        "'taxlist' object: '",
        paste0(tx_parents, collapse = "', '"), "'."
      ))
    }
    tx_views <- TaxonName$ViewID[!is.na(TaxonName$ViewID)]
    tx_views <- tx_views[!tx_views %in%
      taxlist@taxonViews$ViewID]
    if (length(tx_views) > 0) {
      stop(paste0(
        "Following taxon views included in ",
        "'TaxonName' are not occurring in the ",
        "'taxlist' object: '",
        paste0(tx_views, collapse = "', '"), "'."
      ))
    }
    # Merge Tables
    taxlist@taxonRelations <- do.call(rbind, list(
      taxlist@taxonRelations,
      TaxonName[match(names(taxlist@taxonRelations), names(TaxonName))]
    ))
    taxlist@taxonNames <- do.call(rbind, list(
      taxlist@taxonNames,
      TaxonName[match(names(taxlist@taxonNames), names(TaxonName))]
    ))
    # TODO: See issue #30
    return(taxlist)
  }
)

#' @rdname add_concept
#' @aliases add_concept,taxlist,character-method
setMethod(
  "add_concept", signature(taxlist = "taxlist", TaxonName = "character"),
  function(taxlist, TaxonName, ...) {
    TaxonName <- do.call(data.frame, c(
      list(TaxonName = TaxonName),
      list(...)
    ))
    return(add_concept(taxlist = taxlist, TaxonName = TaxonName))
  }
)

#' @rdname add_concept
#' @aliases add_concept,taxlist,taxlist-method
setMethod(
  "add_concept", signature(taxlist = "taxlist", TaxonName = "taxlist"),
  function(taxlist, TaxonName, insert_view = FALSE, ...) {
    tab_names <- list()
    for (i in c("taxonNames", "taxonRelations", "taxonTraits")) {
      tab_names[[i]] <- names(slot(TaxonName, i))
    }
    tab_names <- unique(do.call(c, tab_names))
    TaxonName <- as(TaxonName, "data.frame")[, tab_names]
    # TODO: Insert View (see R/taxon_views.R)
    return(add_concept(taxlist = taxlist, TaxonName = TaxonName))
  }
)

#' @rdname add_concept
#' @aliases update_concept
#' @exportMethod update_concept
setGeneric(
  "update_concept",
  function(taxlist, ConceptID, ...) {
    standardGeneric("update_concept")
  }
)

#' @rdname taxon_relations
#' @aliases update_concept,taxlist,numeric-method
setMethod(
  "update_concept", signature(taxlist = "taxlist", ConceptID = "numeric"),
  function(taxlist, ConceptID, ...) {
    if (any(!ConceptID %in% taxlist@taxonRelations$TaxonConceptID)) {
      stop(paste(
        "Some values of 'ConceptID' are not included as",
        "taxon concept IDs in 'taxlist'."
      ))
    }
    new_entries <- list(...)
    for (i in names(new_entries)[names(new_entries) %in%
      colnames(taxlist@taxonRelations)]) {
      taxlist@taxonRelations[match(
        ConceptID,
        taxlist@taxonRelations$TaxonConceptID
      ), i] <-
        new_entries[[i]]
    }
    if (any(names(new_entries) %in% colnames(taxlist@taxonTraits))) {
      taxlist <- do.call(update_concept, c(
        list(ConceptID = ConceptID),
        new_entries[names(new_entries) %in%
          colnames(taxlist@taxonTraits)]
      ))
    }
    return(taxlist)
  }
)
