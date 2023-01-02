#' @name match_names
#'
#' @title Search matchings between character and taxlist objects
#'
#' @description
#' Names provided in a character vector will be compared with names stored in
#' slot `taxonNames` within an object of class [taxlist-class] by
#' using the function [stringsim()].
#'
#' @param x A character vector with names to be compared.
#' @param object Either a character vector or a [taxlist-class] object
#'     containing the taxonomic list for comparison. If missing, the similarity
#'     of each name in 'x' will be compared with the rest of the names in the
#'     same vector.
#' @param UsageID A vector with IDs for single usage names in the compared list.
#'     If the IDs are duplicated or not as much as names in 'object', the
#'     function retrieves an error message. If missing, this function will
#'     number every name anew (see column 'TaxonUsageID' in the output object).
#' @param best Integer value indicating how many matches should be displayed
#'     in the output. Matches with the same value of similarity will be
#'     considered as one. Note that this argument will be overrode by
#'     'cutlevel'.
#' @param cutlevel A numeric value indicating a cut level of similarity,
#'     considering as match names with similarities equal or bigger than the
#'     cut value. This argument overrides 'best'.
#' @param nomatch A logical value indicating wheter names without matches should
#'     be included in the output (`'nomatch = TRUE'`) or not
#'     (`'nomatch = FALSE'`).
#' @param include_author A logical value indicating whether the author name
#'     in object (method for [taxlist-class]) should be included in the matching
#'     list or not.
#' @param show_concepts Logical value indicating whether the respective taxon
#'     concepts should be displayed in output or not.
#' @param accepted_only Logical value indicating whether only accepted names
#'     should be matched or all usage names (including synonyms).
#' @param method Further arguments passed to [stringsim()].
#' @param ... Further arguments passed among methods.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [stringsim()]
#'
#' @example examples/match_names.R
#'
#' @rdname match_names
#'
#' @exportMethod match_names
setGeneric(
  "match_names",
  function(x, object, ...) {
    standardGeneric("match_names")
  }
)

#' @rdname match_names
#' @aliases match_names,character,character-method
setMethod(
  "match_names", signature(x = "character", object = "character"),
  function(x, object, UsageID, best = 1, nomatch = TRUE, method = "lcs",
           cutlevel = NULL, ...) {
    if (any(is.na(x))) {
      stop("NAs are not allowed in argument 'x'")
    }
    x <- data.frame(idx = seq_along(x), submittedname = x)
    if (!missing(UsageID)) {
      if (length(UsageID) != length(object)) {
        stop(paste(
          "Length of argument 'object' have to be the same",
          "as length of argument 'UsageID'."
        ))
      }
      if (any(duplicated(UsageID))) {
        stop("Duplicated values in 'UsageID' are not allowed.")
      }
      object <- data.frame(TaxonUsageID = UsageID, TaxonName = object)
    } else {
      object <- data.frame(TaxonUsageID = seq_along(object), TaxonName = object)
    }
    SIM <- lapply(split(x, x$idx), function(a, b, c, method, cutlevel) {
      b$idx <- a$idx
      b$similarity <- stringsim(a$submittedname, b$TaxonName, method)
      b <- b[order(b$similarity, decreasing = TRUE), ]
      b$match <- match(b$similarity, unique(b$similarity))
      if (!is.null(cutlevel)) {
        b <- b[b$similarity >= cutlevel, ]
      } else {
        b <- b[b$match <= c, ]
      }
      return(b)
    },
    b = object, c = best, method = method, cutlevel = cutlevel, ...
    )
    SIM <- do.call(rbind, SIM)
    SIM$submittedname <- x$submittedname[match(SIM$idx, x$idx)]
    if (nomatch & !all(x$idx %in% SIM$idx)) {
      x <- x[!(x$idx %in% SIM$idx), ]
      x$match <- 0
      SIM <- insert_rows(SIM, x)
    }
    rownames(SIM) <- NULL
    class(SIM) <- c("matched_names", "data.frame")
    return(SIM[, c(
      "idx", "submittedname", "TaxonUsageID", "TaxonName",
      "match", "similarity"
    )])
  }
)

#' @rdname match_names
#' @aliases match_names,character,missing-method
setMethod(
  "match_names", signature(x = "character", object = "missing"),
  function(x, best, cutlevel, nomatch = TRUE, ...) {
    if (missing(cutlevel)) {
      if (!missing(best)) {
        SIM <- match_names(x, x, best = best + 1, ...)
      } else {
        SIM <- match_names(x, x, best = 2, ...)
      }
    } else {
      SIM <- match_names(x, x, cutlevel = cutlevel, ...)
    }
    SIM$match <- SIM$match - 1
    SIM <- split(SIM, SIM$match == 0)
    if (nomatch) {
      SIM$"TRUE" <- SIM$"TRUE"[!SIM$"TRUE"$idx %in% SIM$"FALSE"$idx, ]
      if (nrow(SIM$"TRUE" > 0)) {
        SIM <- insert_rows(SIM$"FALSE", SIM$"TRUE"[
          ,
          c("idx", "submittedname", "match")
        ])
      } else {
        SIM <- SIM$"FALSE"
      }
    } else {
      SIM <- SIM$"FALSE"
    }
    return(SIM)
  }
)

#' @rdname match_names
#' @aliases match_names,character,taxlist-method
setMethod(
  "match_names", signature(x = "character", object = "taxlist"),
  function(x, object, show_concepts = FALSE, accepted_only = FALSE,
           include_author = FALSE, ...) {
    if (accepted_only) {
      tax_names <- object@taxonNames[object@taxonNames$TaxonUsageID %in%
        object@taxonRelations$AcceptedName, ]
    } else {
      tax_names <- object@taxonNames
    }
    if (include_author) {
      tax_names$TaxonName <- paste(tax_names$TaxonName, tax_names$AuthorName)
    }
    SIM <- match_names(x,
      object = tax_names$TaxonName,
      UsageID = tax_names$TaxonUsageID, ...
    )
    SIM$AuthorName <- object@taxonNames$AuthorName[
      match(SIM$TaxonUsageID, object@taxonNames$TaxonUsageID)
    ]
    if (include_author) {
      SIM$TaxonName <- object@taxonNames$TaxonName[
        match(SIM$TaxonUsageID, object@taxonNames$TaxonUsageID)
      ]
    }
    if (show_concepts) {
      SIM$TaxonConceptID <- object@taxonNames$TaxonConceptID[
        match(SIM$TaxonUsageID, object@taxonNames$TaxonUsageID)
      ]
      tax_concepts <- object@taxonRelations[
        object@taxonRelations$TaxonConceptID %in%
          unique(SIM$TaxonConceptID),
      ]
      tax_concepts$AcceptedTaxonName <- object@taxonNames$TaxonName[
        match(tax_concepts$AcceptedName, object@taxonNames$TaxonUsageID)
      ]
      tax_concepts$AcceptedAuthorName <- object@taxonNames$AuthorName[
        match(tax_concepts$AcceptedName, object@taxonNames$TaxonUsageID)
      ]
      SIM <- merge(SIM, tax_concepts[
        ,
        c(
          "TaxonConceptID", "AcceptedTaxonName", "AcceptedAuthorName",
          "Level"
        )
      ], all.x = TRUE, sort = FALSE)
    }
    col_names <- c(
      "idx", "submittedname", "TaxonUsageID", "TaxonName",
      "AuthorName", "match", "similarity"
    )
    return(SIM[, c(col_names, names(SIM)[!names(SIM) %in% col_names])])
  }
)
