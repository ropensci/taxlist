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
#'     authorities), **AccpetedName** (logical indicating whether the name is
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
#' @param taxonViews A data frame or [lib_df-class] with references of taxonomic
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
#' @examples
#' Cyperus <- read.csv(file = file.path(
#'   path.package("taxlist"), "cyperus",
#'   "names.csv"
#' ))
#' head(Cyperus)
#'
#' ## Convert to 'taxlist' object
#' Cyperus$AcceptedName <- !Cyperus$SYNONYM
#' Cyperus <- df2taxlist(Cyperus)
#' Cyperus
#'
#' ## Create a 'taxlist' object from character vectors
#' Plants <- df2taxlist(c("Triticum aestivum", "Zea mays"), AuthorName = "L.")
#' summary(Plants, "all")
#'
#' @rdname df2taxlist
#'
#' @export
df2taxlist <- function(x, ...) {
  UseMethod("df2taxlist", x)
}

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
  # Mandatory columns
  opt_cols <- c("TaxonConceptID", "TaxonUsageID", "TaxonName")
  opt_cols <- opt_cols[!opt_cols %in% names(x)]
  if (length(opt_cols) > 0) {
    stop(paste0(
      "Mandatory columns missing in 'x':\n",
      paste0(opt_cols, collapse = ", "), "."
    ))
  }
  # Accepted names
  if (!"AcceptedName" %in% names(x)) {
    message(paste(
      "No values for 'AcceptedName' in 'x'.",
      "all names will be considered as accepted names."
    ))
    x$AcceptedName <- TRUE
  }
  # Optional columns
  opt_cols <- c("AuthorName", "Level", "Parent", "ViewID", "Basionym")
  opt_cols <- opt_cols[!opt_cols %in% names(x)]
  for (i in opt_cols) {
    x[, i] <- NA
  }
  # set integer classes
  for (i in c("TaxonUsageID", "TaxonConceptID", "Parent", "ViewID")) {
    if (!is.integer(x[, i, drop = TRUE])) {
      x[, i] <- as.integer(x[, i, drop = TRUE])
    }
  }
  # Duplicated names
  dupl_names <- duplicated(x[, c("TaxonName", "AuthorName")])
  if (any(dupl_names)) {
    dupl_names <- paste(x$TaxonName[dupl_names], x$AuthorName[dupl_names])
    dupl_names <- x[
      paste(x$TaxonName, x$AuthorName) %in% dupl_names,
      c("TaxonUsageID", "TaxonConceptID", "TaxonName", "AuthorName")
    ]
    print(dupl_names)
    stop(paste(
      "Duplicated names detected (see above).",
      "Resolve it and try again."
    ))
  }
  # Duplicated names IDs
  dupl_names <- duplicated(x$TaxonUsageID)
  if (any(dupl_names)) {
    dupl_names <- dupl_names[
      x$TaxonUsageID %in% x$TaxonUsageID[dupl_names],
      c("TaxonUsageID", "TaxonConceptID", "TaxonName", "AuthorName")
    ]
    print(dupl_names)
    stop(paste(
      "Duplicated values in 'TaxonUsageID' (see above).",
      "Resolve it and try again."
    ))
  }
  # Duplicated concept IDs
  dupl_names <- duplicated(x$TaxonConceptID) & x$AcceptedName
  if (any(dupl_names)) {
    dupl_names <- x[
      (x$TaxonConceptID %in%
        x$TaxonConceptID[dupl_names]) & x$AcceptedName,
      c("TaxonUsageID", "TaxonConceptID", "TaxonName", "AuthorName")
    ]
    print(dupl_names)
    stop(paste(
      "Duplicated values in 'TaxonConceptID'",
      "for accepted names (see above). Resolve it and try again."
    ))
  }
  # Wrong concepts for synonyms
  dupl_names <- !(x$TaxonConceptID[!x$AcceptedName] %in%
    x$TaxonConceptID[x$AcceptedName])
  if (any(dupl_names)) {
    synonyms <- x[!x$AcceptedName, ]
    dupl_names <- synonyms[
      dupl_names,
      c("TaxonUsageID", "TaxonConceptID", "TaxonName", "AuthorName")
    ]
    print(dupl_names)
    stop(paste(
      "Values of 'TaxonConceptID' for synonyms",
      "assigned to a missing concept (see above).",
      "Resolve it and try again."
    ))
  }
  # Set taxonomic ranks as factors
  if (!missing(levels)) {
    x$Level <- factor(as.character(x$Level), levels = levels)
  } else {
    if (!is.factor(x$Level) & !all(is.na(x$Level))) {
      x$Level <- as.factor(x$Level)
    }
  }
  # Create empty object
  taxlist <- new("taxlist")
  # 1: Start with taxon views
  if (!missing(taxonViews)) {
    if (clean_strings) {
      taxonViews <- clean_strings(taxonViews)
    }
    if (!is(taxonViews, "data.frame")) {
      stop("Argument for 'taxonViews' have to be of class 'data.frame'.")
    }
    if (!"ViewID" %in% names(taxonViews)) {
      stop("Column 'ViewID' is mandatory in 'taxonViews'.")
    }
    if (any(!x$ViewID[!is.na(x$ViewID)]) %in% taxonViews$ViewID) {
      stop("Some values of 'ViewID' in 'x' are missing in 'taxonViews'.")
    }
    taxlist@taxonViews <- taxonViews
  } else {
    x$ViewID <- NA
  }
  # 2: Taxon Names (detect additional columns for slot taxonNames)
  all_names <- unique(do.call(c, lapply(as.list(taxlist)[
    c("taxonNames", "taxonRelations")
  ], names)))
  extra_cols <- names(x)[!names(x) %in% all_names]
  taxlist@taxonNames <- x[, c(names(taxlist@taxonNames), extra_cols)]
  # 3: Taxon concepts
  taxonRelations <- x[x$AcceptedName, c(names(taxlist@taxonRelations))]
  taxonRelations$AcceptedName <- x$TaxonUsageID[x$AcceptedName]
  taxlist@taxonRelations <- taxonRelations
  # 4: Taxon Traits
  if (!missing(taxonTraits)) {
    if (clean_strings) {
      taxonTraits <- clean_strings(taxonTraits)
    }
    if (!is(taxonTraits, "data.frame")) {
      stop("Argument for 'taxonTraits' have to be of class 'data.frame'.")
    }
    if (!"TaxonConceptID" %in% names(taxonTraits)) {
      stop("Column 'TaxonConceptID' is mandatory in 'taxonTraits'.")
    }
    if (any(!taxonTraits$TaxonConceptID %in%
      taxlist@taxonRelations$TaxonConceptID)) {
      stop(paste(
        "Some values of 'TaxonConceptID' in 'taxonTraits'",
        "are missing in 'x'."
      ))
    }
    taxlist@taxonTraits <- taxonTraits
  }
  return(taxlist)
}

#' @rdname df2taxlist
#' @aliases df2taxlist,character-method
#' @method df2taxlist character
#' @export
df2taxlist.character <- function(x, ...) {
  if (any(duplicated(x))) {
    warning("Some duplicated names will be deleted")
    x <- x[!duplicated(x)]
  }
  x <- data.frame(
    TaxonUsageID = seq_along(x),
    TaxonConceptID = seq_along(x),
    TaxonName = x
  )
  return(df2taxlist(x, ...))
}
