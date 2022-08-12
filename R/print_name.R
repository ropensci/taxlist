#' @name print_name
#'
#' @title Format usage names for publications
#'
#' @description
#' When writing on bio-diversity, usage names could be automatically inserted in
#' documents including the typical italic format for different elements of a
#' scientific name.
#' The function `print_name` can be applied either in markdown documents or
#' for graphics.
#'
#' In **Rmarkdown** documents use \code{`r I(print_name(Easplist, 206))`} for
#' inserting a formatted a species name.
#'
#' @param object An object of class [taxlist-class].
#' @param id Integer containing either a concept or a name ID.
#' @param concept Logical value, whether `id` corresponds to a concept ID
#'     or a taxon usage name ID.
#' @param second_mention Logical value, whether the genus name should be
#'     abbreviated or not.
#' @param include_author Logical value, whether authors of the name should be
#'     mentioned or not.
#' @param secundum Character value indicating the column in slot `taxonViews`
#'     that will be mentioned as *secundum* (according to).
#' @param style Character value indicating the alternative format for italics.
#'     The available options are `"markdown"` (called within Rmarkdown
#'     documents), `"html"` (for documents rendered into html files),
#'     `"expression"` (used for labels in graphics), and `"knitr"` (format in
#'     LaTeX code).
#' @param isolate A character vector with words (usually abbreviations)
#'     appearing in the middle of scientific names, which are not formatted in
#'     italics.
#' @param trim A character vectors with words appearing at the end of scientific
#'     names that are not formatted in italics, either.
#' @param ... Further arguments passed among methods.
#'
#' @return A character value including format to italic font.
#'
#' @seealso [ape::mixedFontLabel()].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' summary(Easplist, 363, secundum = "secundum")
#'
#' ## Empty plot
#' plot(
#'   x = NA, xlim = c(0, 5), ylim = c(7, 1), bty = "n", xaxt = "n", xlab = "",
#'   ylab = "options"
#' )
#'
#' ## Accepted name with author
#' text(
#'   x = 0, y = 1, labels = print_name(Easplist, 363, style = "expression"),
#'   pos = 4
#' )
#'
#' ## Including taxon view
#' text(x = 0, y = 2, labels = print_name(Easplist, 363,
#'   style = "expression",
#'   secundum = "secundum"
#' ), pos = 4)
#'
#' ## Second mention in text
#' text(x = 0, y = 3, labels = print_name(Easplist, 363,
#'   style = "expression",
#'   second_mention = TRUE
#' ), pos = 4)
#'
#' ## Using synonym
#' text(x = 0, y = 4, labels = print_name(Easplist, 50037,
#'   style = "expression",
#'   concept = FALSE
#' ), pos = 4)
#'
#' ## Markdown style
#' text(0, 5, labels = print_name(Easplist, 363, style = "markdown"), pos = 4)
#'
#' ## HTML style
#' text(0, 6, labels = print_name(Easplist, 363, style = "html"), pos = 4)
#'
#' ## LaTeX style for knitr
#' text(x = 0, y = 7, labels = print_name(Easplist, 363, style = "knitr"), pos = 4)
#' @rdname print_name
#'
#' @export
print_name <- function(object, ...) {
  UseMethod("print_name", object)
}

#' @rdname print_name
#' @aliases print_name,character-method
#' @method print_name character
#' @export
print_name.character <- function(object, second_mention = FALSE,
                                 style = "markdown",
                                 isolate = c(
                                   "var.", "ssp.", "subsp.", "f.",
                                   "fma."
                                 ),
                                 trim = c("spp.", "sp.", "species"),
                                 ...) {
  # set style
  style <- pmatch(tolower(style), c(
    "markdown", "html", "knitr", "expression"
  ))
  if (!style %in% c(1:4)) {
    stop("Non-valid value for 'style'")
  }
  if (style == 1) {
    Start <- "*"
    End <- "*"
  }
  if (style == 2) {
    Start <- "<i>"
    End <- "</i>"
  }
  if (style == 3) {
    Start <- "\\textit{"
    End <- "}"
  }
  if (style == 4) {
    Start <- "italic(\""
    End <- "\")"
  }
  # Abbreviate first name if required
  if (second_mention) {
    name_parts <- unlist(lapply(str_split(object, " "), length))
    idx <- name_parts > 1
    first_part <- dissect_name(object, repaste = 1)
    name_abbr <- paste0(substr(first_part, 1, 1), ".")
    object[idx] <- str_replace_all(
      object[idx], first_part[idx],
      name_abbr[idx]
    )
  }
  # Add italics
  object <- paste0(Start, object, End)
  if (style != 4) {
    for (i in isolate) {
      object <- str_replace_all(
        object, fixed(paste0(" ", i, " ")),
        paste0(End, " ", i, " ", Start)
      )
    }
    for (i in trim) {
      object <- str_replace_all(
        object, fixed(paste0(" ", i, End)),
        paste0(End, " ", i)
      )
    }
    return(object)
  } else {
    for (i in isolate) {
      object <- str_replace_all(
        object, fixed(paste0(" ", i, " ")),
        paste0(End, "~\"", i, "\"~", Start)
      )
    }
    for (i in trim) {
      object <- str_replace_all(
        object, fixed(paste0(" ", i, End)),
        paste0(End, "~\"", i, "\"")
      )
    }
    if (length(object == 1)) {
      return(parse(text = object))
    } else {
      return(parse(text = paste0("expression(", object, ")")))
    }
  }
}

#' @rdname print_name
#' @aliases print_name,taxlist-method
#' @method print_name taxlist
#' @export
print_name.taxlist <- function(object, id, concept = TRUE,
                               include_author = TRUE, secundum, style = "markdown",
                               ...) {
  if (!missing(secundum)) {
    if (!secundum %in% names(object@taxonViews)) {
      stop(paste0(
        "The value '", secundum,
        "' is not included as column slot 'taxonViews'."
      ))
    }
  }
  if (missing(id)) {
    if (concept) {
      id <- object@taxonRelations$TaxonConceptID
    } else {
      id <- object@taxonNames$TaxonUsageID
    }
  }
  if (concept) {
    id <- with(object@taxonRelations, AcceptedName[match(id, TaxonConceptID)])
  }
  sp_names <- with(object@taxonNames, TaxonName[match(id, TaxonUsageID)])
  sp_names <- print_name(sp_names, style = style, ...)
  if (style != "expression") {
    if (include_author) {
      sp_names <- with(
        object@taxonNames,
        paste(sp_names, AuthorName[match(id, TaxonUsageID)])
      )
    }
    if (!missing(secundum) & concept) {
      concept_id <- with(
        object@taxonNames,
        TaxonConceptID[match(id, TaxonUsageID)]
      )
      view_id <- with(
        object@taxonRelations,
        ViewID[match(concept_id, TaxonConceptID)]
      )
      sp_names <- paste(
        sp_names, "sec.",
        object@taxonViews[[secundum]][match(
          view_id,
          object@taxonViews$ViewID
        )]
      )
    }
    return(sp_names)
  } else {
    if (include_author) {
      sp_names <- with(
        object@taxonNames,
        paste0(sp_names, "~\"", AuthorName[match(id, TaxonUsageID)], "\"")
      )
    }
    if (!missing(secundum) & concept) {
      concept_id <- with(
        object@taxonNames,
        TaxonConceptID[match(id, TaxonUsageID)]
      )
      view_id <- with(
        object@taxonRelations,
        ViewID[match(concept_id, TaxonConceptID)]
      )
      sp_names <- paste0(
        sp_names, "~\"sec. ",
        object@taxonViews[[secundum]][match(
          view_id,
          object@taxonViews$ViewID
        )], "\""
      )
    }
    if (length(sp_names == 1)) {
      return(parse(text = sp_names))
    } else {
      return(parse(text = paste0("expression(", sp_names, ")")))
    }
  }
}
