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
#' @param style Character value indicating the alternative format for italics
#'     (at the moment only markdown and html implemented).
#' @param ... Further arguments passed among methods.
#' 
#' @details 
#' In **Rmarkdown** documents use \code{`r I(print_name(Easplist, 206))`} for
#' inserting a formatted a species name.
#' 
#' @return A character value including format to italic font.
#' 
#' @seealso [ape::mixedFontLabel()].
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' summary(Easplist, 363, secundum="secundum")
#' 
#' ## Empty plot
#' plot(x=NA, xlim=c(0,5), ylim=c(7,1), bty="n", xaxt="n", xlab="",
#'     ylab="options")
#' 
#' ## Accepted name with author
#' text(x=0, y=1, labels=print_name(Easplist, 363, style="expression"), pos=4)
#' 
#' ## Including taxon view
#' text(x=0, y=2, labels=print_name(Easplist, 363, style="expression",
#'     secundum="secundum"), pos=4)
#' 
#' ## Second mention in text
#' text(x=0, y=3, labels=print_name(Easplist, 363, style="expression",
#'     second_mention=TRUE), pos=4)
#' 
#' ## Using synonym
#' text(x=0, y=4, labels=print_name(Easplist, 50037, style="expression",
#'     concept=FALSE), pos=4)
#' 
#' ## Markdown style
#' text(0, 5, labels=print_name(Easplist, 363, style="markdown"), pos=4)
#' 
#' ## HTML style
#' text(0, 6, labels=print_name(Easplist, 363, style="html"), pos=4)
#' 
#' ## LaTeX style for knitr
#' text(x=0, y=7, labels=print_name(Easplist, 363, style="knitr"), pos=4)
#' 
#' @rdname print_name
#' 
#' @exportMethod print_name
#' 
setGeneric("print_name",
		function(object, id, ...)
			standardGeneric("print_name")
)

#' @rdname print_name
#' 
#' @aliases print_name,taxlist,numeric-method
#' 
setMethod("print_name", signature(object="taxlist", id="numeric"),
		function(object, id, concept=TRUE, second_mention=FALSE,
				include_author=TRUE, secundum, style="markdown", ...) {
			if(length(id) > 1) {
				warning(paste("'print_taxa' will format only the first value",
								"in 'id'."))
				id <- id[1]
			}
			if(concept) {
				id_name <- object@taxonRelations$AcceptedName[
						object@taxonRelations$TaxonConceptID == id]
			} else id_name <- id
			Name <- dissect_name(object@taxonNames$TaxonName[
							object@taxonNames$TaxonUsageID == id_name])[1, ]
			Italic <- !grepl(".", Name, fixed=TRUE)
			if(include_author & !second_mention) {
				Name <- c(Name, object@taxonNames$AuthorName[
								object@taxonNames$TaxonUsageID == id_name])
				Italic <- c(Italic, FALSE)
			}
			if(concept & include_author & !missing(secundum) &
					!second_mention) {
				View <- object@taxonRelations$ViewID[
						object@taxonRelations$TaxonConceptID == id]
				View <- object@taxonViews[object@taxonViews$ViewID == View,
						secundum]
				Name <- c(Name, "sec.", View)
				Italic <- c(Italic, FALSE, FALSE)
			}
			# For second time citations
			if(second_mention)
				Name[1] <- paste0(substring(Name[1], 1, 1), ".")
			# Output style
			style <- pmatch(tolower(style), c("markdown","html","expression",
							"knitr"))
			if(!style %in% c(1:4))
				stop("Non-valid value for 'style'")
			if(style == 1) {
				Start <- "*"
				End <- "*"
			}
			if(style == 2) {
				Start <- "<i>"
				End <- "</i>"
			}
			if(style == 3) {
				Start <- "italic("
				End <- ")"
				for(i in seq_along(Name))
					Name[i] <- paste0("\"", Name[i], "\"")
			}
			if(style == 4) {
				Start <- "\\textit{"
				End <- "}"
			}
			# Construct string
			for(i in seq_along(Name))
				if(Italic[i])
					Name[i] <- paste0(Start, Name[i], End)
			if(style == 3) {
				Name <- paste(Name, collapse="~")
				Name <- gsub(paste0("\"", End, "~", Start, "\""), " ", Name,
						fixed=TRUE)
				return(parse(text=Name))
			} else {
				Name <- paste(Name, collapse=" ")
				Name <- gsub(paste(End, Start), " ", Name, fixed=TRUE)
				return(I(Name))
			}
		}
)
