# TODO:   Method for printing taxon in text
# 
# Author: Miguel Alvarez
################################################################################

# Set function as generic
setGeneric("print_name",
		function(object, id, ...)
			standardGeneric("print_name")
)

# Set method
setMethod("print_name", signature(object="taxlist", id="numeric"),
		print_taxa <- function(object, id, concept=TRUE, second_mention=FALSE,
				include_author=TRUE, secundum, style="markdown", ...) {
			if(concept) {
				id_name <- object@taxonRelations$AcceptedName[
						object@taxonRelations$TaxonConceptID == id]
			} else id_name <- id
			Name <- dissect_name(object@taxonNames$TaxonName[
							object@taxonNames$TaxonUsageID == id_name])[1,]
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
			style <- pmatch(tolower(style), c("markdown","html","expression"))
			if(!style %in% c(1:3))
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
				for(i in 1:length(Name))
					Name[i] <- paste0("\"", Name[i], "\"")
			}
			## style <- pmatch(tolower(style), c("markdown","html","latex"))
			## if(!style %in% c(1:3))
			##     stop("Non-valid value for 'style'")
			## if(style == 3) {
			##     Start <- "\\textit{"
			##     End <- "}"
			## }
			# Construct string
			for(i in 1:length(Name))
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
