#' @name indented_list
#' 
#' @title Print hierarchical structure in indented lists
#' 
#' @description
#' Print taxonomic hierarchies (ranks and parent-child relationships) from
#' [taxlist-class] objects in an indented list.
#' 
#' @param object A [taxlist-class] object containing taxonomic concepts.
#' @param filter A character value (optional) that will be matched with the
#'     taxon usage names to produce a subset of 'object'. Note that this filter
#'     will be also applied to synonyms, independent of the argument applied in
#'     parameter 'synonyms'.
#' @param keep_children A logical value indicating whether children of matched
#'     concept should be included in the result.
#' @param keep_parents A logical value indicating whether parents of matched
#'     concept should be included in the result.
#' @param indent Symbol used for indentation. This symbol will be multiplied by
#'     the depth of the taxonomic rank. The default is a blank space.
#'     This can be also provided as a named vector, with a different indentation
#'     symbol for the respective taxonomic ranks.
#' @param print A logical value indicating whether the indented list should be
#'     printed in the console or not (default = TRUE).
#' @param author A logical value indicating whether the author should be printed
#'     with the name (default = TRUE).
#' @param level A logical value indicating whether the name of the level
#'     (taxonomic rank) should be included before the name or not
#'     (default = FALSE).
#' @param synonyms A logical value indicating whether the synonyms should be
#'     included after accepted names or not (default = FALSE).
#' @param syn_encl A character vector of length 2 including the symbols used to
#'     enclose synonyms. First value will be set before the synonyms and second
#'     value, after the synonyms.
#' @param secundum A character value matching a name in slot 'taxonViews', which
#'     will be printed as secundum (taxon view). It is not printed by default.
#' @param ... Further arguments (not used yet).
#' 
#' @return
#' If 'print = TRUE', the indented list is printed in the console. The result,
#' which is a data frame with the elements used to format the names, can be also
#' assigned to an object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' ## Show taxonomy of papyrus
#' indented_list(Easplist, "papyrus")
#' 
#' ## Include synonyms and taxon views
#' indented_list(Easplist, "papyrus", level = TRUE, synonyms = TRUE,
#'     secundum = "secundum")
#' 
#' @rdname indented_list
#' 
#' @exportMethod indented_list
#' 
setGeneric("indented_list",
		function(object, ...)
			standardGeneric("indented_list")
)

#' @rdname indented_list
#' 
#' @aliases indented_list,taxlist-method
#' 
setMethod("indented_list", signature(object = "taxlist"),
		function(object, filter, keep_children = TRUE, keep_parents = TRUE,
				indent = " ", print = TRUE, author = TRUE, level = FALSE,
				synonyms = FALSE, syn_encl = c("= ", ""), secundum, ...) {
			object <- tax2traits(object)
			# Make subset
			if(!missing(filter)) {
				Temp <- object
				Concepts <- Temp@taxonNames[grepl(filter,
								Temp@taxonNames$TaxonName),
						"TaxonConceptID"]
				Temp@taxonRelations <- Temp@taxonRelations[
						Temp@taxonRelations$TaxonConceptID %in%
								unique(Concepts), ]
				Temp <- clean(Temp)
				if(keep_children)
					Temp <- get_children(object, Temp)
				if(keep_parents)
					Temp <- get_parents(object, Temp)
				object <- Temp
			}
			Names <- accepted_name(object, show_traits = TRUE)
			Names$formatted_name <- Names$TaxonName
			# Set indentation
			if(length(indent) == 1) {
				indent_symbol <- character()
				for(i in seq_along(levels(object)))
					indent_symbol[i] <- paste0(rep(indent, times = i - 1),
							collapse = "")
				Names$indent <- rev(indent_symbol)[match(paste(Names$Level),
								levels(object))]
			} else {
				no_match <- unique(paste(Names$Level))
				no_match <- no_match[!no_match %in% names(indent)]
				if(length(no_match) > 0)
					stop(paste0("Indentations missing in 'indent' ",
									"for following level(s): '",
									paste0(no_match, collapse = "' '"), "'"))
				Names$indent <- indent[match(paste(Names$Level), names(indent))]
			}
			if(level)
				Names$formatted_name <- paste(Names$Level, Names$formatted_name)
			if(author)
				Names$formatted_name <- paste(Names$formatted_name,
						Names$AuthorName)
			if(!missing(secundum))
				Names$formatted_name <- paste(Names$formatted_name, "sec.",
						object@taxonViews[match(Names$ViewID,
										object@taxonViews$ViewID), secundum])
			# indented names
			Names$formatted_name <- paste0(Names$indent, Names$formatted_name)
			# Handle synonyms
			if(synonyms & nrow(synonyms(object)) > 0) {
				Syn <- synonyms(object)
				Syn$formatted_name <- Syn$TaxonName
				Syn$indent <- Names$indent[match(Syn$TaxonConceptID,
								Names$TaxonConceptID)]
				if(author)
					Syn$formatted_name <- paste(Syn$formatted_name,
							Syn$AuthorName)
				Syn$formatted_name <- paste0(Syn$indent, syn_encl[1],
						Syn$formatted_name, syn_encl[2])
				Syn <- split(Syn$formatted_name, paste(Syn$TaxonConceptID))
				Syn <- lapply(Syn, function(x) paste0("\n", paste0(x,
											collapse = "\n")))
				Syn <- do.call(c, Syn)[match(Names$TaxonConceptID,
								as.integer(names(Syn)))]
				Syn[is.na(Syn)] <- ""
				Names$formatted_name <- paste0(Names$formatted_name, Syn)
			}
			# Sort list
			for(i in levels(object))
				if(i %in% colnames(Names)) Names <- Names[order(Names[ , i],
									na.last = FALSE), ]
			if(print)
				cat(paste0(Names$formatted_name, collapse = "\n"), "\n")
			invisible(Names)
		})
