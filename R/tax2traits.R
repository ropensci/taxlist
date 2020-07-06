#' @name tax2traits
#' 
#' @title Set taxonomic information as taxon traits
#' 
#' @description 
#' Taxonomic classification can be included in [taxlist-class]
#' objects within the information provided at slot `taxonRelations`.
#' Nevertheless, for statistical analyses it may be more convenient to insert
#' such information in the slot `taxonTraits`.
#' 
#' @param object An object of class [taxlist-class].
#' @param get_names Logical value indicating whether taxon names should be
#'     retrieved instead of taxon IDs.
#' @param ... Further arguments to be passed among methods.
#' 
#' @details 
#' This function can only be applied to objects containing parent-child
#' relationships and information on taxonomic levels.
#' 
#' @return An object of class [taxlist-class] with taxonomy added
#' as traits.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#' 
#' @examples 
#' ## Family Acanthaceae with children
#' Acanthaceae <- subset(x=Easplist, subset=TaxonName == "Acanthaceae",
#'     slot="names", keep_children=TRUE)
#' summary(Acanthaceae)
#' 
#' ## Insert taxonomy to taxon traits
#' Acanthaceae <- tax2traits(Acanthaceae, get_names=TRUE)
#' head(taxon_traits(Acanthaceae))
#' 
#' @rdname tax2traits
#' 
#' @exportMethod tax2traits
#' 
setGeneric("tax2traits",
		function(object, ...)
			standardGeneric("tax2traits")
)

#' @rdname tax2traits
#' 
#' @aliases tax2traits,taxlist-method
#' 
setMethod("tax2traits", signature(object="taxlist"),
		function(object, get_names=FALSE, ...) {
			# taxonomic table
			TAX <- data.frame(
					TaxonConceptID=object@taxonRelations$TaxonConceptID,
					stringsAsFactors=FALSE)
			# first entry with concepts at level
			if(all(is.na(object@taxonRelations$Level)))
				stop("Input object without taxonomic ranks.")
			if(all(is.na(object@taxonRelations$Parent)))
				stop("Input object without any parent-child relationships.")
			for(i in taxlist::levels(object)) {
				ID <- object@taxonRelations[
						paste(object@taxonRelations$Level) == i,
						"TaxonConceptID"]
				TAX[ ,i] <- ID[match(object@taxonRelations$TaxonConceptID, ID)]
			}
			# second entry parents
			for(i in taxlist::levels(object)[
					-length(taxlist::levels(object))]) {
				if(!all(is.na(TAX[ ,i]))) {
					TAX <- split(TAX, is.na(TAX[ ,i]))
					ID <- TAX[["FALSE"]][ ,i]
					PAR <- object@taxonRelations[match(ID,
									object@taxonRelations$TaxonConceptID),
							"Parent"]
					LEV <- paste(object@taxonRelations[match(PAR,
											object@taxonRelations$TaxonConceptID),
									"Level"])
					LEV[LEV == "NA"] <- NA
					for(j in unique(LEV[!is.na(LEV)])) {
						ID_2 <- ID[LEV == j]
						PAR_2 <- PAR[LEV == j]
						TAX[["FALSE"]][ ,j] <- PAR_2[match(TAX[["FALSE"]][ ,i],
										ID_2)]
					}
					TAX <- do.call(rbind, TAX)
				}
			}
			# supress empty columns
			TAX <- TAX[ ,apply(TAX, 2, function(x) !all(is.na(x)))]
			colnames(TAX)[colnames(TAX) == "TaxonConceptID"] <- "ConceptID"
			object <- do.call(update_trait, c(list(taxlist=object),
							as.list(TAX)))
			if(get_names) {
				Names <- accepted_name(object)
				for(i in taxlist::levels(object)[taxlist::levels(object) %in%
								colnames(TAX)]) {
					object@taxonTraits[ ,i] <- Names[
							match(object@taxonTraits[ ,i], Names$TaxonConceptID),
							"TaxonName"]
				}
			}
			return(object)
		}
)
