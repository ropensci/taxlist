# TODO:   subset methods for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# subset method for taxlist ----------------------------------------------------
setMethod(f="subset", signature(x="taxlist"),
		function(x, Names, Relations, Traits, ...) {
            # Get matchings as logical vectors
			if(!missing(Traits)) {
				Traits <- substitute(Traits)
				Traits <- eval(Traits, x@taxonTraits, parent.frame())
			} else Traits <- rep(TRUE, nrow(x@taxonTraits))
			if(!missing(Relations)) {
				Relations <- substitute(Relations)
				Relations <- eval(Relations, x@taxonRelations, parent.frame())
			} else Relations <- rep(TRUE, nrow(x@taxonRelations))
			if(!missing(Names)) {
				Names <- substitute(Names)
				Names <- eval(Names, x@taxonNames, parent.frame())
			} else Names <- rep(TRUE, nrow(x@taxonNames))
			# Get a list of included Concepts
            Names <- unique(paste(x@taxonNames[Names,"TaxonConceptID"]))
            Relations <- rownames(x@taxonRelations[Relations,])
            Traits <- rownames(x@taxonTraits[Traits,])
            ConceptID <- intersect(intersect(Relations,Traits), Names)
            # re-assembling output object
            x@taxonNames <- subset(x@taxonNames,
                    TaxonConceptID %in% as.integer(ConceptID))
            x@taxonRelations <- x@taxonRelations[ConceptID,]
            x@taxonTraits <- x@taxonTraits[ConceptID,,drop=FALSE]
            return(x)
		})
