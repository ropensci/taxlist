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
                #Traits[is.na(Traits)] <- FALSE
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
            Names <- unique(x@taxonNames[Names,"TaxonConceptID"])
            Relations <- x@taxonRelations[Relations,"TaxonConceptID"]
            Traits <- x@taxonTraits[Traits,"TaxonConceptID",drop=TRUE]
            ConceptID <- intersect(intersect(Relations,Traits), Names)
            # re-assembling output object
            x@taxonNames <- x@taxonNames[x@taxonNames$TaxonConceptID %in%
                            ConceptID,]
            x@taxonRelations <- x@taxonRelations[match(ConceptID,
                            x@taxonRelations$TaxonConceptID),]
            x@taxonTraits <- x@taxonTraits[match(ConceptID,
                            x@taxonTraits$TaxonConceptID),,drop=FALSE]
            return(x)
		})
