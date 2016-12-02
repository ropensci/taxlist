# TODO:   Setting S4 class 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

setClass("taxlist",
		slots=c(taxonNames="data.frame", taxonRelations="ANY",
				taxonTraits="data.frame", taxonViews="data.frame"),
		prototype=list(taxonNames=data.frame(TaxonUsageID=integer(),
						TaxonConceptID=integer(), TaxonName=character(),
						AuthorName=character(), stringsAsFactors=FALSE),
				taxonRelations=data.frame(TaxonConceptID=integer(),
						AcceptedName=integer(), View=integer()),
				taxonTraits=data.frame(TaxonConceptID=integer()),
                taxonViews=data.frame(View=integer())),
		validity=function(object) {
			if(!all(c("TaxonUsageID","TaxonConceptID","TaxonName",
							"AuthorName") %in% colnames(object@taxonNames)))
				return("'TaxonUsageID', 'TaxonConceptID', 'TaxonName' and 'AuthorName' are mandatory columns in slot 'taxonNames'")
			# specific for taxonRelations as data.frame
            if(class(object@taxonRelations) == "data.frame") {
                if(!all(c("TaxonConceptID","AcceptedName","View") %in%
                                colnames(object@taxonRelations)))
                    return("'TaxonConceptID', 'AcceptedName', and 'View' are mandatory columns in slot 'taxonRelations'")
            }
            if(!all(object@taxonNames$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonRelations'")
            if(!all(object@taxonRelations$TaxonConceptID %in%
                            object@taxonNames$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonNames'")
            if(!all(object@taxonRelations$TaxonConceptID %in%
                            object@taxonTraits$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonTraits'")
            if(nrow(object@taxonViews) > 0 &
                    !all(object@taxonRelations$View[
                                            !is.na(object@taxonRelations$View
                    )] %in% object@taxonViews$View))
                return("Some concept views are missing in slot 'taxonViews'")
            if(!all(object@taxonNames$TaxonConceptID[match(
                                    object@taxonRelations$AcceptedName,
                                    object@taxonNames$TaxonUsageID)] ==
                    object@taxonRelations$TaxonConceptID))
                return("Accepted names must be included in their respective concepts!")
		})
