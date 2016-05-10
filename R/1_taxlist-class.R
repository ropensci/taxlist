# TODO:   Setting S4 class 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

setClass("taxlist",
		slots=c(taxonNames="data.frame", taxonRelations="data.frame",
				taxonTraits="data.frame", conceptViews="data.frame"),
		prototype=list(taxonNames=data.frame(TaxonUsageID=integer(),
						TaxonConceptID=integer(), TaxonName=character(),
						AuthorName=character()),
				taxonRelations=data.frame(TaxonConceptID=integer(),
						AcceptedName=integer(), FirstName=integer(),
                        View=integer()),
				taxonTraits=data.frame(TaxonConceptID=integer()),
                conceptViews=data.frame(View=integer())),
		validity=function(object) {
			if(!all(c("TaxonUsageID","TaxonConceptID","TaxonName",
							"AuthorName") %in% colnames(object@taxonNames)))
				return("'TaxonUsageID', 'TaxonConceptID', 'TaxonName' and 'AuthorName' are mandatory columns in slot 'taxonNames'")
			if(!all(c("TaxonConceptID","AcceptedName","FirstName","View") %in%
							colnames(object@taxonRelations)))
				return("'TaxonConceptID', 'AcceptedName', 'FirstName', and 'View' are mandatory columns in slot 'taxonRelations'")
			if(any(rownames(object@taxonNames) !=
                            paste(object@taxonNames$TaxonUsageID)))
                return("'TaxonUsageID' have to be set as row names for slot 'taxonNames'")
            if(any(rownames(object@taxonRelations) !=
                            paste(object@taxonRelations$TaxonConceptID)))
                return("'TaxonConceotID' have to be set as row names for slot 'taxonRelations'")
            if(any(rownames(object@taxonTraits) !=
                            paste(object@taxonTraits$TaxonConceptID)))
                return("'TaxonConceotID' have to be set as row names for slot 'taxonTraits'")
            if(any(rownames(object@conceptViews) !=
                            paste(object@conceptViews$View)))
                return("'View' have to be set as row names for slot 'conceptViews'")
            if(!all(object@taxonNames$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonRelations'")
            if(!all(object@taxonRelations$TaxonConceptID %in%
                            object@taxonNames$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonNames'")
            if(!all(object@taxonRelations$TaxonConceptID %in%
                            object@taxonTraits$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonTraits'")
            if(nrow(object@conceptViews) > 0 &
                    !all(object@taxonRelations$View %in%
                                    object@conceptViews$View))
                return("Some concept views are missing in slot 'conceptViews'")
            if(!all(object@taxonNames$TaxonConceptID[match(
                                    object@taxonRelations$AcceptedName,
                                    object@taxonNames$TaxonUsageID)] ==
                    object@taxonRelations$TaxonConceptID))
                return("Accepted names must be included in their respective concepts!")
		})
