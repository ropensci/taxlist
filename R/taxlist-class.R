# TODO:   Setting S4 class 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

setClass("taxlist",
        # Definition of slots
		slots=c(
                taxonNames="data.frame",
                taxonRelations="data.frame",
                taxonViews="data.frame",
				taxonTraits="data.frame"
        ),
        # Prototype
		prototype=list(
                taxonNames=data.frame(
                        TaxonUsageID=integer(),
						TaxonConceptID=integer(),
                        TaxonName=character(),
						AuthorName=character(),
                        stringsAsFactors=FALSE
                ),
                taxonRelations=data.frame(
                        TaxonConceptID=integer(),
						AcceptedName=integer(),
                        Basionym=integer(),
                        Parent=integer(),
                        Level=factor(),
                        ViewID=integer()
                ),
                hierarchy=character(),
                taxonViews=data.frame(
                        ViewID=integer()
                ),
				taxonTraits=data.frame(
                        TaxonConceptID=integer()
                )
        ),
        # Validity procedures
		validity=function(object) {
            # slot taxonNames
			if(!all(c("TaxonUsageID","TaxonConceptID","TaxonName",
							"AuthorName") %in% colnames(object@taxonNames)))
				return("'TaxonUsageID', 'TaxonConceptID', 'TaxonName' and 'AuthorName' are mandatory columns in slot 'taxonNames'")
            if(any(duplicated(object@taxonNames$TaxonUsageID)))
                return("Duplicated names are not allowed in slot 'taxonNames'")
            if(!all(object@taxonRelations$TaxonConceptID %in%
                            object@taxonNames$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonNames'")
            # slot taxonRelations
            if(!all(c("TaxonConceptID","AcceptedName","Basionym","Parent",
                            "Level") %in% colnames(object@taxonRelations)))
                return("'TaxonConceptID', 'AcceptedName', 'Basionym', 'Parent', 'Level' are mandatory columns in slot 'taxonRelations'")
            if(any(duplicated(object@taxonRelations$TaxonConceptID)))
                return("Duplicated concepts are not allowed in slot 'taxonRelations'")
            if(!all(object@taxonNames$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID))
                return("Some concepts are missing in slot 'taxonRelations'")
            # taxonView
            if(!"ViewID" %in% colnames(object@taxonViews))
                return("'ViewID' is a mandatory column in slot 'taxonViews'")
            if(any(duplicated(object@taxonViews$ViewID)))
                return("Duplicated views are not allowed in slot 'taxonViews'")
            if(nrow(object@taxonViews) > 0 &
                    !all(object@taxonRelations$ViewID[
                                    !is.na(object@taxonRelations$ViewID
                                    )] %in% object@taxonViews$ViewID))
                return("Some concept views are missing in slot 'taxonViews'")
            # taxonTraits
            if(!"TaxonConceptID" %in% colnames(object@taxonTraits))
                return("'TaxonConceptID' is a mandatory column in slot 'taxonTraits'")
            if(any(duplicated(object@taxonTraits$TaxonConceptID)))
                return("Duplicated concepts are not allowed in slot 'taxonTraits'")
            ## if(!all(object@taxonNames$TaxonConceptID[match(
            ##                         object@taxonRelations$AcceptedName,
            ##                         object@taxonNames$TaxonUsageID)] ==
            ##         object@taxonRelations$TaxonConceptID))
            ##     return("Accepted names must be included in their respective concepts!")
		}
)
