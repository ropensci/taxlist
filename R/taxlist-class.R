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
                        ViewID=integer(),
						Secundum=character(),
						stringsAsFactors=FALSE
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
            if(any(duplicated(paste(object@taxonNames$TaxonName,
                                    object@taxonNames$AuthorName))))
                return("Some combinations of name and name's author are duplicated")
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
			if(!all(c("ViewID","Secundum") %in% colnames(object@taxonViews)))
	            return("'ViewID' and 'Secundum' are a mandatory columns in slot 'taxonViews'")
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
            if(any(!object@taxonTraits$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID))
                return("Some concepts in 'taxonTraits' are not included in slot 'taxonRelations'")
            if(any(duplicated(object@taxonTraits$TaxonConceptID)))
                return("Duplicated concepts are not allowed in slot 'taxonTraits'")
            # parent-child relationships
            if(!all(is.na(object@taxonRelations$Parent)) &
                    !all(is.na(object@taxonRelations$Level))) {
                Children <- object@taxonRelations[
                        !is.na(object@taxonRelations$Parent),
                        c("TaxonConceptID","Parent","Level")]
                Parents <- object@taxonRelations[match(Children$Parent,
                                object@taxonRelations$TaxonConceptID),
                        c("TaxonConceptID","Level")]
                if(any(as.numeric(Children$Level) >= as.numeric(Parents$Level)))
                    return("Some parent-child relationships are inconsistent with hierarchical levels")
                rm(Children,Parents)
            }
			# Accepted Names
			if(!all(is.na(object@taxonRelations$AcceptedName))) {
				if(any(!object@taxonRelations$AcceptedName %in%
								object@taxonNames$TaxonUsageID))
					return("Some 'AcceptedName' entries in slot 'taxonRelations' do not exist in slot 'taxonNames'")
				temp <- object@taxonNames[match(object@taxonRelations$AcceptedName,
								object@taxonNames$TaxonUsageID),
						c("TaxonConceptID","TaxonUsageID")]
				if(any(temp$TaxonConceptID !=
								object@taxonRelations$TaxonConceptID))
					return("Some 'AcceptedName' entries in slot 'taxonRelations' are not matching the respective 'TaxonConceptID' in slot 'taxonNames'")
			}
			# The same for basionyms
			if(!all(is.na(object@taxonRelations$Basionym))) {
				if(any(!object@taxonRelations$Basionym %in%
								object@taxonNames$TaxonUsageID))
					return("Some 'Basionym' entries in slot 'taxonRelations' do not exist in slot 'taxonNames'")
				temp <- object@taxonNames[match(object@taxonRelations$Basionym,
								object@taxonNames$TaxonUsageID),
						c("TaxonConceptID","TaxonUsageID")]
				if(any(temp$TaxonConceptID !=
								object@taxonRelations$TaxonConceptID))
					return("Some 'Basionym' entries in slot 'taxonRelations' are not matching the respective 'TaxonConceptID' in slot 'taxonNames'")
			}
		}
)
