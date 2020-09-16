#' @name taxlist-class
#' 
#' @title An S4 class to represent taxonomic lists.
#' 
#' @description
#' Class for taxonomic lists including synonyms, hierarchical ranks,
#' parent-child relationships, taxon views and taxon traits.
#' 
#' Note that each taxon becomes an identifier, represented by the column
#' **TaxonConceptID** in the slot **taxonRelations**, analogous to a primary key
#' in a relational database.
#' This identifier is restricted to an integer in `taxlist` and is specific for
#' the object.
#' 
#' In the same way, each taxon usage name has an identifier in the column
#' **TaxonUsageID**, slot **taxonNames**.
#' The column **ViewID** in slot **taxonViews** is the identifier of the taxon
#' view.
#' 
#' @slot taxonNames (`data.frame`) Table of taxon usage names (accepted names
#'     and synonyms).
#' @slot taxonRelations (`data.frame`) Relations between concepts, accepted
#'     names, basionyms, parents and hierarchical level.
#' @slot taxonTraits Table of taxon traits.
#' @slot taxonViews References used to determine the respective concept
#'     circumscription.
#' 
#' @author Miguel Alvarez
#' 
#' @references
#' \bold{Alvarez M, Luebert F (2018).} The taxlist package: managing plant
#'     taxonomic lists in R. \emph{Biodiversity Data Journal} 6: e23635.
#'     \url{https://doi.org/10.3897/bdj.6.e23635}
#' 
#' @examples 
#' library(taxlist)
#' 
#' showClass("taxlist")
#' 
#' ## Create an empty object
#' Splist <- new("taxlist")
#' 
#' @exportClass taxlist
#' 
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
                taxonViews=data.frame(
                        ViewID=integer(),
						## Secundum=character(),
						stringsAsFactors=FALSE
                ),
				taxonTraits=data.frame(
                        TaxonConceptID=integer()
                )
        ),
        # Validity procedures
		validity=function(object) {
            ## slot taxonNames
			if(!all(c("TaxonUsageID","TaxonConceptID","TaxonName",
							"AuthorName") %in% colnames(object@taxonNames)))
				return(paste("'TaxonUsageID', 'TaxonConceptID', 'TaxonName'",
								"and 'AuthorName' are mandatory columns in",
								"slot 'taxonNames'"))
            if(any(duplicated(object@taxonNames$TaxonUsageID)))
                return("Duplicated names are not allowed in slot 'taxonNames'")
            if(!all(object@taxonRelations$TaxonConceptID %in%
                            object@taxonNames$TaxonConceptID))
                return(paste("Some concepts from slot 'taxonRelations' are",
								"missing in slot 'taxonNames'"))
            if(any(duplicated(paste(object@taxonNames$TaxonName,
                                    object@taxonNames$AuthorName))))
                return(paste("Some combinations of name and name's author",
								"are duplicated"))
            ## slot taxonRelations
            if(!all(c("TaxonConceptID","AcceptedName","Basionym","Parent",
                            "Level") %in% colnames(object@taxonRelations)))
                return(paste("'TaxonConceptID', 'AcceptedName', 'Basionym',",
								"'Parent', 'Level' are mandatory columns in",
								"slot 'taxonRelations'"))
            if(any(duplicated(object@taxonRelations$TaxonConceptID)))
                return(paste("Duplicated concepts are not allowed in slot",
								"'taxonRelations'"))
            if(!all(object@taxonNames$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID))
                return(paste("Some concepts from slot 'taxonNames' are",
								"missing in slot 'taxonRelations'"))
			# taxonView
			## if(!all(c("ViewID","Secundum") %in% colnames(object@taxonViews)))
			##     return(paste("'ViewID' and 'Secundum' are a mandatory",
			##                     "columns in slot 'taxonViews'"))
			if(!"ViewID" %in% colnames(object@taxonViews))
				return("'ViewID' is a mandatory column in slot 'taxonViews'")
            if(any(duplicated(object@taxonViews$ViewID)))
                return("Duplicated views are not allowed in slot 'taxonViews'")
            if(nrow(object@taxonViews) > 0 &
                    !all(object@taxonRelations$ViewID[
                                    !is.na(object@taxonRelations$ViewID
                                    )] %in% object@taxonViews$ViewID))
                return("Some concept views are missing in slot 'taxonViews'")
            ## taxonTraits
            if(!"TaxonConceptID" %in% colnames(object@taxonTraits))
                return(paste("'TaxonConceptID' is a mandatory column in",
								"slot 'taxonTraits'"))
            if(any(!object@taxonTraits$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID))
                return(paste("Some concepts in 'taxonTraits' are not",
								"included in slot 'taxonRelations'"))
            if(any(duplicated(object@taxonTraits$TaxonConceptID)))
                return(paste("Duplicated concepts are not allowed in",
								"slot 'taxonTraits'"))
			# - duplicated variables in taxonRelations compared with taxonTraits
			if(any(colnames(object@taxonTraits)[colnames(object@taxonTraits) !=
									"TaxonConceptID"] %in%
					colnames(object@taxonRelations)))
				return(paste("Some variables at 'taxonTraits' are shared with",
								"slot 'taxonRelations'"))
            ## parent-child relationships
            if(!all(is.na(object@taxonRelations$Parent)) &
                    !all(is.na(object@taxonRelations$Level)) &
					is.factor(object@taxonRelations$Level)) {
                Children <- object@taxonRelations[
                        !is.na(object@taxonRelations$Parent),
                        c("TaxonConceptID","Parent","Level")]
                Parents <- object@taxonRelations[match(Children$Parent,
                                object@taxonRelations$TaxonConceptID),
                        c("TaxonConceptID","Level")]
                if(any(as.numeric(Children$Level) >= as.numeric(Parents$Level)))
                    return(paste("Some parent-child relationships are",
									"inconsistent with hierarchical levels"))
                rm(Children,Parents)
            }
			## Accepted Names
			if(!all(is.na(object@taxonRelations$AcceptedName))) {
				if(any(!object@taxonRelations$AcceptedName %in%
								object@taxonNames$TaxonUsageID))
					return(paste("Some 'AcceptedName' entries in slot",
									"'taxonRelations' do not exist in",
									"slot 'taxonNames'"))
				temp <- object@taxonNames[
						match(object@taxonRelations$AcceptedName,
								object@taxonNames$TaxonUsageID),
						c("TaxonConceptID","TaxonUsageID")]
				if(any(temp$TaxonConceptID !=
								object@taxonRelations$TaxonConceptID))
					return(paste("Some 'AcceptedName' entries in slot",
									"'taxonRelations' are not matching the",
									"respective 'TaxonConceptID' in slot",
									"'taxonNames'"))
			}
			## The same for basionyms
			if(!all(is.na(object@taxonRelations$Basionym))) {
				if(any(!object@taxonRelations$Basionym %in%
								object@taxonNames$TaxonUsageID))
					return(paste("Some 'Basionym' entries in slot",
									"'taxonRelations' do not exist in",
									"slot 'taxonNames'"))
				temp <- object@taxonNames[match(object@taxonRelations$Basionym,
								object@taxonNames$TaxonUsageID),
						c("TaxonConceptID","TaxonUsageID")]
				if(any(temp$TaxonConceptID !=
								object@taxonRelations$TaxonConceptID))
					return(paste("Some 'Basionym' entries in slot",
									"'taxonRelations' are not matching the",
									"respective 'TaxonConceptID' in",
									"slot 'taxonNames'"))
			}
		}
)
