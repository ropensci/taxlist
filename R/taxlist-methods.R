# TODO:   Retrieve, create and overwrite slot taxonRelations
# 
# Author: Miguel Alvarez
################################################################################

# taxon_names ------------------------------------------------------------------
setGeneric("taxon_names",
		function(taxlist, ...)
			standardGeneric("taxon_names")
)

# Set method for taxlist
setMethod("taxon_names", signature(taxlist="taxlist"),
		function(taxlist, ...) taxlist@taxonNames
)

# Replacement methods
setGeneric("taxon_names<-", function(taxlist, value)
			standardGeneric("taxon_names<-"))

# Replacement for taxlist
setReplaceMethod("taxon_names", signature(taxlist="taxlist",
				value="data.frame"), function(taxlist, value) {
			taxlist@taxonNames <- value
			return(taxlist)
		}
)

# taxon_relations --------------------------------------------------------------
setGeneric("taxon_relations",
		function(taxlist, ...)
			standardGeneric("taxon_relations")
)

# Set method for taxlist
setMethod("taxon_relations", signature(taxlist="taxlist"),
		function(taxlist, ...) taxlist@taxonRelations
)

# Replacement methods
setGeneric("taxon_relations<-", function(taxlist, value)
			standardGeneric("taxon_relations<-"))

# Replacement for taxlist
setReplaceMethod("taxon_relations", signature(taxlist="taxlist",
				value="data.frame"), function(taxlist, value) {
			taxlist@taxonRelations <- value
			return(taxlist)
		}
)

# taxon_traits -----------------------------------------------------------------

# 1: Access methods (as in data frames)
setMethod("$", signature(x="taxlist"), function(x, name) {
            if(ncol(x@taxonTraits) == 0)
                stop("no $ method for 'taxlist' object without traits")
            x@taxonTraits[[name]]
        }
)

setReplaceMethod("$", signature(x="taxlist"), function(x, name, value) {
            if(ncol(x@taxonTraits) == 0)
                stop("no $<- method for 'taxlist' object without traits")
            x@taxonTraits[[name]] <- value 
            return(x) 
        }
)

setMethod("[", signature(x="taxlist"), function(x, i, j, ..., drop=FALSE) {
            if(missing(i)) i <- TRUE
            if(missing(j)) j <- TRUE
            # Resolving problems with NAs
            if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
            if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
            x@taxonTraits <- x@taxonTraits[i,j,drop]
            x@taxonRelations <- x@taxonRelations[rownames(x@taxonTraits),]
            x@taxonNames <- x@taxonNames[x@taxonNames$TaxonConceptID %in%
                            rownames(x@taxonRelations),]
            return(x)
        }
)

setReplaceMethod("[", signature(x="taxlist"), function(x, i, j, value) {
            if(missing(i)) i <- TRUE
            if(missing(j)) j <- TRUE
            # Resolving problems with NAs
            if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
            if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
            x@taxonTraits[i,j] <- value
            return(x)
        }
)

# 2: Methods dealing with the whole slot
setGeneric("taxon_traits",
		function(taxlist, ...)
			standardGeneric("taxon_traits")
)

# Set method for taxlist
setMethod("taxon_traits", signature(taxlist="taxlist"),
		function(taxlist, ...) taxlist@taxonTraits
)

# Generic for replacement method
setGeneric("taxon_traits<-", function(taxlist, value)
            standardGeneric("taxon_traits<-"))

# Definition of method
setReplaceMethod("taxon_traits", signature(taxlist="taxlist",
                value="data.frame"),
        function(taxlist, value) {
            if(!"TaxonConceptID" %in% colnames(value))
                stop("'TaxonConceptID' is a mandatory field in 'value'")
            if(class(value$TaxonConceptID) != "integer")
                value$TaxonConceptID <- as.integer(value$TaxonConceptID)
            if(any(duplicated(value$TaxonConceptID))) {
                warning("duplicated concepts will be deleted from 'value'")
                value <- value[unique(value$TaxonConceptID),]
            }
            taxlist@taxonTraits <- data.frame(
                    TaxonConceptID=taxlist@taxonRelations$TaxonConceptID)
            Heads <- colnames(value)[colnames(value) != "TaxonConceptID"]
            for(i in Heads) {
                taxlist@taxonTraits[,i] <- value[match(
                                taxlist@taxonTraits$TaxonConceptID,
                                value$TaxonConceptID),i]
            }
            return(taxlist)
        }
)

# accepted_name --------------------------------------------------------------------
setGeneric("accepted_name",
		function(taxlist, ConceptID, ...)
			standardGeneric("accepted_name")
)

# Set method for taxlist
setMethod("accepted_name", signature(taxlist="taxlist"),
		function(taxlist, ConceptID, ...) {
            AcceptedName <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID ==
                            ConceptID,"AcceptedName"]
			return(taxlist@taxonNames[taxlist@taxonNames$TaxonUsageID ==
                                    AcceptedName,])
		}
)

# Replacement methods
setGeneric("accepted_name<-", function(taxlist, ConceptID, value)
			standardGeneric("accepted_name<-"))

# Replacement for taxlist
setReplaceMethod("accepted_name", signature(taxlist="taxlist"),
		function(taxlist, ConceptID, value) {
            if(class(ConceptID) != "integer") ConceptID <- as.integer(ConceptID)
            if(class(value) != "integer") value <- as.integer(value)
			# first test
			if(length(ConceptID) != length(value))
				stop("ConceptID and value should be of the same length")
            if(!all(taxlist@taxonNames[match(value,
                                    taxlist@taxonNames$TaxonUsageID),
                            "TaxonConceptID"] == ConceptID))
				stop("new value is not included in the respective taxon concept")
			# now replace
            taxlist@taxonRelations[match(ConceptID,
                            taxlist@taxonRelations$TaxonConceptID),
                    "AcceptedName"] <- value
			return(taxlist)
		}
)


# change_concept ---------------------------------------------------------------

# Replacement methods
setGeneric("change_concept<-", function(taxlist, UsageID, value)
			standardGeneric("change_concept<-"))

# Replacement for taxlist
setReplaceMethod("change_concept", signature(taxlist="taxlist"),
		function(taxlist, UsageID, value) {
            if(class(UsageID) != "integer") UsageID <- as.integer(UsageID)
            if(class(value) != "integer") value <- as.integer(value)
			# Test
			if(length(UsageID) != length(value))
				stop("'UsageID' and 'value' should be of the same length")
			if(any(UsageID %in% taxlist@taxonRelations$AcceptedName))
				stop("changes on concept are not allowed for accepted names")
			# now replace
			taxlist@taxonNames[match(UsageID,taxlist@taxonNames$TaxonUsageID),
                    "TaxonConceptID"] <- value
			return(taxlist)
		}
)

# taxon_views ----------------------------------------------------------------
setGeneric("taxon_views",
        function(taxlist, ...)
            standardGeneric("taxon_views")
)

# Set method for taxlist
setMethod("taxon_views", signature(taxlist="taxlist"),
        function(taxlist, ...) return(taxlist@taxonViews)
)

# Replacement methods
setGeneric("taxon_views<-", function(taxlist, value)
            standardGeneric("taxon_views<-"))

# Replacement methods for the assignment to slot taxonRelations
setReplaceMethod("taxon_views", signature(taxlist="taxlist", value="numeric"),
        function(taxlist, value) {
            value <- rep_len(value, nrow(taxlist@taxonRelations))
            taxlist@taxonRelations$View <- value
            if(nrow(taxlist@taxonViews) == 0) {
                taxlist@taxonViews <- data.frame(View=unique(value),
                        row.names=paste(unique(value)))
            }
            return(taxlist)
        }
)

# Replacement methods for the assignment to slot taxonViews
setReplaceMethod("taxon_views", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonViews <- value
            return(taxlist)
        }
)
