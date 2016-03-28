# TODO:   Retrieve, create and overwrite slot taxonRelations
# 
# Author: Miguel Alvarez
################################################################################

# taxonNames -------------------------------------------------------------------
setGeneric("taxonNames",
		function(taxlist, ...)
			standardGeneric("taxonNames")
)

# Set method for taxlist
setMethod("taxonNames", signature(taxlist="taxlist"),
		function(taxlist, ...) taxlist@taxonNames
)

# Replacement methods
setGeneric("taxonNames<-", function(taxlist, value)
			standardGeneric("taxonNames<-"))

# Replacement for taxlist
setReplaceMethod("taxonNames", signature(taxlist="taxlist",
				value="data.frame"), function(taxlist, value) {
			taxlist@taxonNames <- value
			return(taxlist)
		})

# taxonRelations ---------------------------------------------------------------
setGeneric("taxonRelations",
		function(taxlist, ...)
			standardGeneric("taxonRelations")
)

# Method for data frames
setMethod("taxonRelations", signature(taxlist="data.frame"),
		function(taxlist, ...) {
			if(!all(c("TaxonUsageID","TaxonConceptID") %in% colnames(taxlist)))
				stop("TaxonUsageID and TaxonConceptID are mandatory columns",
						call.=FALSE)
			if(any(duplicated(taxlist$TaxonUsageID)))
				stop("duplicates in TaxonUsageID are not allowed",
						call.=FALSE)
			Relations <- unique(taxlist$TaxonConceptID)
			Relations <- data.frame(TaxonConceptID=Relations,
					ValidName=Relations, FirstName=NA, row.names=Relations,
					stringsAsFactors=FALSE)
			return(Relations)
		}
)

# Set method for taxlist
setMethod("taxonRelations", signature(taxlist="taxlist"),
		function(taxlist, ...) taxlist@taxonRelations
)

# Replacement methods
setGeneric("taxonRelations<-", function(taxlist, value)
			standardGeneric("taxonRelations<-"))

# Replacement for taxlist
setReplaceMethod("taxonRelations", signature(taxlist="taxlist",
				value="data.frame"), function(taxlist, value) {
			taxlist@taxonRelations <- value
			return(taxlist)
		})

# taxonTraits ------------------------------------------------------------------
setGeneric("taxonTraits",
		function(taxlist, ...)
			standardGeneric("taxonTraits")
)

# Set method for taxlist
setMethod("taxonTraits", signature(taxlist="taxlist"),
		function(taxlist, ...) taxlist@taxonTraits
)

# Generic for replacement method
setGeneric("taxonTraits<-", function(taxlist, value)
            standardGeneric("taxonTraits<-"))

# Definition of method
setReplaceMethod("taxonTraits", signature(taxlist="taxlist",
                value="data.frame"),
        function(taxlist, value) {
            if(!"TaxonConceptID" %in% colnames(value))
                stop("'TaxonConceptID' is a mandatory field in 'value'",
                        call.=FALSE)
            if(class(value$TaxonConceptID) != "integer")
                value$TaxonConceptID <- as.integer(value$TaxonConceptID)
            if(any(duplicated(value$TaxonConceptID))) {
                warning("duplicated concepts will be deleted from 'value'",
                        call.=FALSE)
                value <- value[unique(value$TaxonConceptID),]
            }
            rownames(value) <- paste(value$TaxonConceptID)
            # Add the columns one by one
            Heads <- colnames(value)[colnames(value) != "TaxonConceptID"]
            for(i in Heads) {
                taxlist@taxonTraits[,i] <- value[match(
                                taxlist@taxonTraits$TaxonConceptID,
                                value$TaxonConceptID),i]
            }
            return(taxlist)
        })

# validName --------------------------------------------------------------------
setGeneric("validName",
		function(taxlist, ConceptID, ...)
			standardGeneric("validName")
)

# Set method for taxlist
setMethod("validName", signature(taxlist="taxlist"),
		function(taxlist, ConceptID, ...) {
			taxlist@taxonRelations[paste(ConceptID),"ValidName"]
		}
)

# Replacement methods
setGeneric("validName<-", function(taxlist, ConceptID, value)
			standardGeneric("validName<-"))

# Replacement for taxlist
setReplaceMethod("validName", signature(taxlist="taxlist"),
		function(taxlist, ConceptID, value) {
            if(class(ConceptID) != "integer") ConceptID <- as.integer(ConceptID)
            if(class(value) != "integer") value <- as.integer(value)
			# first test
			if(length(ConceptID) != length(value))
				stop("ConceptID and value should be of the same length",
						call.=FALSE)
			if(!all(taxlist@taxonNames[paste(value),
                            "TaxonConceptID"] == ConceptID))
				stop("new value(s) is(are) not included in the respective taxon concept(s)",
						call.=FALSE)
			# now replace
			taxlist@taxonRelations[paste(ConceptID),"ValidName"] <- value
			return(taxlist)
		})

# first_name -------------------------------------------------------------------
setGeneric("first_name",
		function(taxlist, ConceptID, ...)
			standardGeneric("first_name")
)

# Set method for taxlist
setMethod("first_name", signature(taxlist="taxlist"),
		function(taxlist, ConceptID, ...) {
            if(class(ConceptID) != "character") ConceptID <- paste(ConceptID)
			taxlist@taxonRelations[ConceptID,"FirstName"]
		}
)

# Replacement methods
setGeneric("first_name<-", function(taxlist, ConceptID, value)
			standardGeneric("first_name<-"))

# Replacement for taxlist
setReplaceMethod("first_name", signature(taxlist="taxlist"),
		function(taxlist, ConceptID, value) {
			# first test
			if(length(ConceptID) != length(value))
				stop("ConceptID and value should be of the same length",
						call.=FALSE)
            if(class(value) != "integer") value <- as.integer(value)
            if(!all(taxlist@taxonNames[paste(value),
                            "TaxonConceptID"] == ConceptID))
				stop("new value(s) is(are) not included in the respective taxon concept(s)",
						call.=FALSE)
            if(class(ConceptID) != "character") ConceptID <- paste(ConceptID)
            # now replace
			taxlist@taxonRelations[ConceptID,"FirstName"] <- value
			return(taxlist)
		})

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
				stop("'UsageID' and 'value' should be of the same length",
						call.=FALSE)
			if(any(UsageID %in% taxlist@taxonRelations$ValidName))
				stop("changes on concept are not allowed for valid names",
						call.=FALSE)
			if(any(UsageID %in% taxlist@taxonRelations$FirstName))
				stop("changes on concept are not allowed for first names",
						call.=FALSE)
			# now replace
			taxlist@taxonNames[paste(UsageID),"TaxonConceptID"] <- value
			return(taxlist)
		})
