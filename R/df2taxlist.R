# TODO:   Convert a data fram3e into a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("df2taxlist",
        function(x, AcceptedName, ...)
            standardGeneric("df2taxlist")
)

# Set method for data frame
setMethod("df2taxlist", signature(x="data.frame", AcceptedName="logical"),
        function(x, AcceptedName, ...) {
            # Some tests previous to run the function
            AcceptedName <- substitute(AcceptedName)
            AcceptedName <- eval(AcceptedName, x, parent.frame())
            if(length(AcceptedName) != nrow(x))
                stop("Argument 'AcceptedName' not matching the size of 'x'")
            Heads <- c("TaxonUsageID","TaxonConceptID","TaxonName")
            if(!all(Heads %in% colnames(x)))
                stop("'TaxonUsageID', 'TaxonConceptID', and 'TaxonName' are mandatory columns in 'x'")
            if(any(duplicated(x$TaxonUsageID)))
                stop("Duplicated usage IDs are not allowed")
            # set classes
            if(!is.integer(x$TaxonUsageID))
                x$TaxonUsageID <- as.integer(x$TaxonUsageID)
            if(!is.integer(x$TaxonConceptID))
                x$TaxonConceptID <- as.integer(x$TaxonConceptID)
            # taxonRelations
            taxonRelations <- x[AcceptedName,c("TaxonConceptID","TaxonUsageID")]
            colnames(taxonRelations)[2] <- "AcceptedName"
            # taxonNames
            taxlist <- new("taxlist")
            for(i in colnames(taxlist@taxonNames))
                if(!i %in% colnames(x)) x[,i] <- NA
            for(i in colnames(taxlist@taxonRelations))
                if(!i %in% colnames(taxonRelations)) taxonRelations[,i] <- NA
            taxlist@taxonNames <- x
            taxlist@taxonRelations <- taxonRelations
            return(taxlist)
        }
)
