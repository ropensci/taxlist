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
            # If author names missing
            if(!"AuthorName" %in% colnames(x))
                x$AuthorName <- NA
            if(any(duplicated(x[,c("TaxonName","AuthorName")]))) {
                warning("Some duplicated combinations will be deleted")
                x <- x[!duplicated(x[,c("TaxonName","AuthorName")]),]
            }
            # Some tests previous to run the function
            AcceptedName <- substitute(AcceptedName)
            AcceptedName <- eval(AcceptedName, x, parent.frame())
            # When all accepted names
            if(length(AcceptedName) == 1) {
                if(!AcceptedName)
                    stop(paste("for 'AcceptedName' of length 1 only value",
									"'TRUE' is allowed"))
                AcceptedName <- rep(AcceptedName, nrow(x))
            }
            if(length(AcceptedName) != nrow(x))
                stop("Argument 'AcceptedName' not matching the size of 'x'")
            Heads <- c("TaxonUsageID","TaxonConceptID","TaxonName","AuthorName")
            if(!all(Heads %in% colnames(x)))
                stop(paste("'TaxonUsageID', 'TaxonConceptID', 'TaxonName',",
								"and 'AuthorName' are mandatory columns",
								"in 'x'"))
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
            extra_cols <- list(...)
            suppressWarnings({
                        if(length(extra_cols > 0))
                            for(i in names(extra_cols)) x[,i] <- extra_cols[[i]]
                    }
            )
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

# Method for missing accepted names in input data frame
setMethod("df2taxlist", signature(x="data.frame", AcceptedName="missing"),
        function(x, ...) return(df2taxlist(x, TRUE))
)

# Method for character vectors
setMethod("df2taxlist", signature(x="character", AcceptedName="missing"),
        function(x, ...) {
            if(any(duplicated(x))) {
                warning("Some duplicated names will be deleted")
                x <- x[!duplicated(x)]
            }
            x <- list(TaxonUsageID=seq_along(x), TaxonConceptID=seq_along(x),
                    TaxonName=x, ...)
            x <- as.data.frame(x, stringsAsFactors=FALSE)
            return(df2taxlist(x, TRUE))
        }
)
