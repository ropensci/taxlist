# TODO:   Extract or replace parts of the object
# 
# Author: Miguel Alvarez
################################################################################

# Access by dollar
setMethod("$", signature(x="taxlist"), function(x, name) {
            if(paste(substitute(name)) %in% colnames(x@taxonRelations))
                x@taxonRelations[[name]] else x@taxonTraits[[name]]
        }
)

setReplaceMethod("$", signature(x="taxlist"), function(x, name, value) {
            if(paste(substitute(name)) %in% colnames(x@taxonRelations))
                x@taxonRelations[[name]] <- value else
                x@taxonTraits[[name]] <- value
            return(x) 
        }
)

# Access by square brackets
setMethod("[", signature(x="taxlist"),
        function(x, i, j, ..., slot="relations", drop=FALSE) {
            if(missing(i)) i <- TRUE
            if(missing(j)) j <- TRUE
            slot <- pmatch(slot, c("relations","traits"))[1]
            if(!slot %in% c(1:2))
                stop("Invalid value for 'slot'")
            # Resolving problems with NAs
            if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
            if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
            if(slot == 1)
                x@taxonRelations <- x@taxonRelations[i,j,drop] else {
                x@taxonTraits <- x@taxonTraits[i,j,drop]
                x@taxonRelations <- x@taxonRelations[
                        x@taxonRelations$TaxonConceptID %in%
                                x@taxonTraits$TaxonConceptID,]
            }
            return(clean(x))
        }
)

setReplaceMethod("[", signature(x="taxlist"),
        function(x, i, j, slot="relations", value) {
            if(missing(i)) i <- TRUE
            if(missing(j)) j <- TRUE
            slot <- pmatch(slot, c("relations","traits"))[1]
            if(!slot %in% c(1:2))
                stop("Invalid value for 'slot'")
            # Resolving problems with NAs
            if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
            if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
            if(slot == 1) x@taxonRelations[i,j] <- value else
                x@taxonTraits[i,j] <- value
            return(x)
        }
)
