# TODO:   Merge sub-specific taxa into species
# 
# Author: Miguel Alvarez
################################################################################

# Internal function to backup old concept IDs ----------------------------------
save_old_concept <- function(x, overwrite_old) {
    if(class(x) != "taxlist") stop("'x' must be of class taxlist")
    stamp <- ".TaxonConceptID_old"
    if(!overwrite_old & stamp %in% colnames(x@taxonNames)) {
        i <- 0
        repeat{
            i <- i + 1
            if(paste0(stamp, i) %in% colnames(x@taxonNames)) next
            if(!paste0(stamp, i) %in% colnames(x@taxonNames)) break
        }
        stamp <- paste0(stamp, i)
    }
    x@taxonNames[,stamp] <- x@taxonNames$TaxonConceptID
    return(x)
}

# Generic function -------------------------------------------------------------
setGeneric("merge_taxa",
        function(object, ConceptID, ...)
            standardGeneric("merge_taxa")
)

# Method for taxlist -----------------------------------------------------------
setMethod("merge_taxa", signature(object="taxlist", ConceptID="numeric"),
        function(object, ConceptID, AcceptedName, overwrite_old=FALSE) {
            # Save the old concept IDs
            object <- save_old_concept(object, overwrite_old)
            # Create new concept
            new_concept <- max(object@taxonNames$TaxonConceptID) + 1
            object@taxonNames[object@taxonNames$TaxonConceptID %in% ConceptID,
                    "TaxonConceptID"] <- new_concept
            # Accepted name
            if(missing(AcceptedName)) AcceptedName <- object@taxonNames[
                        object@taxonNames$TaxonConceptID == new_concept,
                        "TaxonUsageID"][1]
            if(length(AcceptedName) > 1) {
                AcceptedName <- AcceptedName[1]
                warning("Only the first element of 'AccetedName' will be used.")
            }
            # Concept in taxonRelations
            object@taxonRelations[paste(new_concept),] <- c(new_concept,
                    AcceptedName, NA)
            # Clean object
            object@taxonRelations <- object@taxonRelations[
                    object@taxonRelations$TaxonConceptID %in%
                            object@taxonNames$TaxonConceptID,]
            taxon_traits(object) <- object@taxonTraits
            return(object)
        }
)




## setMethod("merge_taxa", signature(taxlist="taxlist"),
##         function(taxlist, level="species", ...) {
##             level <- pmatch(tolower(level), c("genus","species"))[1]
##             if (is.na(level)) stop("invalid 'level' value")
##             # Create a vector with old concept IDs for security
##             if(!"TaxonConceptID_old" %in% colnames(taxlist@taxonNames)) {
##                 taxlist@taxonNames$TaxonConceptID_old <-
##                         taxlist@taxonNames$TaxonConceptID
##             } else warning("column 'TaxonConceptID_old' already exists in 'taxlist'")
##             # Detect taxa sharing one or two first name parts
##             Bin <- taxlist@taxonNames[
##                     paste(taxlist@taxonRelations$AcceptedName), "TaxonName",
##                     drop=TRUE]
##             Bin <- sapply(strsplit(Bin, " ", fixed=TRUE), "[", c(1:level))
##             if(level == 2) Bin <- apply(Bin, 2, paste, collapse=" ")
##             Bin <- data.frame(NAME=Bin, taxlist@taxonRelations,
##                     stringsAsFactors=FALSE)
##             Bin <- split(Bin, duplicated(Bin$NAME))
##             # Assign new concepts
##             Bin[["TRUE"]]$NewID <- with(Bin[["FALSE"]], {
##                         TaxonConceptID[match(Bin[["TRUE"]]$NAME, NAME)]
##                     }
##             )
##             INDEX <- taxlist@taxonNames$TaxonConceptID %in%
##                     Bin[["TRUE"]]$TaxonConceptID
##             taxlist@taxonNames[INDEX,"TaxonConceptID"] <- with(Bin[["TRUE"]], {
##                                 NewID[match(taxlist@taxonNames[INDEX,
##                                                         "TaxonConceptID"],
##                                                 TaxonConceptID)]
##                             }
##             )
##             # Re-arrange object
##             taxlist@taxonRelations <- taxlist@taxonRelations[
##                     taxlist@taxonRelations$TaxonConceptID %in%
##                             taxlist@taxonNames$TaxonConceptID,]
##             taxlist@taxonTraits <- taxlist@taxonTraits[
##                     taxlist@taxonTraits$TaxonConceptID %in%
##                             taxlist@taxonNames$TaxonConceptID,]
##             # Add new column in traits
##             if(level == 1) new_trait <- "genus_name" else
##                 new_trait <- "species_name"
##             taxlist@taxonTraits[,new_trait] <- with(Bin[["FALSE"]], {
##                         NAME[match(taxlist@taxonTraits$TaxonConceptID,
##                                         TaxonConceptID)]
##                     }
##             )
##             return(taxlist)
##         }
## )
