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

# Method for taxlist and vector ------------------------------------------------
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
            object@taxonRelations <- do.call(rbind, list(object@taxonRelations,
                            data.frame(TaxonConceptID=new_concept,
                                    AcceptedName=NA, View=NA)))
            object@taxonRelations <- object@taxonRelations[
                    object@taxonRelations$TaxonConceptID %in%
                            object@taxonNames$TaxonConceptID,]
            accepted_name(object, new_concept) <- AcceptedName
            # Clean traits
            taxon_traits(object) <- object@taxonTraits
            return(object)
        }
)

# Method for taxlist and list --------------------------------------------------
setMethod("merge_taxa", signature(object="taxlist", ConceptID="list"),
        function(object, ConceptID, AcceptedName, overwrite_old=FALSE) {
            # Save the old concept IDs
            object <- save_old_concept(object, overwrite_old)
            # Accepted name
            if(missing(AcceptedName)) {
                old_concepts <- sapply(ConceptID, "[", 1)
                AcceptedName <- object@taxonNames[
                        object@taxonNames$TaxonConceptID %in% old_concepts,
                        c("TaxonUsageID","TaxonConceptID")]
                AcceptedName <- AcceptedName[!duplicated(
                                AcceptedName$TaxonConceptID),]
                AcceptedName <- AcceptedName$TaxonUsageID[match(old_concepts,
                                AcceptedName$TaxonConceptID)]
            }
            # Create new concepts
            new_concept <- max(object@taxonNames$TaxonConceptID) +
                    1:length(ConceptID)
            for(i in 1:length(ConceptID)) {
                ConceptID[[i]] <- data.frame(concept=ConceptID[[i]],
                        new_concept=new_concept[i], stringsAsFactors=FALSE)
            }
            ConceptID <- do.call(rbind, ConceptID)
            object@taxonNames[object@taxonNames$TaxonConceptID %in%
                            ConceptID$concept,"TaxonConceptID"] <-
                    ConceptID$new_concept[match(object@taxonNames[
                                            object@taxonNames$TaxonConceptID %in%
                                                    ConceptID$concept,
                                            "TaxonConceptID"],
                                    ConceptID$concept)]
            # Concept in taxonRelations
            object@taxonRelations <- do.call(rbind, list(object@taxonRelations,
                            data.frame(TaxonConceptID=new_concept,
                                    AcceptedName=NA, View=NA,
                                    row.names=paste(new_concept),
                                    stringsAsFactors=FALSE)))
            object@taxonRelations <- object@taxonRelations[
                    object@taxonRelations$TaxonConceptID %in%
                            object@taxonNames$TaxonConceptID,]
            accepted_name(object, new_concept) <- AcceptedName
            # Clean taxon traits
            taxon_traits(object) <- object@taxonTraits
            return(object)
        }
)
