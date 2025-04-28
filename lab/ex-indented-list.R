# Load package and taxonomic list
library(taxlist)

# Load data
data(Easplist)

object <- Easplist

arrange_taxa <- function(object) {
  # first entry with concepts at level
  if (all(is.na(object@taxonRelations$Level))) {
    stop("Input object without taxonomic ranks.")
  }
  if (all(is.na(object@taxonRelations$Parent))) {
    stop("Input object without any parent-child relationships.")
  }
  TAX <- object@taxonRelations[,"TaxonConceptID", drop = FALSE]
  for (i in levels(object@taxonRelations$Level)) {
    ID <- object@taxonRelations[
      paste(object@taxonRelations$Level) == i,
      "TaxonConceptID"
    ]
    TAX[,i] <- ID[match(object@taxonRelations$TaxonConceptID, ID)]
  }
  # second entry parents
  for (i in levels(object@taxonRelations$Level)[
    -length(levels(object@taxonRelations$Level))
  ]) {
    if (!all(is.na(TAX[, i]))) {
      TAX <- split(TAX, is.na(TAX[, i]))
      ID <- TAX[["FALSE"]][, i]
      PAR <- object@taxonRelations[
        match(
          ID,
          object@taxonRelations$TaxonConceptID
        ),
        "Parent"
      ]
      LEV <- paste(object@taxonRelations[
        match(
          PAR,
          object@taxonRelations$TaxonConceptID
        ),
        "Level"
      ])
      LEV[LEV == "NA"] <- NA
      for (j in unique(LEV[!is.na(LEV)])) {
        ID_2 <- ID[LEV == j]
        PAR_2 <- PAR[LEV == j]
        TAX[["FALSE"]][, j] <- PAR_2[match(
          TAX[["FALSE"]][, i],
          ID_2
        )]
      }
      TAX <- do.call(rbind, TAX)
    }
    }
    rownames(TAX) <- NULL
    return(TAX)
}





x <- arrange_taxa(Easplist)

