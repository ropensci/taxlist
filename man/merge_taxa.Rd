\name{merge_taxa}
\docType{methods}
\alias{merge_taxa}
\alias{merge_taxa,taxlist-method}

\title{Merge Concepts to Species or Corresponding Genera.}
\description{
Merge concepts to species or genus level.
}
\usage{
\S4method{merge_taxa}{taxlist}(taxlist, level="species", ...)
}
\arguments{
\item{taxlist}{Object of class \code{taxlist}.}
\item{level}{Taxonomic level used for merging.}
\item{...}{Further arguments passed to or from other methods.}
}
\details{
This function aggregate all sub-specific syntaxa in the respective species
(\code{level="species"}) or all taxa to their respective genus
(\code{level="genus"}).
Argument \code{level} allows partial matchings and is case insensitive.

This function inserts a new column in the slot \code{taxonNames} called
\code{'TaxonConceptID_old'} with original concept IDs.
If this column is already existing, the step will be skipped with a warning
message.

An additional column will be added to the slot \code{taxonTraits}, which will
indicate the name of the species or genus, depending on \code{level} value.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with merged concepts.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

## Subset with Cyperus species
Cyperus <- subset(Easplist, Traits=GENUS == "Cyperus")
summary(Cyperus)

## Merge to species level
Cyperus <- merge_taxa(Cyperus)
summary(Cyperus)

## Merge Cyperaceae to genus level
Cyperus <- subset(Easplist, Traits=FAMILY == "Cyperaceae")
summary(Cyperus)

## Merge to species level
Cyperus <- merge_taxa(Cyperus, level="genus")
summary(Cyperus)

## Display 3 first genera
Cyperus$genus_name[1:3]
summary(Cyperus[1:3,], "all")
}
