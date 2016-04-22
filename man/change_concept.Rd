\name{change_concept}
\alias{change_concept<-}
\alias{change_concept<-,taxlist-method}

\title{Move a Name to a Different Concept.}
\description{
Move a misplaced name in a proper taxon concept.
}
\usage{
change_concept(taxlist, UsageID) <- value
}
\arguments{
\item{taxlist}{An object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{UsageID}{Corresponding usage ID to be changed from concept.}
\item{value}{Concept ID, where the usage should be moved.}
}
\details{
With \code{'change_concept<-'} a name (usage) can be relocated in a different
taxon concept.
This function check previously that the name to be moved is neither a accepted
name nor a first name for the current taxon, otherwise an error message will
be returned.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with replaced names.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

## Display two Typha species
summary(Easplist, c(50105,573))

## Move the name Typha aethiopica to concept 573 (T. latifolia)
change_concept(Easplist, 53130) <- 573
summary(Easplist, c(50105,573))
\donttest{
## Attempting to move an accepted name
change_concept(Easplist, 50105) <- 573
}
}
