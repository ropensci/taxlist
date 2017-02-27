\name{change_concept}
\docType{methods}
\alias{change_cocept}
\alias{change_concept<-}
\alias{change_concept<-,taxlist-method}

\title{Move a Name to a Different Concept.}
\description{
Move a misplaced name in its respective taxon concept.
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
With \code{change_concept} a name (usage) can be relocated in a different
taxon concept.
This function check previously that the name to be moved is neither an accepted
nor a first name for the current taxon, otherwise an error message will be
returned.

Teh term \sQuote{taxon concept} is used in this package instead of just taxon
and is defined as a taxon interpretation according to a determined
\sQuote{taxon view} (\bold{Jansen & Dengler 2010}) and it is equivalent to the
terms \sQuote{potential taxon} (\bold{Berendsohn 1995}) and \sQuote{taxonym}
(\bold{Koperski et al. 2000}).
This terminology is also adopted by \bold{Jansen & Dengler (2010)} and
\bold{Wiser et al. (2011)}.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with replaced names.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\references{
\bold{Berendsohn WG (1995).} The concept of \dQuote{potential taxa} in
databases. \emph{Taxon} 44: 207--212.

\bold{Koperski M, Sauer M, Braun W, Gradstein SR (2000).} \emph{Referenzliste
der Moose Deutschlands}. Bundesamt für Naturschutz, Bad Godesberg.

\bold{Jansen F, Dengler J (2010).} Plant names in vegetation databases -- a
neglected source of bias. \emph{Journal of Vegetation Science} 21: 1179--1186.

\bold{Wiser SK, Spencer N, de Cáceres M, Kleikamp M, Boyle B, Peet RK (2011).}
Veg-X -- an exchange standard for plot-based vegetation data.
\emph{Journal of Vegetation Science} 22: 598--609.
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
\keyword{methods}
