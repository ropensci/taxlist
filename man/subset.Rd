\name{subset}
\docType{methods}
\alias{subset}
\alias{subset,taxlist-method}

\title{Subset method for \dQuote{taxlist} Objects.}
\description{
A subset of \code{\link[=taxlist-class]{taxlist}} objects will be done either
by \code{taxonNames}, \code{taxonRelations} or \code{taxonTraits} slots.
}
\usage{
\S4method{subset}{taxlist}(x, Names, Relations, Traits, ...)
}
\arguments{
\item{x}{Object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{Names}{Logical vector indicating selected elements in slot
	\code{taxonNames}.}
\item{Relations}{Logical vector indicating selected elements in slot
	\code{taxonRealtions}.}
\item{Traits}{Logical vector indicating selected elements in slot
	\code{taxonTraits}.}
\item{...}{further arguments to be passed to or from other methods.}
}
\details{
This method is applied to objects of class \code{\link[=taxlist-class]{taxlist}}
in order to generate subsets of such objects.
The criteria for subsets can be applied to any of the slots included in those
objects (they can be applied simultaneously to all three slots).

In the case of subset applied to the slot \code{taxonNames}, non-matching names
belonging to the same concepts as the matching ones will be preserved in the
subset.
}
\value{
A S4 object of class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

# subset by Names (all names associated to Pteris)
Splist <- subset(Easplist, Names=grepl("Pteris", TaxonName))
summary(Splist, "all")

# subset by Relations (Pteris muricella)
Splist <- subset(Easplist, Relations=TaxonConceptID == 50579)
summary(Splist, "all")

# subset by Traits (whole family Pteridaceae)
Splist <- subset(Easplist, Traits=FAMILY == "Pteridaceae")
summary(Splist, "all")
}
