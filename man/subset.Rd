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
	\item{x}{object of class \code{\link[=taxlist-class]{taxlist}}.}
	\item{Names}{logical operation to be applied at the \code{taxonNames} slot.}
	\item{Relations}{logical operation to be applied at the
		\code{taxonRealtions} slot.}
	\item{Traits}{logical operation to be applied at the \code{taxonTraits}
		slot.}
	\item{...}{further arguments to be passed to or from other methods.}
}
\details{
Explain more!
}
\value{A S4 object of class \code{\link[=taxlist-class]{taxlist}}.}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
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
