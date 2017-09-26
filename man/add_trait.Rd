\name{add_trait}
\docType{methods}
\alias{add_trait}
\alias{add_trait,taxlist,vector,numeric-method}
\alias{add_trait,taxlist,data.frame,missing-method}

\title{Add trait variables to a taxonomic list.}
\description{
Traits that can be appended to an existing \code{\linkS4class{taxlist}} object.
}
\usage{
\S4method{add_trait}{taxlist,vector,numeric}(taxlist, Trait, ConceptID, ...)

\S4method{add_trait}{taxlist,data.frame,missing}(taxlist, Trait, ConceptID, ...)
}
\arguments{
\item{taxlist}{An object of class \code{\linkS4class{taxlist}}.}
\item{Trait}{A single vector or a data frame to be appended in slot
	\code{taxonTraits}.}
\item{ConceptID}{A numeric vector with the respective concept IDs.}
\item{...}{Further arguments to be passed among methods.}
}
\details{
Taxon traits are contained in a data frame at the slot \code{'taxonTraits'} in
\code{\linkS4class{taxlist}} objects.
To optimize space, this data frame contain only entries for those concepts with
information, while taxa with no information are skipped from this table.
Thus appending new variables may also have to include new rows in this slot,
which is automatically carried out by this function.
}
\value{
An object of class \code{\linkS4class{taxlist}} with added traits.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{taxon_traits}}.
}
\examples{
# TODO: Add example
}
