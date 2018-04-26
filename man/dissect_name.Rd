\name{dissect_name}
\alias{dissect_name}

\title{Split names into its parts.}
\description{
Depending the degree of resolution and specific roles of nomenclature, strings
containing taxon names (scientific names) are constructed with different parts.
A string with names can be consequently split into those components.
}
\usage{
dissect_name(x, split=" ", fixed=TRUE, ...)
}
\arguments{
\item{x}{A character vector containing taxon names.}
\item{split, fixed, ...}{Arguments passed to \code{\link[base]{strsplit}}.}
}
\details{
This function is using \code{\link[base]{strsplit}} for splitting names.
}
\value{
A character matrix with as many rows as names in the input vector.
}
\seealso{
\code{\link[base]{strsplit}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)
data(Easplist)

Easplist <- subset(Easplist, Level == "variety", slot="relations")
Easplist <- accepted_name(Easplist)[c(1:10),"TaxonName"]

dissect_name(Easplist)
}
\keyword{methods}
