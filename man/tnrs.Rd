\name{tnrs}
\docType{methods}
\alias{tnrs}
\alias{tnrs,character-method}
\alias{tnrs,taxlist-method}

\title{Phylotastic Taxonomic Name Resolution Service.}
\description{
Methods of \code{\link[taxize]{tnrs}} for \code{\linkS4class{taxlist}} objects.
}
\usage{
\S4method{tnrs}{character}(query, ...)

\S4method{tnrs}{taxlist}(query, min_score=0.8, source="iPlant_TNRS", ...)
}
\arguments{
\item{query}{Either a character vector or a taxlist object with names to
	search.}
\item{min_score}{Minimum value of score for considering accepted names as
	suggested by the output.}
\item{source}{Source database.}
\item{...}{Further arguments passed to \code{\link[taxize]{tnrs}}.}
}
\details{
This function checks for matching of taxon names in \code{\linkS4class{taxlist}}
objects with the Taxonomic Name Resolution Service (TNRS).
Misspelled names as well as author names will be replaced in the the new object
and new accepted names will be inserted.

A method for character vectors is defined for the original function.
}
\value{
A data frame or an object of class \code{\linkS4class{taxlist}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[taxize]{tnrs}}.
}
