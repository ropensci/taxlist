\name{merge_taxa}
\docType{methods}
\alias{merge_taxa}
\alias{merge_taxa,taxlist,numeric,missing-method}
\alias{merge_taxa,taxlist,missing,character-method}
\alias{change_cocept}
\alias{change_concept<-}
\alias{change_concept<-,taxlist-method}

\title{Merge Concepts}
\description{
Merge taxon concepts form a \code{\linkS4class{taxlist}} object into single
ones.
}
\usage{
\S4method{merge_taxa}{taxlist,numeric,missing}(object, concepts, level,
	print_output=FALSE, ...)

\S4method{merge_taxa}{taxlist,missing,character}(object, concepts, level, ...)

change_concept(taxlist, UsageID) <- value
}
\arguments{
\item{object,taxlist}{Object of class \code{\linkS4class{taxlist}}.}
\item{concepts}{Numeric (integer) vector including taxon concepts to be merged.}
\item{level}{Character vector indicating the lowest level for merging.}
\item{print_output}{Logical value indicating whether the merged concept should
	be displayed in the console.}
\item{UsageID}{Numeric vector with taxon usage IDs to be changed from concept.}
\item{value}{Numeric vector with taxon concept IDs to be assigned to the names.}
\item{...}{Further arguments to be passed to or from other methods.}
}
\details{
Taxon concept