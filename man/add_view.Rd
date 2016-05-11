\name{add_view}
\alias{add_view}

\title{Insert a New Concept View in a Taxonomic List.}
\description{
Add a new reference for taxonomic views.
}
\usage{
add_view(taxlist, View, ...)
}
\arguments{
\item{taxlist}{A object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{View}{An integer value or vector providing the identity of new views.}
\item{...}{Further character vectors to be inserted in the existing columns of
	the slot \code{taxonViews}.}
}
\details{
This function should be applied previous to make changes in concepts according
to new references.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with added views.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link{taxon_views}}.
}
