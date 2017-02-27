\name{add_view}
\docType{methods}
\alias{add_view}
\alias{add_view,taxlist-method}

\title{Insert a New Concept View in a Taxonomic List.}
\description{
Add a new reference for taxonomic views.
}
\usage{
\S4method{add_view}{taxlist}(taxlist, ...)
}
\arguments{
\item{taxlist}{An object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{...}{Further character vectors to be inserted in the existing columns of
	the slot \code{taxonViews}.}
}
\details{
When planning to add new concepts according to a new reference, not yet included
in the \code{taxlist} object, the new view have to be previously inserted using
this function.

View ID will be automatically generated as serial number.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with added views.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{taxon_views}}.
}
\examples{
library(taxlist)
data(Easplist)

## See existing views
taxon_views(Easplist)

## Add a new view
Easplist <- add_view(Easplist, Author="Beentje et al.", Year=1952,
	Title="Flora of Tropical East Africa",
	Published="http://www.kew.org/science/directory/projects/FloraTropEAfrica.html")

taxon_views(Easplist)
}
