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

If one of the new provided IDs for the view (argument \code{View}) is already
occurring in the object, an error message will be retrieved.
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

## Subset with Ebenaceae species
Ebenaceae <- subset(Easplist, Traits=FAMILY == "Ebenaceae")
summary(Ebenaceae)

## Adding new view
Ebenaceae <- add_view(Ebenaceae, View=2, Author="Beentje et al.", Year=1952,
	Title="Flora of Tropical East Africa",
	Published="http://www.kew.org/science/directory/projects/FloraTropEAfrica.html")

## A summary again  
summary(Ebenaceae)
taxon_views(Ebenaceae)
}
