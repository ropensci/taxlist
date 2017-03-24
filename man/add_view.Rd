\name{add_view}
\docType{methods}
\alias{add_view}
\alias{add_view,taxlist-method}

\title{Add a new reference in a taxonomic list.}
\description{
Add a new reference for taxon concept views.
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
Taxon views indicate in \code{\linkS4class{taxlist}} objects the references
determining the circumscription of the respective taxon concepts.
When adding a new concept (see \code{\link{add_concept}}), the respective
reference may not yet occur in the input \code{\linkS4class{taxlist}} object.
}
\value{
An object of class \code{\linkS4class{taxlist}} with added views.
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
