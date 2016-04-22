\name{taxlist-package}
\docType{package}

\alias{taxlist-package}
\title{Handling Taxonomic Lists.}
\description{
This package offers some functions to handle taxonomic lists, focusing on
species lists including synonyms.
For handling taxonomic lists an \emph{ad-hoc} object class is defined, namely
\code{\link[=taxlist-class]{taxlist}}.
Such lists separate names (taxon usages) from taxa (taxon concepts), following
the terminology of \bold{Jansen & Dengler (2010)}.
Beside linking taxon concepts with respective accepted names and synonyms,
taxlist also offers the possibility to assign a first name (first combination)
for the concept, as proposed by \bold{Alvarez & Luebert (in preparation)}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\references{
\bold{Jansen F, Dengler J (2010).} Plant names in vegetation databases -- a
neglected source of bias. \emph{Journal of Vegetation Science} 21: 1179--1186.

\bold{Alvarez M, Luebert F (in preparation).} Some considerations for reference
lists in vegetation-plot databases.
}
\examples{
\dontrun{
## How to install taxlist
library(devtools)
install_github("kamapu/taxlist")
}
}
