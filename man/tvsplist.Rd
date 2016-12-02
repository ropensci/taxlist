% multi-purpose template
\name{tvsplist}
\alias{tvsplist}

\title{Import Species Lists from Turboveg Databases.}
\description{
Importing species lists from Turboveg databases into an object of class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
tvsplist(taxlist, tv_home=tv.home())
}
\arguments{
\item{taxlist}{The name of a species list in Turboveg as character value.}
\item{tv_home}{Character value indicating the path to the main Turboveg	folder.}
}
\details{
This function imports species lists using the function \code{\link{read.dbf}}
from the package \code{foreign}.
If present, also the taxon traits will be imported into the output object
(usually the file \code{ecodbase.dbf} in the respective folder).
During import of \code{ecodbase.dbf} duplicated entries for a same concept will
be discarded as well as entries for non-existing concepts.

By default \code{tv_home} will be set by the function \code{\link{tv.home}} from
the package \code{\link[=vegdata-package]{vegata}}.
Part of the code is extracted from the function \code{\link{tax}}.

Be default, the name of the database sill be set as concept view for all
concepts included in the species list.
If this is not correct, consider setting it manually by using the functions
\code{\link{taxon_views}} and \code{\link{add_view}}.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\seealso{
\code{\link[=taxlist-class]{taxlist}}.
}
\examples{
\donttest{
library(taxlist)

## Loading GermanSL 1.3 (http://www.botanik.uni-greifswald.de/2305.html)
GermanSL <- tvsplist("GermanSL_1.3", file.path(path.package("taxlist"),
	"tv_data"))
summary(GermanSL)
}
}
