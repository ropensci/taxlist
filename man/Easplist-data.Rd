\name{Easplist-data}
\docType{data}
\alias{Easplist-data}
\alias{Easplist}

\title{List of Vascular Plants from East Africa}
\description{
Example of an incomplete taxonomic list including taxa recorded in East Africa.
}
\usage{
data(Easplist)
}
\format{
An object of class \code{\linkS4class{taxlist}}.
}
\details{
This list is a subset of the taxonomic list implemented in the database
\href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg}.
Since this list is being complemented regarding stored vegetation plots, it is
an incomplete list.
}
\source{
\href{http://www.ville-ge.ch/musinfo/bd/cjb/africa/recherche.php}{African Plant Database},
\href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg}.
}
\examples{
library(taxlist)
data(Easplist)
summary(Easplist)
}
\keyword{data sets}
