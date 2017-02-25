\name{Easplist-data}
\docType{data}
\alias{Easplist-data}
\alias{Easplist}

\title{Species List for East Africa.}
\description{
This list contains accepted names and synonyms stored in the database
\href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg}.
Such list is build while plots are imported in the database and may be therefore
incomplete.
For the nomenclature, EA-Splist uses as reference the
\href{http://www.ville-ge.ch/musinfo/bd/cjb/africa/recherche.php}{African Plant
Database}.
}
\usage{
data(Easplist)
}
\format{
An object of class \code{\link[=taxlist-class]{taxlist}}.
}
\source{
\url{http://www.ville-ge.ch/musinfo/bd/cjb/africa/recherche.php},
\url{http://www.givd.info/ID/AF-00-006}.
}
\examples{
library(taxlist)
data(Easplist)
summary(Easplist)
}
\keyword{datasets}
