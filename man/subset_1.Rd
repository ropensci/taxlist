#' Open BibEntry in PDF viewer or web browser.
#'
#' Attempts to open a connection to an entry in a BibEntry object using fields
#' such as \sQuote{file}, \sQuote{DOI}, \sQuote{eprint} + \sQuote{eprinttype},
#' and \sQuote{URL}.
#' @param con BibEntry object to extract connections from.
#' @param entry numeric index or character key of entry in \code{bib} to open.
#' @param open.field character vector of fields to use in \code{bib} to open
#' the BibEntry.  Possible fields are any combination of \dQuote{file},
#' \dQuote{url}, \dQuote{eprint}, or \dQuote{doi}.  \dQuote{eprint} is
#' implemented for \code{eprinttype=} \dQuote{JSTOR}, \dQuote{PubMed},
#' or \dQuote{arXiv}.  When multiple fields are specified, they are tried in
#' the order they appear in the vector.
#' @param viewer character string giving the name of the program to be used
#' as hypertext browser.  It should be in the PATH, or a full path specified.
#' Alternatively, an R function to be called to invoke the browser.  Defaults
#' to \code{getOptions("pdfviewer")} if \code{open.field = "file"} and
#' \code{getOptions("browser")}, otherwise.
#' @param ... not used.
#' @keywords connection utilities
#' @seealso \code{\link{browseURL}}
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @method open BibEntry
#' @export
#' @importFrom utils browseURL
#' @examples
#' \dontrun{
#' testbib <- ReadBib(system.file("REFERENCES.bib", package="bibtex"))
#' open(testbib)
#' testbib$file <- file.path(R.home("doc/manual"), "R-intro.pdf")
#' open(testbib)
#' }
open.BibEntry <- function(con, entry = 1L,
                          open.field = c("file", "url", "eprint", "doi"),
                          viewer, ...){
  ref <- con[entry]
  stopifnot(length(ref) == 1L)
  if (missing(viewer))
    viewer