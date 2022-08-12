# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

#' @examples
#' ## Read the table with names of Cyperus species
#' Cyperus <- read.csv(file = file.path(
#'   path.package("taxlist"), "cyperus",
#'   "names.csv"
#' ), stringsAsFactors = FALSE)
#' head(Cyperus)
#'
#' ## Convert to 'taxlist' object
#' Cyperus <- df2taxlist(Cyperus, AcceptedName = !Cyperus$SYNONYM)
#' summary(Cyperus)
#'
#' ## Create a 'taxlist' object from character vectors
#' Plants <- df2taxlist(c("Triticum aestivum", "Zea mays"), AuthorName = "L.")
#' summary(Plants, "all")
#' @rdname df2taxlist

