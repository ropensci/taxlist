#' @name taxon_views
#'
#' @title Management of concept views in taxonomic lists.
#'
#' @description
#' Retrieve or replace slot `taxonViews` in an object of class [taxlist-class]
#'
#' @param taxlist A [taxlist-class] object.
#' @param taxonViews A data frame with taxon views to be inserted in
#'     `'taxlist'`.
#' @param value An object of class [data.frame] containing the references
#'     used to define the circumscription of taxon concepts included in
#'     `taxlist`.
#' @param ... Further arguments to be passed among methods.
#'
#' @details
#' Taxon views indicate in [taxlist-class] objects the references
#' determining the circumscription of the respective taxon concepts.
#' When adding a new concept (see [add_concept()]), the respective
#' reference may not yet occur in the input [taxlist-class] object.
#'
#' The term taxon view was introduced by **Zhong et al. (1996)** and
#' corresponds to the reference used for the definition of a concept.
#'
#' This function retrieves the slot `taxonViews` from objects of the class
#' [taxlist-class].
#'
#' The replacement method `taxon_views<-` replaces the whole content of slot
#' `taxonViews` and it is only recommended to use when constructing a new
#' [taxlist-class] object from an empty prototype.
#'
#' @return An object of class [taxlist-class] with added views.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @references
#' \bold{Zhong Y, Jung S, Pramanik S, Beaman JH (1996).} Data model and
#' comparison and query methods for interacting classifications in a taxonomic
#' database.
#' \emph{Taxon} 45: 223--241.
#' \doi{10.1093/bioinformatics/15.2.149}
#'
#' @seealso [taxlist-class]
#'
#' @example examples/taxon_views.R
#'
#' @rdname taxon_views
#'
#' @export
taxon_views <- function(taxlist, ...) UseMethod("taxon_views", taxlist)

#' @rdname taxon_views
#' @aliases taxon_views,taxlist-method
#' @method taxon_views taxlist
#' @export
taxon_views.taxlist <- function(taxlist, ...) taxlist@taxonViews

#' @rdname taxon_views
#' @aliases taxon_views<-
#' @export
`taxon_views<-` <- function(taxlist, ..., value) {
  UseMethod("taxon_views<-", taxlist)
}

#' @rdname taxon_views
#' @aliases taxon_views<-,taxlist-method
#' @method taxon_views<- taxlist
#' @export
`taxon_views<-.taxlist` <- function(taxlist, ..., value) {
  if (!"ViewID" %in% names(value)) {
    stop("'ViewID' is a mandatory column in 'value'")
  }
  taxlist@taxonViews <- value
  return(taxlist)
}

#' @rdname taxon_views
#' @aliases add_view
#' @exportMethod add_view
setGeneric(
  "add_view",
  function(taxlist, taxonViews, ...) {
    standardGeneric("add_view")
  }
)

#' @rdname taxon_views
#' @aliases add_view,taxlist,data.frame-method
setMethod(
  "add_view", signature(taxlist = "taxlist", taxonViews = "data.frame"),
  function(taxlist, taxonViews, ...) {
    if (!"ViewID" %in% names(taxonViews)) {
      if (is(taxlist@taxonViews$ViewID, "numeric")) {
        taxonViews$ViewID <- id_generator(len = nrow(taxonViews))
      } else {
        taxonViews$ViewID <- id_generator(
          len = nrow(taxonViews),
          mode = "character",
          nchar = max(nchar(taxlist@taxonViews$ViewID))
        )
      }
    }
    taxonViews$ViewID <- id_solver(
      insert = taxonViews$ViewID,
      to = taxlist@taxonViews$ViewID, sep = "_"
    )
    taxlist@taxonViews <- update_data(
      object = taxlist@taxonViews,
      revision = taxonViews, key = "ViewID", add = TRUE
    )
    return(taxlist)
  }
)
