#' @name taxon_views
#' @aliases taxon_views,taxlist-method
#' @aliases taxon_views<-
#' @aliases taxon_views<-,taxlist,data.frame-method
#' @aliases add_view
#' @aliases add_view,taxlist-method
#' 
#' @title Management of concept views in taxonomic lists.
#' 
#' @description 
#' Retrieve or replace slot \code{taxonViews} in an object of class
#' \code{\linkS4class{taxlist}}
#' 
#' @param taxlist A \code{\linkS4class{taxlist}} object.
#' @param value An object of class \code{'data.frame'} containing the references
#'     used to define the circumscription of taxon concepts included in
#'     `taxlist`.
#' @param ... Further arguments to be passed among methods.
#' 
#' @details 
#' Taxon views indicate in \code{\linkS4class{taxlist}} objects the references
#' determining the circumscription of the respective taxon concepts.
#' When adding a new concept (see \code{\link{add_concept}}), the respective
#' reference may not yet occur in the input \code{\linkS4class{taxlist}} object.
#' 
#' The term taxon view was introduced by \bold{Zhong et al. (1996)} and
#' corresponds to the reference used for the definition of a concept.
#' 
#' This function retrieves the slot `taxonViews` from objects of the class
#' \code{\linkS4class{taxlist}}.
#' 
#' The replacement method `taxon_views<-` replaces the whole content of slot
#' `taxonViews` and it is only recommended to use when constructing a new
#' \code{\linkS4class{taxlist}} object from an empty prototype.
#' 
#' @return An object of class \code{\linkS4class{taxlist}} with added views.
#' 
#' @author Miguel Alvarez.
#' 
#' @references 
#' \bold{Zhong Y, Jung S, Pramanik S, Beaman JH (1996).} Data model and comparison
#' and query methods for interacting classifications in a taxonomic database.
#' \emph{Taxon} 45: 223--241. \url{https://doi.org/10.1093/bioinformatics/15.2.149}
#' 
#' @seealso \code{\linkS4class{taxlist}}.
#' 
#' @examples 
#' library(taxlist)
#' data(Easplist)
#' 
#' ## See existing views
#' taxon_views(Easplist)
#' 
#' ## Add a new view
#' Easplist <- add_view(Easplist, secundum="Beentje et al. (1952)",
#'     Title="Flora of Tropical East Africa",
#'     URL="http://www.kew.org/science/directory/projects/FloraTropEAfrica.html")
#' 
#' taxon_views(Easplist)
#' 
#' @rdname taxon_views
#' @export 
setGeneric("taxon_views",
        function(taxlist, ...)
            standardGeneric("taxon_views")
)

#' @rdname taxon_views
#' 
#' @export 
setMethod("taxon_views", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonViews
)

#' @rdname taxon_views
#' 
#' @export 
setGeneric("taxon_views<-", function(taxlist, value)
            standardGeneric("taxon_views<-"))

#' @rdname taxon_views
#' 
#' @export 
setReplaceMethod("taxon_views", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonViews <- value
            return(taxlist)
        }
)

#' @rdname taxon_views
#' 
#' @export 
setGeneric("add_view",
		function(taxlist, ...)
			standardGeneric("add_view")
)

#' @rdname taxon_views
#' 
#' @export 
setMethod("add_view", signature(taxlist="taxlist"),
		function(taxlist, ...) {
			if(nrow(taxlist@taxonViews) == 0) ViewID <- 1 else
				ViewID <- max(taxlist@taxonViews$ViewID) + 1
			new_view <- list(...)
			ViewID <- ViewID:(ViewID + length(new_view[[1]]) - 1)
			new_view <- list(ViewID=ViewID, ...)
			for(i in colnames(taxlist@taxonViews)[
					!colnames(taxlist@taxonViews) %in% names(new_view)]) {
				new_view[[i]] <- rep(NA, length(ViewID))
			}
			new_view <- as.data.frame(new_view, stringsAsFactors=FALSE)
			if(nrow(taxlist@taxonViews) > 0) {
				old_view <- taxlist@taxonViews
				for(i in colnames(new_view)[!colnames(new_view) %in%
								colnames(old_view)]) {
					old_view[,i] <- rep(NA, nrow(old_view))
				}
				new_view <- do.call(rbind, list(old_view, new_view))
			}
			taxlist@taxonViews <- new_view
			return(taxlist)
		}
)
