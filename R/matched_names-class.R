#' @name matched_names-class
#'
#' @title Names matched with a reference taxonomic list
#'
#' @description
#' An S3 class containing results of names compared with a reference list.
#' This class enables further methods applied to these outputs, for instance
#' an interactive selection of multiple choices.
#'
#' @exportClass matched_names
setOldClass(c("matched_names", "data.frame"))
