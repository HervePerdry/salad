#' @export
varnames <- function(x) UseMethod("varnames")

#' @exportS3Method varnames dual
varnames.dual <- function(x) names(unclass(x@d))

#' @exportS3Method varnames numeric
varnames.numeric <- function(x) character(0)
