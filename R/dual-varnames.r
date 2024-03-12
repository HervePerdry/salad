#' @export
varnames <- function(x) UseMethod("varnames")

#' @exportS3Method varnames dual
varnames.dual <- function(x) names(unclass(x@d))

