#' @rdname d
#' @details The `varnames` function sends back the names of all variables
#' for which a derivative is defined.
#' @export
varnames <- function(x) UseMethod("varnames")

#' @rdname d
#' @exportS3Method varnames dual
varnames.dual <- function(x) names(unclass(x@d))

#' @rdname d
#' @exportS3Method varnames numeric
varnames.numeric <- function(x) character(0)
