#' @title d
#' @aliases d.dual
#' @aliases d.numeric
#' 
#' @usage d(x, varnames)
#' @param x a dual (or numeric) oject
#' @param varnames (optional) a vector or varnames to take derivatives along
#' 
#' @description Get value or differential of an object.
#'
#' @details If `varnames` is provided, a list of corresponding derivatives will be sent back
#'
#' @examples x <- dual(c(3,2))
#' x**2
#' value(x**2)
#' d(x**2)
#' d(x**2, "x1")
#' # you may use a constant
#' value(1)
#' d(1, "x1")
#' 
#' @export
d <- function(x, varnames) UseMethod("d")

#' @exportS3Method d dual
d.dual <- function(x, varnames) {
  if(missing(varnames)) return(unclass(x@d))
  L <- list()
  for(nn in varnames) {
    L[[nn]] <- component(x@d, nn)
  }
  L
}

#' @exportS3Method d numeric
d.numeric <- function(x, varnames = character(0)) {
  x[] <- 0
  L <- rep(list(x), length.out = length(varnames))
  names(L) <- varnames
  L
} 
