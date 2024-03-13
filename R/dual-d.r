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
d.numeric <- function(x, varnames) {
  x[] <- 0
  L <- rep(list(x), length.out = length(varnames))
  names(L) <- varnames
  L
} 
