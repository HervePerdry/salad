#' @export
setGeneric("value", function(x) standardGeneric("value"))
setMethod("value", "dual", function(x) x@x)
setMethod("value", "numericOrArray", function(x) x)


#' @export
setGeneric("d", function(x, varnames) standardGeneric("d"))
d1 <- function(x, varnames) {
  if(missing(varnames)) return(unclass(x@d))
  L <- list()
  for(nn in varnames) {
    L[[nn]] <- component(x@d, nn)
  }
  L
}
#' @export
setMethod("d", "dual", d1)

d2 <- function(x, varnames) {
  x[] <- 0
  L <- rep(list(x), length.out = length(varnames))
  names(L) <- varnames
  L
}
#' @export
setMethod("d", "numericOrArray", d2)

