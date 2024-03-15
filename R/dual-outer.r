
# this is the code of base::outer with almost no change...
#' @export
outer.dual <- function(X, Y, FUN = "*", ...) {
  if(!is.null(dim(X))) {
    dX <- dim(X)
    nx <- dimnames(X)
    no.nx <- is.null(nx)
  } else {
    dX <- length(X)
    no.nx <- is.null(names(X))
    if (!no.nx) nx <- list(names(X))
  }
  if(!is.null(dim(Y))) {
    dY <- dim(Y)
    ny <- dimnames(Y)
    no.ny <- is.null(ny)
  } else {
    dY <- length(Y)
    no.ny <- is.null(names(Y))
    if (!no.ny) ny <- list(names(Y))
  }
  if(is.character(FUN) && FUN == "*") {
    dim(X) <- NULL
    dim(Y) <- NULL
    robj <- tcrossprod(X, Y)
  } else {
    FUN <- match.fun(FUN)
    Y <- rep(Y, rep.int(length(X), length(Y)))
    if(length(X)) 
      X <- rep(X, times = ceiling(length(Y)/length(X)))
    robj <- FUN(X, Y, ...)
  }
  dim(robj) <- c(dX, dY)
  if (!(no.nx && no.ny)) {
    if (no.nx) 
      nx <- vector("list", length(dX))
    else if (no.ny) 
      ny <- vector("list", length(dY))
    dimnames(robj) <- c(nx, ny)
  }
  robj
}

#' @exportMethod outer
setMethod("outer", c(X = "dual", Y = "dual"), outer.dual)
setMethod("outer", c(X = "numericOrArray", Y = "dual"), outer.dual)
setMethod("outer", c(X = "dual", Y = "numericOrArray"), outer.dual)
