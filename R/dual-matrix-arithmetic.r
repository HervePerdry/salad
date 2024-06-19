#' @title Matrix Arithmetic
#' @name matmult
#' @rdname matmult
#'
#' @description Methods and functions for dual matrix arithmetic
#' @param x,y Dual or numeric matrices or vectors
#'
#' @details All methods are the analog of the corresponding methods for matrices.
#' The functions `matrixprod_dd`, `matrixprod_nd` and `matrixprod_dn` are for multiplication
#' of two dual objects, of a numeric and a dual object, or of a dual and a numeric object,
#' respectively. You may use these functions to save the method dispatching time.
#'
#' @return A dual object.
#'
#' @examples x <- dual( matrix(c(0,1,3,1), 2, 2) )
#' y <- x %*% c(2,-2)
#' d(y, "x1.1")

# Note
# It is a bit faster to compute D inside the function instead of simply calling the 
# relevant functions matrixProdDiNu matrixProdNuDi 
# I'll keep it like this for a better code maintenability
#' @export
matrixprod_dn <- function(x, y) {
  V <- x@x %*% y
  D <- matrixProdDiNu(x@d, y)
  fastNewDual(V, D)
}

#' @rdname matmult
#' @export
matrixprod_nd <- function(x, y) {
  V <- x %*% y@x
  D <- matrixProdNuDi(x, y@d)
  fastNewDual(V, D)
}

#' @rdname matmult
#' @export
matrixprod_dd <- function(x, y) {
  X <- x@x
  Y <- y@x
  L <- matrixProdDD(X, x@d, Y, y@d)
  fastNewDual(X %*% Y, L)
}

#' @rdname matmult
#' @exportMethod "%*%"
setMethod("%*%", c(x = "dual", y = "numericOrArray"), matrixprod_dn)
#' @rdname matmult
setMethod("%*%", c(x = "numericOrArray", y  = "dual"), matrixprod_nd)
#' @rdname matmult
setMethod("%*%", c(x = "dual", y = "dual"), matrixprod_dd)

#' @rdname matmult
#' @exportMethod crossprod
setMethod("crossprod",  signature(x="dual",y="dual"),           function(x,y) matrixprod_dd(t(x), y))
#' @rdname matmult
setMethod("crossprod",  signature(x="dual",y="numericOrArray"), function(x,y) matrixprod_dn(t(x), y))
#' @rdname matmult
setMethod("crossprod",  signature(x="numericOrArray",y="dual"), function(x,y) matrixprod_nd(t(x), y))
#' @rdname matmult
setMethod("crossprod",  signature(x="dual",y="missing"),        function(x,y) matrixprod_dd(t(x), x))

#' @rdname matmult
#' @exportMethod tcrossprod
setMethod("tcrossprod", signature(x="dual",y="dual"),           function(x,y) matrixprod_dd(x, t(y)))
#' @rdname matmult
setMethod("tcrossprod", signature(x="dual",y="numericOrArray"), function(x,y) matrixprod_dn(x, t(y)))
#' @rdname matmult
setMethod("tcrossprod", signature(x="numericOrArray",y="dual"), function(x,y) matrixprod_nd(x, t(y)))
#' @rdname matmult
setMethod("tcrossprod", signature(x="dual",y="missing"),        function(x,y) matrixprod_dd(x, t(x)))


if(FALSE) { 
# this version of matrix product without a function call inside it
# is still a little faster... but code maintenance will prime
matrixprod_dd <- function(x, y) {
  dx <- unclass(x@d) 
  dy <- unclass(y@d)
  X <- x@x
  Y <- y@x
  L <- vector("list", length(dx))
  for(i in seq_along(L)) 
    L[[i]] <- dx[[i]] %*% Y + X %*% dy[[i]]
  
  names(L) <- names(dx)
  class(L) <- "differential"
  fastNewDual(X %*% Y, L)
}


# and of course this is even more slower
matrixprod_dd <- function(x,y) {
  X <- x@x
  Y <- y@x
  d <- sum_diff(matrixProdNuDi(X, y@d) , matrixProdDiNu(x@d, Y))
  fastNewDual(X %*% Y, d)
}
}


