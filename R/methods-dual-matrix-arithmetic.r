#' @export
matrixprod_dn <- function(x, y) {
  V <- x@x %*% y
  D <- matrixProdDiNu(x@d, y)
  fastNewDual(V, D)
}

#' @export
matrixprod_nd <- function(x, y) {
  V <- x %*% y@x
  D <- matrixProdNuDi(x, y@d)
  fastNewDual(V, D)
}

#' @export
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

setMethod("%*%", c(x = "dual", y = "numericOrArray"), matrixprod_dn)
setMethod("%*%", c(x = "numericOrArray", y  = "dual"), matrixprod_nd)
setMethod("%*%", c(x = "dual", y = "dual"), matrixprod_dd)




if(FALSE) { 
# this is still a little slower...
matrixProdDuDu <- function(x, y) {
  X <- x@x
  Y <- y@x
  L <- matrixProdDD(X, x@d, Y, y@d)
  fastNewDual(X %*% Y, L)
}

# and of course this is even more slower
matrixProdDuDu0 <- function(x,y) {
  X <- x@x
  Y <- y@x
  d <- sum_diff(matrixProdNuDi(X, y@d) , matrixProdDiNu(x@d, Y))
  fastNewDual(X %*% Y, d)
}
}


