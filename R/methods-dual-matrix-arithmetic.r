setMethod("%*%", c(x = "dual", y = "numericOrArray"),
    function(x, y) {
      x@x <- x@x %*% y
      x@d <- matrixProdDiNu(x@d, y)
      x
    })

setMethod("%*%", c(x = "numericOrArray", y  = "dual"),
    function(x, y) {
      y@x <- x %*% y@x
      y@d <- matrixProdNuDi(x, y@d)
      x
    })

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


