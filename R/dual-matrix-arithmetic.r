# Note
# It is a bit faster to compute D inside the function instead of simply calling the 
# relevant functions matrixProdDiNu matrixProdNuDi 
# I'll keep it like this for a better code maintenability
#' @export
matrixprod_dn <- function(x, y) {
  V <- x@x %*% y
  D <- matrixProdDiNu(x@d, y)
  .Call(`_salad_fastNewDual`, V, D)
}

#' @export
matrixprod_nd <- function(x, y) {
  V <- x %*% y@x
  D <- matrixProdNuDi(x, y@d)
  .Call(`_salad_fastNewDual`, V, D)
}

#' @export
matrixprod_dd <- function(x, y) {
  X <- x@x
  Y <- y@x
  L <- matrixProdDD(X, x@d, Y, y@d)
  .Call(`_salad_fastNewDual`, X %*% Y, L)
}


setMethod("%*%", c(x = "dual", y = "numericOrArray"), matrixprod_dn)
setMethod("%*%", c(x = "numericOrArray", y  = "dual"), matrixprod_nd)
setMethod("%*%", c(x = "dual", y = "dual"), matrixprod_dd)




if(FALSE) { 
# manual dispatching in a S3 method seems to have ~ same performmances...
`%*%.dual` <- function(x, y) {
  if(class(x) == "dual") 
    if(class(y) == "dual") 
      matrixprod_dd(x,y)
    else
      matrixprod_dn(x,y)
  else
    matrixprod_nd(x,y)
}

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
  .Call(`_salad_fastNewDual`, X %*% Y, L)
}


# and of course this is even more slower
matrixprod_dd <- function(x,y) {
  X <- x@x
  Y <- y@x
  d <- sum_diff(matrixProdNuDi(X, y@d) , matrixProdDiNu(x@d, Y))
  .Call(`_salad_fastNewDual`, X %*% Y, d)
}
}


