#' @exportS3Method det dual
det.dual <- function(x) { 
  V <- det(x@x)
  # using sum(t(X)*Y) = trace(X %*% Y)
  if(V != 0) {
    A <- V*t(solve(x@x))
  } else {
    A <- tadjugate(x@x)
  }
  D <- unclass(x@d)
  for(i in seq_along(D))
    D[[i]] <- sum(A * D[[i]])
  class(D) <- "differential"
  fastNewDual(V, D)
}
#' @export
setMethod("det", "dual", det.dual)

#' @exportS3Method determinant dual
determinant.dual <- function(x, logarithm = TRUE, ...) { 
  detx <- determinant(x@x, logarithm, ...)
  if(logarithm) {
    if(!is.infinite(detx$modulus)) {
      A <- t(solve(x@x))
    } else {  
      A <- x@x
      A[] <- NA_real_
    }
  } else {
    V <- detx$modulus # ! ne pas mettre le signe, c'est bien la dérivée de modulus qu'on veut
    if(V != 0) {
      A <- V*t(solve(x@x))
    } else {
      A <- tadjugate(x@x)
    }
  }
  
  D <- unclass(x@d)
  for(i in seq_along(D))
    D[[i]] <- sum(A * D[[i]])
  class(D) <- "differential"

  detx$modulus <- fastNewDual(detx$modulus, D)
  detx
}

#' @export
setMethod("solve", c(a = "dual", b = "dual"), function(a, b, ...) {
  Ai <- solve(a@x, ...)
  dA <- a@d
  dB <- b@d
  fastNewDual(Ai %*% b@x, substract_diff(matrixProdNuDi(Ai, dB) , matrixProdDiNu( matrixProdNuDi(Ai, dA), Ai %*% b@x )))
})

setMethod("solve", c(a = "dual", b = "missing"), function(a, b, ...) {
  Ai <- solve(a@x, ...)
  dA <- a@d
  fastNewDual(Ai, matrixProdDiNu(matrixProdNuDi(-Ai, dA), Ai))
})

setMethod("solve", c(a = "numericOrArray", b = "dual"), function(a, b, ...) {
  Ai <- solve(a, ...)
  dB <- b@d
  fastNewDual(Ai %*% b@x, matrixProdNuDi(Ai, dB))
})

setMethod("solve", c(a = "dual", b = "numericOrArray"), function(a, b, ...) {
  Ai <- solve(a@x, ...)
  dA <- a@d
  fastNewDual(Ai %*% b, matrixProdDiNu( matrixProdNuDi(-Ai, dA), Ai %*% b ))
})


