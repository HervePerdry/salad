#' @name rep
#' @rdname rep
#' @title Replicate elements of a dual vector
#' 
#' @param x a dual vector
#' @param ... extra parameters (typically, 'times', 'length.out' or 'each')
#'
#' @description A method extending `rep` to dual objects
#'
#' @examples x <- rep( dual(1:2), each = 4 )
#' x
#' d(x)
#'
#' @exportS3Method rep dual 
rep.dual <- function(x, ...) {
  x@x <- rep(x@x, ...)
  x@d <- rep.differential(x@d, ...)
  x
}

#' @name t
#' @rdname t
#' @aliases aperm
#' 
#' @title Transposition of matrices and arrays
#' 
#' @param x,a a dual matrix or array
#' @param perm subscript permutation vector
#' @param ... extra arguments (ignored)
#' @param resize if 'TRUE' (default) the array is reshaped
#' 
#' @examples x <- dual( matrix(c(1,2,0,3), 2, 2) )
#' t(x)
#'
#' # creation of an array using dim<-
#' y <- dual( c(1,-1) ) + 1:12
#' dim(y) <- c(2,3,2)
#' z <- aperm(y, c(2,3,1))
#' z
#' d(z, "x1")
#'
# transposition
#' @exportS3Method t dual
t.dual <- function(x) {
  x@x <- t(x@x)
  x@d <- t(x@d)
  x
}
# setMethod("t", c(x = "dual"), t.dual)

# aperm
#' @rdname t
#' @exportS3Method aperm dual
aperm.dual <- function(a, perm = NULL, resize = TRUE, ...) {
  a@x <- aperm(a@x, perm, resize, ...)
  a@d <- aperm(a@d, perm, resize, ...)
  a
}
# setMethod("aperm", c(a = "dual"), aperm.dual)

