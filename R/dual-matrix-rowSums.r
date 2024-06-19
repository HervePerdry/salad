#' @name colSums
#' @rdname colSums
#' @aliases rowSums colMeans rowMeans
#' @title Row and column sums and means
#'
#' @param x a dual matrix or array
#' @param na.rm if 'TRUE', missing values are removed
#' @param dims which dimensions are regarded as rows and cols
#' @param ... extra parameters (ignored)
#'
#' @description Method extending to dual matrices the corresponding methods for dual matrices.
#'
#' @details a dual object (usually a dual vector).
#' 
#' @examples x <- dual( c(1,2) )
#' x <- cbind(x, 2*x+1)
#' rowSums(x)
#' d(rowSums(x), "x1")
#'
#' @exportS3Method rowSums dual
rowSums.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- rowSums(x@x, na.rm, dims)
  D <- rowSums_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @rdname colSums
#' @export
setMethod("rowSums", "dual", rowSums.dual)

#' @rdname colSums
#' @exportS3Method colSums dual
colSums.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- colSums(x@x, na.rm, dims)
  D <- colSums_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @rdname colSums
#' @export
setMethod("colSums", "dual", colSums.dual)

#' @rdname colSums
#' @exportS3Method rowMeans dual
rowMeans.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- rowMeans(x@x, na.rm, dims)
  D <- rowMeans_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @rdname colSums
#' @export
setMethod("rowMeans", "dual", rowMeans.dual)

#' @rdname colSums
#' @exportS3Method colMeans dual
colMeans.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- colMeans(x@x, na.rm, dims)
  D <- colMeans_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @rdname colSums
#' @export
setMethod("colMeans", "dual", colMeans.dual)

