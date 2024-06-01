#' @exportS3Method rowSums dual
rowSums.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- rowSums(x@x, na.rm, dims)
  D <- rowSums_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @export
setMethod("rowSums", "dual", rowSums.dual)


#' @exportS3Method colSums dual
colSums.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- colSums(x@x, na.rm, dims)
  D <- colSums_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @export
setMethod("colSums", "dual", colSums.dual)


#' @exportS3Method rowMeans dual
rowMeans.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- rowMeans(x@x, na.rm, dims)
  D <- rowMeans_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @export
setMethod("rowMeans", "dual", rowMeans.dual)


#' @exportS3Method colMeans dual
colMeans.dual <- function(x, na.rm = FALSE, dims = 1, ...) {
  V <- colMeans(x@x, na.rm, dims)
  D <- colMeans_diff(x@d, na.rm, dims)
  fastNewDual(V, D)
}

#' @export
setMethod("colMeans", "dual", colMeans.dual)

