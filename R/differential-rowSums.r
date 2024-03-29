
rowSums_diff <- function(x, na.rm, dims) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x)) 
    L[[i]] <- rowSums(x[[i]], na.rm, dims)
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

colSums_diff <- function(x, na.rm, dims) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x)) 
    L[[i]] <- colSums(x[[i]], na.rm, dims)
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

rowMeans_diff <- function(x, na.rm, dims) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x)) 
    L[[i]] <- rowMeans(x[[i]], na.rm, dims)
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

colMeans_diff <- function(x, na.rm, dims) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x)) 
    L[[i]] <- colMeans(x[[i]], na.rm, dims)
  names(L) <- names(x)
  class(L) <- "differential"
  L
}
