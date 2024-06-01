
diag_diff <- function(x, nrow, ncol, names = TRUE) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x))
    L[[i]] <- diag(x[[i]], nrow, ncol, names)
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

set_diag_diff <- function(x, value) {
  x <- unclass(x)
  v <- unclass(value)
  for(i in seq_along(x)) 
    diag(x[[i]]) <- v[[i]]
  class(x) <- "differential"
  x
}
