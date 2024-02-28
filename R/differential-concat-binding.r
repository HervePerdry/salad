# ------------------- concatenation  -----------
#' @export
c.differential <- function(x, ...) {
  if(nargs() == 1L) return(x)
  y <- c.differential(...)
  x <- unclass(x)
  y <- unclass(y)
  L <- vector("list", length(x))
  for(i in seq_along(x)) L[[i]] <- c(x[[i]], y[[i]])
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

# ------------------- binding methods -----------
#' @export
rbind.differential <- function(x, ...) {
  L <- list(...)
  if(length(L) == 0) {
    if(is.null(dim(x))) dim(x) <- c(1, length(x))
    return(x)
  }
  y <- do.call(rbind.differential, L)
  x <- unclass(x)
  y <- unclass(y)
  L <- vector("list", length(x))
  for(i in seq_along(x)) L[[i]] <- rbind(x[[i]], y[[i]])
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

#' @export
cbind.differential <- function(x, ...) {
  L <- list(...)
  if(length(L) == 0) {
    if(is.null(dim(x))) dim(x) <- c(length(x), 1)
    return(x)
  }
  y <- do.call(cbind.differential, L)
  x <- unclass(x)
  y <- unclass(y)
  L <- vector("list", length(x))
  for(i in seq_along(x)) L[[i]] <- cbind(x[[i]], y[[i]])
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

