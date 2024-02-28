# rep
#' @export
rep.differential <- function(x, ...) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x)) L[[i]] <- rep(x[[i]], ...)
  names(L) <- names(x)
  class(L) <- "differential"
  L
}


# transposition
#' @exportS3Method t differential
t.differential <- function(x) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x)) L[[i]] <- t(x[[i]])
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

# aperm
#' @exportS3Method aperm differential
aperm.differential <- function(a, perm = NULL, resize = TRUE, ...) {
  a <- unclass(a)
  x <- vector("list", length(a))
  for(i in seq_along(a)) x[[i]] <- aperm(a[[i]], perm, resize, ...)
  names(x) <- names(a)
  class(x) <- "differential"
  x
}

