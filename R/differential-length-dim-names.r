# --------------- length and dimension

#' @export
length.differential <- function(x) length(unclass(x)[[1]])

#' @export
dim.differential <- function(x) dim(unclass(x)[[1]])

#' @export
`dim.differential<-` <- function(x, value) { 
  x <- unclass(x)
  for(i in seq_along(x)) dim(x[[i]]) <- value
  class(x) <- "differential"
  x
}

# --------------- dimnames

#' @export
dimnames.differential <- function(x) dimnames(x[[1]])

#' @export
`dimnames.differential<-` <- function(x, value) {
  x <- unclass(x)
  if(!is.null(dim(x[[1]]))) { # matrices
    for(i in seq_along(x)) dimnames(x[[i]]) <- value
  }
  class(x) <- "differential"
  x
}

# --------------- names 

#' @export
names.differential <- function(x) names(x[[1]])

#' @export
`names.differential<-` <- function(x, value) {
  x <- unclass(x)
  if(is.null(dim(x[[1]]))) { # vecteurs
    for(i in seq_along(x)) names(x[[i]]) <- value
  }
  class(x) <- "differential"
  x
}
