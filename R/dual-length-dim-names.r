# --------------- length and dim

#' @export
length.dual <- function(x) length(x@x)

#' @export
dim.dual <- function(x) dim(x@x)

#' @export
`dim.dual<-` <- function(x, value) {
  dim(x@x) <- value
  dim.differential(x@d) <- value
  x
}

# --------------- dimnames

#' @export
dimnames.dual <- function(x) dimnames(x@x)

#' @export
`dimnames.dual<-` <- function(x, value) {
  if(!is.null(dim(x))) { # matrice
    dimnames(x@x) <- value
    dimnames.differential(x@d) <- value
  }
  x
}

# --------------- names

#' @export
names.dual <- function(x) names(x@x)

#' @export
`names.dual<-` <- function(x, value) {
  if(is.null(dim(x))) { # vecteur
    names(x@x) <- value
    names.differential(x@d) <- value
  }
  x
}
