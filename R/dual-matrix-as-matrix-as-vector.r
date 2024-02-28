
# Pas facile d'en faire une méthode S3 (pb avec les arguments manquants...)

#' @export
setMethod("matrix", c(data = "dual"),
    function(data, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) { 
      if(missing(nrow) & missing(ncol)) {
        ncol <- 1
        nrow <- length(data)
      } else if(missing(nrow) & !missing(ncol)) {
        nrow <- length(data) %/% ncol
      } else if(!missing(nrow) & missing(ncol)) {
        ncol <- length(data) %/% nrow
      }
      if( ((nrow*ncol) %% length(data)) != 0 ) 
        warning("data length doesn't fit well with matrix dimensions")
      if(nrow*ncol != length(data))
        data <- rep.dual(data, length.out = nrow * ncol)
      if(byrow) {
        dim(data) <- c(ncol, nrow)
        data <- t(data)
      } else {
        dim(data) <- c(nrow, ncol)
      }
      dimnames(data) <- dimnames
      data
    })

#' @export
setMethod("array", c(data = "dual"),
    function(data, dim = length(data), dimnames = NULL) {
      dim(data) <- dim
      dimnames(data) <- dimnames
      data
    })

### --------- as.matrix

#' @exportS3Method as.matrix dual
as.matrix.dual <- function(x, ...) {
  if(salad("drop.derivatives")) {
    warning("Dropping derivatives in as.matrix. See ?salad to change this behaviour")
    return(as.matrix(x@x))
  }
  if(is.null(dim(x))) dim(x) <- c(length(x), 1)
  x
}
setAs("dual", "matrix", function(from) as.matrix.dual(from))

#' @export
setMethod("as.matrix", "dual", as.matrix.dual)

### --------- as.vector

#' @exportS3Method as.vector dual
as.vector.dual <- function(x) {
  if(salad("drop.derivatives")) {
    warning("Dropping derivatives in as.vector. See ?salad to change this behaviour")
    return(as.vector(x@x))
  }
  dim(x) <- NULL
  x
}
setAs("dual", "vector", function(from) as.vector.dual(from))

#' @export
setMethod("as.vector", "dual", as.vector.dual)
