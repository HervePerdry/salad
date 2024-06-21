#' @name matrix
#' @rdname matrix
#' @aliases matrix,dual-method
#' @title Methods for `matrix`, `array`, `as.matrix` and `as.vector` 
#'
#' @param data,x A dual object
#' @param nrow the desired number of rows
#' @param ncol the desired number of cols
#' @param byrow if `TRUE` the matrix is filled by rows
#' @param dimnames A `dimnames` attributes for a matrix or an array
#' @param dim A `dim` attributes for an array
#' @param mode The mode of the vector to create
#' @param ... additional arguments (ignored)
#'
#' @details The default behaviour for `as.matrix` dans `as.vector` is to drop the derivatives.
#' This can be modified using `salad` (to use with care). The prefered method to change the
#' shape is to use `dim<-`.
#'
#' @return A dual object for `matrix` and `array`, a base object for `as.matrix` and `as.vector`.
#'
#' @seealso \link{shape}, \code{\link{salad}}, \link{dual-class}
#'
#' @examples x <- dual(c(1,2,0,4))
#' y <- matrix(x, 2, 2)
#' y
#' as.matrix(y)
#' dim(x) <- c(2,2)
#' x

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
#' @rdname matrix
setMethod("array", c(data = "dual"),
    function(data, dim = length(data), dimnames = NULL) {
      dim(data) <- dim
      dimnames(data) <- dimnames
      data
    })

### --------- as.matrix

#' @rdname matrix
#' @exportS3Method as.matrix dual
#' @usage \method{as.matrix}{dual}(x, ...)

as.matrix.dual <- function(x, ...) {
  if(salad("drop.derivatives")) {
    warning("Dropping derivatives in as.matrix. See ?salad to change this behaviour")
    return(as.matrix(x@x, ...))
  }
  if(is.null(dim(x))) dim(x) <- c(length(x), 1)
  x
}
#' @rdname matrix
#' @name as
#' @family dual
setAs("dual", "matrix", function(from) as.matrix.dual(from))

#' @export
#' @rdname matrix
setMethod("as.matrix", "dual", as.matrix.dual)

### --------- as.vector

#' @rdname matrix
#' @exportS3Method as.vector dual
as.vector.dual <- function(x, mode = "any") {
  if(salad("drop.derivatives")) {
    warning("Dropping derivatives in as.vector. See ?salad to change this behaviour")
    return(as.vector(x@x, mode))
  }
  dim(x) <- NULL
  x
}
#' @rdname matrix
#' @name as
#' @family dual
setAs("dual", "vector", function(from) as.vector.dual(from))

#' @export
#' @rdname matrix
setMethod("as.vector", "dual", as.vector.dual)
