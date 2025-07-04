#' @title dual class
#'
#' @description
#' An S4 class for forward differentiation of vector and matrix computations.
#'
#' @slot x the value of the object. Use the function `value` to access this slot.
#' @slot d a (named) list of derivatives. Use the function `d` to access this slot.
#'
#' @details
#' A dual object can be either a vector or a matrix. It can contain derivatives
#' with respect to several variables. The derivatives will have the same shape
#' as the value.
#'
#' The shape of an object can be changed using `dim<-`. Note that by default
#' `as.matrix` and `as.vector` will send back a regular vector/matrix object,
#' dropping the derivatives. See `salad` to change this behaviour if needed
#' (this is not the recommended solution).
#'
#' Many methods and functions have been redefined in the package, in order to
#' allow to apply existing code to `dual` objects, with no or little change.
#' 
#' @seealso \code{\link{value}}, \code{\link{d}}, \code{\link{dual}}, \code{\link{salad}}.
#'
#' @examples # creating a vector of length 4
#' x <- dual( c(1,2,1,0) )
#' x
#' d(x)
#' # turning x into a matrix
#' dim(x) <- c(2,2)
#' x
#' d(x)
#' # and back into a vector
#' dim(x) <- NULL
#' x
#' # weighted sum of the elements of x
#' S <- sum(1:4 * x)
#' S
#' d(S)
#' 
#' @exportClass dual
# d = "list" can be cool if I drop completely the S3 class for differentials (I might)
setClass("dual", slots = c(x = "numericOrArray", d = "differential"))

setMethod("show", "dual",
    function(object) {
      print(object@x)
      wd <- options("width")$width
      s <- "[has derivatives]\n"
      if(wd > 20) { 
        nn <- varnames(object);
        s1 <- paste0("[has derivatives in ", paste(nn, collapse = " "), "]\n")
        if(nchar(s1) <= wd) {
          s <- s1
        } else {
          le <- cumsum(1 + nchar(nn))
          w <- which(le < wd - 24)
          if(length(w) > 0) {
            s <- paste0("[has derivatives in ", paste(nn[1:max(w)], collapse = " "), " ...]\n")
          }
        }
      }
      cat(s)
    })


