#' @name Summary
#' @rdname summary
#' @aliases sum prod min max range which.min which.max
#' 
#' @param x a dual object
#' @param na.rm if 'TRUE', NA values are removed
#' @param ... extra arguments
#'
#' @title Summary methods for objects of class dual
#'
#' @description Methods extending to dual objects the corresponding methods for numeric objects.
#'
#' @details For `max` and `min`, the derivative is equal to the derivative of maximum element
#' as identified by `which.max` and `which.min`. This is unfortunately problematic in presence
#' of ties. If this is an issue, you may redefine this function (at the expense of speed).
#'
#' @return `which.min` and `which.max` return an integer, the other methods return a dual object.
#'
#' @examples x <- dual( c(1,2,4) )
#' sum(x)
#' d(sum(x), "x1")
#'
# --- somme ---
#' @exportS3Method sum dual
sum.dual <- function(x, ..., na.rm = FALSE) {
  if(...length() > 0) x <- c.dual(x, ...)
  if(na.rm) {
    i <- which(!is.na(x@x))
    x <- x[i]
  }
  vx <- x@x
  V <- sum(vx)
  D <- sum(x@d)
  fastNewDual(V, D)
}

# cette méthode ne sera appelée que si un des arguments n'est pas numérique... 
# so if x is dual : sum(x) calls S3 method, sum(1, x) calls S4, sum(1, 2) calls primitive
#' @rdname summary
#' @export
setMethod("sum", c(x = "numericOrArray"), sum.dual) # function(x, ..., na.rm = FALSE) sum.dual(c(x, ...), na.rm = na.rm))

# --- produit ---
#' @exportS3Method prod dual
#' @rdname summary
prod.dual <- function(x, ..., na.rm = FALSE) {
  if(...length() > 0) x <- c.dual(x, ...)
  if(na.rm) {
    i <- which(!is.na(x@x))
    x <- x[i]
  }
  # the efficient computation has to be done at the differential object level...
  do.call(fastNewDual, product_deriv(x@x, x@d))
}
#' @rdname summary
#' @export
setMethod("prod", c(x = "numericOrArray"), prod.dual) # function(x, ..., na.rm = TRUE) prod.dual(c(x, ...), na.rm = na.rm))

# --- max ---
# as we rely on which.max, na.rm is ignored (and is TRUE...)
# is this really an issue?
#' @rdname summary
#' @exportS3Method max dual
max.dual <- function(x, ..., na.rm = TRUE) {
  if(...length() > 0) x <- c.dual(x, ...)
  vx <- x@x
  i <- which.max(vx)
  fastNewDual(vx[i], x@d[i])
}
#' @rdname summary
#' @export
setMethod("max", c(x = "numericOrArray"), max.dual)


# --- min ---
# same comment as max
#' @rdname summary
#' @exportS3Method min dual
min.dual <- function(x, ..., na.rm = TRUE) {
  if(...length() > 0) x <- c.dual(x, ...)
  vx <- x@x
  i <- which.min(vx)
  fastNewDual(vx[i], x@d[i])
}
#' @rdname summary
#' @export
setMethod("min", c(x = "numericOrArray"), min.dual)
   

# --- range ---
#' @exportS3Method range dual
#' @rdname summary
range.dual <- function(x, ..., na.rm = TRUE) c(min.dual(x, ...), max.dual(x, ...))
#' @rdname summary
#' @export
setMethod("range", c(x = "numericOrArray"), range.dual)

# ************** related functions ***************
#' @rdname summary
#' @export
setMethod("which.min", "dual", \(x) which.min(x@x))
#' @rdname summary
#' @export
setMethod("which.max", "dual", \(x) which.max(x@x))

