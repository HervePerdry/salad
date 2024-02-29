
# --- somme ---
#' @exportS3Method sum dual
sum.dual <- function(x, ..., na.rm = FALSE) {
  if(...length() > 0) x <- c.dual(x, ...)
  if(na.rm) {
    i <- which(vx)
    x <- x[i]
  }
  vx <- x@x
  V <- sum(vx)
  D <- sum(x@d)
  fastNewDual(V, D)
}

# cette méthode ne sera appelée que si un des arguments n'est pas numérique... 
# so if x is dual : sum(x) calls S3 method, sum(1, x) calls S4, sum(1, 2) calls primitive
setMethod("sum", c(x = "numericOrArray"), sum.dual) # function(x, ..., na.rm = FALSE) sum.dual(c(x, ...), na.rm = na.rm))

# --- produit ---
#' @exportS3Method prod dual
prod.dual <- function(x, ..., na.rm = FALSE) {
  if(...length() > 0) x <- c.dual(x, ...)
  if(na.rm) {
    i <- which(!is.na(vx))
    x <- x[i]
  }
  # the efficient computation has to be done at the differential object level...
  do.call(fastNewDual, product_deriv(x@x, x@d))
}
setMethod("prod", c(x = "numericOrArray"), prod.dual) # function(x, ..., na.rm = TRUE) prod.dual(c(x, ...), na.rm = na.rm))

# --- max ---
# as we rely on which.max, na.rm is ignored (and is TRUE...)
# is this really an issue?
#' @exportS3Method max dual
max.dual <- function(x, ..., na.rm = TRUE) {
  if(...length() > 0) x <- c.dual(x, ...)
  vx <- x@x
  i <- which.max(vx)
  fastNewDual(vx[i], x@d[i])
}
setMethod("max", c(x = "numericOrArray"), max.dual)


# --- min ---
#' @exportS3Method min dual
min.dual <- function(x, ..., na.rm = FALSE) {
  if(...length() > 0) x <- c.dual(x, ...)
  vx <- x@x
  i <- which.min(vx)
  fastNewDual(vx[i], x@d[i])
}
setMethod("min", c(x = "numericOrArray"), min.dual)
   

# --- range ---
#' @exportS3Method range dual
range.dual <- function(x, ..., na.rm = TRUE) c(min.dual(x, ...), max.dual(x, ...))
setMethod("range", c(x = "numericOrArray"), range.dual)

# ************** related functions ***************
#' @export
setMethod("which.min", "dual", \(x) which.min(x@x))
#' @export
setMethod("which.max", "dual", \(x) which.max(x@x))

