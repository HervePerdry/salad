
# --- somme ---
#' @exportS3Method sum dual
sum.dual <- function(x, na.rm = FALSE) {
  vx <- x@x
  if(na.rm) {
    i <- which(vx)
    x <- x[i]
  }
  V <- sum(vx)
  D <- sum(x@d)
  fastNewDual(V, D)
}

# cette méthode ne sera appelée que si un des arguments n'est pas numérique... 
# so if x is dual : sum(x) calls S3 method, sum(1, x) calls S4, sum(1, 2) calls primitive
setMethod("sum", c(x = "numericOrArray"), function(x, ..., na.rm = FALSE) sum.dual(c(x, ...), na.rm = na.rm))

# --- produit ---
#' @exportS3Method prod dual
prod.dual <- function(x, na.rm = FALSE) {
  vx <- x@x
  dx <- unclass(x@d)
  if(na.rm) {
    i <- which(!is.na(vx))
    x <- x[i]
  }
  V <- prod(vx)
  # sum_i val * dx[i] / x[i]
  L <- lapply(seq_along(x@x), function(i) (val/x@x[i]) * x@d[i])
  d <- do.call(sum.differential, L)
  x@x <- val
  x@d <- d
      x
}
setMethod("prod", c(x = "numericOrArray"), function(x, ..., na.rm = TRUE) prod.dual(c(x, ...), na.rm = na.rm))

# --- max ---
setGeneric("max1", function(x, na.rm) standardGeneric("max1"))
setMethod("max1", c(x = "dual"), 
    function(x, na.rm = FALSE) {
      if(na.rm) {
        i <- which(!is.na(x@x))
        x <- x[i]
      }
      i <- which.max(x@x)
      x@x <- max(x@x)
      x@d <- x@d[i]
      x
    })
setMethod("max1", c(x = "numericOrArray"), function(x, na.rm = FALSE) .Primitive("max")(x, na.rm = na.rm))
setMethod("max", c(x = "numericOrArrayOrDual"), function(x, ..., na.rm = TRUE) max1(c(x, ...), na.rm = na.rm))


# --- min ---
setGeneric("min1", function(x, na.rm) standardGeneric("min1"))
setMethod("min1", c(x = "dual"), 
    function(x, na.rm = FALSE) {
      if(na.rm) {
        i <- which(!is.na(x@x))
        x <- x[i]
      }
      i <- which.min(x@x)
      x@x <- min(x@x)
      x@d <- x@d[i]
      x
    })
setMethod("min1", c(x = "numericOrArray"), function(x, na.rm = FALSE) .Primitive("min")(x, na.rm = na.rm))
setMethod("min", c(x = "numericOrArrayOrDual"), function(x, ..., na.rm = TRUE) min1(c(x, ...), na.rm = na.rm))


# --- range ---
setGeneric("range1", function(x, na.rm) standardGeneric("range1"))
setMethod("range1", c(x = "dual"), function(x, na.rm = FALSE) c(min(x), max(x)))
setMethod("range1", c(x = "numericOrArray"), function(x, na.rm = FALSE) .Primitive("range")(x, na.rm = na.rm))
setMethod("range", c(x = "numericOrArrayOrDual"), function(x, ..., na.rm = TRUE) range1(c(x, ...), na.rm = na.rm))

# ************** related functions ***************
setMethod("which.min", c(x = "dual"), function(x) which.min(x@x))
setMethod("which.max", c(x = "dual"), function(x) which.max(x@x))

