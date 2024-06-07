#' Arithmetic Operators
#' 
#' @description Arithmetic operators for objects of class 'dual'
#' 
#' @name Arithmetic
#' @aliases Arithmetic
#'
#' @param e1 object of class 'dual' or 'numeric'
#' @param e2 object of class 'dual' or 'numeric'
#' 
#' @details The usual operations are performed, with appropriate propagation of the derivatives
#'
#' @return An object of class 'dual'.
#'
#' @examples x <- dual( c(1,2) )
#' a <- 2 * x + 3
#' a
#' d(a)
#' b <- x[1] + 3*x[2]
#' b
#' d(b)
NULL

# ------------------- arithmetic methods ---------------------- 
# additions

#' @rdname Arithmetic
setMethod("+", c(e1 = "dual", e2 = "dual"), function(e1, e2) fastNewDual(e1@x + e2@x, sum_diff(e1@d, e2@d)))

#' @rdname Arithmetic
setMethod("+", c(e1 = "dual", e2 = "numericOrArray"), 
             function(e1, e2) {
               x = e1@x + e2; 
               le <- length(x); 
               if(le != length(e1@x))
                 fastNewDual(x, rep.differential(e1@d, length = le)) 
               else 
                 fastNewDual(x, e1@d)
             })

#' @rdname Arithmetic
setMethod("+", c(e1 = "numericOrArray", e2 = "dual"), 
             function(e1, e2) {
               x <- e1 + e2@x
               le <- length(x)
               if(le != length(e2@x))
                 fastNewDual(x, rep.differential(e2@d, length = le))
               else 
                 fastNewDual(x, e2@d)
})

#' @rdname Arithmetic
setMethod("+", c(e1 = "dual", e2 = "missing"), function(e1, e2) e1) # unary op +e1

# substractions

#' @rdname Arithmetic
setMethod("-", c(e1 = "dual", e2 = "dual"), function(e1, e2) fastNewDual(e1@x - e2@x, substract_diff(e1@d, e2@d)))

#' @rdname Arithmetic
setMethod("-", c(e1 = "dual", e2 = "missing"), function(e1, e2) fastNewDual(-e1@x, neg_diff(e1@d)))

#' @rdname Arithmetic
setMethod("-", c(e1 = "dual", e2 = "numericOrArray"), 
  function(e1, e2) { 
    x <- e1@x - e2
    le <- length(x)
    if(le != length(e1@x))
      fastNewDual(x, rep.differential(e1@d, length = le))
    else
      fastNewDual(x, e1@d)
  })

#' @rdname Arithmetic
setMethod("-", c(e1 = "numericOrArray", e2 = "dual"), function(e1, e2) fastNewDual(e1 - e2@x, neg_diff(e2@d)))

# multiplications

#' @rdname Arithmetic
setMethod("*", c(e1 = "dual", e2 = "dual"), 
  function(e1, e2) fastNewDual(e1@x * e2@x, sum_diff(product_diff(e1@x, e2@d), product_diff(e2@x, e1@d))) )

#' @rdname Arithmetic
setMethod("*", c(e1 = "dual", e2 = "numeric"), function(e1, e2) fastNewDual(e2 * e1@x, e2 * e1@d))

#' @rdname Arithmetic
setMethod("*", c(e1 = "numeric", e2 = "dual"), function(e1, e2) fastNewDual(e1 * e2@x, e1 * e2@d))

# divisions

#' @rdname Arithmetic
setMethod("/", c(e1 = "dual", e2 = "numeric"), function(e1, e2) fastNewDual(e1@x / e2, e1@d / e2))

#' @rdname Arithmetic
setMethod("/", c(e1 = "numeric", e2 = "dual"), function(e1, e2) fastNewDual(e1 / e2@x, (-e1 / e2@x**2) * e2@d))

#' @rdname Arithmetic
setMethod("/", c(e1 = "dual", e2 = "dual"), 
  function(e1, e2) fastNewDual(e1@x / e2@x,  sum_diff(product_diff((-e1@x/e2@x**2), e2@d) , divide_diff(e1@d, e2@x))) )

# exponentiation

#' @rdname Arithmetic
setMethod("^", c(e1 = "dual", e2 = "numeric"), function(e1, e2) fastNewDual(e1@x^e2, (e2*e1@x^(e2-1)) * e1@d))

#' @rdname Arithmetic
setMethod("^", c(e1 = "numeric", e2 = "dual"), function(e1, e2) {
  po <- e1^e2@x
  fastNewDual(po, po*log(e1) * e2@d)
})

#' @rdname Arithmetic
setMethod("^", c(e1 = "dual", e2 = "dual"), function(e1, e2) {
  po <- e1@x^e2@x
  fastNewDual(po, po*e2@x/e1@x  * e1@d + po*log(e1@x) * e2@d)
})

