# ------------------- arithmetic methods ---------------------- 
# additions
setMethod("+", c(e1 = "dual", e2 = "dual"), function(e1, e2) .Call(`_salad_fastNewDual`, e1@x + e2@x, sum_diff(e1@d, e2@d)))

setMethod("+", c(e1 = "dual", e2 = "numericOrArray"), function(e1, e2) .Call(`_salad_fastNewDual`, e1@x + e2, e1@d))

setMethod("+", c(e1 = "numericOrArray", e2 = "dual"), function(e1, e2) .Call(`_salad_fastNewDual`, e1 + e2@x, e2@d))

setMethod("+", c(e1 = "dual", e2 = "missing"), function(e1, e2) e1) # unary op +e1

# substractions
setMethod("-", c(e1 = "dual", e2 = "dual"), function(e1, e2) .Call(`_salad_fastNewDual`, e1@x - e2@x, substract_diff(e1@d, e2@d)))

setMethod("-", c(e1 = "dual", e2 = "missing"), function(e1, e2) .Call(`_salad_fastNewDual`, -e1@x, neg_diff(e1@d)))

setMethod("-", c(e1 = "dual", e2 = "numericOrArray"), function(e1, e2) .Call(`_salad_fastNewDual`, e1@x - e2, e1@d))

setMethod("-", c(e1 = "numericOrArray", e2 = "dual"), function(e1, e2) .Call(`_salad_fastNewDual`, e1 - e2@x, neg_diff(e2@d)))

# multiplications
setMethod("*", c(e1 = "dual", e2 = "dual"), 
  function(e1, e2) .Call(`_salad_fastNewDual`, e1@x * e2@x, sum_diff(product_diff(e1@x, e2@d), product_diff(e2@x, e1@d))) )

setMethod("*", c(e1 = "dual", e2 = "numeric"), function(e1, e2) .Call(`_salad_fastNewDual`, e2 * e1@x, e2 * e1@d))

setMethod("*", c(e1 = "numeric", e2 = "dual"), function(e1, e2) .Call(`_salad_fastNewDual`, e1 * e2@x, e1 * e2@d))

# divisions
setMethod("/", c(e1 = "dual", e2 = "numeric"), function(e1, e2) .Call(`_salad_fastNewDual`, e1@x / e2, e1@d / e2))

setMethod("/", c(e1 = "numeric", e2 = "dual"), function(e1, e2) .Call(`_salad_fastNewDual`, e1 / e2@x, (-e1 / e2@x**2) * e2@d))

setMethod("/", c(e1 = "dual", e2 = "dual"), 
  function(e1, e2) .Call(`_salad_fastNewDual`, e1@x / e2@x,  sum_diff(product_diff((-e1@x/e2@x**2), e2@d) , divide_diff(e1@d, e2@x))) )

# exponentiation
setMethod("^", c(e1 = "dual", e2 = "numeric"), function(e1, e2) .Call(`_salad_fastNewDual`, e1@x^e2, (e2*e1@x^(e2-1)) * e1@d))

setMethod("^", c(e1 = "numeric", e2 = "dual"), function(e1, e2) {
  po <- e1^e2@x
  .Call(`_salad_fastNewDual`, po, po*log(e1) * e2@d)
})

setMethod("^", c(e1 = "dual", e2 = "dual"), function(e1, e2) {
  po <- e1@x^e2@x
  .Call(`_salad_fastNewDual`, po, po*e2@x/e1@x  * e1@d + po*log(e1@x) * e2@d)
})

