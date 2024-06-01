#' @name MathFun
#' @rdname MathFun
#' 
#' @title Mathematical functions
#'
#' @param x function argument (dual or numeric object)
#' @param y first argument of atan2 function (dual or numeric)
#' @param a,b arguments of beta and lbeta (dual or nueumeric)
#' @param n first argument of choose and lchoose (dual)
#' @param k second argument of choose and lchoose (numeric)
#' @param base base to which log is computed
#' @param deriv integer argument to psigamma
#' @param ... extra arguments to trunc (unused)
#'
#' @description various mathematical functions and methods
#'
#' @examples x <- dual(1)
#' y <- log(x)
#' y
#' d(y)

# TODO
# ‘"cummax"’, ‘"cummin"’, ‘"cumprod"’, ‘"cumsum"’,
# f and df are univariate functions !

# ------------------ log exp sqrt
#' @exportS3Method exp dual
exp.dual   <- function(x) { expx <- exp(x@x) ; fastNewDual(expx, product_diff(expx, x@d)) }

#' @rdname MathFun
#' @exportS3Method expm1 dual
expm1.dual <- dualFun1(expm1, exp)

#' @rdname MathFun
#' @export
logNeper <- dualFun1(log, \(x) 1/x)

#' @rdname MathFun
#' @exportS3Method log dual
log.dual   <- function(x, base = exp(1)) if(missing(base)) logNeper(x) else logNeper(x)/log(base)

#' @rdname MathFun
#' @exportS3Method log10 dual
log10.dual <- dualFun1(log10, \(x) 0.43429448190325176/x)

#' @rdname MathFun
#' @exportS3Method log2 dual
log2.dual  <- dualFun1(log2, \(x) 1.4426950408889634/x)

#' @rdname MathFun
#' @exportS3Method log1p dual
log1p.dual <- dualFun1(log1p, \(x) 1/(1+x))

#' @rdname MathFun
#' @exportS3Method sqrt dual
sqrt.dual <- function(x) { sqrtx <- sqrt(x); fastNewDual(sqrtx, product_diff(0.5/sqrtx, x@d)) }

# ------------------ trigo
#' @rdname MathFun
#' @exportS3Method cos dual
cos.dual <- dualFun1(cos, \(x) -sin(x))

#' @rdname MathFun
#' @exportS3Method sin dual
sin.dual <- dualFun1(sin, cos)

#' @rdname MathFun
#' @exportS3Method tan dual
tan.dual <- function(x) { tanx <- tan(x@x) ; fastNewDual(tanx, product_diff(1 + tanx*tanx, x@d)) }

#' @rdname MathFun
#' @exportS3Method cospi dual
cospi.dual <- dualFun1(cospi, \(x) -pi*sin(x))

#' @rdname MathFun
#' @exportS3Method sinpi dual
sinpi.dual <- dualFun1(sin, \(x) pi*cos(x))

#' @rdname MathFun
#' @exportS3Method tanpi dual
tanpi.dual <- function(x) { tanpix <- tanpi(x@x) ; fastNewDual(tanpix, product_diff(pi*(1 + tanpix*tanpix), x@d)) }

#' @rdname MathFun
#' @exportS3Method acos dual 
acos.dual <- dualFun1(acos, \(x) -1/sqrt(1 - x*x))

#' @rdname MathFun
#' @exportS3Method asin dual
asin.dual <- dualFun1(asin, \(x) 1/sqrt(1 - x*x))

#' @rdname MathFun
#' @exportS3Method atan dual
atan.dual <- dualFun1(atan, \(x) 1/(1 + x*x))

setGeneric("atan2")
#' @rdname MathFun
#' @exportMethod atan2
setMethod("atan2", c(y = "dual", x = "dual"), function(y, x) {
  V <- atan2(y@x, x@x)
  # (x@x * y@d - y@x * x@d) / (x@x*x@x + y@x*y@x)
  D <- divide_diff(substract_diff( product_diff(x@x, y@d), product_diff(y@x, x@d) ), x@x*x@x + y@x*y@x) 
  fastNewDual(V, D)
})

#' @rdname MathFun
setMethod("atan2", c(y = "dual", x = "numericOrArray"), function(y, x) {
  V <- atan2(y@x, x)
  # (x * y@d) / (x*x + y@x*y@x)
  D <- divide_diff(product_diff(x, y@d), x*x + y@x*y@x)
  fastNewDual(V, D)
})

#' @rdname MathFun
setMethod("atan2", c(y = "numericOrArray", x = "dual"), function(y, x) {
  V <- atan2(y, x@x)
  #  -(y * x@d) / (x@x*x@x + y*y)
  D <- divide_diff(product_diff(-y , x@d), x@x*x@x + y*y)
  fastNewDual(V, D)
})


# ------------------ hyperbolic trigo
#' @rdname MathFun
#' @exportS3Method cosh dual
cosh.dual <- dualFun1(cosh, sinh)

#' @rdname MathFun
#' @exportS3Method sinh dual
sinh.dual <- dualFun1(sinh, cosh)

#' @rdname MathFun
#' @exportS3Method tanh dual
tanh.dual <- function(x) { tanhx <- tanh(x@x) ; fastNewDual(tanhx, product_diff(1 - tanhx*tanhx, x@d)) }

#' @rdname MathFun
#' @exportS3Method acosh dual
acosh.dual <- dualFun1(acosh, \(x) 1/sqrt(x*x - 1))

#' @rdname MathFun
#' @exportS3Method asinh dual
asinh.dual <- dualFun1(asinh, \(x) 1/sqrt(x*x + 1))

#' @rdname MathFun
#' @exportS3Method atanh dual
atanh.dual <- dualFun1(atanh, \(x) 1/(1 - x*x))



# ------------------ abs sign ceiling floor trunc
#' @rdname MathFun
#' @exportS3Method abs dual
abs.dual <- dualFun1(abs, sign)

#' @rdname MathFun
#' @exportS3Method sign dual
sign.dual <- function(x) {
  V <- sign(x@x)
  D <- product_diff(ifelse(V == 0, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}

#' @rdname MathFun
#' @exportS3Method ceiling dual
ceiling.dual <- function(x) {
  V <- ceiling(x@x)
  D <- product_diff(ifelse(V == x@x, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}

#' @rdname MathFun
#' @exportS3Method floor dual
floor.dual <- function(x) {
  V <- floor(x@x)
  D <- product_diff(ifelse(V == x@x, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}

#' @rdname MathFun
#' @exportS3Method trunc dual
trunc.dual <- function(x, ...) {
  V <- trunc(x@x, ...)
  D <- product_diff(ifelse(V == x@x, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}


# ------------------ gamma lgamma digamma trigamma psigamma
#' @rdname MathFun
#' @exportS3Method gamma dual
gamma.dual <- function(x) { gammax <- gamma(x@x) ; fastNewDual(gammax, product_diff(gammax * digamma(x@x), x@d)) }

#' @rdname MathFun
#' @exportS3Method lgamma dual
lgamma.dual <- dualFun1(lgamma, digamma)

#' @rdname MathFun
#' @exportS3Method digamma dual
digamma.dual <- dualFun1(digamma, trigamma)

#' @rdname MathFun
#' @exportS3Method trigamma dual
trigamma.dual <- dualFun1(trigamma, \(x) psigamma(x, 2))

#' @rdname MathFun
#' @exportS3Method psigamma dual
psigamma.dual <- function(x, deriv = 0) {
  psigammax <- psigamma(x@x, deriv)
  fastNewDual(psigammax, product_diff(psigamma(x@x, deriv + 1), x@d))
}

#' @rdname MathFun
#' @exportMethod psigamma
setMethod("psigamma", c(x = "dual"), psigamma.dual)


# ------------------ beta lbeta

setGeneric("beta")
#' @rdname MathFun
#' @exportMethod beta
setMethod("beta", c(a = "dual", b = "dual"), function(a, b) {
  psiapb <- digamma(a@x + b@x)
  V <- beta(a@x, b@x)
  D <- sum_diff( product_diff(V*(digamma(a) - psiapb), a@d), product_diff(V*(digamma(b) - psiapb), b@d) )
  fastNewDual(V, D)
})

#' @rdname MathFun
setMethod("beta", c(a = "dual", b = "numericOrArray"), function(a, b) {
  psiapb <- digamma(a@x + b)
  V <- beta(a@x, b)
  D <- product_diff(V*(digamma(a) - psiapb), a@d)
  fastNewDual(V, D)
})

#' @rdname MathFun
setMethod("beta", c(a = "numericOrArray", b = "dual"), function(a, b) {
  psiapb <- digamma(a + b@x)
  V <- beta(a, b@x)
  D <- product_diff(V*(digamma(b) - psiapb), b@d)
  fastNewDual(V, D)
})


setGeneric("lbeta")
#' @rdname MathFun
#' @exportMethod lbeta
setMethod("lbeta", c(a = "dual", b = "dual"), function(a, b) {
  psiapb <- digamma(a@x + b@x)
  V <- lbeta(a@x, b@x)
  D <- sum_diff( product_diff(digamma(a) - psiapb, a@d), product_diff(digamma(b) - psiapb, b@d) )
  fastNewDual(V, D)
})

#' @rdname MathFun
setMethod("lbeta", c(a = "dual", b = "numericOrArray"), function(a, b) {
  psiapb <- digamma(a@x + b)
  V <- lbeta(a@x, b)
  D <- product_diff(digamma(a) - psiapb, a@d)
  fastNewDual(V, D)
})

#' @rdname MathFun
setMethod("lbeta", c(a = "numericOrArray", b = "dual"), function(a, b) {
  psiapb <- digamma(a + b@x)
  V <- lbeta(a, b@x)
  D <- product_diff(digamma(b) - psiapb, b@d)
  fastNewDual(V, D)
})


# ------------------ factorial lfactorial

#' @rdname MathFun
#' @exportS3Method factorial dual
factorial.dual <- function(x) { factorialx <- factorial(x@x) ; fastNewDual(factorialx, product_diff(factorialx * digamma(x@x + 1), x@d)) }

#' @rdname MathFun
#' @exportS3Method lfactorial dual
lfactorial.dual <- dualFun1(lfactorial, \(x) gigamma(x+1))


# ------------------ choose lchoose
setGeneric("choose")
#' @rdname MathFun
#' @exportMethod choose
setMethod("choose", c(n = "dual", k = "numeric"), function(n, k) {
  V <- choose(n@x, k)
  D <- product_diff(V * (digamma(n@x + 1) - digamma(n@x - k + 1)), n@d)
  fastNewDual(V, D)
})

setGeneric("lchoose")
#' @rdname MathFun
#' @exportMethod lchoose
setMethod("lchoose", c(n = "dual", k = "numeric"), function(n, k) {
  V <- lchoose(n@x, k)
  D <- product_diff(digamma(n@x + 1) - digamma(n@x - k + 1), n@d)
  fastNewDual(V, D)
})


