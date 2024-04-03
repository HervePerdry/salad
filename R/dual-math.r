# TODO
# ‘"cummax"’, ‘"cummin"’, ‘"cumprod"’, ‘"cumsum"’,

# f and df are univariate functions !
#' @export
dualFun1 <- function(f, df) {
  dual.f <- function(x) {
    vx <- x@x
    fastNewDual(f(vx), product_diff(df(vx), x@d))
  }
  dual.f
}


# ------------------ log exp sqrt
#' @export
logNeper <- dualFun1(log, \(x) 1/x)

#' @exportS3Method exp dual
exp.dual   <- function(x) { expx <- exp(x@x) ; fastNewDual(expx, product_diff(expx, x@d)) }

#' @exportS3Method expm1 dual
expm1.dual <- dualFun1(expm1, exp)

#' @exportS3Method log dual
log.dual   <- function(x, base = exp(1)) if(missing(base)) logNeper(x) else logNeper(x)/log(base)

#' @exportS3Method log10 dual
log10.dual <- dualFun1(log10, \(x) 0.43429448190325176/x)

#' @exportS3Method log2 dual
log2.dual  <- dualFun1(log2, \(x) 1.4426950408889634/x)

#' @exportS3Method log1p dual
log1p.dual <- dualFun1(log1p, \(x) 1/(1+x))

#' @exportS3Method sqrt dual
sqrt.dual <- function(x) { sqrtx <- sqrt(x); fastNewDual(sqrtx, product_diff(0.5/sqrtx, x@d)) }

# ------------------ trigo
#' @exportS3Method cos dual
cos.dual <- dualFun1(cos, \(x) -sin(x))

#' @exportS3Method sin dual
sin.dual <- dualFun1(sin, cos)

#' @exportS3Method tan dual
tan.dual <- function(x) { tanx <- tan(x@x) ; fastNewDual(tanx, product_diff(1 + tanx*tanx, x@d)) }

#' @exportS3Method cospi dual
cospi.dual <- dualFun1(cospi, \(x) -pi*sin(x))

#' @exportS3Method sinpi dual
sinpi.dual <- dualFun1(sin, \(x) pi*cos(x))

#' @exportS3Method tanpi dual
tanpi.dual <- function(x) { tanpix <- tanpi(x@x) ; fastNewDual(tanpix, product_diff(pi*(1 + tanpix*tanpix), x@d)) }

#' @exportS3Method acos dual 
acos.dual <- dualFun1(acos, \(x) -1/sqrt(1 - x*x))

#' @exportS3Method asin dual
asin.dual <- dualFun1(asin, \(x) 1/sqrt(1 - x*x))

#' @exportS3Method atan dual
atan.dual <- dualFun1(atan, \(x) 1/(1 + x*x))

setGeneric("atan2")
#' @exportMethod atan2
setMethod("atan2", c(y = "dual", x = "dual"), function(y, x) {
  V <- atan2(y@x, x@x)
  # (x@x * y@d - y@x * x@d) / (x@x*x@x + y@x*y@x)
  D <- divide_diff(substract_diff( product_diff(x@x, y@d), product_diff(y@x, x@d) ), x@x*x@x + y@x*y@x) 
  fastNewDual(V, D)
})

setMethod("atan2", c(y = "dual", x = "numericOrArray"), function(y, x) {
  V <- atan2(y@x, x)
  # (x * y@d) / (x*x + y@x*y@x)
  D <- divide_diff(product_diff(x, y@d), x*x + y@x*y@x)
  fastNewDual(V, D)
})

setMethod("atan2", c(y = "numericOrArray", x = "dual"), function(y, x) {
  V <- atan2(y, x@x)
  #  -(y * x@d) / (x@x*x@x + y*y)
  D <- divide_diff(product_diff(-y , x@d), x@x*x@x + y*y)
  fastNewDual(V, D)
})


# ------------------ hyperbolic trigo
#' @exportS3Method cosh dual
cosh.dual <- dualFun1(cosh, sinh)

#' @exportS3Method sinh dual
sinh.dual <- dualFun1(sinh, cosh)

#' @exportS3Method tanh dual
tanh.dual <- function(x) { tanhx <- tanh(x@x) ; fastNewDual(tanhx, product_diff(1 - tanhx*tanhx, x@d)) }

#' @exportS3Method acosh dual
acosh.dual <- dualFun1(acosh, \(x) 1/sqrt(x*x - 1))

#' @exportS3Method acosh dual
asinh.dual <- dualFun1(asinh, \(x) 1/sqrt(x*x + 1))

#' @exportS3Method atan dual
atanh.dual <- dualFun1(atanh, \(x) 1/(1 - x*x))



# ------------------ abs sign ceiling floor trunc
#' @exportS3Method abs dual
abs.dual <- dualFun1(abs, sign)

#' @exportS3Method sign dual
sign.dual <- function(x) {
  V <- sign(x@x)
  D <- product_diff(ifelse(V == 0, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}

#' @exportS3Method ceiling dual
ceiling.dual <- function(x) {
  V <- ceiling(x@x)
  D <- product_diff(ifelse(V == x@x, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}

#' @exportS3Method floor dual
floor.dual <- function(x) {
  V <- floor(x@x)
  D <- product_diff(ifelse(V == x@x, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}

#' @exportS3Method trunc dual
trunc.dual <- function(x, ...) {
  V <- trunc(x@x, ...)
  D <- product_diff(ifelse(V == x@x, Inf, 0), x@d)
  fastNewDual(V, nanToZero(D))
}


# ------------------ gamma lgamma digamma trigamma psigamma
#' @exportS3Method gamma dual
gamma.dual <- function(x) { gammax <- gamma(x@x) ; fastNewDual(gammax, product_diff(gammax * digamma(x@x), x@d)) }

#' @exportS3Method lgamma dual
lgamma.dual <- dualFun1(lgamma, digamma)

#' @exportS3Method digamma dual
digamma.dual <- dualFun1(digamma, trigamma)

#' @exportS3Method trigamma dual
trigamma.dual <- dualFun1(trigamma, \(x) psigamma(x, 2))

#' @exportS3Method psigamma dual
psigamma.dual <- function(x, deriv = 0) {
  psigammax <- psigamma(x@x, deriv)
  fastNewDual(psigammax, product_diff(psigamma(x@x, deriv + 1), x@d))
}

#' @exportMethod psigamma
setMethod("psigamma", c(x = "dual"), psigamma.dual)


# ------------------ beta lbeta

setGeneric("beta")
#' @exportMethod beta
setMethod("beta", c(a = "dual", b = "dual"), function(a, b) {
  psiapb <- digamma(a@x + b@x)
  V <- beta(a@x, b@x)
  D <- sum_diff( product_diff(V*(digamma(a) - psiapb), a@d), product_diff(V*(digamma(b) - psiapb), b@d) )
  fastNewDual(V, D)
})

setMethod("beta", c(a = "dual", b = "numericOrArray"), function(a, b) {
  psiapb <- digamma(a@x + b)
  V <- beta(a@x, b)
  D <- product_diff(V*(digamma(a) - psiapb), a@d)
  fastNewDual(V, D)
})

setMethod("beta", c(a = "numericOrArray", b = "dual"), function(a, b) {
  psiapb <- digamma(a + b@x)
  V <- beta(a, b@x)
  D <- product_diff(V*(digamma(b) - psiapb), b@d)
  fastNewDual(V, D)
})


setGeneric("lbeta")
#' @exportMethod lbeta
setMethod("lbeta", c(a = "dual", b = "dual"), function(a, b) {
  psiapb <- digamma(a@x + b@x)
  V <- lbeta(a@x, b@x)
  D <- sum_diff( product_diff(digamma(a) - psiapb, a@d), product_diff(digamma(b) - psiapb, b@d) )
  fastNewDual(V, D)
})

setMethod("lbeta", c(a = "dual", b = "numericOrArray"), function(a, b) {
  psiapb <- digamma(a@x + b)
  V <- lbeta(a@x, b)
  D <- product_diff(digamma(a) - psiapb, a@d)
  fastNewDual(V, D)
})

setMethod("lbeta", c(a = "numericOrArray", b = "dual"), function(a, b) {
  psiapb <- digamma(a + b@x)
  V <- lbeta(a, b@x)
  D <- product_diff(digamma(b) - psiapb, b@d)
  fastNewDual(V, D)
})


# ------------------ factorial lfactorial

#' @exportS3Method factorial dual
factorial.dual <- function(x) { factorialx <- factorial(x@x) ; fastNewDual(factorialx, product_diff(factorialx * digamma(x@x + 1), x@d)) }

#' @exportS3Method lfactorial dual
lfactorial.dual <- dualFun1(lfactorial, \(x) gigamma(x+1))


# ------------------ choose lchoose
setGeneric("choose")
#' @exportMethod choose
setMethod("choose", c(n = "dual", k = "numeric"), function(n, k) {
  V <- choose(n@x, k)
  D <- product_diff(V * (digamma(n@x + 1) - digamma(n@x - k + 1)), n@d)
  fastNewDual(V, D)
})

setGeneric("lchoose")
#' @exportMethod lchoose
setMethod("lchoose", c(n = "dual", k = "numeric"), function(n, k) {
  V <- lchoose(n@x, k)
  D <- product_diff(digamma(n@x + 1) - digamma(n@x - k + 1), n@d)
  fastNewDual(V, D)
})


