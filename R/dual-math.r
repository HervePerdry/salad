# TODO
#          ‘"abs"’, ‘"sign"’, ‘"sqrt"’, ‘"ceiling"’, ‘"floor"’,
#          ‘"trunc"’, ‘"cummax"’, ‘"cummin"’, ‘"cumprod"’, ‘"cumsum"’,
# ‘"acos"’, ‘"acosh"’,
#          ‘"asin"’, ‘"asinh"’, ‘"atan"’, ‘"atanh"’, ‘"exp"’, ‘"expm1"’,
#          ‘"cos"’, ‘"cosh"’, ‘"cospi"’, ‘"sin"’, ‘"sinh"’, ‘"sinpi"’,
#          ‘"tan"’, ‘"tanh"’, ‘"tanpi"’, ‘"gamma"’, ‘"lgamma"’,
#          ‘"digamma"’, ‘"trigamma"’

# f and df are univariate functions !
#' @export
dualFun1 <- function(f, df) {
  dual.f <- function(x) {
    vx <- x@x
    fastNewDual(f(vx), product_diff(df(vx), x@d))
  }
  dual.f
}

#' @export
logNeper <- dualFun1(log, \(x) 1/x)

#' @exportS3Method exp dual
exp.dual   <- function(x) { expx <- exp(x@x) ; fastNewDual(expx, product_diff(expx, x@d)) }

#' @exportS3Method expm1 dual
expm1.dual <- dualFun1(expm1, exp)

#' @exportS3Method log dual
log.dual   <- function(x, base = exp(1)) if(missing(base)) logNeper(x) else logNeper(x)/log(base)

#' @exportS3Method log10 dual
log10.dual <- dualFun1(log10, \(x) 1/(x*2.302585092994046))

#' @exportS3Method log2 dual
log2.dual  <- dualFun1(log2, \(x) 1/(x*0.6931471805599453))

#' @exportS3Method log1p dual
log1p.dual <- dualFun1(log1p, \(x) 1/(1+x))

