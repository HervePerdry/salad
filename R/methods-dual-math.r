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
    fastNewDual(f(vx), df(vx) * x@d)
  }
  dual.f
}

log.neper <- dualFun1(log, \(x) 1/x)

setMethod("exp", "dual", dualFun1(exp, exp))
setMethod("expm1", "dual", dualFun1(expm1, exp))
setMethod("log", "dual", function(x, base = exp(1)) log.neper(x)/log(base))
setMethod("log10", "dual", dualFun1(log10, \(x) 1/(x*2.302585092994046)))
setMethod("log2", "dual", dualFun1(log2, \(x) 1/(x*0.6931471805599453)))
setMethod("log1p", "dual", dualFun1(log1p, \(x) 1/(1+x)))

