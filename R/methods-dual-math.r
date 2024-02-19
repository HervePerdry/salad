# TODO
#          ‘"abs"’, ‘"sign"’, ‘"sqrt"’, ‘"ceiling"’, ‘"floor"’,
#          ‘"trunc"’, ‘"cummax"’, ‘"cummin"’, ‘"cumprod"’, ‘"cumsum"’,
# ‘"acos"’, ‘"acosh"’,
#          ‘"asin"’, ‘"asinh"’, ‘"atan"’, ‘"atanh"’, ‘"exp"’, ‘"expm1"’,
#          ‘"cos"’, ‘"cosh"’, ‘"cospi"’, ‘"sin"’, ‘"sinh"’, ‘"sinpi"’,
#          ‘"tan"’, ‘"tanh"’, ‘"tanpi"’, ‘"gamma"’, ‘"lgamma"’,
#          ‘"digamma"’, ‘"trigamma"’

#' @export
dualFun1 <- function(f, df) {
  dual.f <- function(x) {
    x@d <- x@d * df(x@x)
    x@x <- f(x@x)
    x
  }
  dual.f
}

log.neper <- dualFun1(log, \(x) 1/x)

setMethod("exp", "dual", dualFun1(exp, exp))
setMethod("expm1", "dual", dualFun1(expm1, exp))
setMethod("log", "dual", function(x, base = exp(1)) log.neper(x)/log(base))
setMethod("log10", "dual", dualFun1(log10, \(x) 1/(x*log(10))))
setMethod("log2", "dual", dualFun1(log2, \(x) 1/(x*log(2))))
setMethod("log1p", "dual", dualFun1(log1p, \(x) 1/(1+x)))

