#' Wrapper for optimisation with automatically computed gradient
#' 
#' @description Wrapper for calling \code{stats::optim} with a gradient
#' computed by automatic differentiation
#' 
#' @param par       Initial value
#' @param fn        Function to be minimized
#' @param ...       Further argument to be passed to 'fn'
#' @param method    Optimization method
#' @param lower,upper Bounds on the variables for 'L-BFGS-B'
#' @param control   A list of control parameters passed to `optim`
#' @param hessian   If 'TRUE' a *numerically* differentiated matrix is returned.
#' @param trace     If 'TRUE', keep trace of the visited points
#' 
#' @details The gradient of \code{fn} is computed using \code{unlist(d(fn(x)))}. It is
#' computed at the same time as \code{fn(x)}` and stored for when \code{optim} calls
#' the gradient. In most cases this should be more efficient than defining 
#' \code{gr = \(x) unlist(d(f(dual(x))))}.
#' @details Parameters 'method' 'lower' 'upper' 'control' and 'hessian' are passed directly to 
#' \code{optim}.
#' 
#' @seealso \code{\link[stats]{optim}}
#' 
#' @examples f <- function(x) (x[1] - x[2])**4 + (x[1] + 2*x[2])**2 + x[1] + x[2]
#' 
#' X <- seq(-1, 0.5, by = 0.01)
#' Y <- seq(-1, 0.5, by = 0.01)
#' Z <- matrix(NA_real_, nrow = length(X), ncol = length(Y))
#' for(i in seq_along(X)) for(j in seq_along(Y)) Z[i,j] <- f(c(X[i],Y[j]))
#' 
#' contour(X,Y,Z, levels = c(-0.2, 0, 0.3, 2**(0:6)), main = "BFGS")
#' opt <- optiWrap(c(0,0), f, method = "BFGS", trace = TRUE)
#' lines(t(opt$trace), type = "o", col = "red")
#' 
#' @export
optiWrap <- function(par, fn, ..., method = c("BFGS", "L-BFGS-B", "CG"), lower = -Inf, upper = Inf, control = list(), 
                     hessian = FALSE, trace = FALSE) {
  if("maxit" %in% names(control))
    maxit <- control$maxit
  else
    maxit <- 100

  if(is.infinite(maxit)) trace <- FALSE
  if(trace) {
    X <- matrix(NA_real_, ncol = floor(1.5*maxit), nrow = length(par))
    rownames(X) <- names(par)
  }

  # to keep track of iteration number
  i <- 0L
  # current values of x, fn(x), and gradient fn(x)
  current.x <- NA_real_
  current.fx <- NA_real_
  current.gx <- NA_real_

  FF <- function(x, ...) { 
    if(i > 0L & all(x == current.x)) { # should not happen
      return(current.fx)
    }
    i <<- i + 1L
    # update all values
    fx <- fn( dual(x), ... )
    current.x <<- x
    current.fx <<- value(fx)
    current.gx <<- unlist(d(fx))
    # keep record if asked
    if(trace) {
      if(i > ncol(X)) { # extend X
        X <<- cbind(X, matrix(NA_real_, ncol = maxit, nrow = length(par)))
      }
      X[,i] <<- current.x
    }
    current.fx
  }
  GF <- function(x, ...) { 
    if(!all(x == current.x)) { # force update.
      FF(x, ...)
    }
    current.gx
  }

  opt <- stats::optim(par, FF, GF, ..., method = match.arg(method), lower = lower, upper = upper, control = control, hessian = hessian)
  if(trace) {
    opt$trace <- X[,1:i]
  }
  opt
}
