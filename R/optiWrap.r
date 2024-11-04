

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
    # update all values
    fx <- fn( dual(x), ... )
    current.x <<- x
    current.fx <<- value(fx)
    current.gx <<- unlist(d(fx))
    # keep record if asked
    if(trace) {
      i <<- i + 1L
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

  opt <- optim(par, FF, GF, ..., method = match.arg(method), lower = lower, upper = upper, control = control, hessian = hessian)
  if(trace) {
    opt$trace <- X[,1:i]
  }
  opt
}
