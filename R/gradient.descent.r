#' Gradient descent
#'
#' @description A simple implementation of the gradient descent algorithm
#' 
#' @param par    Initial value
#' @param fn     A function to be minimized (or maximized if 'step' < 0)
#' @param ...    Further arguments to be passed to 'fn'
#' @param step   Step size. Use a negative value to perform a gradient ascent.
#' @param maxit  Maximum number of iterations
#' @param reltol Relative convergence tolerance
#' @param trace  If 'TRUE', keep trace of the visited pointed
#'
#' @details First note that this is not an efficient optimisation method. It
#' is included in the package as a demonstration only.
#' @details The function iterates \eqn{x_{n+1} = x_{n} - step \times grad f(x_n)}{x[n+1] = x[n] - step * grad(f(x[n]))}
#' until convergence. The gradient is computed using automatic differentiation. 
#' @details The convergence criterion is as in \code{optim}
#' \eqn{ \frac{ |f(x_{n+1}) - f(x_n)| }{ |f(x[n])| + reltol } < reltol }{ abs(f(x(n+1]) - f(x[n])) / (abs(f(x[n])) + reltol) < reltol }.
#' 
#' @return a list with components: 'par' is the final value of the parameter,
#' 'value' is the value of 'f' at 'par', 'counts' is the number of iterations performed,
#' 'convergence' is '0' is the convergence criterion was met. If 'trace' is 'TRUE', an
#' extra component 'trace' is included, which is a matrix giving the successive values of
#' \eqn{x_n}{x[n]}.
#' 
#' @examples f <- function(x) (x[1] - x[2])**4 + (x[1] + 2*x[2])**2 + x[1] + x[2]
#' 
#' X <- seq(-1, .5, by = 0.01)
#' Y <- seq(-0.5, 0.5, by = 0.01)
#' Z <- matrix(NA_real_, nrow = length(X), ncol = length(Y))
#' for(i in seq_along(X)) for(j in seq_along(Y)) Z[i,j] <- f(c(X[i],Y[j]))
#' 
#' par(mfrow = c(2,2), mai = c(1,1,1,1)/3)
#' contour(X,Y,Z, levels = c(-0.2, 0, 0.3, 2**(0:6)), main = "step = 0.01")
#' gd1 <- gradient.descent(c(0,0), f, step = 0.01, trace = TRUE)
#' lines(t(gd1$trace), type = "o", col = "red")
#' 
#' contour(X,Y,Z, levels = c(-0.2, 0, 0.3, 2**(0:6)))
#' gd2 <- gradient.descent(c(0,0), f, step = 0.1, trace = TRUE)
#' lines(t(gd2$trace), type = "o", col = "red")
#' 
#' contour(X,Y,Z, levels = c(-0.2, 0, 0.3, 2**(0:6)))
#' gd3 <- gradient.descent(c(0,0), f, step = 0.18, trace = TRUE)
#' lines(t(gd3$trace), type = "o", col = "red")
#' 
#' contour(X,Y,Z, levels = c(-0.2, 0, 0.3, 2**(0:6)))
#' gd4 <- gradient.descent(c(0,0), f, step = 0.2, trace = TRUE)
#' lines(t(gd4$trace), type = "o", col = "red")
#'
#' @export
gradient.descent <- function(par, fn, ..., step = 0.1, maxit = 100, reltol = sqrt(.Machine$double.eps), trace = FALSE) {
  if(is.infinite(maxit)) trace <- FALSE
  if(trace) {
    X <- matrix(NA_real_, ncol = maxit, nrow = length(par))
    rownames(X) <- names(par)
  }

  x0 <- par
  f0 <- fn(x0, ...)
  converged <- FALSE
  fmt.x <- rep("%.2e ", length(x0))
  fmt.f <- "%2.e"
  for(i in 1:maxit) {
    # cat(sprintf(fmt.x, value(x0)), ":", sprintf(fmt.f, value(f0)), "\n")
    if(trace) X[,i] <- value(x0)
    grad <- unlist(d(f0))
    x1 <- x0 - step*grad
    f1 <- fn(x1, ...)
    rel.f <- abs(value(f0) - value(f1)) / (abs(value(f0)) + reltol)
    if(is.na(rel.f)) {
      warning("Failed at x =", sprintf(fmt.x, value(x1)), sprintf("with f(x) = %.2f", value(f1)), "\nYou may try a smaller step size")
      converged <- FALSE
      break
    }
    if(rel.f < reltol) {
      converged <- TRUE
      break
    }
    x0 <- x1
    f0 <- f1
  }
  L <- list(par = value(x1), value = value(value(f1)), counts = i, convergence = as.integer(!converged))
  if(trace) L$trace <- X[,1:i]
  L
}
