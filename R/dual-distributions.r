#' @export
dnorm.dual <- function(x, mean = 0, sd = 1, log = FALSE) {
  if(log) {
    -0.918938533204673 - log(sd) - 0.5*((x - mean)/sd)^2
  } else {
    0.398942280401433/sd *exp(-0.5*((x - mean)/sd)^2)
  }
}

#' @export
dnorm <- function(x, mean = 0, sd = 1, log = FALSE) {
  if(is(x, "dual") | is(mean, "dual") | is(sd, "dual"))
    dnorm.dual(x, mean, sd, log)
  else
    stats::dnorm(x, mean, sd, log)
}

