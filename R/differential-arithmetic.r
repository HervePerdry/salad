# ------------------- arithmetic methods ---------------------- 
# on ne fera pas d'appels +e1 ... (on ne traite pas e1 missing)
sum_diff <- function(e1, e2) {
  e1 <- unclass(e1)
  e2 <- unclass(e2)
  L <- vector("list", length(e1))
  for(i in seq_along(e1)) L[[i]] <- e1[[i]] + e2[[i]]
  names(L) <- names(e1)
  class(L) <- "differential"
  L
}

neg_diff <- function(e1) {
  e1 <- unclass(e1)
  L <- vector("list", length(e1))
  for(i in seq_along(e1)) L[[i]] <- -e1[[i]]
  names(L) <- names(e1)
  class(L) <- "differential"
  L
}

substract_diff <- function(e1, e2) {
  e1 <- unclass(e1)
  e2 <- unclass(e2) 
  L <- vector("list", length(e1))
  for(i in seq_along(e1)) L[[i]] <- e1[[i]] - e2[[i]]
  names(L) <- names(e1)
  class(L) <- "differential"
  L
}

substract_diff_ <- function(e1, e2) {
  e1 <- unclass(e1)
  L <- vector("list", length(e1))
  if(missing(e2)) { # -e1
    for(i in seq_along(e1)) L[[i]] <- -e1[[i]] 
  } else {
    e2 <- unclass(e2) 
    for(i in seq_along(e1)) L[[i]] <- e1[[i]] - e2[[i]]
  }
  names(L) <- names(e1)
  class(L) <- "differential"
  L
}

# on prendra soin de ne faire que des appels numeric * differentiel !
product_diff <- function(e1, e2) {
  e2 <- unclass(e2)
  L <- vector("list", length(e2))
  for(i in seq_along(e2)) L[[i]] <- e1 * e2[[i]]
  names(L) <- names(e2)
  class(L) <- "differential"
  L
}

# appels differential / numeric (les autres n'auraient pas de sens)
divide_diff <- function(e1, e2) {
  e1 <- unclass(e1)
  L <- vector("list", length(e1))
  for(i in seq_along(e1)) L[[i]] <- e1[[i]] / e2
  names(L) <- names(e1)
  class(L) <- "differential"
  L
}

#' @export
`+.differential` <- sum_diff

#' @export
`-.differential` <- substract_diff_

#' @export
`*.differential` <- product_diff

#' @export
`/.differential` <- divide_diff

#' @export
sum.differential <- function(..., na.rm = FALSE) { 
  x <- c.differential(...)
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(x)) L[[i]] <- sum(x[[i]])
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

# a function to compute for each variable
# V * sum_i dx[i] / x[i] where V = prod(x)
# this is the derivative of prod(x) ... 
product_deriv <- function(x, dx) {
  dx <- unclass(dx)
  V <- prod(x)
  if(V == 0) {
    a <- prodskip1(x)
  } else {
    a <- V/x
  }
  D <- vector("list", length(dx))
  for(k in seq_along(dx)) {
    D[[k]] <- sum(a * dx[[k]])
  }
  names(D) <- names(dx)
  class(D) <- "differential"
  list(V, D)
}


# ------------------- matrix arithmetic ---------------------- 
matrixProdDiNu <- function(x, y) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(i in seq_along(L)) L[[i]] <- x[[i]] %*% y
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

matrixProdNuDi <- function(x, y) {
  y <- unclass(y)
  L <- vector("list", length(y))
  for(i in seq_along(L)) L[[i]] <- x %*% y[[i]] 
  names(L) <- names(y)
  class(L) <- "differential"
  L
}

# on suppose que l'appel est soit numeric %*% diff, soit diff %*% num
# `%*%.differential` <- function(x, y) { 
#   if(class(x) == "differential")
#     matrixProdDiNu(x, y) 
#   else
#     matrixProdNuDi(x, y)
# }

# une fonction pour calculer dX Y + X dY
# voir aussi matrixprod_dd dans les méthodes pour dual
matrixProdDD <- function(X, dX, Y, dY) {
  dX <- unclass(dX)
  dY <- unclass(dY)
  L <- vector("list", length(dX))
  for(i in seq_along(L)) 
    L[[i]] <- dX[[i]] %*% Y + X %*% dY[[i]]
  
  names(L) <- names(dX)
  class(L) <- "differential"
  L
}
