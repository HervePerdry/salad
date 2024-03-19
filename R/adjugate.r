# !! the determinant of U and V are not always both 1 or both -1
# need to take this into account..

# compute adjugate matrix of a singular matrix, using
# adj( U D V' ) = adj(V') adj(D) adj(U)
adjugate <- function(x) {
  s <- svd(x)
  det(s$u) * det(s$v) * s$v %*% ( prodskip1(s$d) * t(s$u) )	
}

# tadjugate(x) = t(adjugate(x))
tadjugate <- function(x) {
  s <- svd(x)
  det(s$u) * det(s$v) * s$u %*% ( prodskip1(s$d) * t(s$v) )
}

if(FALSE) {
# this is mainly ok but bad results are obtained when the multiplicity of zero is > 1
# eg for diag(c(1,1,0,0)) : instead of getting only zeros, we get epsilons...
# perturbating on only the diagonal of U doesn't solve completely the problem
det.adjugate(x) <- function(x) {
  lux <- Matrix::lu(x, warnSing = FALSE)
  detx <- det(lux)
  if(detx != 0) {
    adj <- detx * solve(lux)
  } else {
    # small perturbation before computing adjugate
    # this is numerically stable
    # cf https://www.sciencedirect.com/science/article/pii/S0024379598100988
    # and comments https://scicomp.stackexchange.com/questions/33028/
    lux@x <- lux@x + runif(length(lux@x), 1e-14, 1e-13)  
    adj <- det(lux) * solve(lux)
  }
  list(determinant = detx, adjugate = adj)
}

}
