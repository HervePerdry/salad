# compute adjugate matrix of a singular matrix, using
# adj( U D V' ) = adj(V') adj(D) adj(U)
adjugate <- function(x) {
  s <- svd(x)
  solve( t(s$v), prodskip1(s$d) * solve(s$u) )
}

# tadjugate(x) = t(adjugate(x))
tadjugate <- function(x) {
  s <- svd(x)
  solve( t(s$u), prodskip1(s$d) * solve(s$v) )
}
