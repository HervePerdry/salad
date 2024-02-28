## ------------------- subsetting methods ---------------------- 
sub_diff_mm <- function(x, ..., drop = TRUE) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(k in seq_along(x)) L[[k]] <- x[[k]][,, ..., drop = drop]
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

sub_diff_mj <- function(x, j, ..., drop = TRUE) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(k in seq_along(x)) L[[k]] <- x[[k]][, j, ..., drop = drop]
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

sub_diff_i <- function(x, i) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(k in seq_along(x)) L[[k]] <- x[[k]][i]
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

sub_diff_im <- function(x, i, ..., drop = TRUE) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(k in seq_along(x)) L[[k]] <- x[[k]][i,,..., drop = drop]
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

sub_diff_ij <- function(i, j, ..., drop = TRUE) {
  x <- unclass(x)
  L <- vector("list", length(x))
  for(k in seq_along(x)) L[[k]] <- x[[k]][i, j, ..., drop = drop]
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

#' @export
`[.differential` <- function(x, i, j, ..., drop = TRUE) {
  x <- unclass(x)
  L <- vector("list", length(x))
  mi <- missing(i)
  mj <- missing(j)
  if(mi) { 
    if(mj) { # i, j manquants
      for(k in seq_along(x)) 
        L[[k]] <- x[[k]][,, ..., drop = drop]
    } else { # i manquant, j présent
      for(k in seq_along(x)) 
        L[[k]] <- x[[k]][, j, ..., drop = drop]
    }
  } else {
    if(mj) {
      # i présent, j missing
      if(is.null(dim.differential(x))) { # vecteur
        for(k in seq_along(x)) 
          L[[k]] <- x[[k]][i]
      } else { # matrice
        if(nargs() == 2L) { # appel x[i]
          for(k in seq_along(x)) 
            L[[k]] <- x[[k]][i] 
        } else { # appel x[i,]
          for(k in seq_along(x)) 
            L[[k]] <- x[[k]][i,, ..., drop = drop]
        }
      }
    } else { # i et j presents
      for(k in seq_along(x)) 
        L[[k]] <- x[[k]][i, j, ..., drop = drop]
    }
  }
  names(L) <- names(x)
  class(L) <- "differential"
  L
}

