# ------------------- replace methods ---------------------- 
replace_diff_mm <- function(x, ..., value) {
  x <- unclass(x)
  value <- unclass(value)
  for(k in seq_along(x)) x[[k]][,,...] <- value[[k]]
  class(x) <- "differential"
  x
}

replace_diff_mj <- function(x, j, ..., value) {
  x <- unclass(x)
  value <- unclass(value)
  for(k in seq_along(x)) x[[k]][, j,...] <- value[[k]]
  class(x) <- "differential"
  x
}

replace_diff_i <- function(x, i, value) {
  x <- unclass(x)
  value <- unclass(value)
  for(k in seq_along(x)) x[[k]][i] <- value[[k]]
  class(x) <- "differential"
  x
}

replace_diff_im <- function(x, i, ..., value) {
  x <- unclass(x)
  value <- unclass(value)
  for(k in seq_along(x)) x[[k]][i,,...] <- value[[k]]
  class(x) <- "differential"
  x
}

replace_diff_ij <- function(x, i, j, ..., value) {
  x <- unclass(x)
  value <- unclass(value)
  for(k in seq_along(x)) x[[k]][i, j,...] <- value[[k]]
  class(x) <- "differential"
  x
}

`[<-.differential` <- function(x, i, j, ..., value) { 
  mi <- missing(i)
  mj <- missing(j)
  x <- unclass(x)
  value <- unclass(value)
  if(mi) { 
    if(mj) { # i, j manquants
      for(k in seq_along(x)) x[[k]][,,...] <- value[[k]]
    } else { # i manquant, j présent
      for(k in seq_along(x)) x[[k]][, j,...] <- value[[k]]
    }
  } else {
    if(mj) {
      # i présent, j missing
      if(is.null(dim.differential(x))) { # vecteur
        for(k in seq_along(x)) x[[k]][i] <- value[[k]]
      } else { # matrice
        if(nargs() == 3L) { # appel x[i] <-
          for(k in seq_along(x)) x[[k]][i] <- value[[k]]
        } else { # appel x[i,] <-
          for(k in seq_along(x)) x[[k]][i,,...] <- value[[k]]
        }
      }
    } else { # i et j presents
      for(k in seq_along(x)) x[[k]][i, j,...] <- value[[k]]
    }
  }
  class(x) <- "differential"
  x
}

