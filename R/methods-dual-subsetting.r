# ------------------- subset methods ---------------------- 
setMethod("[", c(x = "dual", i = "index", j = "index", drop = "ANY"),
    function(x, i, j, ..., drop) {
      x@x <- x@x[i, j, ..., drop = drop]
      x@d <- x@d[i, j, ..., drop = drop]
      x
    })

setMethod("[", c(x = "dual", i = "missing", j = "index", drop = "ANY"),
    function(x, i, j, ..., drop) {
      x@x <- x@x[, j, ..., drop = drop]
      x@d <- x@d[, j, ..., drop = drop]
      x
    })

setMethod("[", c(x = "dual", i = "index", j = "missing", drop = "ANY"),
    function(x, i, j, ..., drop) {
      if(is.null(dim(x))) { # vecteur
        x@x <- x@x[i]
        x@d <- x@d[i]
      } else { # matrice
        if(nargs() == 2L) {  # appel x[i]
          x@x <- x@x[i, ..., drop = drop]
          x@d <- x@d[i, ..., drop = drop]
        } else { # appel x[i,]
          x@x <- x@x[i,, ..., drop = drop]
          x@d <- x@d[i,, ..., drop = drop]
        }
      }
      x
    })


setMethod("[", c(x = "dual", i = "missing", j = "missing", drop = "ANY"),
    function(x, i, j, ..., drop) {
      x@x <- x@x[,, ..., drop = drop]
      x@d <- x@d[,, ..., drop = drop]
      x
    })

# ---------------- replace methods -------------------------
# dual / dual
setMethod("[<-", c(x = "dual", i = "index", j = "index", value = "dual"),
    function(x, i, j, ..., value) {
      x@x[i,j,...] <- value@x
      x@d[i,j,...] <- value@d
      x
    })

setMethod("[<-", c(x = "dual", i = "missing", j = "index", value = "dual"),
    function(x, i, j, ..., value) {
      x@x[,j,...] <- value@x
      x@d[,j,...] <- value@d
      x
    })

setMethod("[<-", c(x = "dual", i = "index", j = "missing", value = "dual"),
    function(x, i, j, ..., value) {
      if(is.null(dim(x))) { # vecteur
        x@x[i] <- value@x
        x@d[i] <- value@d
      } else { # matrice
        if(nargs() == 2L) {  # appel x[i]<-
          x@x[i,...] <- value@x[i,...]
          x@d[i,...] <- value@d[i,...]
        } else { # appel x[i,]<-
          x@x[i,,...] <- value@x[i,,...]
          x@d[i,,...] <- value@d[i,,...]
        }
      }
      x
    })

setMethod("[<-", c(x = "dual", i = "missing", j = "missing", value = "dual"),
    function(x, i, j, ..., value) {
      x@x[,,...] <- value@x
      x@d[,,...] <- value@d
      x
    })


