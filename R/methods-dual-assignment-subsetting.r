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
          x@x <- x@x[i]
          x@d <- x@d[i]
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
        if(nargs() == 3L) {  # appel x[i]<-
          x@x[i] <- value@x
          x@d[i] <- value@d
        } else { # appel x[i,]<-
          x@x[i,,...] <- value@x
          x@d[i,,...] <- value@d
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

####### dual / numeric
setMethod("[<-", c(x = "dual", i = "index", j = "index", value = "logicalOrNumericOrArray"), 
    function(x, i, j, ..., value) {
      value <- dual(value, varnames = varnames(x), constant = TRUE)
      x[i,j,...] <- value
      x
    })

setMethod("[<-", c(x = "dual", i = "missing", j = "index", value = "logicalOrNumericOrArray"),
    function(x, i, j, ..., value) {
      value <- dual(value, varnames = varnames(x), constant = TRUE)
      x[,j,...] <- value
      x
    })

setMethod("[<-", c(x = "dual", i = "index", j = "missing", value = "logicalOrNumericOrArray"),
    function(x, i, j, ..., value) {
      value <- dual(value, varnames = varnames(x), constant = TRUE)
      if(is.null(dim(x))) { # vecteur
        x[i] <- value
      } else { # matrice
        if(nargs() == 3L) {  # appel x[i]<-
          x[i] <- value
        } else { # appel x[i,]<-
          x[i,,...] <- value
        }
      }
      x
    })

setMethod("[<-", c(x = "dual", i = "missing", j = "missing", value = "logicalOrNumericOrArray"),
    function(x, i, j, ..., value) {
      value <- dual(value, varnames = varnames(x), constant = TRUE)
      x[,,...] <- value
      x
    })


#########  numeric / dual 
# ceci ne fonctionne pas car les methodes sont "Sealed"

# setMethod("[<-", c(x = "numericOrArray", i = "index", j = "index", value = "dual"), 
#     function(x, i, j, ..., value) { browser()
#       x <- dual(x, varnames = varnames(value), constant = TRUE)
#       x[i,j,...] <- value
#       x
#     })
# 
# setMethod("[<-", c(x = "numericOrArray", i = "missing", j = "index", value = "dual"),
#     function(x, i, j, ..., value) {
#       x <- dual(x, varnames = varnames(value), constant = TRUE)
#       x[,j,...] <- value
#       x
#     })
# 
# setMethod("[<-", c(x = "numericOrArray", i = "index", j = "missing", value = "dual"),
#     function(x, i, j, ..., value) {
#       x <- dual(x, varnames = varnames(value), constant = TRUE)
#       if(is.null(dim(x))) { # vecteur
#         x[i] <- value
#       } else { # matrice
#         if(nargs() == 3L) {  # appel x[i]<-
#           x[i] <- value
#         } else { # appel x[i,]<-
#           x[i,,...] <- value
#         }
#       }
#       x
#     })
# 
# setMethod("[<-", c(x = "numericOrArray", i = "missing", j = "missing", value = "dual"),
#     function(x, i, j, ..., value) {
#       x <- dual(x, varnames = varnames(value), constant = TRUE)
#       x[,,...] <- value
#       x
#     })
 

