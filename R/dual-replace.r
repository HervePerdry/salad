# ---------------- replace methods -------------------------
# dual / dual
setMethod("[<-", c(x = "dual", i = "index", j = "index", value = "dual"),
    function(x, i, j, ..., value) {
      V <- x@x
      V[i,j,...] <- value@x
      D <- replace_diff_ij(x@d, i, j, ..., value = value@d)
      fastNewDual(V,D)
    })

setMethod("[<-", c(x = "dual", i = "missing", j = "index", value = "dual"),
    function(x, i, j, ..., value) {
      V <- x@x
      V[,j,...] <- value@x
      D <- replace_diff_mj(x@d, j, ..., value = value@d)
      fastNewDual(V,D)
    })

setMethod("[<-", c(x = "dual", i = "index", j = "missing", value = "dual"),
    function(x, i, j, ..., value) {
      V <- x@x
      if(is.null(dim(x@x))) { # vecteur
        V[i] <- value@x
        D <- replace_diff_i(x@d, i, value = value@d)
      } else { # matrice
        if(nargs() == 3L) {  # appel x[i]<-
          V[i] <- value@x
          D <- replace_diff_i(x@d, i, value = value@d)
        } else { # appel x[i,]<-
          V[i,,...] <- value@x
          D <- replace_diff_im(x@d, i, ..., value = value@d)
        }
      }
      fastNewDual(V,D)
    })

setMethod("[<-", c(x = "dual", i = "missing", j = "missing", value = "dual"),
    function(x, i, j, ..., value) {
      V <- x@x
      V[,,...] <- value@x
      D <- replace_diff_mm(x@d, ..., value = value@d)
      fastNewDual(V,D)
    })

####### dual / numeric
setMethod("[<-", c(x = "dual", i = "index", j = "index", value = "logicalOrNumericOrArray"), 
    function(x, i, j, ..., value) {
      value <- fastNewConstant(value, varnames.dual(x))
      x[i,j,...] <- value
      x
    })

setMethod("[<-", c(x = "dual", i = "missing", j = "index", value = "logicalOrNumericOrArray"),
    function(x, i, j, ..., value) {
      value <- fastNewConstant(value, varnames.dual(x))
      x[,j,...] <- value
      x
    })

setMethod("[<-", c(x = "dual", i = "index", j = "missing", value = "logicalOrNumericOrArray"),
    function(x, i, j, ..., value) {
      value <- fastNewConstant(value, varnames.dual(x))
      if(is.null(dim(x@x))) { # vecteur
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
      value <- fastNewConstant(value, varnames.dual(x))
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
 

