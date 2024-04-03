# --------------- length and dim

#' @export
length.dual <- function(x) length(x@x)

#' @export
dim.dual <- function(x) dim(x@x)

# !! Si je l'exporte j'ai ce warning au 'R CMD check' :
# !! dim:
# !!  function(x)
# !! dim.dual<-:
# !!  function(x, value)
# !!
# !! De toutes façons l'export n'est pas pris en compte 
# !! il faut le register à la main, cf zzz.r 
# !! [je dois louper quelque chose d'important]

`dim.dual<-` <- function(x, value) {
  dim(x@x) <- value
  dim.differential(x@d) <- value
  x
}

# --------------- dimnames

#' @export
dimnames.dual <- function(x) dimnames(x@x)

# !! Même commentaire
`dimnames.dual<-` <- function(x, value) {
  if(!is.null(dim(x))) { # matrice
    dimnames(x@x) <- value
    dimnames.differential(x@d) <- value
  }
  x
}

# --------------- names

#' @export
names.dual <- function(x) names(x@x)

# !! Même commentaire
`names.dual<-` <- function(x, value) {
  if(is.null(dim(x))) { # vecteur
    names(x@x) <- value
    names.differential(x@d) <- value
  }
  x
}
