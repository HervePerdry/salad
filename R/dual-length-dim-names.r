#' @name shape
#' @rdname shape
#' @aliases length.dual dim.dual names.dual dimnames.dual
#'
#' @title Dual objects length, dim, names and dimnames
#'
#' @description S3 methods for length, dim, names and dimnames 
#'
#' @param x a dual object
#' @param value for replacement methods, the new value
#'
#' @details As the methods `dimnames` and `dimnanes<-.dual` have been defined,
#' you can use `rownames` and `colnames` as with numeric matrices (see examples).
#'
#' @return Return values are similar to the base methods.
#'
#' @examples x <- dual( matrix(c(1,0,2,3,2,4), 2, 3) )
#' dim(x)
#' length(x)
#' rownames(x) <- c("L1", "L2")
#' x
#' d(x, "x1.1")
#'
#' # modifying dim is the recommended way to change dual object shape
#' dim(x) <- NULL
#' x
#' 
#' # back to matrix shape
#' dim(x) <- c(2, 3)
#' x
# --------------- length and dim

#' @exportS3Method length dual
length.dual <- function(x) length(x@x)

#' @rdname shape
#' @exportS3Method dim dual
dim.dual <- function(x) dim(x@x)

# !! J'avais défini la fonction `dim.dual<-`
# !! il était impossible de l'exporter
# !! Déclenche ce warning au 'R CMD check' :
# !! dim:
# !!  function(x)
# !! dim.dual<-:
# !!  function(x, value)
# !!
# !! Et j'avais dû ajouter dans zzz.r registerS3method("dim<-", "dual", `dim.dual<-`)
# !!
# !!
# !! SOLUTION 
# !! NE PAS LA DEFINIR COMME `dim.dual<-` mais comme` dim<-.dual` !!!!!
# !! Et utiliser exportS3Method comme ci dessous
# !! Idem pour les autres fonctions en <-
# !!
# !! Note: pour pouvoir utiliser la syntaxe dim.dual(x)<- dans le code
# !! il faudrait définir un alias `dim.dual<-` (cf ci dessous pour dinames)

#' @rdname shape
#' @exportS3Method 'dim<-' dual
`dim<-.dual` <- function(x, value) {
  dim(x@x) <- value
  dim_differential(x@d) <- value
  x
}

# --------------- dimnames

#' @rdname shape
#' @exportS3Method dimnames dual
dimnames.dual <- function(x) dimnames(x@x)

#' @rdname shape
#' @exportS3Method 'dimnames<-' dual
`dimnames<-.dual` <- function(x, value) {
  if(!is.null(dim(x))) { # matrice
    dimnames(x@x) <- value
    dimnames_differential(x@d) <- value
  }
  x
}

# this allow to use it directly in the code
# see e.g in apply dimnames_dual(x) <- (etc)
# ! ne pas l'appeler `dimnames.dual<-` pour éviter une NOTE au check...
`dimnames_dual<-` <- `dimnames<-.dual` 
# --------------- names

#' @rdname shape
#' @exportS3Method names dual
names.dual <- function(x) names(x@x)

#' @rdname shape
#' @exportS3Method 'names<-' dual
`names<-.dual` <- function(x, value) {
  if(is.null(dim(x))) { # vecteur
    names(x@x) <- value
    names_differential(x@d) <- value
  }
  x
}
