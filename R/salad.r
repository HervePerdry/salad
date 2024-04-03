#' Salad options
#' 
#' @description Set or get options values for package 'salad'
#' 
#' @param ... options to be defined, using 'name = value', or name(s) of 
#' option(s) to get.
#'
#' @details Currently, only one option can be defined, \code{drop.derivatives}, 
#' which modifies the bevahiour of S3 methods \code{as.vector} and \code{as.matrix}
#' and corresponding S4 methods. 
#' The default value is set to 'TRUE', which means that
#' \code{as.vector} and \code{as.matrix} will return a 'base' objects, without
#' derivatives. Setting \code{drop.derivatives = FALSE} will make these functions
#' return an object of class dual. This might be useful to re-use exiting code, 
#' but may cause some functions to break, and should be use with care.
#' @details Use \code{salad()} to get the current value of all options, or
#' \code{salad(name)} to get the current value of a given option.
#' 
#' @return A list with the defined options, or a single element when \code{salad(name)}
#' is used.
#' @examples
#' salad("drop.derivatives")
#' x <- dual(matrix(c(1,2,3,4), 2, 2))
#' salad(drop.derivatives = FALSE)
#' as.vector(x)
#' salad(drop.derivatives = TRUE)
#' as.vector(x)
#' @export
salad <- function(...) {
  L <- list(...)
  if(length(L) == 0) return(as.list(salad.opts)) 
  naL <- names(L)
  R <- list()
  if(is.null(naL)) {
    for(x in unlist(L)) R[[x]] <- salad.opts[[x]]
    if(length(R) == 1) return(unlist(R)) else return(R)
  }
  naOpts <- ls(envir = salad.opts)
  for(i in seq_along(naL)) {
    if(naL[i] != "") {
      if(!(naL[i] %in% naOpts)) {
        warning(naL[i], " is not a known option")
        next
      }
      assign(naL[i], L[[i]], envir = salad.opts)
      R[[ naL[i] ]] <- L[[i]]
    } else {
      R[[ L[[i]] ]] <- salad.opts[[ L[[i]] ]]
    }
  }
  R
}

salad.opts <- list2env( list(drop.derivatives = TRUE) )
