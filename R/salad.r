#' Salad options
#'
#' @export
salad <- function(...) {
  L <- list(...)
  naL <- names(L)
  R <- list()
  if(is.null(naL)) {
    for(x in unlist(L)) R[[x]] <- salad.opts[[x]]
    if(length(R) == 1) return(unlist(R)) else return(R)
  }
  for(i in seq_along(naL)) {
    if(naL[i] != "") { 
      assign(naL[i], L[[i]], envir = salad.opts)
      R[[ naL[i] ]] <- L[[i]]
    } else {
      R[[ L[[i]] ]] <- salad.opts[[ L[[i]] ]]
    }
  }
  R
}

salad.opts <- list2env( list(check.names = TRUE) )
