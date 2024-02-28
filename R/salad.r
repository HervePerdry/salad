#' Salad options
#'
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
