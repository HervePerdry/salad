# this is a workaround for getting a nice behaviour in A <- B when A is numeric and class(B) = "dual"
# Workaround : the methods for "[<-" for class numeric / array are "sealed"
# Wanted behaviour : 'promotion' of A to a dual object
#
# for some reason, exporting this function from the package breaks lots of things
# but defining it from the global environment seems to work (at least until I run in new problems)
"[<-" <- function(x, i, j, ..., value) {
  if(is(value, "dual") & is.numeric(x)) {
    x <- dual(x, varnames = varnames(value), constant = TRUE)
  }
  # dispatching...
  if(missing(i)  & missing(j))
    return(base::"[<-"(x, , , ..., value = value))
  if(missing(i)  & !missing(j))
    return(base::"[<-"(x, ,j, ..., value = value))
  if(!missing(i) & missing(j)) {
    if(nargs() == 3L)
      return(base::"[<-"(x,i, value = value))
    else
      return(base::"[<-"(x,i,,..., value = value))
  }
  # !missing(i) & !missing(j)
  return(base::"[<-"(x,i,j, ..., value = value))
}

