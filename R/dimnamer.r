dimnamer <- function(dims) {
  if(length(dims) == 1) 
    as.character(seq_len(dims[1]))
  else
    as.vector( outer(sprintf("%d.",seq_len(dims[1])), dimnamer(dims[-1]), paste0 ) )
}
