normVec <- function(v){
  stopifnot(is.numeric(v))
  
  if (sum(v==0)==length(v)){
    return(v)
  } else {
    # all elements of v must be positive or zero 
    return(v/sum(v, na.rm=TRUE))
  }
}

# vec <- 1:10
# sum(normVec(vec))
# plot(vec, normVec(vec))
