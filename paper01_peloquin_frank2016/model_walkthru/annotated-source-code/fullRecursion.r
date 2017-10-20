fullRecursion <- function(m, costs, priors, alpha=1){
  
  cost_mat <- matrix(
    nrow=nrow(m), ncol=ncol(m), 
    dimnames=list(paste0("row", 1:5), c("none","some","all")), 
    data=rep(costs, times=1, each=nrow(m))
  )
  
  likelihood <- t(mapply(utility, 
                         items=split(m, f=rownames(m)), 
                         costs=split(cost_mat, row(cost_mat)), 
                         alpha=alpha))
  # identical(cost_mat, costsAsMatrix)
  
  
  unnorm_posterior <- apply(likelihood, MARGIN=2, function(col) priors * col)
  
  posterior <- apply(unnorm_posterior, MARGIN=2, normVec)
  
  dimnames(posterior) <- list(rownames(m), colnames(m))
  
  return(posterior)
}




