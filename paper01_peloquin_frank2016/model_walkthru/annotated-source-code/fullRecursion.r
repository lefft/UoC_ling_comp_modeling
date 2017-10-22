# semantics_matrix <- matrix(
#   nrow=5, ncol=3, dimnames=list(paste0("row", 1:5), c("none","some","all")), 
#   data=c(1.0, .00, .00, 
#          .00, .00, .00, 
#          .25, .25, .25, 
#          .25, .00, .00, 
#          .00, .00,  1)
# )
# costs <- c(none=0, some=0, all=0)
# priors <- rep(.2, 5)
# alpha <- 1
# fullRecursion(semantics_matrix, costs, priors, alpha)

fullRecursion <- function(m, costs, priors, alpha){
  
  cost_mat <- matrix(
    nrow=nrow(m), ncol=ncol(m), 
    dimnames=dimnames(m), 
    data=rep(as.numeric(costs), times=1, each=nrow(m))
  )
  
  # the likelihood function is `utility()` 
  likelihood <- t(sapply(1:nrow(m), function(row){
    utility(items=m[row, ], costs=cost_mat[row, ], alpha=alpha)
  })) 
  
  unnorm_posterior <- apply(likelihood, MARGIN=2, function(col) priors * col)
  
  posterior <- apply(unnorm_posterior, MARGIN=2, normVec)
  
  dimnames(posterior) <- list(rownames(m), colnames(m))
  
  return(posterior)
}

### THIS IS HOW IT'S DONE ORIGINALLY -- THE SAPPLY WAY IS CLEANER IMO THO 
# likelihood <- t(mapply(
#   utility, 
#   items=split(m, f=rownames(m)), 
#   costs=split(cost_mat, row(cost_mat)), 
#   alpha=alpha
# ))

# cost_mat <- matrix(
#   nrow=nrow(m), ncol=ncol(m), 
#   dimnames=list(paste0("row", 1:5), c("none","some","all")), 
#   data=rep(costs, times=1, each=nrow(m))
# )
# identical(cost_mat, costsAsMatrix)
