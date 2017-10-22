utility <- function(items, costs, alpha){
  # costs must have same length as items, alpha must be a positive scalar 
  
  # apply `informativity()` to each item with specified costs and alpha. 
  # then collect them all and return with the same shape/order as `items`
  inform <- mapply(informativity, m_u=items, cost=costs, alpha=alpha)
  
  # normalization here is just dividing each number by the sum of them all 
  inform_norm <- normVec(inform)
  
  return(inform_norm)
}

# sapply(list(1,2,3,4), function(x) x^2)
# lapply(list(1,2,3,4), function(x) x^2)

# ?mapply()


# ### ORIGINAL 
# split(m, row(m))
# split(cost_mat, row(cost_mat))
# split(costsAsMatrix, row(costsAsMatrix))
# alpha=alpha
# 
# (function (items, costs = rep(0, length(items)), alpha = 1) 
# {
#   rsa.normVec(mapply(rsa.informativity, items, costs, alpha = alpha))
#   
#   
# })(split(m, row(m))[[1]], split(costsAsMatrix, row(costsAsMatrix))[[1]], alpha=alpha)
# 
# utility(split(m, row(m))[[1]], split(costsAsMatrix, row(costsAsMatrix))[[1]], alpha=alpha)
# 
# utility(split(m, row(m))[[1]], split(costsAsMatrix, row(costsAsMatrix))[[1]], alpha=alpha)
# 
# utility(items=split(m, row(m))[[1]], costs=split(costsAsMatrix, row(costsAsMatrix))[[1]], alpha=alpha)
# 
# rrrsa::rsa.utility(items=split(m, row(m))[[1]], costs=split(costsAsMatrix, row(costsAsMatrix))[[1]], alpha=alpha)


# (split(m, row(m))[[1]], split(m, row(m))[[1]], alpha=alpha)