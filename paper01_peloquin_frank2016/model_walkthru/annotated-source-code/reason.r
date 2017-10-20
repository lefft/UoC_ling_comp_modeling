# `rsa.reason()` is a wrapper function for `rsa.fullRecursion` which provides an additional `depth` parameter specifying the recursive depth during reasoning. If depth is not provided, default value is $1$.
reason <- function(m, costs, priors, alpha, depth, recycle_priors){
  depth_counter <- depth
  while (depth_counter > 0){
    if (!recycle_priors & depth_counter!=1){
      m <- fullRecursion(m, costs=costs, priors=rep(1,nrow(m)), alpha=alpha)
    } else {
      m <- fullRecursion(m, costs=costs, priors=priors, alpha=alpha)
    }
    depth_counter <- depth_counter - 1
  }
  return(m)
}



