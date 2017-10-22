informativity <- function(m_u, cost, alpha){
  # `m_u` must be a probability, `alpha` a positive scalar 
  inform <- ifelse(m_u == 0, 0, exp(-alpha * (-log(m_u) - cost)))
  return(inform)
}

# m_us <- seq(from=0, to=1, by=.1)
# alphas <- seq(from=1, to=10, by=2)
# costs <- seq(from=0, to=5, by=1)
# 
# n <- 100
# 
# use combn() or outer() to put together??
# then plot it by varying each param 

# 
# function (m_u, alpha = 1, cost = 0) 
# {
#   if (m_u < 0 | m_u > 1) 
#     stop("Invalid semantic 'm_u' value, must be between [0, 1]")
#   if (alpha < 0) 
#     stop("Invalid alpha, must be a positive, non-zero numerical expression")
#   ifelse(m_u == 0, 0, exp(-alpha * (-log(m_u) - cost)))
# }
# 
