lefftpack::lazy_setup() # dplyr, magrittr, reshape2, ggplot2 
source("LG15_funcs.r")


# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
### section 3 -- scalar implicature example -----------------------------------
# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~

# QUD: how many cookies did charlie eat? 

# L0/L1 = emma 
# S1    = dan (kind of!) 


u_space <- c("none", "some", "all") # the space of u's (words/utterances)
A_space <- c(0, 1, 2, 3, 4, 5, 6) # the space of A's (num cookies, aka 'n')

### (15), page 11 -- listener priors over A 
sapply(A_space, function(A) prior_A(A=A)) %>% set_names(A_space)





####### examples starting with "suppose n=0" on page 11 

### page 11-12 -- 
# to find the listener posterior for A, u, we need to know 
# the speaker likelihood for A, u' for all u' in u_space
alpha <- 4
cost <- 4
# 
# 
## CASE WHERE A = 0 (page 11-12)
A <- 0 
l0_posteriors <- sapply(u_space, function(u) L0_prob(A, u=u))
sapply(l0_posteriors, function(p) exp(4 * (log(p) - 4)))
# same as: 
sapply(u_space, function(u) S1_prob(u, A=0, uAlt=u_space))
# 
# (17), page 12 (where utterance is "some", and A=0 still)
u <- "some"
L1_prob(A=A, u=u, uAlt=u_space, A_space=A_space, alpha=4) # LHS of (17)
S1_prob(u=u, A=A, uAlt=u_space, alpha=4) *                # RHS of (17)
  prior_A(A=A)   



## CASE WHERE A=1 (page 12-13)
# (18), page 12 
A <- 1
u <- "some"
# 
L0_prob(A=A, u=u)                                              # LHS of (18) 
prior_A(A=A) /                                                 # RHS of (18) 
  sum(sapply(A_space[A_space >= A], function(A) prior_A(A)))    
# 
### derivation of speaker model (bottom of page 12)
l0_prob_A1_given_uSome <- sapply(u_space, function(u) L0_prob(A=A, u=u))
# now fead these into the speaker model 
sapply(l0_prob_A1_given_uSome, function(p){
  exp(4 * (log(p) - 4))
})
# 
# NOTE: gives us the same thing as this, which hides the cost and alpha parts
sapply(u_space, function(u) S1_prob(u=u, A=A, uAlt=u_space))
# 
# 
# (19), page 13 [NOTE ABOUT NORMALIZATION] 
# posterior probability of A=1 given u="some" 
L1_prob(A=1, u="some", uAlt=u_space, A_space=A_space, norm=TRUE)
S1_prob(u="some", A=1, uAlt=u_space, norm=TRUE) * prior_A(A=1)



## CASE WHERE A=6 (page 13-14)
A <- 6
# page 13, bullets below "this asymmetry..."
sapply(u_space, function(u) S1_prob(u=u, A=6, uAlt=u_space, norm=FALSE)) 

# page 13, bulets below "as a function of..."
sapply(u_space, function(u) S1_prob(u=u, A=6, uAlt=u_space, norm=TRUE)) 

# (20), page 14

L1_prob(A=A, u="some", uAlt=u_space, A_space=A_space, norm=TRUE)

prior_A(A=A)


# numerator of (20), RHS 
num20 <- S1_prob(u="some", A=A, uAlt=u_space, norm=TRUE) * prior_A(A=A)

# denominator of (20), RHS 
den20 <- sum(sapply(A_space, function(A){
  S1_prob(u="some", A=A, uAlt=u_space, norm=TRUE) * prior_A(A=A)
}))

num20 / den20

# quote, page 14: 
# "L 1 's posterior probabilities for the other states (1-5) are much higher, 
# around .197 each."








# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
### ANOTHER WAY TO SPECIFY THINGS FOR THE SCALAR IMPLIC EXAMPLE 
library("rrrsa")

u_space <- c("none", "some", "all")
costs <- c(4, 4, 4)

A_space <- 0:6
priors <- sapply(A_space, prior_A)

# probability of A given that u is true 
sem <- sapply(u_space, function(u){
  sapply(A_space, function(A){
    L0_prob(A, u)
  })
}) %>% set_rownames(paste0(A_space, "_cookies"))
prag <- rsa.fullRecursion(semantics_matrix, costs=costs, priors=priors, alpha=4)

make_rsa_plot(sem, prag, facet_words=FALSE)








# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
### section 4 -- scalar adjectives example ------------------------------------
# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~







