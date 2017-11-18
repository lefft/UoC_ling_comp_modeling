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
# NOTE: want to divide prior by sum of priors for all states where u is TRUE 
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
# 
# page 13, bulets below "as a function of..."
sapply(u_space, function(u) S1_prob(u=u, A=6, uAlt=u_space, norm=TRUE)) 


# (20), page 14

L1_prob(A=6, u="some", uAlt=u_space, A_space=A_space, norm=TRUE)
prior_A(A=6)


# numerator of (20), RHS 
num20 <- S1_prob(u="some", A=6, uAlt=u_space, norm=TRUE) * prior_A(A=6)

# denominator of (20), RHS 
den20 <- sum(
  sapply(A_space, function(A){
  S1_prob(u="some", A=A, uAlt=u_space, norm=TRUE) * prior_A(A=A)
  })
)

# SO THIS IS THE CORRECT ANSWER (TYPOS IN EQUATION 20)
#   - should be a sum of products (THE PRIOR TERM NEEDS TO VARY WITH N')
#   - also the numerator is only what it says if you round 
# CONSIDER THE DIFFERENCE BETWEEN: 
#   -  0 + .01 + .01 + .01 + .01 + .01 + .001   * .01 
#   - (0 + .01 + .01 + .01 + .01 + .01 + .001)  * .01 
num20 / den20


# prior for n=6 ~~~> .01
# speaker prob for u=some given A=6 ~~~> 0.00077101
# normalizing constant ~~~> 0.05000771 
(.01 * .00077101) / .05000771



# The posterior probability of n = 6, given the observed utterance SOME, is much lower for the pragmatic listener L1 than it is for a literal listener. The difference is driven by the fact that the pragmatic listener considers not only what the speaker actually chose to say, but also other things that the speaker could have chosen. The pragmatic listener reasons about the latent causes of the speaker’s observed choice, using a model which predicts what choices would likely have been observed given various possible configurations of the latent causes (states of the world). This type of reasoning derives the intuitive inference that n is probably not six when SOME is used — i.e., that some strongly implicates not all.


S1_prob(u="some", A=6, uAlt=u_space, norm=TRUE)

sapply(A_space, function(A){
  S1_prob(u="some", A=A, uAlt=u_space, norm=TRUE) * prior_A(A=A) / den20
})

# quote, page 14: 
# "L 1 's posterior probabilities for the other states (1-5) are much higher, 
# around .197 each."





# TO INTEGRATE 'MOST', NEED TO ADJUST LISTENER + PRIOR FUNCS 
# TO INTEGRATE 'MOST', NEED TO ADJUST LISTENER + PRIOR FUNCS 
# TO INTEGRATE 'MOST', NEED TO ADJUST LISTENER + PRIOR FUNCS 






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

# plot(from=50, to=90, function(x) dnorm(x, mean=70, sd=10))


# corresponds to ['none', 'some', 'all']
u_space_tall <- c("tall", "not_tall")

# corresponds to [.94,.01,...] 
prior_A_height <- function(height){
  dnorm(height, mean=70, sd=10)
}
# doesn't correspond to anything in SI example 
prior_theta_tall <- function(height){
  dunif(height, min=60, max=80)
}
is_tall <- function(height, theta_tall){
  if (height >= theta_tall) return(TRUE) else return(FALSE)
}
is_not_tall <- function(height, theta_tall){
  if (height < theta_tall) return(TRUE) else return(FALSE)
}
# aka "heights", corresponds to number of cookies [0,...,6] 
A_space_tall <- seq(from=60, to=80, by=1)


# prob of A given u and V 
L0_prob_tall <- function(A, u, V){
  # assume u == "tall"
  
  # probability of height A given that u is true 
  
}

### NEED TO ARRIVE AT SITUATION WHERE SOLID ARE DERIVED FROM DOTTED 
### [see figure 5, page21 in paper]
n <- 1e5
theta_prior <- runif(n, min=4.5, max=7.5)
theta_post <- rnorm(n, mean=6, sd=.3)
ht_prior <- rnorm(n, mean=5.8, sd=.5)
ht_post <- rnorm(n, mean=6.2, sd=.25)

figg <- data.frame(theta_prior, theta_post, ht_prior, ht_post) 

figg %>% melt(id.vars=NULL) %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(pp = ifelse(grepl("prior", variable), "prior", "posterior")) %>% 
  mutate(variable = gsub("_prior|_post", "", variable)) %>% 
  ggplot(aes(x=value, color=variable, linetype=pp)) + 
  geom_density() + 
  scale_x_continuous(limits=c(4.5, 7.5))


