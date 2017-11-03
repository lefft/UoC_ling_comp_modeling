lefftpack::lazy_setup()
source("LG15_funcs.r")


### FIDDLING WITH LG15 EXAMPLES -----------------------------------------------

### SCALAR IMPLICATURE EXAMPLE 
u_space <- c("none", "some", "all") # the space of u's (words)
A_space <- c(0, 1, 2, 3, 4, 5, 6) # the space of A's (num cookies)

### p11
# - A = 0 
L0_prob(A=0, u="none") == 1
L0_prob(A=0, u="some") == 0
L0_prob(A=0, u="all") == 0

S1_prob(u="some", A=1, uAlt=u_space)

# PATTERN IS RIGHT BUT 0/1 FLIPPED! 
sapply(u_space, function(u){
  sapply(A_space, function(A){
    S1_prob(u=u, A=A, uAlt=u_space, alpha=4)
  })
})

# THE WRONG SIGN IN HERE THIS SEEMS TO BE THE CULPRIT(?!) 
# (but also qualitatively seems weird...)
sapply(u_space, function(u){
  sapply(A_space, function(A){
    S1_utility(u, A)
  })
})

# SAME SIGN PROBLEM HERE 
# probability of A given that u is true 
sapply(u_space, function(u){
  sapply(A_space, function(A){
    try(L1_prob(A=A, u=u, uAlt=u_space, A_space=A_space, alpha=4, norm=TRUE))
  })
}) %>% set_rownames(paste0(0:6, "_cookies"))



### REPRODUCE SOME EXAMPLES FROM THE PAPER 
# u="none"; A=0
eqn16 <- function(u, A){
  if (L0_prob(A, u) == 0){
    return(0)
  } else {
    exp(4 * (log(L0_prob(A, u)) - 4))
  }
}
eqn16(u="all", A=0)
eqn16(u="some", A=0)
eqn16(u="none", A=0) # shd be 1.12e-7 ish 
# 

### [THIS BLOCK IS RIGHT BUT HAS A QUIRK]
### p12, (18)
L0_prob(A=1, u="some") # shd be 1/6 
# 
## THIS COMES OUT RIGHT (19), P13 [BUT SAYS IT IS NON-NORMALIZED...]
L1_prob(A=1, u="some", uAlt=u_space, A_space=A_space, norm=TRUE) # .01 
S1_prob(u="some", A=1, uAlt=u_space, norm=FALSE) # shd be 8.68327e-11
eqn16(u="some", A=1) 


## BUT THIS DOESNT... 
# L1_prob(A=0, u="some", uAlt=u_space, A_space=A_space, norm=TRUE)
# S1_prob(u="some", A=0, uAlt=u_space, norm=FALSE)


### [BOTTOM OF PAGE 13] 
# PERFECT EXCEPT THAT NONE IS 1 INSTEAD OF ZERO?!?! 
# (MORE GENERALLY, 0/1'S GET FLIPPED -- REVISIT)
sapply(u_space, function(u) S1_prob(u, A=6, uAlt=u_space, norm=FALSE))
sapply(u_space, function(u) S1_prob(u, A=6, uAlt=u_space, norm=TRUE))
sapply(u_space, function(u) S1_prob(u, A=6, uAlt=u_space, norm=FALSE))

S1_prob(u="none", A=6, uAlt=u_space, norm=TRUE) # 1 if F, .999 if T ... 

ps1(u="none", A=6, uAlt=u_space)
#

# alternative formulation of speaker prob 
ps1 <- function(u, A, uAlt){
  exp(alpha * (log(L0_prob(A, u)) - 4)) / 
    sum(sapply(uAlt, function(u_prime){
      exp(alpha * (log(L0_prob(A, u_prime)) - 4))
    }))
}

# 


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




### START ADJECTIVES EXAMPLE HERE ... 






### SCRATCH AREA --------------------------------------------------------------
### SCRATCH AREA --------------------------------------------------------------
### SCRATCH AREA --------------------------------------------------------------

Vectorize(L0_prob)(A_space, "none")
Vectorize(L0_prob)(A_space, "some")
Vectorize(L0_prob)(A_space, "all")


# p12, suppose n=0
# P1_prob("none", 0) ~~ exp(4 * (log(1) - 4) )
# paper says: exp(4 * (log(1)-4))
exp(4 * (log(1)-4))
S1_prob(u="none", A=0, alpha=4, Alt=u_space, norm=TRUE)




### WHOA...SERIOUS NAMESPACE PROBLEM WITH ALPHA HERE...WATCH OUT FOR `scales::`
full_recursion(semantics_matrix, costs=costs, priors=priors, alpha=4)



heights <- seq(from=60, to=80, by=1)
height_prior <- function(height){
  pnorm(height, mean=70, sd=3)
}
height_prior(70) 

