lefftpack::lazy_setup() # dplyr, magrittr, reshape2, ggplot2 
source("LG15_funcs.r")


### FIDDLING WITH LG15 EXAMPLES -----------------------------------------------

### SCALAR IMPLICATURE EXAMPLE 
u_space <- c("none", "some", "all") # the space of u's (words)
A_space <- c(0, 1, 2, 3, 4, 5, 6)   # the space of A's (num cookies)

### LITERAL LISTENER INTERPRETATION WITH L0_prob()
### p11
# - A = 0 
L0_prob(A=0, u="none")
L0_prob(A=0, u="some")
L0_prob(A=0, u="all")


sapply(u_space, function(u){
  sapply(A_space, function(A) L0_prob(A=A, u=u))
})

### PRIOR OVER NUMBER OF COOKIES EATEN WITH A_prior()
sapply(A_space, function(A) prior_A(A=A))


### APPLY EQUATION 16 TO GET SOMETHING PROPORTIONAL TO THE SPEAKER PROB OF u|A
# eqn 10 and 11 combined (utilty plugged into 11)
eqn16 <- function(u, A){
  if (L0_prob(A, u) == 0){
    return(0)
  } else {
    exp(4 * (log(L0_prob(A, u)) - 4))
  }
}

# the unnormalized prob 
eqn16(u="none", A=0) 
S1_prob(u="none", A=0, uAlt=u_space, norm=FALSE)

# top of page12 says this should be the normalized prob of "none" given A=0
eqn16(u="none", A=0) / sum(sapply(u_space, function(u) eqn16(u=u, A=0)))
S1_prob(u="none", A=0, uAlt=u_space, norm=TRUE)



# ex18, close to bottom of p12 [correct]
L0_prob(A=1, u="some") # .166667
# bottom of page12, P_S1(SOME|n=1) ... [correct]
S1_prob(u="some", A=1, uAlt=u_space) # 8.68327e-11


# NO LONGER FLIPPED, BC WE ADDED IFELSE CLAUSE TO THE DEFN OF S1_PROB() 
# [IF YOU USE LG15 DESCRIPTION, 0 AND 1 GET FLIPPED] 
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



# THESE JUST GIVE THE PRIORS BUT WITH NA'S INSTEAD OF ZEROS WHEN NORM=TRUE, 
# BUT WHEN NORM=FALSE IT GIVES REASONABLE-LOOKING PROBABILITIES...
# GIVES REASONA
# probability of A given that u is true 
# 
# 
### THE PROBLEM IS PROBABLY THAT YOU TRY TO DIVIDE ZERO BY ZERO, WHICH 
### WILL GIVE YOU NAN'S -- SO NEED TO HAVE A CLAUSE WHICH SAYS THAT 
### IF THE NUMERATOR IS ZERO, THEN JUST RETURN ZERO, REGARDLESS OF WHETHER 
### NORM=TRUE OR NORM=FALSE
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


