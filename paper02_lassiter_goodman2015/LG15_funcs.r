############ FUNCTIONS FOR LASSITER AND GOODMAN (2015) PAPER ------------------

# number of cookies eaten, priors for each value 
prior_A <- function(A){
  stopifnot(A %in% 0:6)
  if (A==0)
    return(.94) else return(.01)
}


cost_u <- function(u, force_to_be=2){
  stopifnot(u %in% c("none", "some", "all"))
  return(force_to_be)
  # technically it's this but let's keep it simple 
  # 2/3 * length(u)
}


### 2.3 LITERAL LISTENER 
# ex9, page 8 
# The literal listener L0 is defined as an agent who responds to an utterance 
# u in two steps: calculate  u , the literal interpretation of u in the 
# relevant language, and condition the prior information state on 
# the truth of  u .
L0_prob <- function(A, u){
  stopifnot(A %in% 0:6, u %in% c("none", "some", "all"))
  # probability of state A given that [[u]] is true 
  # e.g. probability that i ate 3 cookies given that 'some' is true 
  if (u=="none"){
    if (A==0) return(1) else return(0)
  }
  if (u=="some"){
    if (A==0) return(0) else return(1/6)
  }
  if (u=="all"){
    if (A==6) return(1) else return(0)
  }
}

### 2.4 SPEAKER 
S1_utility <- function(u, A){
  stopifnot(A %in% 0:6, u %in% c("none", "some", "all"))
  # utility of utterance u given state A 
  # e.g. utility of saying 'some' given i ate 3 cookies 
  l0 <- L0_prob(A, u)
  if (l0==0){
    return(0)
  } else {
    return(log(l0) - cost_u(u))
  }
}


S1_prob <- function(u, A, uAlt, alpha=4, norm=FALSE){
  # probability of choosing utterance u from Alt, given state A 
  # e.g. probability of saying 'some' given i ate 3 
  numerator <- ifelse(
    S1_utility(u, A) == 0, 0, exp(alpha * S1_utility(u, A))
  )
  
  denominator <- sum(sapply(uAlt, function(u_prime){
    ifelse(S1_utility(u_prime, A)==0, 0, exp(alpha * S1_utility(u_prime, A)))
  }))
  
  if (!norm){
    return(numerator)
  } else {
    return(numerator / denominator)
  }
}


### 2.5 PRAGMATIC LISTENER 
L1_prob <- function(A, u, uAlt, A_space, alpha=4, norm=FALSE){
  # probability of state A given utterance u, basing inference 
  # procedure on speaker probabilities. 
  # e.g. probability that i ate 3 cookies given that u is uttered 
  numerator <- S1_prob(u, A, uAlt, alpha, norm=norm) * prior_A(A)
  
  denominator <- sum((sapply(A_space, function(A_prime){
    S1_prob(u, A, uAlt, alpha, norm=norm) * prior_A(A_prime) 
  })))
  
  if (!norm){
    return(numerator)
  } else {
    return(numerator / denominator)
  }
}


cost_u <- function(u){
  return(4)
}







### PLOTTING FUNCTIONS ------

rsa_matrix_to_df <- function(rsa_matrix, response_colname="prob"){
  out <- as.data.frame(rsa_matrix) %>% 
    mutate(quantity = as.numeric(gsub("[^0-9]", "", 
                                      rownames(rsa_matrix)))) %>% 
    melt(id.vars="quantity", variable.name="word") %>% 
    mutate(word = as.character(word)) %>% 
    arrange(quantity, word) 
  # rename(semantics = "value")
  
  names(out)[names(out)=="value"] <- response_colname
  
  return(out)
}


make_rsa_plot <- function(input_matrix, output_matrix, facet_words=TRUE){
  
  input_df <- rsa_matrix_to_df(input_matrix, response_colname="semantics")
  output_df <- rsa_matrix_to_df(output_matrix, response_colname="pragmatics")
  
  input_output <- 
    full_join(input_df, output_df, by=c("quantity", "word")) %>% 
    melt(id.vars=c("quantity", "word")) %>% 
    rename(type = variable) %>% 
    mutate(type = as.character(type))
  
  input_output$type <- factor(input_output$type, 
                              levels=c("semantics","pragmatics"))
  
  if (facet_words){
    the_plot <- 
      ggplot(input_output, aes(x=quantity, y=value, color=type)) + 
      geom_point(size=rel(.5)) + 
      geom_line() + 
      scale_color_manual(values=c("blue", "orange")) + 
      facet_wrap(~word)
    
  } else {
    the_plot <- 
      ggplot(input_output, aes(x=quantity, y=value, color=word)) + 
      geom_line() + 
      facet_wrap(~type)
  }
  
  return(the_plot)
}

rsaplot <- function(smat, costs, priors, alpha, facet_words=TRUE){
  if (!all(colSums(smat)==1)){
    smat <- normalize_matrix(smat, rows_or_cols="cols")
  }
  if (sum(priors)!=1){
    priors <- normalize(priors)
  }
  pmat <- full_recursion(smat, costs, priors, alpha)
  return(make_rsa_plot(smat, pmat, facet_words=facet_words))
}


# NOTE: THIS IS MODIFIED TO FIT EQNS 10 AND 11 FROM LG15 
informativity <- function(prob, cost, alpha=1){
  # `prob` must be a probability 
  stopifnot(prob >= 0, prob <= 1)
  inform <- ifelse(prob == 0, 0, exp(alpha * (log(prob) - cost)))
  return(inform)
}

# alpha determines/affects how much the probability of an event/proposition/thingy affects the informativity
#   probz <- seq(from=0, to=1, by=.01) 
#   plot(probz, sapply(probz, function(p) informativity(prob=p, cost=1, alpha=.01)))

#   plot(probz, sapply(probz, function(p) informativity(prob=p, cost=100, alpha=1)))
# the more costly an utterance is, then the less the probability matters for its informativity 


#   sapply(probz, function(p) informativity(prob=p, cost=1, alpha=1))

#   plot(function(p) informativity(prob=p, cost=1, alpha=1), from=0, to=1)


# utility is just informativity minus cost 
# informativity is just -surprisal 
# surprsial is just -log(prob) 
# therefore: informativity is just log(prob) 
# 
#   
# insight from julian convo: 
# informativity with worlds is monotonic with the information 
# theoretic notion of informativity (defn'd via entropy) 
# 
