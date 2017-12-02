### FUNCTIONS FROM FRANK & GOODMAN (2012) SCIENCE PAPER 

### posterior -----------------------------------------------------------------
prob_ref_given_word_in_context <- function(object, word, objects, words){
  message("calculate posterior density:\n", 
          "  >> eqn1(",word,", ",object,", objects, words)\n")
  
  message("==========================================================\n",
          "we will multiply the likelihood by the obj prior:\n", 
          "    prob(", word, "|", obj, ") * prob(", obj, ") \n\n", 
          "and divide by the sum of lhoods for ", 
          word, " for all objects:\n",
          "    sum o in objects: prob(", word, "|o)\n", 
          "==========================================================\n")
  eqn1(object=object, word, objects, words)
}

prob_word_given_ref_in_context <- function(word, object, objects, words){
  message("calculate likelihood:\n", 
          "  >> eqn2(", word, ", ", object, ", objects, words)")
  eqn2(word, object, objects, words)
}




# eqn1(obj, word, objects, words) = 
#      (eqn2 applied to the args) * prior(obj, objects) 
#   ------------------------------------------------------
#   sum for i in objects: [eqn2(word, i, objects, words)]
eqn1 <- function(object, word, objects, words){
  
  # to get the posterior prob of object given word:
  
  # calculate the likelihood -- prob of word given object (equation 2)
  likhood <- eqn2(word, object, objects, words) 
  
  # calculate the prior prob of object 
  prior <- prior_obj(object, objects)
  
  # multiply the likelihood and prior 
  numer <- likhood * prior 
  
  # calculate the likelihood for `word` for every object, and sum them up 
  denom <- sum(
    sapply(objects, function(r_prime){
      eqn2(word, r_prime, objects, words)
    })
  )
  
  # normalize (likhood*prior) by the sum of the likelihoods 
  out <- numer/denom
  
  # show the final calculation 
  message(">> numer of eqn1:\n    (likhood * obj_prior) = ", numer)
  message(">> denom of eqn1:\n    sum of the (likhood * obj_priors) = ", denom)
  message(">> value of eqn1 for ", word, " and ", object, 
          "\n  >> numer/denom = ", out, "\n")
  
  return(out)
}




### prior ---------------------------------------------------------------------
prior_word <- function(word, words){
  # message("getting word prior for ", word)
  (length(word) / length(words))
}
prior_obj <- function(object, objects){
  # message("getting object prior for ", object)
  (length(object) / length(objects))
}




### likelihood ----------------------------------------------------------------

# eqn2(w, obj, objs, ws) = 
#      (1 / number of o in objs that 'word' applies to)
#   ------------------------------------------------------
#   sum for w' in words s.t. w'(obj)=True: [1 / num o in objs w' applies to]
eqn2 <- function(word, object, objects, words){
  
  # to calculate the prob of word given object (likelihood), 
  
  # get the number of objects the word applies to, and divide 1 by it 
  numer <- length(extension(word, objects))^-1
  # message("  numer of eqn2 for ", word, " and ", object, 
  #         "\n    |w|^-1 = ", numer)
  
  # get the set W of all words applying to object
  W <- words_applying_to_obj(words=words, object, objects) 
  
  # sum up for w' in W: the number of objects w' applies to divided by 1
  denom <- sum(sapply(W, function(w_prime){
    length(extension(w_prime, objects))^-1
  }))
  # message("  denom of eqn2 for ", word, " and ", object, 
  #         "\n    SUM_w' |w'|^-1 = ", denom)
  
  # divide the number of objects word applies to by the sum across W 
  out <- numer / denom
  message("value of likelihood (eqn2) for '", word, "' and '", object, "':",
          "\n  |", word, "|^-1 / (SUM_w': |w'|^-1) = ", 
          round(numer, 4), " / ", round(denom, 4), " = ", round(out, 4), "\n")
  # message("  value of eqn2 for ", word, " and ", object, 
  #         "\n    >> numer/denom = ", out, "\n")
  return(out)
}


# NOTE: THIS IS MODIFIED TO FIT EQNS 10 AND 11 FROM LG15 
informativity <- function(prob, cost, alpha=1){
  message("watch out -- informativity() conforms to LG15 pattern, not GF12")
  # `prob` must be a probability 
  stopifnot(prob >= 0, prob <= 1)
  inform <- ifelse(prob == 0, 0, exp(alpha * (log(prob) - cost)))
  return(inform)
}






### interpretive machinery ----------------------------------------------------

words_applying_to_obj <- function(words, object, objects){
  # message(paste0("\ngetting set of words that apply to `", object, "`"))
  out <- c()
  for (w in words){
    if (satisfies(object, w, objects)){
      out <- c(out, w)
    }
  }
  return(out)
}
extension <- function(word, objects){
  # message("called: extension(", word, ")")
  objects[which(grepl(word, objects))]
}
satisfies <- function(object, word, objects){
  stopifnot(object %in% objects) # o is an object in objects
  # message("called: satisfies(", object, ", ", word, ", objects)")
  return(object %in% extension(word, objects))
}



### misc small funcs to make process more transparent -------------------------
e <- function(digits=3){round(exp(1), digits=digits)}
random_utterance <- function(words){sample(words, size=1)}
random_referent  <- function(objects){sample(objects, size=1)}














