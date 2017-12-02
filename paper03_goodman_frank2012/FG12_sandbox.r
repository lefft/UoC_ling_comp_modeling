lefftpack::lazy_setup()
source("FG12_funcs.r")

# words that could be used
words <- c("blue","green","fun",  "square","dog","owl","emu")
# objects that could be referred to
objects <- c("blue_square","blue_owl","blue_dog","blue_emu",
             "green_square","green_emu", "green_dog", 
             "fun_dog", "fun_emu", 
             "square_owl")


# get a random obj and a random utt
# obj <- random_referent(objects)  # --> "square_owl"
# utt <- random_utterance(words)   # --> "square"
# LETS JUST DECIDE TO USE THIS EXAMPLE INSTEAD: 
obj <- "square_owl"
utt <- "square"

sapply(words, function(w) length(extension(w, objects=objects)))


c(`using object:` = obj, `   using utterance:` = utt)
##################### 

# ten objects, so object prior is 1/10 = .1  
(sq_owl_prior <- prior_obj(obj, objects))

# seven words, so utterance prior is 1/7 = .143 
(sq_prior <- prior_word(utt, words))


# probability of choosing 'square' to refer to the square owl 
(likhood <- eqn2(utt, obj, objects, words))

# the normalizing constant we use to get a probability for the posterior 
(norm_constant <- sum(sapply(objects, function(o) eqn2(utt,o,objects,words))))

# now we can calculate the posterior prob of referring to the square owl 
# by saying 'square' in the context via bayes rule: 
(sq_owl_posterior <- (sq_owl_prior * likhood) / norm_constant)


# this is the same thing as just using equation 1: 
eqn1(obj, utt, objects, words)

# which is wrapped by 
prob_ref_given_word_in_context(obj, utt, objects, words)  # .0714 

square_owl_prior <- prior_obj(obj, objects)
the_likhood      <- eqn2(utt, obj, objects, words) 
norm_constant    <- sum(sapply(objects, function(o) eqn2(utt,o,objects,words)))
square_owl_prior * the_likhood / norm_constant







prob_word_given_ref_in_context("square", "square_owl", objects, words)
# just applies equation 2: 
#   >> say we have "square owl", call it SO 
#   >> there are ten words, and eight objects 
#   >> two objects are owls ("blue_owl", "square_owl")
#   >> three things are square ("blue_square", "green_square", "square_owl")
#   >> two words apply to SO ("square", "owl")
#   >> then we have: 
#         (1 / 3)  /  ((1/3) + (1/2))


# ANOTHER EXAMPLE 
# words <- c("blue","green","red",     "owl","dog")
# objects <- c("blue_dog","green_dog","red_dog","green_owl","red_owl")
# utt <- "owl"; obj <- "green_owl"
# sapply(words, function(w) length(extension(w, objects=objects)))



