### ---------------------------------------------------------------------------
# this is a scratchpad for playing with ben peloquin's `rrrsa::` package. 
# the package contains data/code for peloquin + frank 2016. 
# 
# this file can be used as starter code, but a more extensive, well-organized, 
# careful, etc. walkthru will be made available in advance of the oct20 meeting
# 
#  
# NOTE: to use the `rrrsa::` package, you need to first install it. 
#       because it is not a fully vetted package (those usually live on CRAN), 
#       you will need to install it with the `install_github()` function from 
#       the `devtools::` package. 
#       aaaand as you might expect, to use that, you gotta install it first. 
# 
# 
#   - tim leffel, oct11/2017
### ---------------------------------------------------------------------------




### SETUP ---------------------------------------------------------------------

# if you don't know whether you have the `devtools::` package, then first say: 
# install.packages("devtools") 
# after that, say: 
# devtools::install_github("benpeloquin7/rrrsa")
# then you can load/attach `rrrsa::` to make its contents directly available: 
library("rrrsa")


# the package `lefftpack::` is a bundle of functions and data that i use 
# frequently. the function `lazy_setup()` loads a handful of very useful 
# packages (without startup messages), and also sets a `ggplot2::` theme 
# with some convenient defaults. 
# 
# if you wanna use the package, you can install it just like `rrrsa::` above: 
# devtools::install_github("lefft/lefftpack")
lefftpack::lazy_setup()

# alternatively, uncomment the next line to load some packages we'll use 
# library("reshape2"); library("ggplot2"); library("magrittr"); library("dplyr")




### CONTENTS OF `rrrsa::` PACKAGE ---------------------------------------------

# SIX DATASETS

# THREE HELPER FUNCTIONS

# SIX ANALYSIS FUNCTIONS




### DATASETS ------------------------------------------------------------------

# note: on any dataset `df`, start by calling `?df` to see info 

# example docs for `rrrsa::peloquinFrank_2Alts`
# 
#   This data set contains empirical literal listener semantics and pragmatic
#   judgments for entailment items (two alternatives) from Peloquin & Frank 2016
# 
#   1) quantityVarName :: "stars"
#   2) semanticsVarName :: "speaker.p"
#   3) itemVarName :: "words"
#   4) groupVarName :: "scale"
#   other) experiment number :: "exp"
#   other) Pragmatic judgments study 1 (e6), study 2 (e11)
# 
#   see also: https://github.com/langcog/scalar_implicature


# 50x7 df, data from pel+frk16, 
d2 <- rrrsa::peloquinFrank_2Alts 

# unique(d2$scale)
c("good_excellent", "liked_loved", "memorable_unforgettable",
  "palatable_delicious", "some_all")

# unique(d2$words)

d2$scale_pos <- case_when(
  d2$words %in% c("good","liked","memorable","palatable","some") ~ "low", 
  d2$words %in% c("excellent","loved","unforgettable","delicious","all") ~ "high"
)

d2summ <- d2 %>% 
  group_by(scale, words, scale_pos, stars) %>% summarize(
    # unq = lu(exp)
    mean_speaker_p = mean(speaker.p),
    mean_e11 = mean(e11), 
    mean_e6 = mean(e6)
  )

d2summ %>% melt(measure.vars=c("mean_e6", "mean_e11")) %>%  # , "mean_speaker_p"
  ggplot(aes(x=stars, y=value, shape=variable, color=scale_pos)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~scale)

d2summ %>% ggplot(aes(x=stars, y=mean_e6, color=scale_pos)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~scale)


# 75x7 df
d3 <- rrrsa::peloquinFrank_3Alts 


# 100x7 df
rrrsa::peloquinFrank_4Alts

# 125x7 df 
rrrsa::peloquinFrank_5Alts

# 136x16 df, data from frank et al. (under review)
# rrrsa::d_pragmods



### HELPER FUNCTIONS ----------------------------------------------------------

# converts vec2 to whatever type vec1 is
rrrsa::rsa.convertVecType(vec1, vec2)

# this is just `function(x) x/sum(x)` for positive `x`
rrrsa::rsa.normVec(v)

# just changes desired column names 
rrrsa::rsa.renameCol(df, currNames, replacements)




### ANALYSIS FUNCTIONS --------------------------------------------------------

### `rrrsa::rsa.informativity(m_u, alpha, cost)` -------
# ifelse(m_u == 0, 0, exp(-alpha * (-log(m_u) - cost)))
rrrsa::rsa.informativity(m_u, alpha, cost)


### `rrrsa::rsa.utility(items, costs, alpha)` ------
# rsa.normVec(mapply(rsa.informativity, items, costs, alpha = alpha))
rrrsa::rsa.utility(items, costs, alpha)


### `rrrsa::rsa.reason(m, costs, priors, depth, alpha)` ----
rrrsa::rsa.reason(m, costs, priors, depth, alpha, usePriorEveryRecurse)


### `rrrsa::rsa.runDf()` ----------------
rrrsa::rsa.runDf(data, quantityVarName, semanticsVarName, 
                 itemVarName, costsVarName, priorsVarName, depth, alpha,
                 usePriorEveryRecurse)


### `rrrsa::rsa.tuneDepthAlpha()` --------
rrrsa::rsa.tuneDepthAlpha()


### rrrsa::rsa.fullRecursion(m, costs, priors, alpha) ------
# `m` a matrix
# `costs` a vector of costs, w length == ncol(m) (defaults to zeros)
# `priors` vector of priors, w length == nrow(m) (defaults to ones)
# `alpha` scalar noise param, defaults to 1 
# returns a posterior distribution 
rrrsa::rsa.fullRecursion(m, costs, priors, alpha)

# pdist=runif; nvals=100; ncols=4; params=c(0,1)
fullrec <- function(pdist, nvals, ncols, params){
  set.seed(3369)
  m <- matrix(pdist(nvals, params[1], params[2]), ncol=ncols)
  if (min(m) < 0) message("converting negative vals to positive")
  m <- abs(m); m <- m/sum(m)
  m_post <- rrrsa::rsa.fullRecursion(m=m)
  par(mfrow=c(1,3)); print(hist(m)); print(hist(m_post)); print(plot(m, m_post))
  title(paste0("with nvals=",nvals," ncols=",ncols, 
               "\nparams=",paste(params, collapse=",")))
}
lapply(c(2,4,5,10,100), function(ncols){
  fullrec(pdist=runif, nvals=1e3, ncols=ncols, params=c(0,1))
})
lapply(c(2,4,5,10,100), function(ncols){
  fullrec(pdist=rnorm, nvals=1e3, ncols=ncols, params=c(0,1))
})




