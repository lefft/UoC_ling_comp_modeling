lefftpack::lazy_setup()
library("rrrsa")

source("annotated-source-code/normVec.r")
source("annotated-source-code/utility.r")
source("annotated-source-code/informativity.r")
source("annotated-source-code/fullRecursion.r")
source("annotated-source-code/reason.r")  
source("annotated-source-code/tuneDepthAlpha.r")
# source("annotated-source-code/runDf.r") # DOES NOT EXISST HYETS!


### CONTENTS OF `rrrsa::` PACKAGE ---------------------------------------------

# FIVE DATASETS
rsa_data <- list(
  # from frank et al submitted
  frank = rrrsa::d_pragmods,
  # from pelfrank16 
  pf2   = rrrsa::peloquinFrank_2Alts,
  pf3   = rrrsa::peloquinFrank_3Alts,
  pf4   = rrrsa::peloquinFrank_4Alts,
  pf5   = rrrsa::peloquinFrank_5Alts
)

# THREE HELPER FUNCTIONS
rsa_helper <- list(
  # converts vec2 to whatever type vec1 is
  convertVecType = rrrsa::rsa.convertVecType, 
  # this is just `function(x) x/sum(x)` for positive `x`
  normVec        = rrrsa::rsa.normVec, 
  # just changes desired column names 
  renameCol      = rrrsa::rsa.renameCol
)

# SIX ANALYSIS FUNCTIONS
rsa_model <- list(
  # computes informativity given params `m_u`, `alpha`, `cost`
  informativity  = rrrsa::rsa.informativity,
  # computes utility given params `items`, `costs`, `alpha`
  utility        = rrrsa::rsa.utility,
  # wrapper that runs `depth`-many iterations of `rsa.fullRecursion()` 
  reason         = rrrsa::rsa.reason,
  # wrapper around `rsa.reason` that accepts input as a data frame 
  runDf          = rrrsa::rsa.runDf,
  # explores correlation between data and model preds for various of `alpha` 
  tuneDepthAlpha = rrrsa::rsa.tuneDepthAlpha,
  # main function implementing RSA model 
  fullRecursion  = rrrsa::rsa.fullRecursion
)


column_key <- setNames(
  object=c("stars","speaker.p","words","scale","expt1","expt2"), 
  nm=c("quantityVarName", "semanticsVarName", "itemVarName", 
       "groupVarName","expt1_mean_judgment","expt2_mean_judgment")
)

# rsa_matrix=dat_post
# response_colname="pragmatics"
rsa_matrix_to_df <- function(rsa_matrix, response_colname){
  out <- as.data.frame(rsa_matrix) %>% 
    mutate(quantity = as.numeric(gsub("row", "", rownames(rsa_matrix)))) %>% 
    melt(id.vars="quantity", variable.name="word") %>% 
    mutate(word = as.character(word)) %>% 
    arrange(quantity, word) 
    # rename(semantics = "value")
  
  names(out)[names(out)=="value"] <- response_colname
  
  return(out)
}


make_rsa_plot <- function(input_matrix, output_matrix){
  
  input_df <- rsa_matrix_to_df(input_matrix, response_colname="semantics")
  output_df <- rsa_matrix_to_df(output_matrix, response_colname="pragmatics")
  
  input_output <- 
    full_join(input_df, output_df, by=c("quantity", "word")) %>% 
    melt(id.vars=c("quantity", "word")) %>% 
    rename(type = variable) %>% 
    mutate(type = as.character(type))
  
  the_plot <- 
    ggplot(input_output, aes(x=quantity, y=value, col=word)) + 
    geom_line() + 
    facet_wrap(~type)
  
  return(the_plot)
}


