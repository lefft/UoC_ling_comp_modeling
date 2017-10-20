


# function that ...
rsa.tuneDepthAlpha <- function(
  data, quantityVarName, semanticsVarName, itemVarName, 
  groupName = NA, compareDataName, costsVarName = NA, priorsVarName = NA, 
  depths = 1, alphas = 1, compareIndices = NA, usePriorEveryRecurse = TRUE
){
  # initialize empty df object 
  cors <- data.frame(cor = NA, depth = NA, alpha = NA)

  # if `groupName` is NA, then nothing happens and empty df is returned 
  if (!is.na(groupName)) {

    # for each `a` in `alphas` 
    for (a in alphas) {

      # and each `d` in `depths` 
      for (d in depths) {

        # execute an RSA iteration on `data` with `depth=d` and `alpha=a` 
        # (can also do this by-groups, if `groupName` is specified)
        currRun <- plyr::ddply(data, .fun = rsa.runDf, 
                               .variables = c(groupName), 
                               quantityVarName = quantityVarName, 
                               semanticsVarName = semanticsVarName, 
                               itemVarName = itemVarName, 
                               costsVarName = costsVarName, 
                               priorsVarName = priorsVarName, 
                               depth = d, alpha = a, 
                               usePriorEveryRecurse = usePriorEveryRecurse)

        # if `compareIndices` is NA, then compute correlation between 
        # entire `compareDataName` and model predictions for the current run 
        # (also recording `a` and `d` for inspection) 
        if (length(compareIndices) == 1 & is.na(compareIndices[1])) {
          res <- c(
            cor = cor(currRun[, compareDataName], currRun[, "preds"], 
                      use="pairwise.complete.obs"), 
            depth = d, 
            alpha = a
          )

          # add the correlation for this run to the correlations df
          cors <- rbind(cors, res)

        } else {
          # if `compareIndices` are specified, then cut the model output df 
          # to just those indices, then compute + record the correlation coef
          compareData <- currRun[compareIndices, ]
          res <- c(
            cor = cor(compareData[, compareDataName], compareData[, "preds"], 
                      use = "pairwise.complete.obs"), 
            depth = d, 
            alpha = a
          ) 
          # and add the coef to the correlations df 
          cors <- rbind(cors, res)
        }
      }
    }
  } 

  # remove all rows with non-existent coefficients 
  cors <- na.omit(cors)

  # return the correlations df, sorted by coef (descending) 
  return(cors[order(-cors$cor), ])
}

