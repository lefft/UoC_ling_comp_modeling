
### NEED TO FINISH THIS ONE UP + THEN EVERYTHING IS RE-WRITTEN 
### NEED TO FINISH THIS ONE UP + THEN EVERYTHING IS RE-WRITTEN 
### NEED TO FINISH THIS ONE UP + THEN EVERYTHING IS RE-WRITTEN 
runDf <- function(data, quantityVarName, semanticsVarName, itemVarName, 
                  costsVarName = NA, priorsVarName = NA, depth = 1, alpha = 1, 
                  usePriorEveryRecurse = FALSE){
  
  # `c(quantityVarName, semanticsVarName, itemVarName)` must be in names(data)
  # `semanticsVarName` needs to be numeric 
  # need to specify costs column costsVarName in data
  # same thing for priors column `priorsVarName`
  
  originalData <- data
  
  matrixLabels <- c(quantityVarName, semanticsVarName, itemVarName)
  
  matrixData <- data %>% 
    dplyr::select_(quantityVarName, semanticsVarName, itemVarName) %>% 
    tidyr::spread_(itemVarName, semanticsVarName) %>% 
    as.data.frame()
  
  rownames(matrixData) <- matrixData[[quantityVarName]]
  
  matrixData <- matrixData %>% dplyr::select(-1) %>% data.matrix()
  
  if (is.na(costsVarName)){
    costs <- rep(0, length(unique(data[[itemVarName]])))
    names(costs) <- unique(data[[itemVarName]])
  } else {
    
    costsData <- data %>% 
      dplyr::select_(itemVarName, costsVarName) %>% unique()
    
    costs <- costsData[[costsVarName]]
    
    names(costs) <- costsData[[itemVarName]]
  }
  
  if (is.na(priorsVarName)){
    priors <- rep(1, length(unique(data[[quantityVarName]])))
    
    names(priors) <- unique(data[[quantityVarName]])
  } else {
    
    priorsData <- data %>% 
      dplyr::select_(quantityVarName, priorsVarName) %>% unique()
    
    priors <- priorsData[[priorsVarName]]
    
    names(priors) <- priorsData[[quantityVarName]]
    
    quantityVec <- priorsData[[quantityVarName]]
  }
  
  posteriors <- rsa.reason(
    matrixData, depth = depth, alpha = alpha, costs = costs, priors = priors,
    usePriorEveryRecurse = usePriorEveryRecurse
  )
  
  tidyPosterior <- as.data.frame(posteriors) %>% 
    dplyr::mutate(quantityVarName = rownames(posteriors)) %>% 
    tidyr::gather(itemVarName, "preds", -quantityVarName)
  
  tidyPosterior[, "quantityVarName"] <- rsa.convertVecType(
    originalData[[quantityVarName]], 
    tidyPosterior[["quantityVarName"]]
  )
  tidyPosterior[, "itemVarName"] <- rsa.convertVecType(
    originalData[[itemVarName]], 
    tidyPosterior[["itemVarName"]]
  )
  
  renamedDf <- tidyPosterior %>% rsa.renameCol(
    c("quantityVarName", "itemVarName"), 
    c(quantityVarName, itemVarName)
  )
  
  mergedData <- suppressMessages(suppressWarnings(
    dplyr::left_join(originalData, renamedDf)
  ))
  
  return(mergedData)
}
