###### Script to calculate AICc for Maxent model

calc_maxent_AIC <- function(max.obj, max.model, occ.pts){
  
  # standardize Maxent output so cells sum to 1:
  std.raster1 <- max.model/cellStats(max.model, sum)
  
  # extract values of raster at all occurrence points
  like1 <- extract(std.raster1, occ.pts)
  
  # Calculate the log likelihood by taking the log of each of those
  #   values. Please note that Warren and Seifer (2011) do not 

  loglike1 <- log(like1)
  
  # and to get the overall likelihood by adding those together:
  overall1 <- sum(loglike1, na.rm=TRUE)
  
  # Extract the "lambda" parameters from the model
  # to estimate 'k' (the number of parameters):

  lambdas1 <- strsplit(max.obj@lambdas, ",")
  correct.rows <- sapply(lambdas1, length) == 4
  correct.lambdas1 <- lambdas1[correct.rows]
  is.it.zero <- NULL
  for(i in 1:length(correct.lambdas1)){
    is.it.zero[i] <- correct.lambdas1[[i]][2] == " 0.0"
  }
  sum(is.it.zero)
  k1 <- sum(!is.it.zero)
  
  n1 <- nrow(occ.pts)
  
  # Okay, we've got 'k', we've got the log-likelihood,
  # and we've got 'n'. Calculate AICc:
  
  AICc1 <- 2*k1 - 2*overall1 + (2*k1*(k1+1))/(n1-k1-1)
  aic.table <- data.frame(n1, k1, overall1, AICc1)
  colnames(aic.table) <- c("n", "k", "ll", "aic")
  return(aic.table)
}