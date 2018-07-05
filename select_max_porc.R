## Porcupine SDM using occurrence records from Oregon, Washington, and northern California

library(dismo)

## Import dataframe of pres/bg points with predictor values

    cur.data <- read.csv('sdm_data/cur_data_070518.csv')
    head(cur.data)   
    cur.data <- cur.data[,-1] #get rid of row index column
    table(cur.data$pres)
    sapply(cur.data, class)


data <- cur.data
cor.thresh = 0.5
regMult = c(seq(0.5, 3, by = 0.5))
classes = "default"
testClasses = TRUE
out = c("model", "table")
anyway = TRUE
verbose = TRUE
scratchDir = NULL
resp = names(data)[1]
preds = names(data)[2:ncol(data)]

 
selectMax <- function (data, resp = names(data)[1], preds = names(data)[2:ncol(data)], 
                       cor.thresh = 0.5,
                       regMult = c(seq(0.5, 3, by = 0.5)), classes = "default", testClasses = TRUE, 
                       out = c('model', 'table'), anyway = TRUE, verbose = TRUE, scratchDir = NULL, 
                       path = getwd()) 
{
  if (class(resp) %in% c("integer", "numeric")) 
    resp <- names(data)[resp]
  if (class(preds) %in% c("integer", "numeric")) 
    preds <- names(data)[preds]
  
  scratchDir <- if (is.null(scratchDir)) {
    base::tempfile(pattern = "_maxentTempFiles/")
  } else {
    base::tempfile(pattern = "_maxentTempFiles/", tmpdir = scratchDir)
  }
  dir.create(scratchDir)  
  
  presentBg <- data[, resp]
  data <- data[, preds, drop = FALSE]
  pred.classes <- unlist(lapply(data, class))
  
  all.pred.combos <- expand.grid(rep(list(c(1,0)), length(pred.classes)))
  colnames(all.pred.combos) <- names(pred.classes)
  all.pred.combos <- all.pred.combos[-which(rowSums(all.pred.combos) == 0),]
  
  
  # 1. Test correlations b/w numeric vs. numeric
  numeric.preds <- data[,pred.classes %in% "numeric"]
  numeric.cors <- cor(numeric.preds) > cor.thresh
  for(i in 1:ncol(numeric.cors)){
    for(j in 1:ncol(numeric.cors)){
      if(i < j){
        if(numeric.cors[i,j]){
          print(paste(i, j, "Too correlated!"))
          k <- 1
          while(k <= nrow(all.pred.combos)){
            cur.row <- all.pred.combos[k,]
            first.cor <- colnames(numeric.cors)[i]
            second.cor <- colnames(numeric.cors)[j]
            if(cur.row[,colnames(cur.row)==first.cor] &&
               cur.row[,colnames(cur.row)==second.cor]){
              all.pred.combos <- all.pred.combos[-k,]
            } else { k <- k + 1 }
          }
        }
      }
    }
  }
  
  # only 1 factor at a time
  for(i in 1:length(pred.classes)){
    for(j in 1:length(pred.classes)){
      if(i < j){
        if(pred.classes[i] == "factor" && pred.classes[j] == "factor"){
          k <- 1
          while(k <= nrow(all.pred.combos)){
            cur.row <- all.pred.combos[k,]
            if(cur.row[,i] && cur.row[,j]){
              all.pred.combos <- all.pred.combos[-k,]
            } else { k <- k + 1 }
          }
        }
        
      }
    }
  }
  
  # remove single predictor models
  k <- 1
  while(k <= nrow(all.pred.combos)){
    cur.row <- all.pred.combos[k,]
    if(rowSums(cur.row) == 1){
      all.pred.combos <- all.pred.combos[-k,]
    } else { k <- k + 1 }
  }
  
  presences <- data[which(presentBg == 1), ]
  if (class(presences) != "data.frame") 
    presences <- as.data.frame(presences)
  names(presences) <- names(data)
  bg <- data[which(presentBg == 0), ]
  if (class(bg) != "data.frame") 
    bg <- as.data.frame(bg)
  names(bg) <- names(data)
  
  # classesToTest = preds
  # classGrid = all.pred.combos
  
  for (thisRegMult in regMult) {
    if (verbose) {
      print(paste("Current reg:", thisRegMult, Sys.time()))
    }
    for (countCombo in 1:nrow(all.pred.combos)) {
      #print(paste("Current combo:", countCombo, "out of", 
      #            nrow(all.pred.combos), Sys.time()))
      thesePreds <- paste(preds[as.logical(unlist(all.pred.combos[countCombo, 
                                                                  ]))], collapse = " ")
      curPreds <- preds[as.logical(unlist(all.pred.combos[countCombo,]))]
      thisData <- data[,colnames(data) %in% curPreds]
      thisPresences <- presences
      thisBg <- bg
      
      params <- c(paste("betamultiplier=", thisRegMult, sep=""), "jackknife=false",
                  "outputformat=raw")
      
      trialModel <- dismo::maxent(x = thisData, p = as.vector(presentBg), 
                                  path = scratchDir, args = params)
      predPres <- dismo::predict(object = trialModel, x = thisPresences, 
                                 na.rm = TRUE, args = "outputformat=raw")
      predBg <- dismo::predict(object = trialModel, x = thisBg, 
                               na.rm = TRUE, args = "outputformat=raw")
      bgSum <- sum(predBg)
      ll <- sum(log(predPres/bgSum), na.rm = TRUE)
      K <- 0
      for (thisLambda in trialModel@lambdas) {
        if (!grepl(thisLambda, pattern = "linearPredictorNormalizer") & 
            !grepl(thisLambda, pattern = "densityNormalizer") & 
            !grepl(thisLambda, pattern = "numBackgroundPoints") & 
            !grepl(thisLambda, pattern = "entropy")) {
          split <- strsplit(thisLambda, ", ")
          paramValue <- as.numeric(split[[1]][2])
          if (paramValue != 0) 
            K <- K + 1
        }
      }
      AICc <- -2 * ll + 2 * K + (2 * K * (K + 1))/(sum(presentBg) - 
                                                     K - 1)
      thisAicFrame <- data.frame(regMult = thisRegMult, 
                                 n = sum(presentBg), model = thesePreds, logLik = ll, 
                                 K = K, AICc = AICc)
      if (exists("tuning", inherits = FALSE)) 
        tuning <- rbind(tuning, thisAicFrame)
      if (!exists("tuning", inherits = FALSE)) 
        tuning <- thisAicFrame
    }
    if (verbose) 
      omnibus::say("")
  }
  tuning <- tuning[which(tuning$n > tuning$K & tuning$K > 0), ]
  if (nrow(tuning) > 0) {
    tuning <- tuning[order(tuning$regMult, decreasing = TRUE), 
                     ]
    tuning <- tuning[order(tuning$AICc, tuning$K, tuning$regMult), 
                     ]
    tuning$deltaAICc <- tuning$AICc - min(tuning$AICc)
    tuning$relLike <- exp(-0.5 * tuning$deltaAICc)
    tuning$aicWeight <- tuning$relLike/sum(tuning$relLike)
  }
  if (verbose) {
    omnibus::say("")
    print(tuning)
    omnibus::say("")
  }
  if ("model" %in% out) {
    if (nrow(tuning) > 0) {
      
      best.preds <- unlist(strsplit(as.character(tuning$model[1]), split=" "))
      best.beta <- tuning$regMult[1]
      params <- c(paste("betamultiplier=", best.beta, sep=""), 
                  "jackknife=false", "responsecurves=true")
      print(params)
      print(best.preds)
      thisData <- data[,best.preds]
      thisPresences <- presences
      thisBg <- bg
      model <- dismo::maxent(x = thisData, p = as.vector(presentBg), 
                             removeDuplicates = FALSE, path = path, 
                             args = params)
      
    }
    if ("model" %in% out & !("table" %in% out)) {
      return(model)
    } 
    if (!("model" %in% out) & "table" %in% out) {
      return(tuning) } 
    if ("model" %in% out & "table" %in% out) {
      return(list(tuning = tuning, model = model))
    }
  }
}


selectMax(data)


## got error: could not find function "dirCreate"
## - changed 'dirCreate' to 'dir.create' on line 43 (is this right/necessary?)

## got error: Error in if (numeric.cors[i, j]) { : missing value where TRUE/FALSE needed
## -  I think it's because of all the 0s. see:

head(numeric.preds)
numeric.cors
