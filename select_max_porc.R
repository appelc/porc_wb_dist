## Porcupine SDM using occurrence records from Oregon, Washington, and northern California

## If running on vlab computer: 

##  1. Install packages:
insall.packages('dismo')
install.packages('rJava')
##  2. Download maxent.jar (from Google Drive or Maxent website) and save in 
##      C:/Program Files/R/R-3.4.4/library/dismo/java/maxent.jar
##  3. Download 'cur_data_070518.csv' from Google Drive and save in Documents
##  4. Set scratchDir = 'C:/Users/cla236/Documents' and create a folder '_maxentTempFiles' 
##      in that location 
##    (If running locally, set scratchDir = 'C:/Users/Cara/Documents/porc_wb_dist/sdm_data'
##    and change pattern = "_maxentTempFiles/" to "_maxentTempFiles\\")

library(dismo)
library(rJava)

## Import dataframe of pres/bg points with predictor values

    cur.data <- read.csv('cur_data_070518.csv')
    head(cur.data)   
    cur.data <- cur.data[,-1] #get rid of row index column
    table(cur.data$pres)
    sapply(cur.data, class)
    
    ## change integers to factors
    
      for(i in 1:ncol(cur.data)){
          if(is(cur.data[,i], 'integer')){
            cur.data[,i] <- as.factor(cur.data[,i])
          }
        }
        
      head(cur.data)
      sapply(cur.data, class)

    ## Remove rows with NAs
    
        sapply(cur.data, function(y) sum(length(which(is.na(y)))))
        cur.data <- cur.data[complete.cases(cur.data),] 
        table(cur.data$pres) #removed 37 rows with NAs

    ## try with only categorical predictors
        
        cur.data <- cur.data[,c(1:6)]
        head(cur.data)
        setwd('C:/Users/cla236/Documents/porc_maxent_071018_wo_cat')        
        
data <- cur.data
cor.thresh = 0.5
regMult = c(seq(0.5, 3, by = 0.5))
classes = "default"
testClasses = TRUE
out = c("model", "table")
anyway = TRUE
verbose = FALSE
scratchDir = 'C:/Users/cla236/Documents/'
resp = names(data)[1]
preds = names(data)[2:ncol(data)]
path = getwd()


selectMax <- function (data, resp = names(data)[1], preds = names(data)[2:ncol(data)], 
                       cor.thresh = 0.5,
                       regMult = c(seq(0.5, 3, by = 0.5)), classes = "default", testClasses = TRUE, 
                       out = c('model', 'table'), anyway = TRUE, verbose = FALSE, 
                       scratchDir = 'C:/Users/cla236/Documents', path = getwd()) 
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
  dir.create(scratchDir)  #got error: could not find function "dirCreate"; changed to 'dir.create'
  
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


porc_max <- selectMax(data)

porc_max$model ## view top model
head(porc_max$tuning) ## view model selection table

write.csv(porc_max$tuning, 'selection_table_071018_no_cat.csv')

numeric.cors ## which predictors were correlated?


## Create habitat suitability raster

    ## we need the raster stack of predictors (from porc_sdm_prep script)

    predictor_stack <- stack('predictors_stack.tif') ## how to get names to import?
    
    plot(predictor_stack)

    suit_raster <- dismo::predict(porc_max$model, predictor_stack, progress = 'text',
                                  filename = 'porc_suitability_071018.png')
    plot(suit_raster)
    
    writeRaster(suit_raster, filename = 'porc_suitability_071018.tif')
    
    
