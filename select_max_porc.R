## Porcupine SDM using occurrence records from Oregon and Washington

library(rgdal) 
library(raster)
library(googlesheets)
library(prism)


## 1. LOAD PRESENCE POINTS

    ## occurrence records (OR/WA and northern CA):
      porc_occur_orwa <- readOGR(dsn = './shapefiles/observations', layer = 'orwa_occur_052218')
      porc_occur_nca <- readOGR(dsn = './shapefiles/observations', layer = 'nca_occur_013117')
        porc_occur_nca <- spTransform(porc_occur_nca, CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
        porc_occur_nca@proj4string <- porc_occur_orwa@proj4string    
    
    ## wood block detections:
      porc_wb <- readOGR(dsn = './shapefiles/observations', layer = 'porc_wb_pts')  
        porc_wb <- spTransform(porc_wb, CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
        porc_wb@proj4string <- porc_occur@proj4string
    
    ## combine:
      porcs <- bind(porc_occur_orwa, porc_occur_nca, porc_wb)
    
      
## 2. GENERATE BACKGROUND POINTS WITHIN STUDY AREA
  
    aoi <- readOGR(dsn = './shapefiles/admin', layer = 'OR_WA_NCA_dissolved')
    aoi@proj4string <- porcs@proj4string
        
    bg.pts <- spsample(aoi, n = 10000, type = 'random')
      
  
## 3. COMBINE PRESENCE/BACKGROUND POINTS
      
    porc.pts <- as.data.frame(porcs)
    porc.pts$pres <- rep(1, nrow(porc.pts)) # 1 = presence
    porc.pts <- porc.pts[,c(353,351:352)] # keep only pres, lon, lat
    colnames(porc.pts) <- c('pres', 'x', 'y')
      
    bg.pts <- as.data.frame(bg.pts)
    bg.pts$pres <- rep(0, nrow(bg.pts)) # 0 = background

    all.pts <- rbind(porc.pts, bg.pts)
      head(all.pts)
      table(all.pts$pres)
      
  # convert back to SPDF
    sp.all.pts <- SpatialPointsDataFrame(data.frame(all.pts$x, all.pts$y), data = all.pts, 
                                           proj4string = porcs@proj4string)
      
  
## 4. LOAD PREDICTORS & EXTRACT VALUES
   
    ## a. distance to water (Euclidean Distance raster calculated from TIGER lines in ArcMap)
  
          water <- raster('shapefiles/ORWANCA_riv.tif')
      
    ## b. PRISM climate data
            
          options(prism.path = './shapefiles/prism')
          get_prism_normals(type = 'ppt', resolution = '4km', annual = TRUE, keepZip = FALSE)            
        
          ppt <- prism_stack(ls_prism_data()[1,1]) ##raster file of data
            
          ppt <- crop(ppt, water)
          water <- resample(water, ppt)          
          
    ## c. NLCD data
            
        ## I clipped this in ArcMap because the file was really big. 
        ## Is there a better way to deal with large rasters like this in R?
          
          land_cover <- raster('shapefiles/nlcd/nlcd_2011_landcover_clip.img')
          
          lc <- crop(land_cover, ppt)   ## need to be same projection   
          lc <- projectRaster(land_cover, ppt) ## too big. should I re-project in ArcMap too?
            
  ## extract raster values

      sp.all.pts$water_dist <- extract(water_dist, sp.all.pts)
      sp.all.pts$ppt_annual <- extract(ppt_annual, sp.all.pts)
        sp.all.pts$ppt_annual <- as.numeric(sp.all.pts$ppt_annual) ## why is it a matrix?
      
     
  ## simplify dataframe (pres, predictor values)
      
      cur.data <- data.frame(sp.all.pts@data[,c(1,4:5)])
      
  
 data <- cur.data
# cor.thresh = 0.5
# regMult = c(seq(0.5, 3, by = 0.5))
# classes = "default"
# testClasses = TRUE
# out = c("model", "table")
# anyway = TRUE
# verbose = TRUE
# scratchDir=NULL
# resp = names(data)[1]
# preds = names(data)[2:ncol(data)]
 path = getwd()

 
selectMax <- function (data, resp = names(data)[1], preds = names(data)[2:ncol(data)], 
                       cor.thresh = 0.5,
                       regMult = c(seq(0.5, 3, by = 0.5)), classes = "default", testClasses = TRUE, 
                       out = c('model', 'table'), anyway = TRUE, verbose = TRUE, scratchDir='C:/Users/Cara/Documents/porc_wb_dist/model_temp', 
                       path = getwd()) 
{
  if (class(resp) %in% c("integer", "numeric")) 
    resp <- names(data)[resp]
  if (class(preds) %in% c("integer", "numeric")) 
    preds <- names(data)[preds]
  
  scratchDir <- if (is.null(scratchDir)) {
    base::tempfile(pattern = "/_maxentTempFiles/")
  } else {
    base::tempfile(pattern = "/_maxentTempFiles/", tmpdir = scratchDir)
  }
  dirCreate(scratchDir)  ## Error in selectMax(data) : could not find function "dirCreate" (should be dir.create?)
  
  
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
