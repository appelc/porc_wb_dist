## Porcupine OR/WA/nCA distribution & wood block paper
## Preparing predictor rasters and occurrence data for species distribution model

library(raster)
library(rgdal)
library(spThin)

## 6/27/18: I used tools in ArcMap to project and resample/aggregate the rasters already:
##          Resample (for categorical, using 'majority')
##          Aggregate (for continuous, using either 'median' or 'min' -- see below)
##    Ideally this would all be done in R, but I wanted cell sizes/alignment to be the same,
##    and I can set cell size, projection, and snap raster in the ArcMap processing environment

## First, download 'sdm_data' and 'shapefiles' folders from Google Drive 
## and save to project folder/ working directory


## 1. IMPORT RESAMPLED PREDICTOR RASTERS

      ppt <- raster('sdm_data/ppt_800m_clip')
      rivers <- raster('sdm_data/aggregated/rivers_agg') #min
      cancovcon <- raster('sdm_data/aggregated/cancovcon_agg') #median
      cancovhdw <- raster('sdm_data/aggregated/cancovhdw_agg') #median
      tphge3 <- raster('sdm_data/aggregated/tphge3_agg') #median
      
      stack1 <- stack(ppt, rivers, cancovcon, cancovhdw, tphge3)
      
    ## for some reason the ones that were resampled don't have the same extent as the others 
      
    #  fortypba <- raster('sdm_data/aggregated/fortypba_res') ##leave this one out
      struccond <- raster('sdm_data/aggregated/struccond_res')
      vegclass <- raster('sdm_data/aggregated/vegclass_res')
      
      stack2 <- stack(struccond, vegclass)
      stack3 <- crop(stack2, ppt)
      stack4 <- stack(stack1, stack3)
      
    ## and the NLCD raster still won't stack
      
      nlcd2011_res <- raster('sdm_data/aggregated/nlcd2011_res')
      nlcd2011_res <- crop(nlcd2011_res, ppt)
      
      predictors <- stack(stack4, nlcd2011_res)
      
      nlayers(predictors)
      names(predictors)
      res(predictors) ## 800m

    ## save raster (how to get the names to save?)
      writeRaster(predictors, filename = 'predictors_stack.tif', options = 'INTERLEAVE=BAND')

## 2. LOAD PRESENCE POINTS
      
    ## occurrence records (OR/WA and northern CA):
      porc_occur_orwa <- readOGR(dsn = './shapefiles/observations', layer = 'orwa_occur_062218')
      porc_occur_nca <- readOGR(dsn = './shapefiles/observations', layer = 'nca_occur_013117')
        porc_occur_nca <- spTransform(porc_occur_nca, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
      
    ## wood block detections:
      porc_wb <- readOGR(dsn = './shapefiles/wood blocks', layer = 'porc_wb_062518')  
        porc_wb <- spTransform(porc_wb, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        porc_wb@data$source <- rep('wood_block', nrow(porc_wb@data))
        porc_wb@data$year <- rep(2016, nrow(porc_wb@data)) #they are all 2015-2018; not always clear
      
    ## combine:
      porcs <- bind(porc_occur_orwa, porc_occur_nca, porc_wb)
      #porcs <- bind(porc_occur_orwa, porc_occur_nca) ## if running w/ no wood block data
      #porcs <- porcs[porcs@data$year > 1980,] ## if running w/ only points >= 1981
      porcs <- porcs[porcs@data$year >= 2012,] ## if running w/ only points >= 2012
      
## 3. THIN PRESENCE POINTS
      
      porc.pts <- as.data.frame(porcs)
      porc.pts$pres <- rep(1, nrow(porc.pts)) # 1 = presence
      porc.pts <- porc.pts[,c('pres', 'coords.x1', 'coords.x2')]
      colnames(porc.pts) <- c('pres', 'x', 'y')
      
    ## Remove duplicates. Previously we only removed dups with same date. (Observations at 
    ## same location but different date were treated as independent observations.) But now
    ## we need to remove all dups (n = 40):
      
      porc.pts <- data.frame(porc.pts[!duplicated(porc.pts[, c('x', 'y')]),])
      nrow(porc.pts)

    ## how to find best thinning distance? look at semivariograms? what's the avg dist btwn points?
          
      head(porc.pts)
      #distance <- dist(porc.pts[,2:3], method = 'euclidean', diag = FALSE) 
      #summary(distance) ## why doesn't this work now? it worked on vlab computer
      
    ## this funtion is from package spThin (it will take a little while)
    ## try 10 reps with thin parameter = 0.8 km (800 m) to match predictor resolution
      
      thinned <- thin(porc.pts, lat.col = 'y', long.col = 'x', spec.col = 'pres',
                      thin.par = 0.8, reps = 10, locs.thinned.list.return = TRUE,
                      write.files = TRUE, max.files = 5, out.dir = 'sdm_data/',
                      out.base = 'porc_thinned_recent2012', write.log.file = TRUE, 
                      log.file = 'porc_thinned_full_log_file.txt', verbose = TRUE)
      
      plotThin(thinned) ## looks like only ~2 repetitions are needed to retain max # records
      
    ## using thinning distance of 0.8 km reduced the number of points from 1293 to 1032;
    ## maybe use a larger distance like 1 km?
      
      porcs.thin <- read.csv('sdm_data/porc_thinned_thin1.csv')
      porcs.thin.no.wb <- read.csv('sdm_data/porc_thinned_no_wb_thin1.csv') ## reduced from 1253 to 991
      porcs.thin.recent <- read.csv('sdm_data/porc_thinned_recent_thin1.csv') ## reduced from 1102 to 844
      porcs.thin.recent2012 <- read.csv('sdm_data/porc_thinned_recent2012_thin1.csv') ## reduced from 908 to 670
      
      ## shouldn't matter which one I import; it only saves the ones that had max 
      ## number of points preserved (there were only 2 in this case)
      
      head(porcs.thin)
      nrow(porcs.thin)
      
      head(porcs.thin.no.wb)
      nrow(porcs.thin.no.wb)      
      
      head(porcs.thin.recent)
      nrow(porcs.thin.recent)
      
      head(porcs.thin.recent2012)
      nrow(porcs.thin.recent2012)

## 3. GENERATE BACKGROUND POINTS WITHIN STUDY AREA
      
      aoi <- readOGR(dsn = './shapefiles/admin', layer = 'OR_WA_NCA_dissolved')
      bg.pts <- spsample(aoi, n = 10000, type = 'random')
      
      bg.pts <- as.data.frame(bg.pts)
      bg.pts$pres <- rep(0, nrow(bg.pts)) # 0 = background
      
    ## thin bg points too?
      #head(bg.pts)
      #dist.bg <- dist(bg.pts[,1:2])    
      #summary(dist.bg)
      
    ## combine presence/background points
      all.pts <- rbind(porcs.thin, bg.pts)
        head(all.pts)
        tail(all.pts)
        table(all.pts$pres) # 10000 bg, 1032 pres
        
      all.pts.no.wb <- rbind(porcs.thin.no.wb, bg.pts)
        table(all.pts.no.wb$pres)
        
      all.pts.recent <- rbind(porcs.thin.recent, bg.pts)
        table(all.pts.recent$pres)  
        
      all.pts.recent2012 <- rbind(porcs.thin.recent2012, bg.pts)
        table(all.pts.recent2012$pres)
        
    ## convert back to SPDF
      sp.all.pts <- SpatialPointsDataFrame(data.frame(all.pts$x, all.pts$y), data = all.pts, 
                                           proj4string = porcs@proj4string)
      
        plot(predictors[[9]])  
        points(sp.all.pts[sp.all.pts@data$pres == 1,], col = 'red')
        points(sp.all.pts[sp.all.pts@data$pres == 0,], col = 'blue')
    
      sp.all.pts.no.wb <- SpatialPointsDataFrame(data.frame(all.pts.no.wb$x, all.pts.no.wb$y),
                                                 data = all.pts.no.wb, proj4string = porcs@proj4string)

      sp.all.pts.recent <- SpatialPointsDataFrame(data.frame(all.pts.recent$x, all.pts.recent$y),
                                                  data = all.pts.recent, proj4string = porcs@proj4string)
      
      sp.all.pts.recent2012 <- SpatialPointsDataFrame(data.frame(all.pts.recent2012$x, all.pts.recent2012$y),
                                                      data = all.pts.recent2012, proj4string = porcs@proj4string)
      
## 4. EXTRACT RASTER VALUES
      
      names(predictors)      
      
      sp.all.pts$pred_ <- extract(predictors, sp.all.pts)  
      head(sp.all.pts@data)
      
      sp.all.pts.no.wb$pred_ <- extract(predictors, sp.all.pts.no.wb)
      head(sp.all.pts.no.wb@data)  
      
      sp.all.pts.recent$pred_ <- extract(predictors, sp.all.pts.recent)
      head(sp.all.pts.recent@data)
      
      sp.all.pts.recent2012$pred_ <- extract(predictors, sp.all.pts.recent2012)
      head(sp.all.pts.recent2012@data)  
      
    ## simplify dataframe (pres, predictor values)
      cur.data <- cbind(sp.all.pts@data$pres, data.frame(sp.all.pts@data$pred_))
      colnames(cur.data)[1] <- 'pres' 
      
        write.csv(cur.data, 'sdm_data/cur_data_070518.csv')
    
      cur.data.no.wb <- cbind(sp.all.pts.no.wb@data$pres, data.frame(sp.all.pts.no.wb@data$pred_))
      colnames(cur.data.no.wb)[1] <- 'pres'  
        
        write.csv(cur.data.no.wb, 'sdm_data/cur_data_no_wb_071118.csv')  
      
      cur.data.recent <- cbind(sp.all.pts.recent@data$pres, data.frame(sp.all.pts.recent@data$pred_))
      colnames(cur.data.recent)[1] <- 'pres'    
      
        write.csv(cur.data.recent, 'sdm_data/cur_data_recent_073018.csv')
        
      cur.data.recent2012 <- cbind(sp.all.pts.recent2012@data$pres, data.frame(sp.all.pts.recent2012@data$pred_))
      colnames(cur.data.recent2012)[1] <- 'pres'    
      
      write.csv(cur.data.recent2012, 'sdm_data/cur_data_recent2012_081518.csv')  
