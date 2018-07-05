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


## 2. LOAD PRESENCE POINTS
      
    ## occurrence records (OR/WA and northern CA):
      porc_occur_orwa <- readOGR(dsn = './shapefiles/observations', layer = 'orwa_occur_062218')
      porc_occur_nca <- readOGR(dsn = './shapefiles/observations', layer = 'nca_occur_013117')
        porc_occur_nca <- spTransform(porc_occur_nca, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
      
    ## wood block detections:
      porc_wb <- readOGR(dsn = './shapefiles/wood blocks', layer = 'porc_wb_062518')  
        porc_wb <- spTransform(porc_wb, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        porc_wb@data$source <- rep('wood_block', nrow(porc_wb@data))
      
    ## combine:
      porcs <- bind(porc_occur_orwa, porc_occur_nca, porc_wb)
      
      
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
      distance <- dist(porc.pts[,2:3], method = 'euclidean', diag = FALSE) 
      summary(distance) ## why doesn't this work now? it worked on vlab computer
      
    ## this funtion is from package spThin (it will take a little while)
    ## try 10 reps with thin parameter = 0.8 km (800 m) to match predictor resolution
      
      thinned <- thin(porc.pts, lat.col = 'y', long.col = 'x', spec.col = 'pres',
                      thin.par = 0.8, reps = 10, locs.thinned.list.return = TRUE,
                      write.files = TRUE, max.files = 5, out.dir = 'sdm_data/',
                      out.base = 'porc_thinned', write.log.file = TRUE, 
                      log.file = 'porc_thinned_full_log_file.txt', verbose = TRUE)
      
      plotThin(thinned) ## looks like only ~2 repetitions are needed to retain max # records
      
    ## using thinning distance of 0.8 km reduced the number of points from 1293 to 1032;
    ## maybe use a larger distance like 1 km?
      
      porcs.thin <- read.csv('shapefiles/sdm/porc_thinned_thin1.csv')

      ## shouldn't matter which one I import; it only saves the ones that had max 
      ## number of points preserved (there were only 2 in this case)
      
      head(porcs.thin)
      nrow(porcs.thin)
      
      
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
      
    ## convert back to SPDF
      sp.all.pts <- SpatialPointsDataFrame(data.frame(all.pts$x, all.pts$y), data = all.pts, 
                                           proj4string = porcs@proj4string)
      
    plot(predictors[[9]])  
    points(sp.all.pts[sp.all.pts@data$pres == 1,], col = 'red')
    points(sp.all.pts[sp.all.pts@data$pres == 0,], col = 'blue')
    
    
## 4. EXTRACT RASTER VALUES
      
      names(predictors)      
      
      sp.all.pts$pred_ <- extract(predictors, sp.all.pts)  
      head(sp.all.pts@data)
      
    ## simplify dataframe (pres, predictor values)
      cur.data <- cbind(sp.all.pts@data$pres, data.frame(sp.all.pts@data$pred_))
      colnames(cur.data)[1] <- 'pres' 
      
      write.csv(cur.data, 'sdm_data/cur_data_070518.csv')
      
    ## are these all the right class? (numeric, factor, etc.) -- does it matter?

      sapply(cur.data, class)
