## some data exploration / hypothesis testing with porcupine species distribution model

library(raster)


## 1. are wood block detections representative of overall habitat suitability?

    ## 1a. using model created WITH wood block detections included:

      ## load habitat suitability raster
          suit <- raster('shapefiles/sdm/porc_suitability_081518_no_cat_2012to2018.tif')
    
      ## load occurrence records (OR/WA and northern CA):
          porc_occur_orwa <- readOGR(dsn = './shapefiles/observations', layer = 'orwa_occur_062218')
          porc_occur_nca <- readOGR(dsn = './shapefiles/observations', layer = 'nca_occur_013117')
          porc_occur_nca <- spTransform(porc_occur_nca, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
          
      ## load wood block detections:
          porc_wb <- readOGR(dsn = './shapefiles/wood blocks', layer = 'porc_wb_062518')  
          porc_wb <- spTransform(porc_wb, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
          porc_wb@data$source <- rep('wood_block', nrow(porc_wb@data))
          porc_wb@data$year <- rep(2016, nrow(porc_wb@data)) #they are all 2015-2018; not always clear
          
      ## combine:
          porcs <- bind(porc_occur_orwa, porc_occur_nca, porc_wb)
          #porcs <- bind(porc_occur_orwa, porc_occur_nca) ## if running w/ no wood block data
          #porcs <- porcs[porcs@data$year > 1980,] ## if running w/ only points >= 1981
          porcs <- porcs[porcs@data$year >= 2012,] ## if running w/ only points >= 2012    
      
      ## extract raster values at wood block points
          porcs_wb$suitability <- extract(suit, porc_wb)
          head(porc_wb@data)
      
          plot(suit)
          points(porc_wb)
              
      ## extract raster values at all other points
          porcs$suitability <- extract(suit, porcs)    
                    
      ## how do they compare?
          par(mfrow = c(2,1))
          hist(porc_wb$suitability, main = 'model with wood block detections included',
               xlab = 'habitat suitability at wood block detections', xlim = c(0,1))          
          hist(porcs$suitability, main = NULL, 
               xlab = 'habitat suitability at all other points', xlim = c(0,1))
    
    ##  1b. using model created WITHOUT wood block detections:
          
          suit2 <- raster('shapefiles/sdm/porc_suitability_071118_no_wb.tif')
          
          porc_wb$suitability2 <- extract(suit2, porc_wb)
          porcs$suitability2 <- extract(suit2, porcs)
          
          par(mfrow = c(2,1))
          hist(porc_wb$suitability2, main = 'model without wood block detections',
               xlab = 'habitat suitability at wood block detections', xlim = c(0,1))         
          hist(porcs$suitability2, main = NULL,
               xlab = 'habitat suitability at all other points', xlim = c(0,1))          
          

## 2. are roadkill detections representative of overall habitat suitability?
          
    ## 2a. using model created WITH roadkill detections included:
          
    ## 2b. using model created WITHOUT roadkill detections:
          

## 3. create binary habitat suitability polygon based on 90th and 95th percentiles

          nrow(porcs)
          
          porcs$suitability <- extract(suit, porcs)

          quantile(porcs$suitability, probs = c(0.05, 0.1, 0.5, 0.9, 0.95), na.rm = TRUE)
      
    ## w/ all points:    
      ## 5%: 0.468
      ## 10%: 0.505
      ## 50%: 0.690
      ## 90%: 0.775
      ## 95%: 0.843
          
    ## w/ only points > 2012 (oops):
      ## 5%: 0.431
      ## 10%: 0.431
      ## 50%: 0.731
      ## 90%: 0.775
      ## 95%: 0.916
          
    ## w/ only points >= 2012 (oops):
      ## 5%: 0.433
      ## 10%: 0.441
      ## 50%: 0.730
      ## 90%: 0.798
      ## 95%: 0.927
          
  ### 10/12/18
          
    ### now with historical (1981-2010) and recent (2012-2018) hab suitability rasters
          
       ## load habitat suitability rasters
        suit_hist <- raster('C:/Users/Cara/Documents/__PORC_WB/Maxent_results/maxent_porc_101118_no_cat_1981-2010_FINAL/porc_suitability_101118_no_cat_1981to2010.tif')
        suit_rec <- raster('C:/Users/Cara/Documents/__PORC_WB/Maxent_results/maxent_porc_101018_no_cat_2012-2018_FINAL/porc_suitability_101018_no_cat_2012to2018.tif')
        
    ## NEW: normalize each raster so that they're on the same scale
        suit_hist_norm <- (suit_hist - suit_hist@data@min) / (suit_hist@data@max - suit_hist@data@min)
        suit_rec_norm <- (suit_rec - suit_rec@data@min) / (suit_rec@data@max - suit_rec@data@min)
        
        writeRaster(suit_hist_norm, filename = 'C:/Users/Cara/Documents/__PORC_WB/Maxent_results/suit_hist_normal.tif',
                    options = 'INTERLEAVE=BAND')
        writeRaster(suit_rec_norm, filename = 'C:/Users/Cara/Documents/__PORC_WB/Maxent_results/suit_rec_normal.tif',
                    options = 'INTERLEAVE=BAND')
        
    ## then subtract to create the change raster
        change_raster <- suit_rec_norm - suit_hist_norm
        writeRaster(change_raster, filename = 'C:/Users/Cara/Documents/__PORC_WB/Maxent_results/diff_raster_normal.tif', 
                    options = 'INTERLEAVE=BAND')
        
      ## load occurrence records (OR/WA and northern CA):
        porc_occur_hist <- readOGR(dsn = './shapefiles', layer = 'all_porc_locs_1981to2010')
        porc_occur_rec <- readOGR(dsn = './shapefiles', layer = 'all_porc_locs_2012to2018')
        
      ## (REDO FROM HERE ON W/ NEW NORMALIZED RASTERS...)  
      ## extract habitat suitability at locations (NEW: from the standardized rasters)
        porc_occur_hist$suitability <- extract(std_raster_hist, porc_occur_hist)
        porc_occur_rec$suitability <- extract(std_raster_rec, porc_occur_rec)
        
      ## percentiles (HISTORICAL)
        quantile(porc_occur_hist$suitability, probs = c(0.05, 0.1, 0.5, 0.9, 0.95), na.rm = TRUE)
        
        ## 5%: 0.5167236 / 4.819978e-07 ** this means 95% of porcupine locations are included above this 
        ## 10%: 0.5343892 / 4.984762e-07
        ## 50%: 0.6629255 / 6.183743e-07
        ## 90%: 0.7610195 / 7.098761e-07
        ## 95%: 0.7723016 / 7.204000e-07
        ## (raw / normalized)
        
      ## percentiles (RECENT)
        quantile(porc_occur_rec$suitability, probs = c(0.05, 0.1, 0.5, 0.9, 0.95), na.rm = TRUE)

        ## 5%: 0.4325066 / 7.384362e-07
        ## 10%: 0.4407008 / 7.524264e-07
        ## 50%: 0.7297165 / 1.245875e-06
        ## 90%: 0.7979165 / 1.362315e-06
        ## 95%: 0.9268353 / 1.582424e-06