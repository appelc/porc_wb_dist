## some data exploration / hypothesis testing with porcupine species distribution model

library(raster)


## 1. are wood block detections representative of overall habitat suitability?

    ## 1a. using model created WITH wood block detections included:

      ## load habitat suitability raster
          suit <- raster('shapefiles/sdm/porc_suitability_081518_no_cat_recent2012.tif')
    
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
          
      ## we extracted raster values at wb locations & all other locations but not together

          nrow(porcs)
          
          porcs$suitability <- extract(suit, porcs)

          quantile(porcs$suitability, probs = c(0.05, 0.1, 0.5, 0.9, 0.95), na.rm = TRUE)
      
    ## w/ all points:    
      ## 5%: 0.468
      ## 10%: 0.505
      ## 50%: 0.690
      ## 90%: 0.775
      ## 95%: 0.843
          
    ## w/ only points >= 2012:
      ## 5%: 0.431
      ## 10%: 0.431
      ## 50%: 0.731
      ## 90%: 0.775
      ## 95%: 0.916