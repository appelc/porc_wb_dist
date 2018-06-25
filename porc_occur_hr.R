## Porcupine MCP and KDE from all occurrence records (WA/OR/NCA)

library(rgdal)
library(adehabitatHR)


## 1. LOAD PRESENCE POINTS

    ## occurrence records (OR/WA and northern CA):
      porc_occur_orwa <- readOGR(dsn = './shapefiles/observations', layer = 'orwa_occur_060618')
      porc_occur_nca <- readOGR(dsn = './shapefiles/observations', layer = 'nca_occur_013117')
        porc_occur_nca <- spTransform(porc_occur_nca, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
      
    ## wood block detections:
      porc_wb <- readOGR(dsn = './shapefiles/observations', layer = 'porc_wb_pts')  
      porc_wb@data$type <- rep('wood_block', nrow(porc_wb@data))
      porc_wb@data$source <- porc_wb@data$Source
        porc_wb <- spTransform(porc_wb, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
      
    ## combine:
      porcs <- bind(porc_occur_orwa, porc_occur_nca, porc_wb)

    ## remove unnecessary columns & create SPDF
      porc.pts <- as.data.frame(porcs)
        porc.pts$pres <- rep(1, nrow(porc.pts))  #we don't use bg points here but 1 serves as "animal ID" for HR
        porc.pts <- porc.pts[,c(353,351:352)] 
        colnames(porc.pts) <- c('pres', 'x', 'y')
        
      porc.sp <- SpatialPointsDataFrame(data.frame(porc.pts$x, porc.pts$y), data = data.frame(porc.pts$pres), 
                                        proj4string = porcs@proj4string)

      
## 2. CALCULATE MCP
      
      mcp95 <- mcp(porc.sp, percent = 95, unin = 'm', unout = 'km2') ## unin meters?
      mcp90 <- mcp(porc.sp, percent = 90, unin = 'm', unout = 'km2')
      mcp50 <- mcp(porc.sp, percent = 50, unin = 'm', unout = 'km2')
      
      plot(mcp95, col = 'green')      
      plot(mcp50, col = 'blue', add = TRUE)
      plot(aoi, add = TRUE)

    
## 3. CALCULATE KDE

      porc.kud <- kernelUD(xy = porc.sp, h = 'href', grid = 1000, extent = 0.3) ## didn't converge with LSCV (porc.kud[[1]]@h)
      
      image(porc.kud)
      plot(aoi, add = TRUE)
      
      porc.hr <- getverticeshr(porc.kud, percent=95)
      plot(porc.hr, col = 1:2)
      
      porc.contour1 <- as.image.SpatialGridDataFrame(porc.kud[[1]])
      image(porc.kud[[1]])
      plot(aoi, add = TRUE)
      contour(porc.contour1, add=TRUE)
      
      porc.vud <- getvolumeUD(porc.kud)
      porc.contour2 <- as.image.SpatialGridDataFrame(porc.vud[[1]])
      image(porc.vud[[1]])
      plot(aoi, add = TRUE, lwd = 2)
      contour(porc.contour2, add=TRUE)
      
      porc1vud.gtiff <- create2GDAL(as(porc.vud[[1]], "SpatialPixelsDataFrame"),
                                  drivername="GTiff")
      saveDataset(porc1vud.gtiff, "porc1vud.tif")
      
      
    ## use a predictor raster to set the grid and extent parameters
    ## (convert the raster to SpatialPixelsDataFrame, then use grid = 'rastername' in kernelUD code)

##  e.g.: 
##  dens.spdf <- as(dens, "SpatialPixelsDataFrame")
##  jayssp@proj4string <- dens.spdf@proj4string		# define projection
##  jaysud.match <- kernelUD(jayssp, grid=dens.spdf)
          
      jays1a <- kernelUD(porc.sp, extent=0.2, grid=20)
      jays1b <- kernelUD(porc.sp, extent=0.2, grid=100)
      jays1c <- kernelUD(porc.sp, extent=2, grid=20)
      jays1d <- kernelUD(porc.sp, extent=2, grid=100)
      
      
    kern <- kernelUD(xy = porc.sp, h = 'href', grid = 60, extent = 1, same4all = TRUE)
    kde <- kernel.area(kern, percent = c(50, 90, 95, 99), unin = 'm', unout = 'km2', standardize = FALSE)
    cont95 <- getverticeshr.estUD(kern, percent = 95, unin = 'm', unout = 'km2', standardize = FALSE)
    
    data.frame(kde, row.names = c("50", "90", "95", "99"))
    

## 4. SOME HYPOTHESIS TESTING

    ### ARE ROADKILL WITHIN/OUTSIDE MCP OF ALL OTHER POINTS?
    
      table(porcs@data$type)
        porcs.roadkill <- subset(porcs, type == 'roadkill')
        porcs.no.roadkill <- subset(porcs, type != 'roadkill')
        
      porcs.no.roadkill <- as.data.frame(porcs.no.roadkill)
      porcs.no.roadkill$pres <- rep(1, nrow(porcs.no.roadkill))  #we don't use bg points here but 1 serves as "animal ID" for HR
      porcs.no.roadkill <- porcs.no.roadkill[,c(353,351:352)] 
      colnames(porcs.no.roadkill) <- c('pres', 'x', 'y')
        
        no.roadkill.sp <- SpatialPointsDataFrame(data.frame(porcs.no.roadkill$x, porcs.no.roadkill$y), data = data.frame(porcs.no.roadkill$pres), 
                                          proj4string = porcs@proj4string)
      
      mcp95.no.roadkill <- mcp(no.roadkill.sp, percent = 95, unin = 'm', unout = 'km2') ## unin meters?
  
      plot(mcp95.no.roadkill)
      points(porcs.roadkill, add = TRUE)      
    
    ### ARE WOOD BLOCK DETECTIONS WITHIN/OUTSIDE MCP OF ALL OTHER POINTS?
      
      porcs.wb <- subset(porcs, type == 'wood_block')
      porcs.no.wb <- subset(porcs, type != 'wood_block')
      
      porcs.no.wb <- as.data.frame(porcs.no.wb)
      porcs.no.wb$pres <- rep(1, nrow(porcs.no.wb))  #we don't use bg points here but 1 serves as "animal ID" for HR
      porcs.no.wb <- porcs.no.wb[,c(353,351:352)] 
      colnames(porcs.no.wb) <- c('pres', 'x', 'y')
      
      no.wb.sp <- SpatialPointsDataFrame(data.frame(porcs.no.wb$x, porcs.no.wb$y), data = data.frame(porcs.no.wb$pres), 
                                               proj4string = porcs@proj4string)
      
      mcp95.no.wb <- mcp(no.wb.sp, percent = 95, unin = 'm', unout = 'km2') ## unin meters?
      
      plot(mcp95.no.wb)
      points(porcs.wb, add = TRUE)      
      
      