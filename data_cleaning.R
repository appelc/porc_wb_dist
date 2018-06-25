## Porcupine occurrence records for OR & WA

## Import porcupine location records from spreadsheets & combine with GBIF data
# 1. WSDOT roadkill
# 2. ODOT roadkill
# 3. Misc. (Flickr, iNaturalist)
# 4. ORBIC records (OSU / NatureServe)
# 5. NRM/NRIS (US Forest Service)
# 6. BISON (USGS) *skip*
# 7. Conservation Northwest CWMP
# 8. GBIF

library(googlesheets)
library(rgdal) 
library(lubridate) ## for extracting year from dates/posix
library(dplyr)
library(jsonlite) ## for JSON file format
library(dismo)
library(sp)
library(ggplot2)
library(extrafont) ## for ggplot2


## load Oregon/Washington shapefile (area of interest) in lat/lon WGS84

    aoi <- readOGR(dsn = "./shapefiles/admin", layer = 'OR_WA_NCA_boundary')
#    aoi <- spTransform(aoi, CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))


#######################################################

## 1. load WSDOT roadkill records

  gs_ls()
  WA_DOT_roadkill <- gs_title('WSDOTPorcupineCarcassRemovals_1990-Apr2018')
  
    wsdot <- data.frame(gs_read(ss=WA_DOT_roadkill, ws='Porcupine Carcass Removals', is.na(TRUE), range=cell_cols(1:21)))
    colnames(wsdot) <- c('record_id', 'date', 'species', 'region', 'route', 'milepost', 'lat', 'lon', 'mp_to_coord',
                         'type', 'sex', 'age', 'disposal', 'observer_f', 'observer_l', 'source_code', 
                         'event_id', 'permit_no', 'org_code', 'comments', 'hwy_dir')

   ## format date, etc.  
    wsdot$date <- as.POSIXct(strptime(wsdot$date, '%m/%d/%Y'), tz = 'America/Los_Angeles')
    wsdot$year <- year(wsdot$date)
    wsdot$source <- rep('WSDOT', nrow(wsdot))
    wsdot$id <- paste('WSDOT', 1:nrow(wsdot), sep = '')
    wsdot$type <- rep('roadkill', nrow(wsdot))

   ## add decade
    wsdot$decade <- paste((wsdot$year - wsdot$year %% 10), 's', sep = '')

   ## reorder columns
    wsdot <- wsdot[,c('source', 'id', 'type', 'date', 'year', 'decade', 'lat', 'lon', 'route', 'milepost',
                      'mp_to_coord', 'age', 'sex', 'record_id', 'species', 'region', 'disposal', 'observer_f', 
                      'observer_l', 'source_code', 'event_id', 'permit_no', 'org_code', 'comments', 'hwy_dir')] 
    
  ## what to do with NAs in coord?
    nrow(wsdot[is.na(wsdot$lat),]) # 192 out of 720 records have no coordinates (I filled in 106 of them manually so there are 86 left to match)

  ## use online WSDOT GeoPortal tool to enter route and milepost, then export coordinates: http://www.wsdot.wa.gov/data/tools/geoportal/
    ## (it's time-consuming but easier than figuring out linear interpolation?)
    
  ## load file of milepost coordinates from WSDOT GeoPortal (JSON file is easier to work with than KML)

    json <- fromJSON('./shapefiles/observations/ExportedGraphics.json', flatten = TRUE)
    json2 <- fromJSON('./shapefiles/observations/ExportedGraphics_2.json', flatten = TRUE) # I missed some the 1st time so had to do it again
    
      mps1 <- json[[1]][,c(1:4, 7)]  # mps for 'mileposts'
      mps2 <- json2[[1]][,c(1:4, 7)]
      colnames(mps1) <- c('lon', 'lat', 'spatial_ref', 'route', 'milepost')
      colnames(mps2) <- c('lon', 'lat', 'spatial_ref', 'route', 'milepost')

    mps_both <- rbind(mps1, mps2) # combine both because I missed some the 1st time    
          
  ## now match rows with same route & milepost from 'wsdot' to fill in lat/lon

      head(mps_both)
      head(wsdot)    
    
  ## use a for-loop to fill in 'lat' & 'lon' by matching 'route' & 'milepost: (could do 'merge' for an inner join but creates new columns) 

      df1 <- wsdot
      df2 <- mps_both
      
      df1$route <- as.numeric(df1$route) # not ideal but it won't recognize factor levels
      df2$route <- as.numeric(df2$route)
      
      for (i in 1:nrow(df1)){
        for (j in 1:nrow(df2)){
          df1$lat[i] <- ifelse(df1$route[i] == df2$route[j] & df1$milepost[i] == df2$milepost[j], df2$lat[j], df1$lat[i])
          df1$lon[i] <- ifelse(df1$route[i] == df2$route[j] & df1$milepost[i] == df2$milepost[j], df2$lon[j], df1$lon[i])
        }
      }
      
      df1$mp_to_coord[is.na(df1$mp_to_coord)] <- 'j' # 'n' = coord were already entered, 'y' = I manually entered coord for mp, 'j' means I imported it from json file
      df1$route <- as.character(df1$route) # change back to character
      
      wsdot <- df1
      
    ## create SPDF, plot, export
    wsdot.spdf <- SpatialPointsDataFrame(data.frame(wsdot_cleaned$lon, wsdot_cleaned$lat),
                                         data = data.frame(wsdot_cleaned))
    wsdot.spdf@proj4string <- aoi@proj4string                                     
    
    plot(aoi)
    points(wsdot.spdf)

    writeOGR(wsdot.spdf, dsn = '.', layer = 'shapefiles/observations/wsdot_cleaned_051718', driver = 'ESRI Shapefile')
    write.csv(wsdot.spdf@data, 'spreadsheets/wsdot_cleaned_051718.csv')


#######################################################

## 2. load the ODOT roadkill records
    
    gs_ls()
    OR_DOT_roadkill <- gs_title('ODOT_Porcupines')
    
    odot <- data.frame(gs_read(ss = OR_DOT_roadkill, ws = 'PorcipineCarcassData', is.na(TRUE), range = cell_cols(1:12)))
    colnames(odot) <- c('record', 'date', 'route', 'hwy_id', 'milepost', 'cross_street', 'location', 'species', 
                        'info', 'lat', 'lon', 'year')
    
  ## format date etc.
    odot$date <- as.POSIXct(strptime(odot$date, '%m/%d/%Y %H:%M:%S'), tz = 'America/Los_Angeles')
    odot$source <- rep('ODOT', nrow(odot))
    odot$id <- paste('ODOT', 1:nrow(odot), sep = '')
    odot$type <- rep('roadkill', nrow(odot))
    
  ## add decade (all from 2007 - 2013 bc of reporting system)
    odot$decade <- paste((odot$year - odot$year %% 10), 's', sep = '')
    
  ## reorder columns
    odot <- odot[,c('source', 'id', 'type', 'date', 'year', 'decade', 'lat', 'lon', 'route', 'milepost',
                    'record', 'species', 'info', 'location', 'hwy_id', 'cross_street')] 

  ## create SPDF, plot, export
    odot.spdf <- SpatialPointsDataFrame(data.frame(odot$lon, odot$lat),
                                         data = data.frame(odot))
    odot.spdf@proj4string <- aoi@proj4string                                     
    
    plot(aoi)
    points(odot.spdf)
    
    writeOGR(odot.spdf, dsn = '.', layer = 'shapefiles/observations/odot_cleaned_051718', driver = 'ESRI Shapefile')
    write.csv(odot.spdf@data, 'spreadsheets/odot_cleaned_051718.csv')
    
    
#######################################################
        
## 3. load the Miscellaneous records

    gs_ls()
    sheet <- gs_title('Misc porc records OR_WA') 
    misc <- data.frame(gs_read(ss=sheet, ws='Records', is.na(TRUE), range=cell_cols(1:19)))
    
    misc$date <- as.POSIXct(strptime(misc$date, '%m/%d/%Y'), tz = 'America/Los_Angeles')
    misc$year <- year(misc$date)
    
    ## only keep approved / relevant ones
      misc <- misc[misc$include == 'y' & !is.na(misc$include),] #eventually all should be y/n; remove the 'NA' part 
      
    ## clean up a little
      misc$source2 <- misc$source
      misc$source <- rep('MISC', nrow(misc))
      misc$id <- paste('MISC', 1:nrow(misc), sep = '')
      misc$utm_zone <- as.character(misc$utm_zone)
      
    ## add decade
      misc$decade <- paste((misc$year - misc$year %% 10), 's', sep = '')
      
    ## reorder
      misc <- misc[,c('source', 'id', 'type', 'date', 'year', 'decade', 'lat', 'lon', 'location', 'observer', 'utm_e', 'utm_n', 'county', 'source2', 'info',
                      'utm_zone', 'geotagged', 'accuracy..m.','link', 'proj_notes', 'proj_notes2', 'reply', 'include')]
      
    ## separate to UTM zone 10N / zone 11N (e.g., misc_10n <- misc[misc$utm_zone == '10',]) or just use lat/lon

      misc.spdf <- SpatialPointsDataFrame(data.frame(misc$lon, misc$lat), data = data.frame(misc))
      misc.spdf@proj4string <- aoi@proj4string                                     
      
      plot(aoi)
      points(misc.spdf, col = 'red')
      
      writeOGR(misc.spdf, dsn = '.', layer = 'shapefiles/observations/misc_cleaned_060618', driver = 'ESRI Shapefile')
      write.csv(misc.spdf@data, 'spreadsheets/misc_cleaned_060618.csv')
      

#######################################################

## 4. load ORBIC data (OSU Institute for Natural Resources / Oregon Biodiversity Information Center / NatureServe)
      
      gs_ls()
      orbic <- gs_title('ORBIC_ERDO') 
      orbic <- data.frame(gs_read(ss=orbic, ws='ORBIC_ERDO', is.na(TRUE), range=cell_cols(1:71)))
      
      orbic$Observatio <- as.POSIXct(strptime(orbic$Observatio, '%m/%d/%Y'), tz = 'America/Los_Angeles')
      colnames(orbic)[7] <- 'date'
      orbic$year <- year(orbic$date)
      
     ## can't keep ones w/o date
      orbic <- orbic[!is.na(orbic$date),]
    
     ## check reliability ("taxonomic ID reliability" where 1 = excellent w QC; 4 = poor)
      table(orbic$Tax_ID_Rel)
    # orbic <- orbic[orbic$Tax_ID_Rel != 3,] ## OPTIONALLY, REMOVE THOSE WITH RELIABILITY 3 (n = 4). There are no 4s.
      
     ## clean up a little
      orbic$source <- rep('ORBIC', nrow(orbic))
      orbic$id <- paste('ORBIC', 1:nrow(orbic), sep = '')
      colnames(orbic)[64] <- 'lat'
      colnames(orbic)[65] <- 'lon' 
     
    ## kind of messy -- but want to show whether these are NRM/NRIS, BLM, etc.
      unique(orbic$Dataset)
        orbic$source2 <- rep(NA, nrow(orbic))
        orbic$source2[orbic$Agency == 'BLM'] <- 'BLM_GeoBOB'
        orbic$source2[orbic$Dataset == 'FY11 Carnivore Monitoring' | orbic$Dataset == 'Oregon scientific taking permit annual reports, 2002-2013.'] <- 'misc'
        orbic$source2[orbic$Agency == 'Malheur National Forest'] <- 'other'
        orbic$source2[is.na(orbic$source2)] <- 'NRM'
         
     ## add type
      orbic$type <- rep(NA, nrow(orbic))
      orbic$type[orbic$Observat_4 == 'Visual'] <- 'sighting'
      orbic$type[orbic$Observat_4 == 'Other' | is.na(orbic$Observat_4)] <- 'unknown'
      orbic$type[orbic$Observat_4 == 'Excrement' | orbic$Observat_4 == 'Track'] <- 'sign'
      orbic$type[orbic$Observat_4 == 'Camera Set'] <- 'camera'
      orbic$type[orbic$Observat_4 == 'Found Dead'] <- 'carcass'
      
    ## some have more info in the "Species_Ob" column; correct those here
        orbic[c(1,5,9:10,12,17,31,33:34,36), 76] <- 'sighting' 
        orbic[c(14:15,47:48), 76] <- 'carcass' 
        
    ## add decade
      orbic$decade <- paste((orbic$year - orbic$year %% 10), 's', sep = '')
      
    ## reorder
      orbic <- orbic[,c(73,74,76,7,72,77,64,65,75,1:6,8:63,66:71)]
      
    ## create SPDF, plot, export
      orbic.spdf <- SpatialPointsDataFrame(data.frame(orbic$lon, orbic$lat), data = data.frame(orbic))
      orbic.spdf@proj4string <- aoi@proj4string                                     
      
      plot(aoi)
      points(orbic.spdf)
      
      writeOGR(orbic.spdf, dsn = '.', layer = 'shapefiles/observations/orbic_cleaned_051718', driver = 'ESRI Shapefile')
      write.csv(orbic.spdf@data, 'spreadsheets/orbic_cleaned_051718.csv')
      
      
#######################################################
      
## 5. load NRM / NRIS data (USDA FS)
      
      gs_ls()
      nrm <- gs_title('NRM_Wildlife_porcupine_05162018_pts_ORWA')
      nrm <- data.frame(gs_read(ss=nrm, ws='NRM_Wildlife_porcupine_05162018_pts_ORWA.csv', is.na(TRUE), range=cell_cols(1:55)))
        colnames(nrm) 
    
    ## format date, etc.  
      nrm$date <- as.POSIXct(strptime(nrm$SURVEY_OBS, '%m/%d/%Y'), tz = 'America/Los_Angeles')
      nrm$year <- year(nrm$date)
      
      nrm$source <- rep('NRM', nrow(nrm))
      nrm$id <- paste('NRM', 1:nrow(nrm), sep = '')
      
      colnames(nrm)[54] <- 'lon'
      colnames(nrm)[55] <- 'lat'
      
      unique(nrm$OBS_METH_1)
          nrm$type[nrm$OBS_METH_1 == 'Other' ] <- 'other'
          nrm$type[nrm$OBS_METH_1 == 'Visual'] <- 'sighting'
          nrm$type[nrm$OBS_METH_1 == 'Camera Set' | nrm$OBS_METH_1 == 'Image'] <- 'camera'  
          nrm$type[nrm$OBS_METH_1 == 'Track' | nrm$OBS_METH_1 == 'Excrement'] <- 'sign'
          nrm$type[nrm$OBS_METH_1 == 'Found Dead'] <- 'carcass'
      table(nrm$type)
          
    ## add decade
      nrm$decade <- paste((nrm$year - nrm$year %% 10), 's', sep = '')
      
    ## reorder columns
      nrm <- nrm[,c(58,59,60,56,57,61,55,54,33,45,1:32,34:44,46:53)]

    ## create SPDF, plot, export
      nrm.spdf <- SpatialPointsDataFrame(data.frame(nrm$lon, nrm$lat), data = data.frame(nrm))
      nrm.spdf@proj4string <- aoi@proj4string                                     
      
      plot(aoi)
      points(nrm.spdf)
      
      writeOGR(nrm.spdf, dsn = '.', layer = 'shapefiles/observations/nrm_cleaned_051718', driver = 'ESRI Shapefile')
      write.csv(nrm.spdf@data, 'spreadsheets/nrm_cleaned_051718.csv')
      

#######################################################
      
## 6. load BISON data (USGS) 
      
   ## I think it's safe to skip these. From what I can tell, they are all duplicates of GBIF, in
   ##   addition to some from NPS Inventory & Monitoring, but I think these are just park units where
   ##   porcupine is on the species list, not actual occurrences. (Do NPS records exist anywhere?)
      
#      gs_ls()
#      sheet <- gs_title('bison-Erethizon') 
#      bison <- data.frame(gs_read(ss=sheet, ws='bison-Erethizon.csv', is.na(TRUE), range=cell_cols(1:44)))
      
    ## I should have a better method for checking for duplicates, but see if these are also in GBIF:

#      table(bison$ownerInstitutionCollectionCode) 
      
      ## match this up w/ 'table(gbif.pts$institutionCode)' -- there are 4 in BISON not in GBIF (b/c I removed them from GBIF w/ no date)
      ## the only ones we need are from NPS Inventory and Monitoring Program (7):
      
#      bison <- bison[bison$institutionCode == 'BISON',]
      
    ## clean up a little  
#      bison$date <- as.Date(bison$eventDate) ## already posix
#      bison$year <- as.numeric(bison$year) ## already has year column
#      bison$source <- rep('BISON', nrow(bison))
#      bison$id <- paste('BISON', 1:nrow(bison), sep = '')
      
    ## add decade
#      bison$decade <- paste((bison$year - bison$year %% 10), 's', sep = '')
      
    ## rename some columns
#      colnames(bison)[] <- 

    ## reorder
#      bison <- bison[,c('source', 'id', 'type', 'date', 'year', 'decade', 'lat', 'lon', 'route', 'milepost',
#                      'record', 'species', 'info', 'location', 'hwy_id', 'cross_street')] 
      

#######################################################
      
## 7. load Conservation Northwest (CNW) Citizen Wildlife Monitoring Program (CWMP) camera records
      
      gs_ls()
      cwmp <- gs_title('CWMP Porcupine event export')
      
      cwmp <- data.frame(gs_read(ss=cwmp, ws='List View', is.na(TRUE), range=cell_cols(1:8)))
      colnames(cwmp) <- c('camera', 'location', 'species', 'date', 'caption', 'lat', 'lon', 'lure')
      
    ## format date, etc.  
      cwmp$date <- as.POSIXct(strptime(cwmp$date, '%m/%d/%y'), tz = 'America/Los_Angeles')
      cwmp$year <- year(cwmp$date)
      
      cwmp$source <- rep('CWMP', nrow(cwmp))
      cwmp$id <- paste('CWMP', 1:nrow(cwmp), sep = '')
      cwmp$type <- rep('camera', nrow(cwmp))
      
    ## add decade
      cwmp$decade <- paste((cwmp$year - cwmp$year %% 10), 's', sep = '')
      
    ## reorder columns
      cwmp <- cwmp[,c(10:12, 4, 9, 13, 6:7, 2, 1, 3, 5, 8)]
      
    ## create SPDF, plot, export
      cwmp.spdf <- SpatialPointsDataFrame(data.frame(cwmp$lon, cwmp$lat), data = data.frame(cwmp))
      cwmp.spdf@proj4string <- aoi@proj4string                                     
      
      plot(aoi)
      points(cwmp.spdf)
      
      writeOGR(cwmp.spdf, dsn = '.', layer = 'shapefiles/observations/cwmp_cleaned_060618', driver = 'ESRI Shapefile')
      write.csv(cwmp.spdf@data, 'spreadsheets/cwmp_cleaned_060618.csv')
      
  ## these 5 records include 3 detections at the same camera from different dates (within ~3 months)
  ## and 2 detections at other cameras
      
#######################################################
      
## 8. load GBIF
      
    ## download ERDO occurrences from GBIF
      
      sp.occur <- gbif(genus = 'Erethizon', species = '', geo=TRUE) ## 6104 records found
      write.csv(sp.occur, 'spreadsheets/gbif-all-20june18.csv', row.names=FALSE)
      
    #sp.occur <- read.csv('spreadsheets/gbif-all-20june18.csv')  ## the exact download we used
    
      sp.occur <- subset(sp.occur, !is.na(sp.occur$lat))  ## exclude ones w/ no coordinates
      sp.occur.spdf <- SpatialPointsDataFrame(data.frame(sp.occur$lon, sp.occur$lat), 
                                              data = sp.occur)
      sp.occur.spdf@proj4string <- aoi@proj4string
      
    ## crop to just OR & WA:
      aoi2 <- readOGR(dsn = "./shapefiles/admin", layer = 'OR_WA_boundary')
      sp.occur.spdf$state <- over(sp.occur.spdf, aoi2)$NAME # add state names using spatial overlay
        plot(aoi2)  
        points(sp.occur.spdf)  # make sure those with state = NA are actually outside OR/WA 
        points(sp.occur.spdf[is.na(sp.occur.spdf@data$state),], col = 'green') 
      
    ## good, we included ones on the coast. now remove those outside OR/WA
      orwa.occur <- sp.occur.spdf[!is.na(sp.occur.spdf@data$state),]
      points(orwa.occur, col = 'red') # looks good
      
    ## now make data frame
      gbif.pts <- orwa.occur@data
      
    ## clean up data
      gbif.pts$institutionCode <- factor(gbif.pts$institutionCode)
      gbif.pts$date <- as.POSIXct(strptime(gbif.pts$eventDate, '%Y-%m-%d'), tz = 'America/Los_Angeles')
      gbif.pts$source2 <- gbif.pts$institutionCode
      
    ## remove fossil specimens & duplicates
      gbif.pts <- gbif.pts[gbif.pts$basisOfRecord != 'FOSSIL_SPECIMEN',]
      gbif.pts <- gbif.pts[!duplicated(gbif.pts[ , c('lon', 'lat')]),]
      
    ## make everything the same as other occurrence data sources
      gbif.pts$source <- rep('GBIF', nrow(gbif.pts))
      gbif.pts$id <- paste('GBIF', 1:nrow(gbif.pts), sep = '')
      gbif.pts$type_gbif <- gbif.pts$type
      gbif.pts$type <- rep(NA, nrow(gbif.pts))
      
    ## add observation type
      unique(gbif.pts$basisOfRecord)
        gbif.pts$type[gbif.pts$basisOfRecord == 'HUMAN_OBSERVATION'] <- 'sighting'
        gbif.pts$type[gbif.pts$basisOfRecord == 'PRESERVED_SPECIMEN'] <- 'specimen'
        gbif.pts$type[gbif.pts$basisOfRecord == 'MACHINE_OBSERVATION'] <- 'sign' #there is one sound recording! (call it 'audio'?)
      
    ## change any manually? 
     #View(gbif.pts[,c(9,82,94,107,116,117,132,158,159,164,166:169)])
      View(gbif.pts[,c(9,87,122,126,139,158,164,172,175,176)])
        gbif.pts$type[gbif.pts$id == 'GBIF4' | gbif.pts$id == 'GBIF5' | gbif.pts$id == 'GBIF8' | gbif.pts$id == 'GBIF22'] <- 'roadkill'
        gbif.pts$type[gbif.pts$id == 'GBIF18'| gbif.pts$id == 'GBIF23' | gbif.pts$id == 'GBIF24' | gbif.pts$id == 'GBIF25' | gbif.pts$id == 'GBIF26'| gbif.pts$id == 'GBIF28'] <- 'sign'

    ## look up original source of ones w/o dates (all ARCTOS or VERTNET) & remove if no date
      gbif.pts$year[gbif.pts$id == 'GBIF90'] <- '1977' #per Burke Museum mammalogy collection website (accession 197769)
        gbif.pts$date[gbif.pts$id == 'GBIF90'] <- '1977-10-01' #day is unknown (BM uses '1977-10-99') but POSIX won't allow 99
      gbif.pts <- gbif.pts[gbif.pts$id != 'GBIF91',] # no date; correct URL is http://portal.vertnet.org/o/psm/mammal?id=urn-catalog-psm-mammal-mammal-26341
      gbif.pts$year[gbif.pts$id == 'GBIF92'] <- '1931'
      gbif.pts <- gbif.pts[gbif.pts$id != 'GBIF93',] #no date
      gbif.pts$year[gbif.pts$id == 'GBIF94'] <- '1976' #per Burke Museum mammalogy collection website (accession 197871)
        gbif.pts$date[gbif.pts$id == 'GBIF94'] <- '1976-08-01' #day is unknown (BM uses '1976-08-99') but POSIX won't allow 99
      gbif.pts$year[gbif.pts$id == 'GBIF95'] <- '1980' #date is 1980-90 but decade will be 1980s regardless
      gbif.pts$year[gbif.pts$id == 'GBIF96'] <- '2011' #per Burke Museum mammalogy collection website (accession 201102)
        gbif.pts$date[gbif.pts$id == 'GBIF96'] <- '2011-06-01' #day is unknown (BM uses '2011-06-99') but POSIX won't allow 99
      
    ## add decade 
      gbif.pts$year <- as.numeric(gbif.pts$year)
      gbif.pts$decade <- paste((gbif.pts$year - gbif.pts$year %% 10), 's', sep = '')
      
    ## reorder
      gbif.pts <- gbif.pts[,c(174:175,158,172,169,177,99,112,173,176,171,1:98,100:111,113:157,159:168,170)]

    ## export as shapefile & CSV
      gbif.spdf <- SpatialPointsDataFrame(data.frame(gbif.pts$lon, gbif.pts$lat), data = gbif.pts)
      gbif.spdf@proj4string <- aoi@proj4string
      
      table(gbif.pts$institutionCode)
      
      # names(gbif.spdf) <- strtrim(names(points),10)        
      
      writeOGR(gbif.spdf, dsn = '.', layer = 'shapefiles/observations/GBIF_062218', driver = 'ESRI Shapefile')
      write.csv(gbif.pts, "spreadsheets/gbif-subset-22june2018-cleaned.csv")

      
##################################################
      
## Combine all into one dataframe

    occur <- bind_rows(gbif.pts, nrm, orbic, wsdot, odot, misc, cwmp) #order in which I want to keep if duplicates
    nrow(occur)
    
    table(occur$source)
          
    ## Rearrange
      occur <- occur[,c(1:11, 178:332)] #leave off colunms 12:177 (all GBIF)
      occur$gbif_info <- 'see GBIF download for details'
    
    ## Find duplicates. NRM/ORBIC don't have the same OBJECTID. Use date and coord:
      dups <- duplicated(occur[ , c('lat', 'lon', 'date')])    
      dups.occur <- occur[dups,]      
      table(dups.occur$source)
      
    ## Remove duplicates from NRM and ORBIC, but not WSDOT (actually it's only 5, probably OK)
      final.occur <- data.frame(occur[!duplicated(occur[, c('lon', 'lat', 'date')]),])
      nrow(final.occur)
      
      table(final.occur$source)      
      table(final.occur$type) # consolidate a few:
      
        final.occur$type[final.occur$type == 'tracks'] <- 'sign'
        final.occur$type[final.occur$type == 'other'] <- 'unknown'
        
    ## make SPDF
      occur.spdf <- SpatialPointsDataFrame(data.frame(final.occur$lon, final.occur$lat), data = final.occur)
      occur.spdf@proj4string <- aoi@proj4string
      
    ## overlay county
      counties <- readOGR(dsn = 'shapefiles/admin', layer = 'OR_WA_counties_cb2017_500k', verbose = TRUE)
      proj4string(counties) <- proj4string(occur.spdf)
      occur.spdf$county <- over(occur.spdf, counties)$NAME 
      table(occur.spdf$county)
      
    ## overlay state
      counties@data$STATE <- rep(NA, nrow(counties@data))
        counties@data$STATE[counties$STATEFP == 41] <- 'Oregon'
        counties@data$STATE[counties$STATEFP == 53] <- 'Washington'
      occur.spdf$state <- over(occur.spdf, counties)$STATE
      table(occur.spdf$state)
      
    ## crop to AOI
      plot(aoi)
      points(occur.spdf, col = 'red', pch = 19)
      
        outside <- occur.spdf[aoi,]
        points(outside, pch = 19)      
        nrow(outside)     ## good, looks like they are all inside AOI; don't need to crop
        
    ## convert back to dataframe
        final.occur <- data.frame(occur.spdf)

    ## final output
      writeOGR(occur.spdf, dsn = '.', layer = 'shapefiles/observations/orwa_occur_062218', driver = 'ESRI Shapefile')
      write.csv(occur.spdf@data, "spreadsheets/orwa_occur_062218.csv")
        
        
##################################################
        
    ## Summarize observations
        
    ## by source
        table(final.occur$source)
        table(final.occur$source2)
        
    ## by type
        table(final.occur$type)
        table(final.occur$type_gbif)        

    ## matrix (source and type)
        matrix <- table(final.occur$type, final.occur$source)
        write.csv(matrix, 'spreadsheets/source_type_matrix_062218.csv')

    ## matrix (source and state)
        matrix2 <- table(final.occur$source, final.occur$state)
             
    ## make a histogram by decade
        hist(final.occur$year)
        cols <- c('#67001F', '#B2182B', '#D6604D', '#F4606D', '#FDDBC7', '#F7F7F7', '#D1E5F0', '#92C5DE',
                  '#4393C3', '#2166AC', '#053061', '#001233') ## match colors to map
        
        font_import(pattern = '[A/a]rial')
        font_import(pattern = '[T/t]imes')
        loadfonts(device = 'win')
        
        ggplot(final.occur, aes(decade, fill = decade)) + geom_bar(colour = 'black') + 
          labs(x = 'Decade', y = 'Number of records') +
          scale_fill_manual(values = cols) +
          theme(axis.text = element_text(size = 12, colour = 'black', family = 'Arial'),
                axis.text.x = element_text(angle = 50, hjust = 1),
                axis.title = element_text(size = 14, colour = 'black', family = 'Arial'),
                axis.line.x = element_line(size = 1, colour = 'black'),
                axis.line.y = element_line(size = 1, colour = 'black'),
                axis.ticks = element_line(size = 0.5, colour = 'black'),
                axis.ticks.length = unit(.2, 'cm'),
                panel.background = element_rect(fill = 'white'),
                legend.position = 'none')

              ## not very useful b/c of overwhelming n from 2010s (WSDOT roadkill all from >= 2014)
              ## just present as a table instead (or indicate sample size in map figure)
        
        table(final.occur$decade)
       