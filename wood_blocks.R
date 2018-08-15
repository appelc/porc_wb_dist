## Data cleaning for wood block deployment/detections

library(googlesheets)
library(irr) ## for calculating kappa (interrater reliability)
library(rgdal)

## 1. load wood block ID data

    ## by Jeremy/Claire (blocks from Katie's crew)
    gs_ls()
      jb_cb <- gs_title('jb_cb_porc_pts')
      jb_cb <- data.frame(gs_read(ss = jb_cb, ws = 'jb_cb_porc_pts.csv', is.na(TRUE), range = cell_cols(1:41)))
      jb_cb$id_by <- rep('Brown/Bortot', nrow(jb_cb))
      
    ## by Claire (blocks from Stirling & Katie's crew)
      cb <- gs_title('Salt Blocks - corrected CA')
      cb <- data.frame(gs_read(ss = cb, ws = 'Salt Blocks - corrected CA.csv', is.na(TRUE), range = cell_cols(1:79)))      
      cb <- cb[,c(1:6)] #match up the rest with master later
      colnames(cb)[2:3] <- c('block_ID_verbatim', 'block_ID')
      cb$id_by <- rep('Bortot', nrow(cb))
      
    ## by Cara via Jessica (blocks from Katie's crew)
      ca <- gs_title('Salt Blocks - from Jessica')
      ca <- data.frame(gs_read(ss = ca, ws = 'Salt Blocks - from Jessica.csv', is.na(TRUE), range = cell_cols(1:79)))
      ca <- ca[,c(1:6)]
      colnames(ca)[2:3] <- c('block_ID_verbatim', 'block_ID')
      ca$id_by <- rep('Appel', nrow(ca))
  
        ## get collection dates for these 5 from Jessica?
      

## 2. load wood block deployment master data
      
    ## from Katie (US FS)
      usfs <- gs_title('Active_ERDO_170523WGS')
      usfs <- data.frame(gs_read(ss = usfs, ws = 'Active_ERDO_170523WGS.csv', is.na(TRUE), range = cell_cols(1:60)))
      usfs$block_ID <- paste(usfs$COUNTY, usfs$SAMPLEUNIT, sep = ' ')

    ## from Sean/Dave (Stirling)      
      stirling <- gs_title('Stirling_PorcupineBoards_2017-11-27')
      stirling <- data.frame(gs_read(ss = stirling, ws = 'Stirling_PorcupineBoards_2017-11-27.csv', is.na(TRUE), range = cell_cols(1:26)))
      colnames(stirling)[1] <- 'block_ID'
      colnames(stirling)[5] <- 'Date'
      stirling[34,5] <- '10/24/2017' ## date was written on board
      
    ## unknown??
      #Export_Output_PorcupinePoints
      

## 3. match id'ed blocks with master datasets and calculate agreement
      
    ## Forest Service 
      cb_ca <- rbind(cb, ca)      
      usfs_wb <- merge(usfs, cb_ca, all.x = TRUE)
        usfs_wb_id <- usfs_wb[!is.na(usfs_wb$id_by),]  ## collected/id'ed blocks only
        
        usfs_wb_id$porcupine_agree <- 0
          usfs_wb_id$porcupine_agree[usfs_wb_id$ERDOchew == 'TRUE' & usfs_wb_id$Porcupine.Presence == 1] <- 1
        usfs_wb_id$no_porcupine_agree <- 0      
          usfs_wb_id$no_porcupine_agree[usfs_wb_id$ERDOchew == 'FALSE' & usfs_wb_id$Porcupine.Presence == 0] <- 1
      
        usfs_wb_id$porcupine <- NA
        usfs_wb_id$porcupine[usfs_wb_id$porcupine_agree == 1 & usfs_wb_id$no_porcupine_agree == 0] <- 1
        usfs_wb_id$porcupine[usfs_wb_id$porcupine_agree == 0 & usfs_wb_id$no_porcupine_agree == 1] <- 0
        
      ## how many agree?
        nrow(subset(usfs_wb_id, porcupine == 1)) ## 3
        nrow(subset(usfs_wb_id, porcupine == 0)) ## 11
        nrow(subset(usfs_wb_id, is.na(porcupine))) ## 8 (-3 that were not id'ed by crew)
        
      ## calculate kappa
        usfs_wb_id$ERDOchew_binary[usfs_wb_id$ERDOchew == 'TRUE'] <- 1
        usfs_wb_id$ERDOchew_binary[usfs_wb_id$ERDOchew == 'FALSE'] <- 0
        kappa2(usfs_wb_id[,c(63,69)], 'unweighted')
          ## kappa = 0.228 (pretty low) -- excluded 3 rows with NAs (crew didn't ID when collected)
        
   ## for the Forest Service blocks: 
   ## (there were 25 but 3 were not ID'ed by crew)
   ##     the 2 observers agree that there is porcupine chewing on only 3 out of 22 blocks (14%)
   ##     the 2 observers agree that there is NO porcupine chewing on 11 blocks (50%)
   ##     the 2 observers disagree on the remaining 8 blocks (36%)
   ##   *total agreement is 64%, disagreement is 36%
        
         
    ## Stirling     
      stirling_wb <- merge(stirling, cb_ca, all.x = TRUE)      
        stirling_wb_id <- stirling_wb[!is.na(stirling_wb$id_by),] ## collected/id'ed blocks      
        ## there's 1 that didn't match up from Claire's sheet (it's ok; P-A2 had 3 blocks for some reason)      
        
      ## crews just id'ed chewing Y/N (not porcupine Y/N specifically)
        stirling_wb_id$Chewed_crew[stirling_wb_id$Chewed. == 'Y'] <- 1
          stirling_wb_id$Chewed_crew[stirling_wb_id$Chewed. == 'N' | stirling_wb_id$Chewed. == 'N?'] <- 0
        stirling_wb_id$Chewed_Bortot <- 0
          stirling_wb_id$Chewed_Bortot[stirling_wb_id$Porcupine.Presence == 1 | stirling_wb_id$Notes == 'Possible woodrat'] <- 1
        stirling_wb_id$chewed_agree <- 0
          stirling_wb_id$chewed_agree[stirling_wb_id$Chewed_crew == 1 & stirling_wb_id$Chewed_Bortot == 1] <- 1
        stirling_wb_id$no_chewed_agree <- 0
          stirling_wb_id$no_chewed_agree[stirling_wb_id$Chewed_crew == 0 & stirling_wb_id$Chewed_Bortot == 0] <- 1      
          
        stirling_wb_id$chew <- NA    
        stirling_wb_id$chew[stirling_wb_id$chewed_agree == 1 & stirling_wb_id$no_chewed_agree == 0] <- 1
        stirling_wb_id$chew[stirling_wb_id$chewed_agree == 0 & stirling_wb_id$no_chewed_agree == 1] <- 0
        
      ## how many agree?
        nrow(subset(stirling_wb_id, chew == 1)) ## 22
        nrow(subset(stirling_wb_id, chew == 0)) ## 37
        nrow(subset(stirling_wb_id, is.na(chew))) ## 7 (-1 that wasn't id'ed)
        
      ## calculate kappa
        kappa2(stirling_wb_id[,c(15,16)], 'unweighted')
          ## kappa = 0.778 (pretty high) -- excluded 1 row with NA (crew didn't ID when collected)
      
      ## are there ones that Claire id'ed as porcupine but crew found no chewing?
        nrow(stirling_wb_id[stirling_wb_id$Chewed_crew == 0 & stirling_wb_id$Porcupine.Presence == 1,])
          ## yes, 1
        
   ## for the Stirling blocks: 
   ##     the 2 observers agree that there is chewing on 22 out of 66 blocks (33%)
   ##     the 2 observers agree that there is NO chewing on 37 blocks (56%)
   ##     the 2 observers disagree on the remaining 8 blocks (11%)
   ##   *total agreement is 89%, disagreement is 11%
   ##   (Including 1 block id'ed as porcupine chewing but crew id'ed as no chewing at all)


## 3. so what do we want to keep as a porcupine detection?

        jb_cb$porc <- jb_cb$PorcupineB
        usfs_wb$porc <- usfs_wb$Porcupine.Presence
        stirling_wb$porc <- stirling_wb$Porcupine.Presence
          
        jb_cb$Study[is.na(jb_cb$Study)] <- 'Klamath NF'
        klamath_nf <- jb_cb[jb_cb$Study == 'Klamath NF',]
        or_coast <- jb_cb[jb_cb$Study == 'OR_Coast',]
        
        klamath_spdf <- SpatialPointsDataFrame(data.frame(klamath_nf$UTM_E, klamath_nf$UTM_N), 
                                               data = data.frame(klamath_nf), 
                                               proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        or_coast_spdf <- SpatialPointsDataFrame(data.frame(or_coast$X, or_coast$Y),
                                                data = data.frame(or_coast),
                                                proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        usfs_spdf <- SpatialPointsDataFrame(data.frame(usfs_wb$X, usfs_wb$Y),
                                            data = data.frame(usfs_wb),
                                            proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        stirling_spdf <- SpatialPointsDataFrame(data.frame(stirling_wb$X, stirling_wb$Y),
                                                data = data.frame(stirling_wb),
                                                proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        
        writeOGR(klamath_spdf, dsn = '.', layer = 'shapefiles/wood blocks/klamath_nf_062518', driver = 'ESRI Shapefile')
        writeOGR(or_coast_spdf, dsn = '.', layer = 'shapefiles/wood blocks/or_coast_062518', driver = 'ESRI Shapefile')
        writeOGR(usfs_spdf, dsn = '.', layer = 'shapefiles/wood blocks/usfs_062518', driver = 'ESRI Shapefile')
        writeOGR(stirling_spdf, dsn = '.', layer = 'shapefiles/wood blocks/stirling_062518', driver = 'ESRI Shapefile')
        
        
## how many detections?  (make sure this matches up with ArcMap)
      table(klamath_spdf$porc, useNA = 'always') #13
      table(or_coast$porc, useNA = 'always') #4
      table(usfs_wb$porc, useNA = 'always') #10 (with 58 stations not collected)
      table(stirling_spdf$porc, useNA = 'always') #13 (with 7 stations not collected)
 
## 40 detections
## 254 blocks collected & IDed
## 319 stations
## (65 blocks never collected)

## number collected & IDed:    
nrow(jb_cb) + nrow(usfs_wb_id) + nrow(stirling_wb_id) #254

## total number stations deployed:
nrow(jb_cb) + nrow(usfs_wb) + nrow(stirling_wb) #319

  # although I don't actually know how many were deployed for the jb_cb set, just that 162 were
  # collected/analyzed


## overall "detection rate"? (16%)
(40/254)*100 

## klamath NF (15%)
(13/(72+13))*100

## OR coast (5%)
(4/(73+4))*100

## southern OR (USFS) (40%)
(10/(15+10))*100

## stirling (%)
(13/(54+13))*100


## How long were stations deployed?
colnames(jb_cb)

    jb_cb_days <- jb_cb$DaysDeploy[jb_cb$DaysDeploy != 0]
    length(jb_cb_days)
    min(jb_cb_days)
    max(jb_cb_days)
    mean(jb_cb_days)
    sd(jb_cb_days) / sqrt(length(jb_cb_days))
    sd(jb_cb_days) / sqrt(77)
    
 
colnames(usfs_wb)

    ## most of these don't have dates, and none have both deployment and collection dates...
    ## "Date" column are either 06/2016 or 12/2016 (is this collection?)


colnames(stirling_wb)

    stirling_wb$Date <- as.Date(stirling_wb$Date, '%m/%d/%Y')
    stirling_wb$Set <- as.Date(stirling_wb$Set, '%m/%d/%Y')
    stirling_wb$DaysDeploy <- stirling_wb$Date - stirling_wb$Set

    stirling_days <- stirling_wb$DaysDeploy[!is.na(stirling_wb$DaysDeploy)]
    length(stirling_days)
    min(stirling_days)
    max(stirling_days)
    mean(stirling_days)
    sd(stirling_days) / sqrt(length(stirling_days))
    
## combine jb_cb and Stirling
    
    jbcb_stirling <- c(jb_cb_days, stirling_days)

    length(jbcb_stirling)    
    min(jbcb_stirling)    
    max(jbcb_stirling)    
    mean(jbcb_stirling)    
    sd(jbcb_stirling) / sqrt(length(jbcb_stirling))    
    
    hist(jb_cb_days)
    hist(as.numeric(stirling_days))
    