## Data cleaning for wood block deployment/detections

library(googlesheets)
library(dplyr)
library(irr) ## for calculating kappa (interrater reliability)
library(rgdal)

## 1. load wood block ID data ('My Drive > Porc wood block paper > wood block data')

    ## by Jeremy/Claire (blocks from Katie's crew)
    gs_ls()
      jb_cb <- gs_title('jb_cb_porc_pts')
      jb_cb <- data.frame(gs_read(ss = jb_cb, ws = 'jb_cb_porc_pts.csv', is.na(TRUE), range = cell_cols(1:41)))
      jb_cb$id_by <- rep('Brown/Bortot', nrow(jb_cb))
      
    ## by Claire (new blocks from Stirling & Katie's crew)
      cb <- gs_title('Salt Blocks - corrected CA')
      cb <- data.frame(gs_read(ss = cb, ws = 'Salt Blocks - corrected CA.csv', is.na(TRUE), range = cell_cols(1:10)))      
      cb <- cb[,c(1:6)] #match up the rest with master later
      colnames(cb)[2:3] <- c('block_ID_verbatim', 'block_ID')
      cb$id_by <- rep('Bortot', nrow(cb))
      
    ## by Cara via Jessica (5 blocks from Katie's crew via Jessica)
      ca <- gs_title('Salt Blocks - from Jessica')
      ca <- data.frame(gs_read(ss = ca, ws = 'Salt Blocks - from Jessica.csv', is.na(TRUE), range = cell_cols(1:10)))
      ca <- ca[,c(1:6)]
      colnames(ca)[2:3] <- c('block_ID_verbatim', 'block_ID')
      ca$id_by <- rep('Appel', nrow(ca))
  
        ## get collection dates for these 5 from Jessica?
      

## 2. load wood block deployment master data
      
    ## Southern OR 
      or_southern <- gs_title('Active_ERDO_170523WGS')
      or_southern <- data.frame(gs_read(ss = or_southern, ws = 'Active_ERDO_170523WGS.csv', is.na(TRUE), range = cell_cols(1:60)))
      or_southern$block_ID <- paste(or_southern$COUNTY, or_southern$SAMPLEUNIT, sep = ' ')
      or_southern$Study <- rep('OR_Southern', nrow(or_southern))
      
    ## OR coast / Klamath / NCascades
      or_all_orig <- gs_title('Export_Output_PorcupinePoints')
      or_all_orig <- data.frame(gs_read(ss = or_all_orig, ws = 'Export_Output_PorcupinePoints', is.na(TRUE), range = cell_cols(1:39)))
      
      or_all_extra <- gs_title('ERDO-checkpts')
      or_all_extra <- data.frame(gs_read(ss = or_all_extra, ws = 'ERDO-checkpts', is.na(TRUE), range = cell_cols(1:12)))
      colnames(or_all_extra) <- c('Study', 'Name', 'COUNTY', 'SAMPLEUNIT', 'STATION', 'ERDOc', 
                                    'X', 'Y', 'Retrieved', 'COMMENTS', 'DATE_Set', 'obs')
      
      nrow(or_all_orig)
      nrow(or_all_extra)
      
      table(or_all_orig$Study, useNA = 'always')
      table(or_all_extra$Study, useNA = 'always') ## there are 4 OR_Coast in this one not in the other
    
      or_all <- merge(or_all_orig, or_all_extra, all.y = TRUE, all.x = TRUE)
      or_all <- or_all[-c(101,157),] ## 101 (UNIT 5167): "forgot porcupine board" / 157 (UNIT 5141): "porc  block moved" to other 5141 set
        nrow(or_all)
        table(or_all$Study)
      
    or_coast <- or_all[or_all$Study == 'OR_Coast',]
    klamath_nf <- or_all[or_all$Study == 'Klamath_NF',]
    ## (We don't need OR_NorthCascades -- not sure whether these were collected/IDed.)
    
    ## ** None of the OR coast / Klamath blocks were IDed for chewing when blocks were collected.
    ## (I believe the PorcupineB column here is for block yes/no, not chewing yes/no.)
      
    ## Stirling
      stirling <- gs_title('Stirling_PorcupineBoards_2017-11-27')
      stirling <- data.frame(gs_read(ss = stirling, ws = 'Stirling_PorcupineBoards_2017-11-27.csv', is.na(TRUE), range = cell_cols(1:26)))
      stirling$Study <- rep('Stirling', nrow(stirling))
    
    ## total?
      nrow(or_southern) + nrow(or_coast) + nrow(klamath_nf) + nrow(stirling)
      
    ## keep relevant columns & merge
      
      or_southern_crop <- or_southern[,c('Study', 'SAMPLEUNIT', 'X', 'Y', 'ERDOblock', 'ERDOchew', 'COMMENTS')]
        or_southern_crop$SAMPLEUNIT <- as.character(or_southern_crop$SAMPLEUNIT)
      or_coast_crop <- or_coast[,c('Study', 'SAMPLEUNIT', 'X', 'Y', 'COMMENTS', 'DaysDeploy')]
        or_coast_crop$SAMPLEUNIT <- as.character(or_coast_crop$SAMPLEUNIT)
          ## I don't know what the ERDOc column is (it's from ERDO-checkpts which I think was before they were collected, so can't be chewing y/n)
          ## and don't include PorcupineB column from bc I'm also not sure what that is
      klamath_nf_crop <- klamath_nf[,c('Study', 'Site', 'UTM_E', 'UTM_N')]
        colnames(klamath_nf_crop) <- c('Study', 'SAMPLEUNIT', 'X', 'Y')      
      stirling_crop <- stirling[,c('Study', 'Board_ID', 'X', 'Y', 'DaysDeploy', 'Chewed.', 'Comments')]
        stirling_crop$DaysDeploy <- as.integer(stirling_crop$DaysDeploy)
        stirling_crop$AnyChew <- NA
          stirling_crop$AnyChew[stirling_crop$Chewed. == 'N' | stirling_crop$Chewed. == 'N?'] <- 'FALSE'
          stirling_crop$AnyChew[stirling_crop$Chewed. == 'Y'] <- 'TRUE'
          stirling_crop$AnyChew <- as.logical(stirling_crop$AnyChew)
          stirling_crop <- stirling_crop[,-6]
        colnames(stirling_crop) <- c('Study', 'SAMPLEUNIT', 'X', 'Y', 'DaysDeploy', 'COMMENTS', 'AnyChew')
        
    all_blocks <- bind_rows(or_southern_crop, or_coast_crop, klamath_nf_crop, stirling_crop)
    all_blocks <- all_blocks[, c(1,2,8,5,6,9,3,4,7)]
   
      nrow(all_blocks)
        ## total 322
      write.csv(all_blocks, 'spreadsheets/all_blocks_010919.csv')
    
    
## 3. Match IDed blocks to master list
    
  jb_cb[1:85,]$Study <- 'Klamath_NF'
      
      ided_klamath <- jb_cb[1:85, c('Study', 'Site', 'PorcupineB', 'id_by')]
        colnames(ided_klamath) <- c('Study', 'SAMPLEUNIT', 'Porcupine', 'id_by')
      
      ided_OR_coast <- jb_cb[86:162, c('Study', 'SAMPLEUNIT', 'PorcupineB', 'id_by')]
        colnames(ided_OR_coast) <- c('Study', 'SAMPLEUNIT', 'Porcupine', 'id_by')
        ided_OR_coast$SAMPLEUNIT <- as.character(ided_OR_coast$SAMPLEUNIT)
       
  cb_ca <- rbind(cb, ca)
    cb_ca <- cb_ca[, c('Source', 'block_ID', 'Porcupine.Presence', 'id_by', 'Notes')]
    colnames(cb_ca) <- c('Study', 'SAMPLEUNIT', 'Porcupine', 'id_by', 'Notes')
    cb_ca <- cb_ca[!is.na(cb_ca$SAMPLEUNIT),]
    
      ided_stirling <- cb_ca[cb_ca$Study == 'Stirling',]
        
      ided_s_or <- cb_ca[cb_ca$Study != 'Stirling',]
        ided_s_or$Study <- 'OR_Southern'
        ided_s_or$SAMPLEUNIT_full <- ided_s_or$SAMPLEUNIT
        ided_s_or$SAMPLEUNIT <-  substr(ided_s_or$SAMPLEUNIT_full, 6, 9)
        
      all_ided <- bind_rows(ided_klamath, ided_OR_coast, ided_s_or, ided_stirling)
      
      nrow(all_ided) #Claire/Jeremy/Cara IDed 254 for porcupine y/n
      
  ## how many blocks on master list were IDed by crew for chewing/no?
      
    table(all_blocks$ERDOchew, useNA = 'always') # 51 false, 20 true (so 71 IDed); 251 NA (not IDed)
    table(all_blocks$AnyChew, useNA = 'always')  # 38 false, 29 true (so 67 IDed) - Stirling only
      
      ## overall, 138 were IDed by crews (either porcupine y/n or any chewing y/n)
      ## and Claire/Jeremy/Cara IDed  254 for porcupine y/n
    
  ## how many were deployed / IDed by study area?
    
    table(all_blocks$Study, useNA = 'always') # deployed: 85 Klamath, 80 OR_coast, 83 Southern, 74 Stirling
    table(all_ided$Study, useNA = 'always')   # IDed: 85 Klamath, 77 OR_Coast, 25 Southern, 67 Stirling
    
  ## MATCH:
    
    ## first, make all 'SAMPLEUNIT' unique in both master lists
    ## (e.g., duplicates of 'P100-S' are changed to 'P100-S_1' and 'P100-S_2')
    
    unique(all_blocks$SAMPLEUNIT) #295 unique
    class(all_blocks$SAMPLEUNIT) # shd be character
    all_blocks$SAMPLEUNIT <- make.unique(all_blocks$SAMPLEUNIT, sep = '_') #now 322 unique, good!
    
    unique(all_ided$SAMPLEUNIT) #230 unique
    class(all_ided$SAMPLEUNIT)
    all_ided$SAMPLEUNIT <- make.unique(all_ided$SAMPLEUNIT, sep = '_') #now 254 unique, good!
    
  ## and merge:
    
    all_matched <- merge(all_blocks, all_ided, by = c('Study', 'SAMPLEUNIT'), all = TRUE)
      
    nrow(all_matched)
    table(all_matched$id_by, useNA = 'always')
    table(all_matched$Study, useNA = 'always')  #Klamath 85, OR_coast 82, Southern 84, Stirling 75    
      
  ## Summary
    nrow(all_matched) ## 2 stations had 2 blocks for some reason, hence 324 instead of 322
    table(all_matched$Porcupine, useNA = 'always') ## and 70 un-IDed instead of 68

    write.csv(all_matched, 'spreadsheets/all_blocks_matched_010919.csv')

    
    
## 3B. Match up blocks to cameras
    
    cameras <- gs_title('CamStations_180817')
    cameras <- data.frame(gs_read(ss = cameras, ws = 'CamStations_180817', is.na(TRUE), range = cell_cols(1:19)))
    
    cameras$SAMPLEUNIT <- substr(cameras$site, 6, 9)
    

      
      
## *** SCRATCH ALL OF STEP 4... CREWS DIDN'T HAVE TRAINING AND ONLY 22 WERE EXAMINED FOR 
## *** ERDO BY BOTH SO IT DOESN'T MAKE SENSE TO COMPARE.
    
  
## 4. Calculate agreement
    
    nrow(all_matched) ## total blocks = 324
      
    ## blocks that were not examined at all = 20
    ## **THESE WERE PRESUMABLY NOT COLLECTED**
        nrow(all_matched[is.na(all_matched$ERDOchew) & is.na(all_matched$AnyChew) &
                           is.na(all_matched$Porcupine),])
    
    ## blocks that were examined by both crews AND 2nd observers = 88
        nrow(all_matched[(!is.na(all_matched$ERDOchew) | !is.na(all_matched$AnyChew)) & 
                           !is.na(all_matched$Porcupine),])
        
    ## blocks that were examined by crews but NOT by 2nd observers = 50
    ## **THESE NEVER MADE IT TO JEREMY/CLAIRE**
        nrow(all_matched[(!is.na(all_matched$ERDOchew) | !is.na(all_matched$AnyChew)) & 
                            is.na(all_matched$Porcupine),])
    
    ## blocks that were examined by 2nd observers but NOT by crews  = 166 
        nrow(all_matched[is.na(all_matched$ERDOchew) & is.na(all_matched$AnyChew) &
                           !is.na(all_matched$Porcupine),])
    
     20+88+50+166 ## good, this adds up to the total number of blocks (324)

     88+50+166 ## this is the number that were presumably collected

    ## the extra 2 were from the OR coast
     table(all_matched$Study, useNA = 'always') ## Klamath 85, Coast 82, Southern 83, Stirling 74
      
    
  #################
     
    ## remove ones that weren't collected now (there should be 304 left):
      
      all_matched_ids <- all_matched[!is.na(all_matched$ERDOchew) | !is.na(all_matched$AnyChew) |
                                       !is.na(all_matched$Porcupine),]
        nrow(all_matched_ids)    
        
    ## make sure columns are all 0/1/NA 
    ## (columns 'ERDOchew' and 'AnyChew' are from crews; 'Porcupine' is from Jeremy/Claire/Cara)
        
        all_matched_ids$ERDOchew_obs1 <- NA
          all_matched_ids$ERDOchew_obs1[all_matched_ids$ERDOchew == 'TRUE'] <- 1
          all_matched_ids$ERDOchew_obs1[all_matched_ids$ERDOchew == 'FALSE'] <- 0

        all_matched_ids$ERDOchew_obs2 <- all_matched_ids$Porcupine #already 0/1
    
        all_matched_ids$ANYchew_obs1 <- NA 
          all_matched_ids$ANYchew_obs1[all_matched_ids$ERDOchew == 'TRUE' | all_matched_ids$AnyChew == 'TRUE'] <- 1  
          all_matched_ids$ANYchew_obs1[all_matched_ids$ERDOchew == 'FALSE' | all_matched_ids$AnyChew == 'FALSE'] <- 0
        
        all_matched_ids$ANYchew_obs2 <- all_matched_ids$Porcupine #already 0/1
          
    ## simplify dataframe
        
        all_matched_ids <- all_matched_ids[, c(1,2,11,14:17)]
        
    ## add columns for agreement (ERDO chewing)
        
        all_matched_ids$ERDO_1_agree <- NA
          all_matched_ids$ERDO_1_agree[all_matched_ids$ERDOchew_obs1 == 1 & all_matched_ids$ERDOchew_obs2 == 1] <- 1
          all_matched_ids$ERDO_1_agree[all_matched_ids$ERDOchew_obs1 == 1 & all_matched_ids$ERDOchew_obs2 == 0] <- 0
          all_matched_ids$ERDO_1_agree[all_matched_ids$ERDOchew_obs1 == 0 & all_matched_ids$ERDOchew_obs2 == 1] <- 0
        all_matched_ids$ERDO_0_agree <- NA  
          all_matched_ids$ERDO_0_agree[all_matched_ids$ERDOchew_obs1 == 0 & all_matched_ids$ERDOchew_obs2 == 0] <- 1
          all_matched_ids$ERDO_0_agree[all_matched_ids$ERDOchew_obs1 == 0 & all_matched_ids$ERDOchew_obs2 == 1] <- 0
          all_matched_ids$ERDO_0_agree[all_matched_ids$ERDOchew_obs1 == 1 & all_matched_ids$ERDOchew_obs2 == 0] <- 0
          
        all_matched_ids$ANY_1_agree <- NA
          all_matched_ids$ANY_1_agree[all_matched_ids$ANYchew_obs1 == 1 & all_matched_ids$ANYchew_obs2 == 1] <- 1
          all_matched_ids$ANY_1_agree[all_matched_ids$ANYchew_obs1 == 1 & all_matched_ids$ANYchew_obs2 == 0] <- 0
          all_matched_ids$ANY_1_agree[all_matched_ids$ANYchew_obs1 == 0 & all_matched_ids$ANYchew_obs2 == 1] <- 0
        all_matched_ids$ANY_0_agree <- NA
          all_matched_ids$ANY_0_agree[all_matched_ids$ANYchew_obs1 == 0 & all_matched_ids$ANYchew_obs2 == 0] <- 1
          all_matched_ids$ANY_0_agree[all_matched_ids$ANYchew_obs1 == 0 & all_matched_ids$ANYchew_obs2 == 1] <- 0
          all_matched_ids$ANY_0_agree[all_matched_ids$ANYchew_obs1 == 1 & all_matched_ids$ANYchew_obs2 == 0] <- 0
  
    ################
          
    ## how many were examined for ERDO chewing by BOTH observers? (not just any chewing)
          
      nrow(all_matched_ids[!is.na(all_matched_ids$ERDOchew_obs1) & !is.na(all_matched_ids$ERDOchew_obs2),])
          
      ## of those 22 blocks:
        table(all_matched_ids$ERDO_1_agree, useNA = 'always') 
          # agree on porcupine = 3 blocks (14%) 
          # [8 additional blocks were classified as ERDO by 1 of the observers]
        table(all_matched_ids$ERDO_0_agree, useNA = 'always') 
          # agree on no porcupine = 11 blocks (50%)
          # [8 additional blocks were classified as NO ERDO by 1 of the observers]
        
        ## overall agreement 64%, disagreement 36%

        
    ##############
        
    ## how many were examined for ANY chewing by BOTH observers?
        
        nrow(all_matched_ids[!is.na(all_matched_ids$ANYchew_obs1) & !is.na(all_matched_ids$ANYchew_obs2),])
        
       ## of those 88 blocks:
          table(all_matched_ids$ANY_1_agree, useNA = 'always') 
            # agree on chewing = 11 blocks (13%)
            # [33 additional blocks were classified as chewed by 1 of the observers]
          table(all_matched_ids$ANY_0_agree, useNA = 'always') 
            # agree on no chewing = 44 blocks (50%)
            # [33 additional blocks were classified as chewed by 1 of the observers]
          
          ## overall agreement 63%, disagreement 37%
          
      ## calculate Kappa    
          
       kappa.ERDO <- kappa2(data.frame(all_matched_ids$ERDOchew_obs1, all_matched_ids$ERDOchew_obs2), 
                            weight = 'unweighted') ## 0.228
       
       kappa.ANY <- kappa2(data.frame(all_matched_ids$ANYchew_obs1, all_matched_ids$ANYchew_obs2),
                           weight = 'unweighted') ## 138

          
  ## this doesn't seem right...
       
       write.csv(all_matched, 'wb_all_matched_121818.csv')
          
          
          
          
          
          
          
          
          
                  
        
          
## 3. match id'ed blocks with master datasets and calculate agreement
      
    cb_ca <- rbind(cb, ca)
    colnames(cb_ca) <- c('Study', 'block_ID', 'SAMPLEUNIT', 'Date', 'Porcupine_Pres', 'Notes', 'id_by')
    
    
    ## Forest Service 
      cb_ca <- rbind(cb, ca)      
      or_klamath_wb <- merge(or_klamath, cb_ca, all.x = TRUE)
        or_klamath_wb_id <- or_klamath_wb[!is.na(or_klamath_wb$id_by),]  ## collected/id'ed blocks only
        
        or_klamath_wb_id$porcupine_agree <- 0
          or_klamath_wb_id$porcupine_agree[or_klamath_wb_id$ERDOchew == 'TRUE' & or_klamath_wb_id$Porcupine.Presence == 1] <- 1
        or_klamath_wb_id$no_porcupine_agree <- 0      
          or_klamath_wb_id$no_porcupine_agree[or_klamath_wb_id$ERDOchew == 'FALSE' & or_klamath_wb_id$Porcupine.Presence == 0] <- 1
      
        or_klamath_wb_id$porcupine <- NA
        or_klamath_wb_id$porcupine[or_klamath_wb_id$porcupine_agree == 1 & or_klamath_wb_id$no_porcupine_agree == 0] <- 1
        or_klamath_wb_id$porcupine[or_klamath_wb_id$porcupine_agree == 0 & or_klamath_wb_id$no_porcupine_agree == 1] <- 0
        
      ## how many agree?
        nrow(subset(or_klamath_wb_id, porcupine == 1)) ## 3
        nrow(subset(or_klamath_wb_id, porcupine == 0)) ## 11
        nrow(subset(or_klamath_wb_id, is.na(porcupine))) ## 8 (-3 that were not id'ed by crew)
        
      ## calculate kappa
        or_klamath_wb_id$ERDOchew_binary[or_klamath_wb_id$ERDOchew == 'TRUE'] <- 1
        or_klamath_wb_id$ERDOchew_binary[or_klamath_wb_id$ERDOchew == 'FALSE'] <- 0
        kappa2(or_klamath_wb_id[,c(63,69)], 'unweighted')
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
        or_klamath_wb$porc <- or_klamath_wb$Porcupine.Presence
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
        or_klamath_spdf <- SpatialPointsDataFrame(data.frame(or_klamath_wb$X, or_klamath_wb$Y),
                                            data = data.frame(or_klamath_wb),
                                            proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        stirling_spdf <- SpatialPointsDataFrame(data.frame(stirling_wb$X, stirling_wb$Y),
                                                data = data.frame(stirling_wb),
                                                proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
        
        writeOGR(klamath_spdf, dsn = '.', layer = 'shapefiles/wood blocks/klamath_nf_062518', driver = 'ESRI Shapefile')
        writeOGR(or_coast_spdf, dsn = '.', layer = 'shapefiles/wood blocks/or_coast_062518', driver = 'ESRI Shapefile')
        writeOGR(or_klamath_spdf, dsn = '.', layer = 'shapefiles/wood blocks/or_klamath_062518', driver = 'ESRI Shapefile')
        writeOGR(stirling_spdf, dsn = '.', layer = 'shapefiles/wood blocks/stirling_062518', driver = 'ESRI Shapefile')
        
        
## how many detections?  (make sure this matches up with ArcMap)
      table(klamath_spdf$porc, useNA = 'always') #13
      table(or_coast$porc, useNA = 'always') #4
      table(or_klamath_wb$porc, useNA = 'always') #10 (with 58 stations not collected)
      table(stirling_spdf$porc, useNA = 'always') #13 (with 7 stations not collected)
 
## 40 detections
## 254 blocks collected & IDed
## 319 stations
## (65 blocks never collected)

## number collected & IDed:    
nrow(jb_cb) + nrow(or_klamath_wb_id) + nrow(stirling_wb_id) #254

## total number stations deployed:
  #nrow(jb_cb) + nrow(or_klamath_wb) + nrow(stirling_wb) #319 ## should redo this with master list
    # although I don't actually know how many were deployed for the jb_cb set, just that 162 were
    # collected/analyzed

nrow(all_blocks)

## overall "detection rate"? (16%)
(40/254)*100 

## klamath NF (15%)
(13/(72+13))*100

## OR coast (5%)
(4/(73+4))*100

## southern OR (or_klamath) (40%)
(10/(15+10))*100

## stirling (%)
(13/(54+13))*100


## How long were stations deployed?

## REDO W NEW MASTER LIST:

    all_blocks_days <- all_blocks$DaysDeploy[!is.na(all_blocks$DaysDeploy)]
    length(all_blocks_days)
    min(all_blocks_days)
    max(all_blocks_days)
    mean(all_blocks_days)
    sd(all_blocks_days) / sqrt(length(all_blocks_days))

  ## 143 out of 322 have dates
  ## for these, the mean is 365, SE is 4
  ## min 166, max 520
    
  ## none of the OR_Southern or Klamath_NF have dates
  ## (as well as 5 from OR_Coast and 6 from Stirling , but I presume these were not collected)
    
    coast_blocks_days <- all_blocks$DaysDeploy[!is.na(all_blocks$DaysDeploy) & all_blocks$Study == 'OR_Coast']
    stirling_blocks_days <- all_blocks$DaysDeploy[!is.na(all_blocks$DaysDeploy) & all_blocks$Study == 'Stirling']
    
      length(coast_blocks_days)
      min(coast_blocks_days)
      max(coast_blocks_days)
      mean(coast_blocks_days)      
      sd(coast_blocks_days) / sqrt(length(coast_blocks_days))    
      
      length(stirling_blocks_days)
      min(stirling_blocks_days)
      max(stirling_blocks_days)      
      mean(stirling_blocks_days)      
      sd(stirling_blocks_days) / sqrt(length(stirling_blocks_days))
      
## EXPORT OREGON COAST SHAPEFILE
    
    or_coast_spdf <- SpatialPointsDataFrame(data.frame(or_coast$X, or_coast$Y),
                                            data = data.frame(or_coast),
                                            proj4string = CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))
    
    writeOGR(or_coast_spdf, dsn = '.', layer = 'shapefiles/wood blocks/or_coast_111418', driver = 'ESRI Shapefile')
    
    

## how many chewed at all?
    
    
    


## OLD:

colnames(jb_cb)

    jb_cb_days <- jb_cb$DaysDeploy[jb_cb$DaysDeploy != 0]
    length(jb_cb_days)
    min(jb_cb_days)
    max(jb_cb_days)
    mean(jb_cb_days)
    sd(jb_cb_days) / sqrt(length(jb_cb_days))
    sd(jb_cb_days) / sqrt(77)
    
 
colnames(or_klamath_wb)

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
    