
cameras <- read.csv('C:/Users/Cara/Documents/__PORC_WB/from Moriarty/CamStations_180817.csv')

head(cameras)
colnames(cameras)
class(cameras$operating_days)

  cameras_days <- cameras$operating_days[!is.na(cameras$operating_days)]
  cameras_days <- cameras_days[-c(119,746)]
  length(cameras_days)
  min(cameras_days)
  max(cameras_days)
  mean(cameras_days)
  sd(cameras_days) / sqrt(length(cameras_days)) # mean 60 SE 2 (should get rid of 0s too; there are 23)

  hist(cameras_days)
  median(cameras_days)
  
  
  ############
  
cdfw_stacy <- read.csv('C:/Users/Cara/Documents/__PORC_WB/spreadsheets/wood blocks/From Stacy/CDFW_2017PILOTDATA__.csv')

head(cdfw_stacy)  
colnames(cdfw_stacy)
class(cdfw_stacy$DAYS.STATION.UP)

  cdfw_sac_spring <- cdfw_stacy[cdfw_stacy$LOCATION == 'SACRIVER' & cdfw_stacy$SEASON == 'SPRING',]
  cdfw_sac_spring_days <- cdfw_sac_spring$DAYS.STATION.UP[!is.na(cdfw_sac_spring$DAYS.STATION.UP)]
  length(cdfw_sac_spring_days)
  min(cdfw_sac_spring_days)
  max(cdfw_sac_spring_days)
  mean(cdfw_sac_spring_days)
  sd(cdfw_sac_spring_days) / sqrt(length(cdfw_sac_spring_days)) ## mean 44 SE 10 (n 5)
  
  cdfw_sac_fall <- cdfw_stacy[cdfw_stacy$LOCATION == 'SACRIVER' & cdfw_stacy$SEASON == 'FALL',]
  cdfw_sac_fall_days <- cdfw_sac_fall$DAYS.STATION.UP[!is.na(cdfw_sac_fall$DAYS.STATION.UP)]
  length(cdfw_sac_fall_days)
  min(cdfw_sac_fall_days)
  max(cdfw_sac_fall_days)
  mean(cdfw_sac_fall_days)
  sd(cdfw_sac_fall_days) / sqrt(length(cdfw_sac_fall_days)) ## mean 27 SE 4 (n 7)
  
  cdfw_sierra_spring <- cdfw_stacy[cdfw_stacy$LOCATION == 'SIERRA NEVADA' & cdfw_stacy$SEASON == 'SPRING',]
  cdfw_sierra_spring_days <- cdfw_sierra_spring$DAYS.STATION.UP[!is.na(cdfw_sierra_spring$DAYS.STATION.UP)]
  length(cdfw_sierra_spring_days)
  min(cdfw_sierra_spring_days)
  max(cdfw_sierra_spring_days)
  mean(cdfw_sierra_spring_days)
  sd(cdfw_sierra_spring_days) / sqrt(length(cdfw_sierra_spring_days)) ## mean 55 SE 5 (n 7)
  
  cdfw_sierra_fall <- cdfw_stacy[cdfw_stacy$LOCATION == 'SIERRA NEVADA' & cdfw_stacy$SEASON == 'FALL',]
  cdfw_sierra_fall_days <- cdfw_sierra_fall$DAYS.STATION.UP[!is.na(cdfw_sierra_fall$DAYS.STATION.UP)]
  length(cdfw_sierra_fall_days)
  min(cdfw_sierra_fall_days)
  max(cdfw_sierra_fall_days)
  mean(cdfw_sierra_fall_days)
  sd(cdfw_sierra_fall_days) / sqrt(length(cdfw_sierra_fall_days)) ## mean 37 SE 9 (n 6)
  
  ############
  
  
cdfw_evan <- read.csv('C:/Users/Cara/Documents/__PORC_WB/spreadsheets/wood blocks/From Evan/Central Region Porcupine Study-Trap Nights w GPS.csv')
  
head(cdfw_evan)  
colnames(cdfw_evan)

## do manually instead:

c_s_sierra_days <- c(1346, 1920, 1740, 1536)
c_s_sierra_number_cam <- 23
sum(c_s_sierra_days) / c_s_sierra_number_cam       # mean 284
# need full #s to calculate SD 
## I did it in Excel...                            # SE 25
