##05_DOC_FDOM_regression
# This imports the DOC data and FDOM timeseries and fits a statitstical model between DOC samples and sonde FDOM 
# We can use this to convert sonde FDOM to DOC for later analyses 

source("paths+packages.R")

# load necesscary data
FDOM21 <- read.csv('outputs/04_FDOM21TS.csv')
FDOM22 <- read.csv('outputs/04_FDOM22TS.csv')
core_sites <- read.csv('outputs/01_grabsample_core_sites.csv') 

core_sites$Datetime <- as.Date(core_sites$Datetime)
rep_str = c('stream_gauge' = 'gage','Forest'= 'forest', 'shrub_creek' = 'shrub', 'Tundra' = 'tundra', 'Nellie_Juan' = 'nellie', 'glacier_hut' = 'glacier')
core_sites$Site <- str_replace_all(core_sites$Site, rep_str)
core_sites$Datetime <- round_date(core_sites$Datetime, "15 minutes")

core_lab22 <- core_sites[core_sites$yearS == 2022,] 
core_lab21 <- core_sites[core_sites$yearS == 2021,]

rownames(core_lab22)<- seq(1,nrow(core_lab22),1)
rownames(core_lab21)<- seq(1,nrow(core_lab21),1)
core_lab22$'Sonde' <- rep(NA, nrow(core_lab22))
core_lab21$'Sonde' <- rep(NA, nrow(core_lab21))

#####----- INPUT VALUES BY LOOPING THROUGH EACH VARIABLE NAME -----#####
i <- 2
k<- 4
for (i in 2:6){
  inds <- which(core_lab22$'Site' == colnames(FDOM22TS)[i])
  times <- core_lab22$Datetime[inds]
  for (k in 1:length(times)){
    time_inds <- which(FDOM22TS$'datetime' == times[k])
    temp <- FDOM22TS[(time_inds-8):(time_inds+8),i]
    if (all(is.na(temp))){
      core_lab22$'Sonde'[inds[k]] <-NA}
    else{
      core_lab22$'Sonde'[inds[k]] <- mean(na_remove(temp))}
  }
} 

for (i in 2:6){
  inds <- which(core_lab21$'Site' == colnames(FDOM21TS)[i])
  times <- core_lab21$Datetime[inds]
  for (k in 1:length(times)){
    time_inds <- which(FDOM21TS$'datetime' == times[k])
    temp <- FDOM21TS[(time_inds-8):(time_inds+8),i]
    if (all(is.na(temp))){
      core_lab21$'Sonde'[inds[k]] <-NA}
    else{
      core_lab21$'Sonde'[inds[k]] <- mean(na_remove(temp))}
  }
} 

core_site20s <- rbind(core_lab21, core_lab22)
