##04_loading_timeseries
# This will load the sonde timeseries, combine, organize, and get all sites and data streams on the same timestep and comparable. 
rm(list= ls())
source("paths+packages.R")


############### loading FDOM data ################
load_FDOM <- function(input){
  FDOM_dat <- read.csv(input, skip = 14, header = T) %>%
    subset(select = c('Timestamp..UTC.09.00.', 'Value'))%>%
    rename('datetime' = 'Timestamp..UTC.09.00.', 'FDOM' = 'Value' ) %>%
    mutate(datetime = as.POSIXct(datetime , tz='America/Anchorage', format = '%Y-%m-%d %H:%M:%S')) %>%
    mutate(datetime = round_date(datetime, "15 mins"))
}

FDOM_gage <- load_FDOM('Data/fDOM,_water,_in_situ.fDOM.QSE.corrected.WT.Turb.research.only@15236900.20210101.csv')

FDOM_shrub <- load_FDOM('Data/fDOM,_water,_in_situ.FDOM.QSE.WTcorrected@15236902.20210101.csv')

FDOM_tundra <- load_FDOM('Data/fDOM,_water,_in_situ.FDOM.QSE.WTcorrected@15236987.20210101.csv')

FDOM_nellie <- load_FDOM('Data/fDOM,_water,_in_situ.fDOM.QSE.corrected.WT.Turb@15237000.20210101.csv')

FDOM_forest <- load_FDOM('Data/fDOM,_water,_in_situ.FDOM.QSE.WTcorrected@15237003.20210101.csv')

start <- FDOM_gage$datetime[1]
datetime_target <- data.frame(seq(start, start + months(33), by = "15 min"))
colnames(datetime_target)<- ('datetime')
colnames(FDOM_shrub)<- c('datetime','shrub')
colnames(FDOM_forest)<- c('datetime','forest')
colnames(FDOM_tundra)<- c('datetime','tundra')
colnames(FDOM_nellie)<- c('datetime','nellie')
colnames(FDOM_gage)<- c('datetime','gage')


FDOM_fullTS <- merge(datetime_target,FDOM_forest, by = 'datetime',all.x = TRUE)
FDOM_fullTS <- merge(FDOM_fullTS,FDOM_shrub, by = 'datetime',all.x = TRUE)
FDOM_fullTS <- merge(FDOM_fullTS,FDOM_tundra, by = 'datetime',all.x = TRUE)
FDOM_fullTS <- merge(FDOM_fullTS,FDOM_nellie, by = 'datetime',all.x = TRUE)
FDOM_fullTS <- merge(FDOM_fullTS,FDOM_gage, by = 'datetime',all.x = TRUE)

readr::write_csv(FDOM_fullTS, file = file.path("outputs", "04_FDOM_fullTS.csv"))

## Loading NWIS data (gage and WX990)
#setting time bounds to pull data
bounds <- as.POSIXct(c('01/01/2021 00:00:00','10/01/2023 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

#pulling Q data form NWIS server
gauge_data <- readNWISdata(sites = '15236900', service = 'iv', parameterCd = '00060', 
                           startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2])) %>%
  select(datetime = dateTime, Q = X_00060_00000) %>%
  mutate(datetime = with_tz(datetime, tz = 'America/Anchorage'),Q_m3s = Q*0.028316847)

#pulling met data from NWIS server 
met_data <- readNWISdata(sites = '15236895', service = 'iv', parameterCd = '72194', 
                         startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2]))  %>%
  select(datetime = dateTime, Precip1 = X_..2.._72194_00000, Precip2 = X_72194_00000) %>%
  mutate(datetime = with_tz(datetime, tz = 'America/Anchorage'))
# met data from server is the raw cumulative time series which includes draining of the TPG
# the following few lines clumsily deal with that

precip2 <- diff(met_data$Precip2) #taking the difference from one time step to the next to get 15 min instantaneous data 
precip2 <- data.frame(met_data$datetime[2:length(met_data$datetime)],precip2) #pulling the time stamp along with the 15 min instantaneous data
colnames(precip2)[1] ="datetime"  #changing the column header
precip2$precip2[precip2$precip2 > 300 |precip2$precip2 < 0 ] = NA #when the gage was drained there are artificially high values, this turns those and any negative values into no data

precip_hourly <- aggregate(precip2["precip2"], list(hour=cut(as.POSIXct(precip2$datetime), "hour")),sum) 
# summing 15 min data to hourly precip totals
precip_hourly$precip2[precip_hourly$precip2 == 0 ] = NA  # turning any hour that has no precip to an NA so it doesn't display as a 0 in plots. 
colnames(precip_hourly)<- c('datetime','precip_mm')
precip_hourly$datetime <- as.POSIXct(precip_hourly$datetime, tz='America/Anchorage')

#merging to one data frame
start <- precip_hourly$datetime[1] #finding the first time step with data (i.e. Jan 1 00:00)
datetime_target <- data.frame(seq(start, start + months(33), by = "15 min")) #making the 15 min timeseries all other data will be matched to. 
# changing column names in all data frames so they can be merged more easily
colnames(datetime_target)<- ('datetime') 

Precip_q_ts <- merge(datetime_target,gauge_data, by = 'datetime',all.x = TRUE)
Precip_q_ts <- merge(Precip_q_ts,precip_hourly, by = 'datetime',all.x = TRUE)
Precip_q_ts <- merge(Precip_q_ts,precip2, by = 'datetime',all.x = TRUE)

## Write file to outputs to use in analysis scripts 
readr::write_csv(Precip_q_ts, file = file.path("outputs", "04_Precip_q_ts.csv"))



############### loading EC data ################
load_EC <- function(input){
  EC_dat <- read.csv(input, skip = 14, header = T) %>%
    subset(select = c('Timestamp..UTC.09.00.', 'Value'))%>%
    rename('datetime' = 'Timestamp..UTC.09.00.', 'Sp_Cond' = 'Value' ) %>%
  mutate(datetime = as.POSIXct(datetime , tz='America/Anchorage', format = '%Y-%m-%d %H:%M:%S')) %>%
  mutate(datetime = round_date(datetime, "15 mins"))
}

EC_gage <- load_EC('Data/Specific_cond_at_25C.uS_cm.research.only@15236900.20210101.csv')

EC_shrub <- load_EC('Data/Specific_cond_at_25C.uS_cm@15236902.20210101.csv')

EC_tundra <- load_EC('Data/Specific_cond_at_25C.uS_cm@15236987.20210101.csv')

EC_nellie <- load_EC('Data/Specific_cond_at_25C.uS_cm@15237000.20210101.csv')

EC_forest <- load_EC('Data/Specific_cond_at_25C.uS_cm@15237003.20210101.csv')

EC_gage <- EC_gage[!duplicated(EC_gage),] 

start <- EC_gage$datetime[1]

datetime_target <- data.frame(seq(start, start + months(33), by = "15 min"))
colnames(datetime_target)<- ('datetime')
colnames(EC_shrub)<- c('datetime','shrub')
colnames(EC_forest)<- c('datetime','forest')
colnames(EC_tundra)<- c('datetime','tundra')
colnames(EC_nellie)<- c('datetime','nellie')
colnames(EC_gage)<- c('datetime','gage')


EC_fullTS <- merge(datetime_target,EC_forest, by = 'datetime',all.x = TRUE)
EC_fullTS <- merge(EC_fullTS,EC_shrub, by = 'datetime',all.x = TRUE)
EC_fullTS <- merge(EC_fullTS,EC_tundra, by = 'datetime',all.x = TRUE)
EC_fullTS <- merge(EC_fullTS,EC_nellie, by = 'datetime',all.x = TRUE)
EC_fullTS <- merge(EC_fullTS,EC_gage, by = 'datetime',all.x = TRUE)


readr::write_csv(EC_fullTS, file = file.path("outputs", "04_EC_fullTS.csv"))


############### loading Stage data ################
load_St <- function(input){
  St_dat <- read.csv(input, skip = 14, header = T) %>%
    subset(select = c('Timestamp..UTC.09.00.', 'Value'))%>%
    rename('datetime' = 'Timestamp..UTC.09.00.', 'stage' = 'Value' ) %>%
    mutate(datetime = as.POSIXct(datetime , tz='America/Anchorage', format = '%Y-%m-%d %H:%M:%S')) %>%
    mutate(datetime = round_date(datetime, "15 mins"))
}

St_gage <- load_St('Data/Gage_height.ft@15236900.20210101.csv')

St_shrub <- load_St('Data/Gage_height.ft@15236902.20210101.csv')

St_tundra <- load_St('Data/Gage_height.ft@15236987.20210101.csv')

St_nellie <- load_St('Data/Gage_height.ft@15237000.20210101.csv')

St_forest <- load_St('Data/Gage_height.ft@15237003.20210101.csv')


start <- St_forest$datetime[1]
datetime_target <- data.frame(seq(start, start + months(33), by = "15 min"))
colnames(datetime_target)<- ('datetime')
colnames(St_shrub)<- c('datetime','shrub')
colnames(St_forest)<- c('datetime','forest')
colnames(St_tundra)<- c('datetime','tundra')
colnames(St_nellie)<- c('datetime','nellie')
colnames(St_gage)<- c('datetime','gage')


Stage_fullTS <- merge(datetime_target,St_forest, by = 'datetime',all.x = TRUE)
Stage_fullTS <- merge(Stage_fullTS,St_shrub, by = 'datetime',all.x = TRUE)
Stage_fullTS <- merge(Stage_fullTS,St_tundra, by = 'datetime',all.x = TRUE)
Stage_fullTS <- merge(Stage_fullTS,St_nellie, by = 'datetime',all.x = TRUE)
Stage_fullTS <- merge(Stage_fullTS,St_gage, by = 'datetime',all.x = TRUE)

readr::write_csv(Stage_fullTS, file = file.path("outputs", "04_Stage_fullTS.csv"))
