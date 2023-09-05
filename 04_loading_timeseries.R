##04_loading_timeseries
# This will load the sonde timeseries, combine, organize, and get all sites and data streams on the same timestep and comparable. 

source("paths+packages.R")

## FDOM 2021 ~~ this may need to change with Zan's updated datasets
# loading files and getting the right time zone set 
FDOM21 <- read.csv('Data/2021.combined.corrected.fDOM.csv')
FDOM21$Forest.datetime <- mdy_hm(FDOM21$Forest.datetime, tz='America/Anchorage')
FDOM21$Tundra.datetime <- mdy_hm(FDOM21$Tundra.datetime, tz='America/Anchorage')
FDOM21$Shrub.datetime <- mdy_hm(FDOM21$Shrub.datetime, tz='America/Anchorage')
FDOM21$Nellie.datetime <- mdy_hm(FDOM21$Nellie.datetime, tz='America/Anchorage')

WlFDOM21 <- read.csv('Data/wolv.2021.WT.cor.fDOM.csv')
WlFDOM21$datetime <- mdy_hm(WlFDOM21$datetime, tz='America/Anchorage')

GlFDOM21 <- read.csv('Data/glacier.2021.wt.cor.fdom.csv')
GlFDOM21$datetime <- mdy_hm(GlFDOM21$datetime, tz='America/Anchorage')

# pulling out each location into it's own dataframe so it can more easily be used in the next step 
glacier <- data.frame((GlFDOM21[,3:4]))
shrub = data.frame((FDOM21[,1:2]))
forest = data.frame((FDOM21[,3:4]))
tundra = data.frame((FDOM21[,5:6]))
nellie = data.frame((FDOM21[,7:8]))

start <- FDOM21$Shrub.datetime[1] #finding the first time step with data (i.e. Jan 1 00:00)
datetime_target <- data.frame(seq(start, start + days(365), by = "15 min")) #making the 15 min timeseries all other data will be matched to. 
# changing column names in all data frames so they can be merged more easily
colnames(datetime_target)<- ('datetime') 
colnames(shrub)<- c('datetime','shrub')
colnames(forest)<- c('datetime','forest')
colnames(tundra)<- c('datetime','tundra')
colnames(nellie)<- c('datetime','nellie')

# Merging all data frames from each site so they all align with a common time step and NA anywhere there is no observation due to sonde being down or different logging interval. 
FDOM21TS <- merge(datetime_target,shrub, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,forest, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,tundra, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,nellie, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,glacier, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,WlFDOM21, by = 'datetime',all.x = TRUE)



## FDOM 2022 ~~ this may need to change with Zan's updated datasets
# All the same steps as used for 2021 above

# repeating all of the above steps for 2022  
FDOM22 <- read.csv('Data/2022.combined.corrected.fDOM.csv')
FDOM22$Forest.datetime <- mdy_hm(FDOM22$Forest.datetime, tz='America/Anchorage')
FDOM22$Tundra.datetime <- mdy_hm(FDOM22$Tundra.datetime, tz='America/Anchorage')
FDOM22$Shrub.datetime <- mdy_hm(FDOM22$Shrub.datetime, tz='America/Anchorage')
FDOM22$Nellie.datetime <- mdy_hm(FDOM22$Nellie.datetime, tz='America/Anchorage')

WlFDOM22 <- read.csv('Data/wolv.2022.WT.cor.fDOM.csv')
WlFDOM22$datetime <- mdy_hm(WlFDOM22$datetime, tz='America/Anchorage')

forest = data.frame((FDOM22[,1:2]))
tundra= data.frame((FDOM22[,3:4]))
shrub = data.frame((FDOM22[,5:6]))
nellie = data.frame((FDOM22[,7:8]))

start <- FDOM22$Forest.datetime[1]
datetime_target <- data.frame(seq(start, start + days(365), by = "15 min"))
colnames(datetime_target)<- ('datetime')
colnames(shrub)<- c('datetime','shrub')
colnames(forest)<- c('datetime','forest')
colnames(tundra)<- c('datetime','tundra')
colnames(nellie)<- c('datetime','nellie')

# forest and Nellie data not started on an even time step starting on may 4 (i.e. 1:06, 1:21, 1:36 ...)
#this is just manually adjusting times to bring it back to closest 15 min step (i.e. 1:00, 1:15, 1:30 ...)
forest$datetime[forest$datetime > mdy_hms('05/04/2022 12:00:00', tz='America/Anchorage') & forest$datetime < mdy_hms('08/04/2022 11:00:00', tz='America/Anchorage') & !is.na(forest$datetime)] =  
  forest$datetime[forest$datetime > mdy_hms('05/04/2022 12:00:00', tz='America/Anchorage') & forest$datetime < mdy_hms('08/04/2022 11:00:00', tz='America/Anchorage') & !is.na(forest$datetime)] - minutes(6)

nellie$datetime[ !is.na(nellie$datetime)] =  
  nellie$datetime[ !is.na(nellie$datetime)] - minutes(9)

FDOM22TS <- merge(datetime_target,forest, by = 'datetime',all.x = TRUE)
FDOM22TS <- merge(FDOM22TS,shrub, by = 'datetime',all.x = TRUE)
FDOM22TS <- merge(FDOM22TS,tundra, by = 'datetime',all.x = TRUE)
FDOM22TS <- merge(FDOM22TS,nellie, by = 'datetime',all.x = TRUE)
FDOM22TS <- merge(FDOM22TS,WlFDOM22, by = 'datetime',all.x = TRUE)



## Loading NWIS data (gage and WX990)
#setting time bounds to pull data
bounds <- as.POSIXct(c('01/01/2021 00:00:00','10/01/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

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
datetime_target <- data.frame(seq(start, start + days(730), by = "15 min")) #making the 15 min timeseries all other data will be matched to. 
# changing column names in all data frames so they can be merged more easily
colnames(datetime_target)<- ('datetime') 

Precip_q_ts <- merge(datetime_target,gauge_data, by = 'datetime',all.x = TRUE)
Precip_q_ts <- merge(Precip_q_ts,precip_hourly, by = 'datetime',all.x = TRUE)
Precip_q_ts <- merge(Precip_q_ts,precip2, by = 'datetime',all.x = TRUE)

## Write three files to outputs to use in analysis scripts 
readr::write_csv(FDOM21TS, file = file.path("outputs", "04_FDOM21TS.csv"))
readr::write_csv(FDOM22TS, file = file.path("outputs", "04_FDOM22TS.csv"))
readr::write_csv(Precip_q_ts, file = file.path("outputs", "04_Precip_q_ts.csv"))



############### loading EC data ################
load_EC <- function(input){
  EC_dat <- read.csv(input, skip = 14, header = T) %>%
    subset(select = c('Timestamp..UTC.09.00.', 'Value'))%>%
    rename('datetime' = 'Timestamp..UTC.09.00.', 'Sp_Cond' = 'Value' ) %>%
  mutate(datetime = as.POSIXct(datetime , tz='America/Anchorage', format = '%Y-%m-%d %H:%M:%S')) %>%
  mutate(datetime = round_date(datetime, "15 mins"))
}

EC_gage <- load_EC('Data/Specific_cond_at_25C.uS_cm.research.only@15236900.20190831.csv')

EC_shrub <- load_EC('Data/Specific_cond_at_25C.uS_cm@15236902.20190831.csv')

EC_tundra <- load_EC('Data/Specific_cond_at_25C.uS_cm@15236987.20190831.csv')

EC_nellie <- load_EC('Data/Specific_cond_at_25C.uS_cm@15237000.20190831.csv')

EC_forest <- load_EC('Data/Specific_cond_at_25C.uS_cm@15237003.20190831.csv')


start <- EC_gage$datetime[1]
datetime_target <- data.frame(seq(start, start + years(4), by = "15 min"))
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

