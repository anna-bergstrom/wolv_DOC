bounds_Mmay22<- as.POSIXct(c('04/01/2020 00:00:00','10/30/2020 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Mmay22 <- Precip_q_ts %>%
  filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2]) 


Mmay22EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2])

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <- 1 # set the shrink coeffcient of Precipitation

DOC2<- ggplot()+
  geom_point(data = Mmay22, aes(x=as.POSIXct(datetime), y= SC/10), color = "#73DFFF", size = 0.5)+
  
  geom_line(data = Mmay22, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Mmay22, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Mmay22)+
  
  xlab('')+
  theme_cust()
DOC2

##################### Loading NWIS data (gage and WX990) ####################
#setting time bounds to pull data
bounds <- as.POSIXct(c('01/01/2020 00:00:00','12/31/2020 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
params <- c('00060', '00095')
#pulling Q data form NWIS server
gauge_data <- readNWISdata(sites = '15236900', service = 'iv', parameterCd = params, 
                           startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2])) %>%
  select(datetime = dateTime, Q = X_00060_00000, SC = X_00095_00000) %>%
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

