##06_Plotting_timeseries
# This brings in the FDOM, Precip, and Q timeseries for plotting
# Convert FDOM to DOC using the Kendall's robust line analysis
# Plot all the data as a time series and pulls out events as examples

source("paths+packages.R")

# load necessary data
FDOM21TS <- read.csv('outputs/04_FDOM21TS.csv')
FDOM22TS <- read.csv('outputs/04_FDOM22TS.csv')
Precip_Q <- read.csv('outputs/04_Precip_q_ts.csv') 
fit_model <- readRDS('outputs/DOC_fdom_model.RData')

# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
FDOM21TS$datetime <- strptime(FDOM21TS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
FDOM22TS$datetime <- strptime(FDOM22TS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Precip_Q$datetime <- strptime(Precip_Q$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

############## Applying linear regression to convert FDOM time series to DOC
DOC21TS <- fit_model$slope*FDOM21TS[2:7]+fit_model$intercept
DOC21TS <-  cbind(FDOM21TS[1], DOC21TS)

DOC22TS <- fit_model$slope*FDOM22TS[2:6]+fit_model$intercept
DOC22TS <-  cbind(FDOM22TS[1], DOC22TS)

########## Creating a multi-panel plot of the full two years of data ######################

#FDOM data 
ts1<- ggplot()+
  geom_line(data = DOC21TS, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = DOC21TS, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = DOC21TS, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = DOC21TS, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = DOC21TS, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  geom_line(data = DOC21TS, aes(x=as.POSIXct(datetime), y= glacier), color = "#0084A8", size = 0.5)+
  
  geom_line(data = DOC22TS, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = DOC22TS, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = DOC22TS, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = DOC22TS, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = DOC22TS, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  ylab(bquote('DOC' (mgl^-1)))+
  xlab('')+
  theme_cust()

#Precip and Q data  
# Calculate the range needed to avoid having your hyetograph and hydrograph overlap 
maxRange <- 1000 # set how wide of the first axis (streamflow)
coeff <- .05 # set the shrink coeffcient of Precipitation
# Use geom_tile to create the inverted hyetograph
# y = the center point of each bar
# maxRange - Precipitation/coeff/2
ts2<- ggplot(data= Precip_Q, aes(x= as.POSIXct(datetime)))+
  geom_tile( aes( y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+
  # Plot your discharge data
  geom_line(aes( y = Q), alpha = 0.8, size = 0.7) +
  # Create a second axis with sec_axis() and format the labels to display the original precipitation units.
  scale_y_continuous(name = "Streamflow (cfs)",limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlab('')+
  theme_cust()

bothTS <- plot_grid(ts2, ts1, ncol=1, align = "v")
bothTS

########## Rain Sep 2021 ##########
bounds_Rsep21<- as.POSIXct(c('09/09/2021 00:00:00','09/15/2021 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Rsep21 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 

Rsep21DOC <- DOC21TS %>%
  filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <- 1 # set the shrink coeffcient of Precipitation

ggplot()+
  geom_point(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.2)+
  geom_point(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.2 )+
  geom_point(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.2)+
  geom_point(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.2)+
  geom_point(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.2)+
  geom_point(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= glacier), color = "#0084A8", size = 0.2)+
  
  geom_line(data = Rsep21, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Rsep21, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Rsep21)+
  xlab('')+
  theme_cust()+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))

########## Rain August 2022 ##########
bounds_Rjul22<- as.POSIXct(c('08/16/2022 00:00:00','08/22/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")


Rjul22 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 

Rjul22DOC <- DOC22TS %>%
  filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <- 1 # set the shrink coeffcient of Precipitation

ggplot()+
  geom_point(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.2)+
  geom_point(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.2 )+
  geom_point(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.2)+
  geom_point(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.2)+
  geom_point(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.2)+
  
  geom_line(data = Rjul22, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Rjul22, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Rjul22)+
  
  xlab('')+
  theme_cust()+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))
