##06_Plotting_timeseries
# This brings in the FDOM, Precip, and Q timeseries for plotting
# Convert FDOM to DOC using the Kendall's robust line analysis
# Plot all the data as a time series and pulls out events as examples
rm(list= ls())
source("paths+packages.R")

# load necessary data
Stage_FullTS <- read.csv('outputs/04_Stage_FullTS.csv')
FDOM_fullTS <- read.csv('outputs/04_FDOM_FullTS.csv')
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')
Precip_Q <- read.csv('outputs/04_Precip_q_ts.csv') 
fit_model <- readRDS('outputs/DOC_fdom_model.RData')

# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
Stage_FullTS$datetime <- strptime(Stage_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
FDOM_fullTS$datetime <- strptime(FDOM_fullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS$datetime <- strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Precip_Q$datetime <- strptime(Precip_Q$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

############## Applying linear regression to convert FDOM time series to DOC
DOC_FullTS <- fit_model$slope*FDOM_fullTS[2:6]+fit_model$intercept
DOC_FullTS <-  cbind(FDOM_fullTS[1], DOC_FullTS)

readr::write_csv(DOC_FullTS, file = file.path("outputs", "06_DOC_FullTS.csv"))

############ Calculating a relative stage ###############

normalized<-function(y) {
  x<-(y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
  return(x)
}

for_rel <- normalized(Stage_FullTS$forest)
shrub_rel <- normalized(Stage_FullTS$shrub)
tundra_rel <- normalized(Stage_FullTS$tundra)
nellie_rel <- normalized(Stage_FullTS$nellie)
gage_rel<- normalized(Stage_FullTS$gage)
RelST_FullTS <-  as.data.frame(cbind(Stage_FullTS[1], for_rel, shrub_rel, tundra_rel, nellie_rel, gage_rel))
colnames(RelST_FullTS) <- c("datetime","forest", "shrub", "tundra", "nellie", "gage")


readr::write_csv(RelST_FullTS, file = file.path("outputs", "06_relative_stageTS.csv"))

########## Creating a multi-panel plot of the full two years of data ######################

#FDOM data 


ts1<- ggplot()+
  geom_line(data = DOC_FullTS, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = DOC_FullTS, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = DOC_FullTS, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = DOC_FullTS, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = DOC_FullTS, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
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

tundra_sub <- EC_FullTS[!is.na(EC_FullTS$tundra), ]
forest_sub <- EC_FullTS[!is.na(EC_FullTS$forest), ]
nellie_sub <- EC_FullTS[!is.na(EC_FullTS$nellie), ]
shrub_sub <- EC_FullTS[!is.na(EC_FullTS$shrub), ]
gage_sub <- EC_FullTS[!is.na(EC_FullTS$gage), ]
ts3<- ggplot()+
  geom_line(data = tundra_sub,aes(x=as.POSIXct(datetime), y= tundra), color = col.tundra, size = 0.5)+
  geom_line(data = forest_sub,aes(x=as.POSIXct(datetime), y= forest), color = col.forest, size = 0.5)+
  geom_line(data = nellie_sub,aes(x=as.POSIXct(datetime), y= nellie), color = col.nellie, size = 0.5)+
  geom_line(data = shrub_sub,aes(x=as.POSIXct(datetime), y= shrub), color = col.shrub, size = 0.5)+
  geom_line(data = gage_sub,aes(x=as.POSIXct(datetime), y= gage), color = col.gage, size = 0.5)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlab('')+
  theme_cust()




######### Plotting full Stage time series ######### 
tundra_sub <- RelST_FullTS[!is.na(Stage_FullTS$tundra), ]
forest_sub <- RelST_FullTS[!is.na(Stage_FullTS$forest), ]
nellie_sub <- RelST_FullTS[!is.na(Stage_FullTS$nellie), ]
shrub_sub <- RelST_FullTS[!is.na(Stage_FullTS$shrub), ]
gage_sub <- RelST_FullTS[!is.na(Stage_FullTS$gage), ]
ts4<- ggplot()+
  geom_line(data = tundra_sub,aes(x=as.POSIXct(datetime), y= tundra), color = col.tundra, size = 0.5)+
  geom_line(data = forest_sub,aes(x=as.POSIXct(datetime), y= forest), color = col.forest, size = 0.5)+
  geom_line(data = nellie_sub,aes(x=as.POSIXct(datetime), y= nellie), color = col.nellie, size = 0.5)+
  geom_line(data = shrub_sub,aes(x=as.POSIXct(datetime), y= shrub), color = col.shrub, size = 0.5)+
  geom_line(data = gage_sub,aes(x=as.POSIXct(datetime), y= gage), color = col.gage, size = 0.5)+
  ylab("Stage") + 
  xlab('')+
  theme_cust()

bothTS <- plot_grid(ts2, ts1, ts3, ts4, ncol=1, align = "v")
#bothTS

Gage_EC_comp <- merge(Precip_Q,EC_FullTS, by = 'datetime',all.x = TRUE)
gagehyst <- ggplot(data = Gage_EC_comp)+
  geom_point(aes(x = log(Q), y= log(gage)), size = 0.5)+
  ylab("EC") + 
  xlab('Q')+
  theme_cust()
  


######### Adding hydroperiods ################

breaks <- as.POSIXct(c('04/15/2021 00:00:00', #1
                      '05/29/2021 00:00:00', #2
                       '06/24/2021 23:45:00', #3
                       '09/09/2021 23:45:00', #4
                       '09/22/2021 23:45:00', #5
                       '04/22/2022 23:45:00', #6
                       '05/25/2022 23:45:00', #7
                       '06/23/2022 23:45:00', #8
                       '08/23/2022 23:45:00', #9
                       '10/17/2022 23:45:00', #10
                      '05/10/2023 00:00:00', #11
                       '05/21/2023 23:45:00', #12
                       '06/19/2023 23:45:00', #13
                       '08/25/2023 23:45:00', #14
                       '09/21/2023 23:45:00'
                       ), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

hydroperiod<-function(y) {
  y$period <- NA
  y$period[y$datetime <= breaks[1] ] <- 0
  y$period[ y$datetime > breaks[5]  & y$datetime <= breaks[6] ] <- 0
  y$period[y$datetime  > breaks[10] & y$datetime <= breaks[11] ] <- 0
  y$period[y$datetime > breaks[15] ] <- 0
  y$period[ y$datetime > breaks[1]  & y$datetime <= breaks[2] ] <- 1
  y$period[ y$datetime > breaks[6]  & y$datetime <= breaks[7] ] <- 1
  y$period[ y$datetime > breaks[11]  & y$datetime <= breaks[12] ] <- 1
  y$period[ y$datetime > breaks[2]  & y$datetime <= breaks[3] ] <- 2
  y$period[ y$datetime > breaks[7]  & y$datetime <= breaks[8] ] <- 2
  y$period[ y$datetime > breaks[12]  & y$datetime <= breaks[13] ] <- 2
  y$period[ y$datetime > breaks[3]  & y$datetime <= breaks[4] ] <- 3
  y$period[ y$datetime > breaks[8]  & y$datetime <= breaks[9] ] <- 3
  y$period[ y$datetime > breaks[13]  & y$datetime <= breaks[14] ] <- 3
  y$period[ y$datetime > breaks[4]  & y$datetime <= breaks[5] ] <- 4
  y$period[ y$datetime > breaks[9]  & y$datetime <= breaks[10] ] <- 4
  y$period[ y$datetime > breaks[14]  & y$datetime <= breaks[15] ] <- 4
  return(y)
}

EC_FullTS <- hydroperiod(EC_FullTS)
DOC_FullTS <- hydroperiod(DOC_FullTS)
Stage_FullTS <- hydroperiod(RelST_FullTS)

FullTS <- merge(EC_FullTS,DOC_FullTS, by = 'datetime',all.x = TRUE)
FullTS <- merge(FullTS,Stage_FullTS, by = 'datetime',all.x = TRUE)

FEC <- ggplot(FullTS, aes(x= forest, y= forest.x)) +geom_point(aes(col=as.factor(period)), size=.5,show.legend = FALSE)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,200)+
  theme_cust()


SEC<- ggplot(FullTS, aes(x= shrub, y= shrub.x)) +geom_point(aes(col=as.factor(period)), size=.5,show.legend = FALSE )+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,200)+
  theme_cust()

GEC<- ggplot(FullTS, aes(x= gage, y= gage.x)) +geom_point(aes(col=as.factor(period)), size=.5,show.legend = FALSE)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,200)+
  theme_cust()

TEC<- ggplot(FullTS, aes(x= tundra, y= tundra.x)) +geom_point(aes(col=as.factor(period)), size=.5,show.legend = FALSE )+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,200)+
  theme_cust()

NEC<- ggplot(FullTS, aes(x= nellie, y= nellie.x)) +geom_point(aes(col=as.factor(period)), size=.5,show.legend = FALSE )+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,200)+
  theme_cust()

FDOC<- ggplot(FullTS, aes(x= forest, y= forest.y)) +geom_point(aes(col=as.factor(period)), size=.5,show.legend = FALSE)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4.5)+
  theme_cust()

SDOC<- ggplot(FullTS, aes(x= shrub, y= shrub.y)) +geom_point(aes(col=as.factor(period)), size=.5 ,show.legend = FALSE)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4.5)+
  theme_cust()

GDOC<- ggplot(FullTS, aes(x= gage, y= gage.y)) +geom_point(aes(col=as.factor(period)), size=.5 ,show.legend = FALSE)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4.5)+
  theme_cust()

TDOC<- ggplot(FullTS, aes(x= tundra, y= tundra.y)) +geom_point(aes(col=as.factor(period)), size=.5 ,show.legend = FALSE)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4.5)+
  theme_cust()

NDOC<- ggplot(FullTS, aes(x= nellie, y= nellie.y)) +geom_point(aes(col=as.factor(period)), size=.5,show.legend = FALSE )+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4.5)+
  theme_cust()

plot_grid(FDOC, NDOC, SDOC, GDOC, TDOC, FEC, NEC, SEC, GEC, TEC, ncol=5, align = "v")

