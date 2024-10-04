##12.1_Plotting_flushing_subsets
# This brings in the DOC, Precip, EC, and Relative Stage timeseries for plotting
# This script is used to identify events that have all the needed data to calculate a flushing index

rm(list= ls())
source("paths+packages.R")

# load necessary data
RelST_FullTS <- read.csv('outputs/06_relative_stageTS.csv')
DOC_FullTS <- read.csv('outputs/06_DOC_FullTS.csv')
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')


#New met and gage data from Seward, all getting loaded in as separate files 
Seward_precip <- read.csv('outputs/04_Seward_precip.csv')


# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
RelST_FullTS$datetime <- strptime(RelST_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
DOC_FullTS$datetime <- strptime(DOC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS$datetime <- strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

Seward_precip$datetime <- strptime(Seward_precip$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')


############### Reading in dates and calculating indicies ######################

event_dates <- read.csv('Data/flushing_events_dates_only.csv')
event_dates$Site <- tolower(event_dates$Site)
event_dates$Start <- strptime(event_dates$Start, "%m/%d/%y %H:%M", tz = 'UTC')
event_dates$Peak <- strptime(event_dates$Peak, "%m/%d/%y %H:%M", tz = 'UTC')
event_dates$End <- strptime(event_dates$End, "%m/%d/%y %H:%M", tz = 'UTC')

###### FI point ########
#Pre-allocating columns for event base flow and peak EC and DOC
event_dates <- event_dates %>%
  mutate(ECs = NaN, ECb = NaN, ECp = NaN, DOCs = NaN, DOCb = NaN, DOCp = NaN)

#for loop to find and write each event baseflow and peak EC and DOC (Point values)
for (i in 1:nrow(event_dates)){
  event_dates$ECs[i]<- EC_FullTS[which(as.POSIXct(EC_FullTS$datetime) == as.POSIXct(event_dates$Peak[i])), which(colnames(EC_FullTS)== event_dates$Site[i])]
  event_dates$ECb[i] <- EC_FullTS[which(as.POSIXct(EC_FullTS$datetime) == as.POSIXct(event_dates$Start[i])), which(colnames(EC_FullTS)== event_dates$Site[i])]
  if (event_dates$ECs[i] >event_dates$ECb[i]){
    event_dates$ECp[i] <- event_dates$ECs[i]
    }else{
    event_dates$ECp[i] <- event_dates$ECb[i]
  }
  event_dates$DOCs[i] <- DOC_FullTS[which(as.POSIXct(DOC_FullTS$datetime) == as.POSIXct(event_dates$Peak[i])), which(colnames(DOC_FullTS)== event_dates$Site[i])]
  event_dates$DOCb[i] <- DOC_FullTS[which(as.POSIXct(DOC_FullTS$datetime) == as.POSIXct(event_dates$Start[i])), which(colnames(DOC_FullTS)== event_dates$Site[i])]
  
  if (event_dates$DOCs[i] >event_dates$DOCb[i]){
    event_dates$DOCp[i] <- event_dates$DOCs[i]
    }else{
    event_dates$DOCp[i] <- event_dates$DOCb[i]
    }
}

#Calculating FI for EC and DOC
event_dates <- event_dates %>%
  mutate(EC_FI = (event_dates$ECs -event_dates$ECb)/event_dates$ECp, DOC_FI = (event_dates$DOCs-event_dates$DOCb)/event_dates$DOCp)

###### FI mean ########
#Pre-allocating columns for event base flow and peak EC and DOC
event_dates <- event_dates %>%
  mutate(ECst = NaN, ECbf = NaN, ECpm = NaN, DOCst = NaN, DOCbf = NaN, DOCpm = NaN)
#for loop to find and write each event baseflow and peak EC and DOC (Point values)
for (i in 1:nrow(event_dates)){
  temp <- EC_FullTS %>% filter(as.POSIXct(datetime)>= as.POSIXct(event_dates$Start[i])-(2*60*60), as.POSIXct(datetime)<= as.POSIXct(event_dates$Start[i]))
  event_dates$ECbf[i]<- mean(temp[,which(colnames(temp) == event_dates$Site[i])], na.rm = TRUE)
  
  temp <- EC_FullTS %>% filter(as.POSIXct(datetime)> as.POSIXct(event_dates$Start[i]), as.POSIXct(datetime)<= as.POSIXct(event_dates$End[i]))
  event_dates$ECst[i] <-mean(temp[,which(colnames(temp) == event_dates$Site[i])], na.rm = TRUE)
  
  if (event_dates$ECst[i] >event_dates$ECbf[i]){
    event_dates$ECpm[i] <- event_dates$ECst[i]
  }else{
    event_dates$ECpm[i] <- event_dates$ECbf[i]
  }
  
  temp <- DOC_FullTS %>% filter(as.POSIXct(datetime)>= as.POSIXct(event_dates$Start[i])-(2*60*60), as.POSIXct(datetime)<= as.POSIXct(event_dates$Start[i]))
  event_dates$DOCbf[i] <-  mean(temp[,which(colnames(temp) == event_dates$Site[i])], na.rm = TRUE)
  
  temp <- DOC_FullTS %>% filter(as.POSIXct(datetime)> as.POSIXct(event_dates$Start[i]), as.POSIXct(datetime)<= as.POSIXct(event_dates$End[i]))
  event_dates$DOCst[i] <- mean(temp[,which(colnames(temp) == event_dates$Site[i])], na.rm = TRUE)
  
  if (event_dates$DOCst[i] >event_dates$DOCbf[i]){
    event_dates$DOCpm[i] <- event_dates$DOCst[i]
  }else{
    event_dates$DOCpm[i] <- event_dates$DOCbf[i]
  }
}

#Calculating FI for EC and DOC
event_dates <- event_dates %>%
  mutate(EC_FIm = (event_dates$ECst -event_dates$ECbf)/event_dates$ECpm, DOC_FIm = (event_dates$DOCst-event_dates$DOCbf)/event_dates$DOCpm)

################ Calculating API for time period surrounding example fall rain events  and comparing to a running average ##############

# Defining the get API function 
getApi <- function(x,k=0.9,n=5,finite=TRUE) {
  l <- length(x)
  y <- rep(NA,times=l)
  if(finite) {
    if(length(k)==1) {
      kn <- rep(NA,times=n)
      for(i in 1:n) kn[i] <- k^(n-i)
    } else {
      n <- length(k)
      kn <- sort(k)
    }
    for(i in (n+1):l) {
      y[i] <- t(kn)%*%x[(i-n):(i-1)]
    }
  } else {
    k <- max(k)
    y[2] <- x[1]
    for(i in 3:l) {y[i] <- k*y[i-1]+x[i-1]}
  }
  return(y)
}

# manipulating the Seward precip data to feed it into the API function
#calculating daily precip totals
day_sum <- aggregate(Seward_precip["precip"], list(hour=cut(as.POSIXct(Seward_precip$datetime), "day")),sum, na.rm = TRUE) %>%
  mutate(datetime = with_tz(hour, tz = 'UTC'))
#Calculating API
API_990 <- getApi(day_sum$precip, k = 0.9, n= 7) 
day_sum <- data.frame(day_sum,API_990) 
day_sum$datetime <- round_date(day_sum$datetime, unit = 'day')

#ans <-day_sum$API_990[day_sum$datetime == bounds_sub[1]] #Finding an API for a specific time


event_dates <- event_dates %>% 
  mutate(API = NaN)

for (i in 1:nrow(event_dates)){
  temp = round_date(event_dates$Start[i],unit = "day")
  event_dates$API[i] <- day_sum$API_990[which(as.POSIXct(day_sum$datetime) == as.POSIXct(temp))]
}

event_dates <- event_dates %>%
  mutate(API_rescaled = rescale(event_dates$API, to = c(0.5,5)))

##### Removing winter events #######

event_dates_sub <- event_dates %>% 
  filter(month(as.POSIXct(event_dates$Start)) > 4 & month(as.POSIXct(event_dates$Start)) < 11)

###### linear regressions with mean FI ###########

Forest_FI <- event_dates_sub[which(event_dates_sub$Site == "forest"),] 
forest_lm <- lm(DOC_FIm ~ EC_FIm, Forest_FI)
summary(forest_lm) #significant

gage_FI <- event_dates_sub[which(event_dates_sub$Site == "gage"),] 
gage_lm <- lm(DOC_FIm ~ EC_FIm, gage_FI)
summary(gage_lm) # p-value = 0.057 

nellie_FI <- event_dates_sub[which(event_dates_sub$Site == "nellie"),] 
nellie_lm <- lm(DOC_FIm ~ EC_FIm, nellie_FI)
summary(nellie_lm) #not significant

tundra_FI <- event_dates_sub[which(event_dates_sub$Site == "tundra"),] 
tundra_lm <- lm(DOC_FIm ~ EC_FIm, tundra_FI)
summary(tundra_lm) #not significant 

shrub_FI <- event_dates_sub[which(event_dates_sub$Site == "shrub"),] 
shrub_lm <- lm(DOC_FIm ~ EC_FIm, shrub_FI)
summary(shrub_lm) #significant 


############# Plotting ########################

### Using mean FI #######
#first plot: point sizes all scaled by API
ggplot()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_abline(intercept = coef(forest_lm)[1], slope = coef(forest_lm)[2], colour = col.forest)+
  geom_abline(intercept = coef(shrub_lm)[1], slope = coef(shrub_lm)[2], colour = col.shrub)+
  geom_abline(intercept = coef(gage_lm)[1], slope = coef(gage_lm)[2], colour = col.gage)+
  geom_point(data = event_dates_sub, aes(x=EC_FIm, y= DOC_FIm, color = Site, size = event_dates_sub$API_rescaled) )+
  scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage), breaks = c( "forest" , "nellie" , "shrub" , "tundra" , 'gage'))+

  ylim(-0.65,0.65)+
  xlim(-0.65,0.65)+
  xlab('EC Flushing Index')+
  ylab('DOC Flushing Index')+
  theme_cust()


#second plot: color ramp for time of year 
cols <- c("#4934eb","#ba03fc", '#03fc62', "#fcba03"  ,'#4934eb')
event_dates <- event_dates %>%
  mutate(dayyear = rescale(as.numeric(strftime(event_dates$Start, format = "%j"),to = c(0,1))))

ggplot()+
  geom_point(data = event_dates, aes(x=EC_FIm, y= DOC_FIm, color = dayyear),  size = event_dates$API_rescaled)+
  scale_color_gradientn(colours = c("blue", "green", "red", "yellow", "blue"), values = c(0, 90, 180, 270, 360)/360)+
  #scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage), breaks = c( "forest" , "nellie" , "shrub" , "tundra" , 'gage'))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  ylim(-0.65,0.65)+
  xlim(-0.65,0.65)+
  xlab('EC Flushing Index')+
  ylab('DOC Flushing Index')+
  theme_cust()

### Using point FI #######
#first plot: point sizes all scaled by API
ggplot()+
  geom_point(data = event_dates_sub, aes(x=EC_FI, y= DOC_FI, color = Site, size = event_dates_sub$API_rescaled))+
  scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage), breaks = c( "forest" , "nellie" , "shrub" , "tundra" , 'gage'))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  ylim(-0.65,0.65)+
  xlim(-0.65,0.65)+
  xlab('EC Flushing Index')+
  ylab('DOC Flushing Index')+
  theme_cust()


#second plot: color ramp for time of year 
cols <- c("#4934eb","#ba03fc", '#03fc62', "#fcba03"  ,'#4934eb')
event_dates_sub <- event_dates_sub %>%
  mutate(dayyear = rescale(as.numeric(strftime(event_dates_sub$Start, format = "%j"),to = c(0,1))))

ggplot()+
  geom_point(data = event_dates_sub, aes(x=EC_FI, y= DOC_FI, color = dayyear),  size = event_dates_sub$API_rescaled)+
  scale_color_gradientn(colours = c("blue", "green", "red", "yellow", "blue"), values = c(0, 90, 180, 270, 360)/360)+
  #scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage), breaks = c( "forest" , "nellie" , "shrub" , "tundra" , 'gage'))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  ylim(-0.65,0.65)+
  xlim(-0.65,0.65)+
  xlab('EC Flushing Index')+
  ylab('DOC Flushing Index')+
  theme_cust()

########## Subsetting for event identification ##########
bounds<- as.POSIXct(c('06/10/2023 00:00:00','06/15/2023 00:00:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

st_sub <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2]) 

DOC_sub <- DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2])

EC_sub <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2])

Sew_sub <- Seward_precip %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2])

ggplot()+
  geom_line(data = st_sub, aes(x=as.POSIXct(datetime), y= tundra*5), color = "#E2725B", size = 0.5)+ #forest color
  geom_point(data = DOC_sub, aes(x=as.POSIXct(datetime), y= tundra*5), color = "#A80084", size = 0.5)+ #tundra color
  geom_point(data = EC_sub, aes(x=as.POSIXct(datetime), y= tundra/10), color = "#FFAA00", size = 0.5)+ #shrub color
  geom_point(data = Sew_sub, aes(x=as.POSIXct(datetime), y= precip), color = "#73DFFF", size = 0.5)+ #gage color
  scale_x_datetime(limits = as.POSIXct(bounds))+
  xlab('')+
  #ylab(bquote('DOC' (mgl^-1)))+
  theme_cust()

st_sub[which.min(st_sub$tundra),] 




