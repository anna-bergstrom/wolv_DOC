##08_calculating flushing indicies
# This is analysis based off that used in Shatilla et al., 2023 multiyear high frequency...
# It uses the Hydrun toolkit originally made for Matlab but reproduced for R

rm(list= ls())
source("paths+packages.R")

#install.packages("remotes")
#remotes::install_github("codyalbertross/HydRun")
library(HydRun)

# load necessary data
Stage_FullTS <- read.csv('outputs/04_Stage_FullTS.csv')
DOC_FullTS <- read.csv('outputs/06_DOC_FullTS.csv')
DOC_FullTS$datetime <-strptime(DOC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')
EC_FullTS$datetime <-strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Precip_Q <- read.csv('outputs/04_Precip_q_ts.csv') 
Precip_Q$datetime <-strptime(Precip_Q$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

###### Testing Hydrun with Gage #######
#organizing and formatting data

bounds_21<- as.POSIXct(c('05/11/2021 00:00:00','10/27/2021 06:30:00'), format="%m/%d/%Y %H:%M:%S", TZ = "UTC")

Gage21 <- Precip_Q %>%
  select(datetime, Q_m3s) %>%
  filter(as.POSIXct(datetime) >= bounds_21[1], as.POSIXct(datetime) <= bounds_21[2]) 
#Gage21$datetime <-strptime(Gage21$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

#Baseflow separation
gage_base <- HydRun::separate.baseflow(Gage21, 0.9,20)

#Sub-setting to plot
Stormflow21 <- gage_base$stormflow  
baseflow21 <- gage_base$baseflow 

# Plotting Baseflow separation
ggplot()+
  geom_point(aes(x= as.POSIXct(Gage21$datetime), y = Gage21$Q_m3s),size = 0.5, color = col.tundra )+
  geom_point(aes(x= Stormflow21$datetime, y = Stormflow21$stormflow), size = 0.5, color = col.gage )+
  geom_point(aes(x = baseflow21$datetime, y = baseflow21$baseflow), size = 0.5)


#Identifying runoff events
#extract.runoff cannot handle NAs in the time series , the math in the smooth.curve function internal to the extract.runoff function causes everything to return as NA if there are any in there
Stormflow21_filled = na_interpolation(Stormflow21, option = 'linear', maxgap = Inf) #function to fill NA
runoff_events <- HydRun::extract.runoff(Stormflow21_filled, 0.1, 0.001, 0.001, 0.35, 1, 4, 0.001) 

#Pulling DOC and EC data for each identified event
combined_events <- list()
for (i in 1:length(runoff_events$RunoffEvents)){
temp <- runoff_events$RunoffEvents[[i]]
DOC_sub <- DOC_FullTS %>%
  select(datetime, gage) %>%
  filter(datetime >= temp$datetime[1], datetime <= last(temp$datetime))
EC_sub <- EC_FullTS %>%
  select(datetime, gage) %>%
  filter(datetime >= temp$datetime[1], datetime <= last(temp$datetime))
if (sum(is.na(DOC_sub$gage)) < nrow(temp)) {
 combined_events[[i]] <- mutate(temp, DOC = DOC_sub$gage, EC = EC_sub$gage) # ,EC = EC_sub$gage
}
}
combined_events = combined_events[-which(sapply(combined_events, is.null))] #taking out the events that don't have data

events_long <- bind_rows(combined_events, .id = "number") #turning into one dataframe for plotting

ggplot(events_long)+ 
  geom_point( aes(x= as.POSIXct(datetime), y = event_stormflow),size = 0.5, color = col.tundra )+
  geom_point( aes(x= as.POSIXct(datetime), y = DOC*10),size = 0.5, color = col.gage )+
  geom_point( aes(x= as.POSIXct(datetime), y = EC/10), size = 0.5, color = col.forest )+
  facet_wrap(vars(as.numeric(number)), scales = "free")

## operationalizing as a series of functions to use for stage data at each site 


