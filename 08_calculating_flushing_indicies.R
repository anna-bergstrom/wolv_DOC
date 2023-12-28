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
Precip_Q <- read.csv('outputs/04_Precip_q_ts.csv') 

###Testing Hydrun with Gage
#organizing and formatting data

bounds_21<- as.POSIXct(c('05/11/2021 00:00:00','10/27/2021 06:30:00'), format="%m/%d/%Y %H:%M:%S", TZ = "UTC")

Gage21 <- Precip_Q %>%
  select(datetime, Q_m3s) %>%
  filter(as.POSIXct(datetime) >= bounds_21[1], as.POSIXct(datetime) <= bounds_21[2]) 
Gage21$datetime <-strptime(Gage21$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

#Baseflow separation
gage_base <- HydRun::separate.baseflow(Gage21, 0.95,20)

#Sub-setting to plot

Stormflow21 <- gage_base$stormflow  
baseflow21 <- gage_base$baseflow 

# Plotting Baseflow separation
ggplot()+
  geom_point( aes(x= as.POSIXct(Gage21$datetime), y = Gage21$Q_m3s),size = 0.5, color = col.tundra )+
  geom_point( aes(x= Stormflow21$datetime, y = Stormflow21$stormflow),size = 0.5, color = col.gage )+
  geom_point(aes(x = baseflow21$datetime, y = baseflow21$baseflow) ,size = 0.5)


#Identifying runoff events
#extract.runoff cannot handle NAs in the time series , the math in the smooth.curve function internal to the extract.runoff function causes everything to return as NA if there are any in there
Stormflow21_filled = na_interpolation(Stormflow21, option = 'linear', maxgap = Inf) #function to fill NA
runoff_events <- HydRun::extract.runoff(Stormflow21_filled, 1.1, 0.001, 0.001, 0.35, 1, 4, 0.001) 

combined_events <- list()
for (i in 1:length(runoff_events$RunoffEvents)){
temp <- runoff_events$RunoffEvents[[i]]
DOC_sub <- DOC_FullTS %>%
  select(datetime, gage) %>%
  filter(datetime >= temp$datetime[1], datetime <= last(temp$datetime))
if (sum(is.na(DOC_sub$gage)) < nrow(temp)) {
 combined_events[[i]] <- mutate(temp,gage = DOC_sub$gage) 
}
}
combined_events = combined_events[-which(sapply(combined_events, is.null))]

events_plot <- vector(mode = "list", length = length(combined_events))
for (i in 1:length(combined_events)) {
  temp <- combined_events[[i]]
  events_plot[[i]]<- ggplot()+ #### Error here, rewriting all items in the list with the new plot in each step in the loop. Need to fix this the next time around
  geom_point( aes(x= as.POSIXct(temp$datetime), y = temp$gage*10),size = 0.5, color = col.tundra )+
  geom_point( aes(x= as.POSIXct(temp$datetime), y = temp$event_stormflow), size = 0.5, color = col.gage )

}
library(gridExtra)
n <- length(events_plot)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(events_plot, ncol=nCol))

gridExtra::grid.arrange(grobs = events_plot) 

