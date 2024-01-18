##09_operationalizing flushing indicies
# This is analysis based off that used in Shatilla et al., 2023 multiyear high frequency...
# It uses the Hydrun toolkit originally made for Matlab but reproduced for R
# O8 is written to work with the wolverine gage data
# 09 (this script) is written to work with stage data and operationalized to run site by site and period by period

rm(list= ls())

setwd("/Users/anna/BSU_Drive/Projects/AK_post-doc/DOC/wolv_DOC")
source("paths+packages.R")

#install.packages("remotes") 
#remotes::install_github("codyalbertross/HydRun")
library(HydRun)

# load necessary data
Stage_FullTS <- read.csv('outputs/04_Stage_FullTS.csv')
Stage_FullTS$datetime <-strptime(Stage_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
DOC_FullTS <- read.csv('outputs/06_DOC_FullTS.csv')
DOC_FullTS$datetime <-strptime(DOC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')
EC_FullTS$datetime <-strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')


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
Stage_FullTS <- hydroperiod(Stage_FullTS)

# Data Subsetting Function

base_subset <- function(stage, site, year, Hperiod){
  stage %>%
    select(datetime, !!site, period) %>%
    filter(period == Hperiod & year(datetime)==year) %>%
    na_interpolation(option = 'linear', maxgap = Inf) 
}


For22_3filled <- base_subset(Stage_FullTS, "forest", 2022, 3)
For22_3_base <- HydRun::separate.baseflow(For22_3filled, 0.95,10)

#Sub-setting to plot
Stormflow <- For22_3_base$stormflow  
baseflow <- For22_3_base$baseflow 

# Plotting Baseflow separation
ggplot()+
  geom_point(aes(x= as.POSIXct(For22_3filled$datetime), y = For22_3filled$forest),size = 0.5, color = col.tundra )+
  geom_point(aes(x= Stormflow$datetime, y = Stormflow$stormflow), size = 0.5, color = col.gage )+
  geom_point(aes(x = baseflow$datetime, y = baseflow$baseflow), size = 0.5)

runoff_events <- HydRun::extract.runoff(Stormflow, 0.1, 0.001, 0.001, 0.35, 1, 4, 0.001) 

#Pulling DOC data for each identified event
combined_events <- list()
for (i in 1:length(runoff_events$RunoffEvents)){
  temp <- runoff_events$RunoffEvents[[i]]
  DOC_sub <- DOC_FullTS %>%
    select(datetime, forest) %>%
    filter(datetime >= temp$datetime[1], datetime <= last(temp$datetime))
  EC_sub <- EC_FullTS %>%
    select(datetime, forest) %>%
    filter(datetime >= temp$datetime[1], datetime <= last(temp$datetime))
  if (sum(is.na(DOC_sub$forest)) < nrow(temp)) {
    combined_events[[i]] <- mutate(temp, DOC = DOC_sub$forest, EC = EC_sub$forest) 
  }
}
#combined_events = combined_events[-which(sapply(combined_events, is.null))] #taking out the events that don't have data

events_long <- bind_rows(combined_events, .id = "number") #turning into one dataframe for plotting

ggplot(events_long)+ 
  geom_point( aes(x= as.POSIXct(datetime), y = event_stormflow),size = 0.5, color = col.tundra )+
  geom_point( aes(x= as.POSIXct(datetime), y = DOC),size = 0.5, color = col.gage )+
  geom_point( aes(x= as.POSIXct(datetime), y = EC/10), size = 0.5, color = col.forest )+
  facet_wrap(vars(as.numeric(number)), scales = "free")
  