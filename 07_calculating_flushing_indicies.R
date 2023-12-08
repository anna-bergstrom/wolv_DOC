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
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')
Precip_Q <- read.csv('outputs/04_Precip_q_ts.csv') 

#Testig Hydrun with Shrub 
ShrubST <- Precip_Q %>%
  select(datetime, Q_m3s)
#ShrubST$datetime <- as.double(strptime(ShrubST$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC'))/86400 
ShrubST$datetime <-strptime(ShrubST$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

shrub_base <- HydRun::separate.baseflow(ShrubST, 0.97,20)

bounds_21<- as.POSIXct(c('05/01/2021 00:00:00','11/02/2021 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "UTC")

Stormflow21 <- shrub_base$stormflow %>%
  filter(as.POSIXct(datetime) >= bounds_21[1], as.POSIXct(datetime) <= bounds_21[2]) 
baseflow21 <- shrub_base$baseflow %>%
  filter(as.POSIXct(datetime) >= bounds_21[1], as.POSIXct(datetime) <= bounds_21[2]) 
flow21 <- ShrubST %>%
  filter(as.POSIXct(datetime) >= bounds_21[1], as.POSIXct(datetime) <= bounds_21[2]) 

ggplot()+
  geom_point( aes(x= as.POSIXct(flow21$datetime), y = flow21$Q_m3s),size = 0.5, color = col.tundra )+
  geom_point( aes(x= Stormflow21$datetime, y = Stormflow21$stormflow),size = 0.5, color = col.gage )+
  geom_point(aes(x = baseflow21$datetime, y = baseflow21$baseflow) ,size = 0.5)

