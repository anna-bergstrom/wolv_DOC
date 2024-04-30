## 01_GrabSample_processing
# This will load the grab sample datasets, combine, organize,  and subset the core sites

rm(list= ls())

source("paths+packages.R")

#reading in 2019 to present data
#recent_samp <- read.csv('Data/GrabSample_compiled_2019-2022.csv')  #Old data set that definitely works in this code
recent_samp <- read.csv('Data/Hydrology_WolverineGlacier_Chemistry_Koch.csv')  #New cleaned dataset that will be sent for data release

#reading in 2016 & 2017 published data
early_samp <- read.csv('Data/Hydrology_WolverineGlacier_GeochemSamples_Koch_2016_2017.csv') %>%
  rename(Site = site) 

#combining datasets
full_data <- bind_rows(recent_samp,early_samp) %>%
  mutate(Datetime = as.POSIXct(Datetime, format = "%m/%d/%Y %H:%M", tz = 'America/Anchorage')) %>%
  mutate(Datetime = Datetime+years(2000)) %>%
  mutate(.before = "Temperature" , doy = yday(Datetime)) %>%  #adding a column for the julian day the sample was collected
  mutate(.before = "Temperature" , yearS = year(Datetime))    #adding a column for the year the sample was collected

#reordering datasets by sample collection time 
temp<- order(full_data$Datetime)
full_data <- full_data[temp,]

# Subsetting data to only include core sites 
core_sites <- full_data %>%
  filter(Site == "forest" | Site == "nellie_juan" | Site == "shrub_creek" | Site == "tundra" |Site == "stream_gauge" | Site == "terminus" |Site == "glacier_2" | Site == "lake_inlet")

core_sites <- core_sites[!grepl("replicate", core_sites$SampleID ),] #removing replicate samples from the core sites data set

## Save output 
readr::write_csv(full_data, file = file.path("outputs", "01_grabsample_full_data.csv"))
readr::write_csv(core_sites, file = file.path("outputs", "01_grabsample_core_sites.csv"))