##### Add all necesscary libraries #####
library(tidyverse)
library(lubridate)
library(zoo)
library(imputeTS)
library(ggpubr)
library(xts)
library(dygraphs)
library(factoextra)
library(dataRetrieval)


########### Setting up details for this script #############
# generating a custom theme to get rid of the ugly ggplot defaults 
theme_cust <- function(base_size = 11, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black")
    )
}

########### Loading and organizing grab sample data #############

#reading in 2016 & 2017 published data
recent_samp <- read.csv('GrabSample_compiled_2019-2022DOC_analysis.csv') 

#reading in 2019 to present data
early_samp <- read.csv('Hydrology_WolverineGlacier_GeochemSamples_Koch_2016_2017.csv') %>%
  rename(Site = site) 

#combining datasets
full_data <- bind_rows(recent_samp,early_samp) %>%
  mutate(Datetime = as.POSIXct(Datetime, format = "%m/%d/%Y %H:%M", tz = 'America/Anchorage')) %>%
  mutate(.before = "Temperature" , doy = yday(Datetime)) %>%
  mutate(.before = "Temperature" , yearS = year(Datetime))

#reordering datasets by sample collection time 
temp<- order(full_data$Datetime)
full_data <- full_data[temp,]


#################### Subsetting data to only include core sites ####################
core_sites <- full_data %>%
  filter(Site == "forest" | Site == "nellie_juan_delta" | Site == "shrub_creek" | Site == "tundra_stream" |Site == "stream_gauge" | Site == "terminus" |Site == "glacier_hut")

ggplot(core_sites, aes(x=Site, y= DOC, color= Site)) +
  scale_colour_brewer(palette = "Paired")+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("DOC (ppm)")+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(legend.position = "none")  

##### Subsetting and plotting by site #######
forest <- full_data %>%
  filter(Site == "forest")  

ggplot(forest, aes(x=doy, y= DOC, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 


NJ <- full_data %>%
  filter(Site == "nellie_juan_delta")  

ggplot(NJ, aes(x=doy, y= DOC, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 

Wolv <- full_data %>%
  filter(Site == "stream_gauge")  

ggplot(Wolv, aes(x=doy, y= DOC, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 

shrub <- full_data %>%
  filter(Site == "shrub_creek")  

ggplot(shrub, aes(x=doy, y= DOC, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 

term <- full_data %>%
  filter(Site == "terminus")  

ggplot(term, aes(x=doy, y= DOC, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 

glac <- full_data %>%
  filter(Site == "bench_outlet"|Site == "glacier_hut"|Site == "glacier_lake")  

ggplot(glac, aes(x=doy, y= DOC, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 