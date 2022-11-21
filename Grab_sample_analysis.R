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
recent_samp <- read.csv('GrabSample_compiled_2019-2022names.csv') 

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
  filter(Site == "Forest" | Site == "Nellie_Juan" | Site == "Shrub" | Site == "Tundra" |Site == "stream_gauge" | Site == "Terminus" |Site == "Glacier"|Site == "glacier_lake"|Site == "bench_outlet")

ggplot(core_sites, aes(x=Site, y= DOC, color= Site)) +
  scale_colour_brewer(palette = "Paired")+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("FI")+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(legend.position = "none")  

ggsave(file ="FI_boxplot.pdf",width=6, height=5, units = "in" )

##### Subsetting and plotting by site #######
site_names <- c( "forest" , "nellie_juan_delta" , "shrub_creek" , "tundra_stream" , "stream_gauge" ,"terminus" , "glacier_hut", "glacier_lake", "bench_outlet")
for (k in 1:length(site_names)) {
  temp <- full_data %>%
    filter(Site == site_names[k])
  
  print(ggplot(temp, aes(x=doy, y= FI, group = yearS)) +
    geom_point(aes(shape = as.factor(yearS)))+
    ylab("FI")+
    labs(title=site_names[k])+
    theme_cust() +
    scale_fill_discrete(name = "Year Collected")+
    xlim(0,365)+
    ylim(0,3.2))
}

forest <- full_data %>%
  filter(Site == "forest")  

ggplot(forest, aes(x=doy, y= DOC, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 


glac <- full_data %>%
  filter(Site == "bench_outlet"|Site == "glacier_hut"|Site == "glacier_lake")  

ggplot(glac, aes(x=HIX, y= DOC, group = Site)) +
  geom_point(aes(color = Site))+
  ylab("DOC (ppm)")+
  theme_cust() 

###### Comparing DOC Metrics to other variables #########

ggplot(core_sites, aes(x=d2H, y= DOC, color = Site)) +
  scale_colour_brewer(palette = "Paired")+
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("DOC (ppm)")+
  theme_cust() 


###### Importing Incubation Data ##########

#reading in 2022 Incubation results
incub22 <- read.csv('incubation_results22.csv') 
NOSAMS <- read.csv('NOSAMS_results.csv') 

incub_names <- unique(incub22$Site)

    incub_summary <- incub22 %>%
      group_by(Site,Time.Point) %>%
      summarise(meanconc = mean(DOC_mgL),stdconc = sd(DOC_mgL)) 
    
    grab_summary <- full_data %>%
      filter(Site == incub_names[1]|Site == incub_names[2]|Site == incub_names[3]|Site == incub_names[4]|Site == incub_names[5]|Site == incub_names[6])%>%
      group_by(Site) %>%
      summarise(meanFI = mean(FI, na.rm = TRUE),meanHIX = mean(HIX, na.rm = TRUE)) 
    
    
     incub_summary <-incub_summary %>%
      group_by(Site) %>% 
      mutate(roll_pct_change = ((meanconc[1]-meanconc)/meanconc[1]) * 100)
    
    ggplot(incub_summary, aes(x=Time.Point, y = meanconc, group= Site, color =Site))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=meanconc-stdconc, ymax=meanconc+stdconc ), width=.6,position=position_dodge(0.05))+
    theme_cust()+
    xlim(0,15)+
    ylab("DOC (ppm)")+
    xlab("Days since incubation start") 
    ggsave(file ="Incub_TS_conc.pdf",width=6, height=5, units = "in" )
    
    ggplot(incub_summary, aes(x=Time.Point, y = roll_pct_change, group= Site, color =Site))+
      geom_line()+
      geom_point()+
      theme_cust()+
      geom_hline(yintercept = 0)+
      xlim(0,15)+
      ylab("DOC percent lost")+
      xlab("Days since incubation start") 
    ggsave(file ="Incub_TS_percentlost.pdf",width=6, height=5, units = "in" )
  

  DOC_lost <- rep(NA,length(incub_names))
  
  for (k in 1:length(incub_names)) {
    temp <- incub_summary %>%
    filter(Site == incub_names[k] & (Time.Point == 0 |Time.Point == 14)) 
    
    DOC_lost[k] <- ((temp$meanconc[1] - temp$meanconc[2])/temp$meanconc[1])*100  
  }
  
  DOC_lost <- data.frame(DOC_lost,incub_names) %>%
    rename(Site = incub_names)
  
  DOC_full <- merge(DOC_lost, NOSAMS, by="Site")
  DOC_full <- merge(DOC_full, grab_summary, by="Site")
  
  ggplot(DOC_full, aes(x=Age, y = DOC_lost, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("DOC percent lost")+
    xlab("Apparent Age (years)") 
  ggsave(file ="Incub_percent_age.pdf",width=6, height=5, units = "in" )
  
  ggplot(DOC_full, aes(x=meanHIX, y = DOC_lost, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("DOC percent lost")+
    xlab("mean HIX") 
  ggsave(file ="Incub_percent_HIX.pdf",width=6, height=5, units = "in" )
  
  ggplot(DOC_full, aes(x=Age, y = meanHIX, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("mean HIX")+
    xlab("Apparent Age (years)")  
  ggsave(file ="Incub_HIX_age.pdf",width=6, height=5, units = "in" )
  