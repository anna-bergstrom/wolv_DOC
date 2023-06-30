##### Add all necesscary libraries #####
library(tidyverse)
library(lubridate)
library(zoo)
library(imputeTS)
library(ggpubr)
library(stringr)
library(xts)
library(dygraphs)
library(factoextra)
library(dataRetrieval)
library(cowplot)


########### Setting up details for this script #############
# generating a custom theme to get rid of the ugly ggplot defaults 
theme_cust <- function(base_size = 16, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black")
    )
}


########### Loading and organizing grab sample data #############

#reading in 2016 & 2017 published data
recent_samp <- read.csv('GrabSample_compiled_2019-2022.csv') 

#reading in 2019 to present data
early_samp <- read.csv('Hydrology_WolverineGlacier_GeochemSamples_Koch_2016_2017.csv') %>%
  rename(Site = site) 

#combining datasets
full_data <- bind_rows(recent_samp,early_samp) %>%
  mutate(Datetime = as.POSIXct(Datetime, format = "%m/%d/%Y %H:%M", tz = 'America/Anchorage')) %>%
  mutate(Datetime = Datetime+years(2000)) %>%
  mutate(.before = "Temperature" , doy = yday(Datetime)) %>%
  mutate(.before = "Temperature" , yearS = year(Datetime))

#reordering datasets by sample collection time 
temp<- order(full_data$Datetime)
full_data <- full_data[temp,]


#################### Subsetting data to only include core sites ####################
core_sites <- full_data %>%
  filter(Site == "Forest" | Site == "Nellie_Juan" | Site == "shrub_creek" | Site == "Tundra" |Site == "stream_gauge" | Site == "Terminus" |Site == "glacier_hut" | Site == "lake_inlet")

#core_sites[core_sites == 0] <- NA

site_names <- c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet")

ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= DOC, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab(bquote('DOC' (mgl^-1)))+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  

ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= FI, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  geom_hline(yintercept=1.9, linetype="dashed", color = "#1A237E", size=1)+
  geom_hline(yintercept=1.4, linetype="dashed", color = "#43A047", size=1)+
  ylab("Fluorescence Index")+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))


ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= Nitrate, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab(bquote('Nitrate' (mgl^-1)))+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))

ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= Phosphate_P, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab(bquote('Phosphate' (mgl^-1)))+
  xlab("")+
  ylim(0,0.1)+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))

#TN-(Ammonium_N+Nitrate_plus_Nitrite_N)

ggplot(core_site20s, aes(x=reorder(Site,DOC,na.rm = TRUE), y= ((Nitrate)), color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab('Nitrate IC')+
  xlab("")+
  #ylim(0,1)+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))


ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= HIX, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC"), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("HIX")+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))+
  theme(legend.position = "none")  

########## Not being used in MS ###############
ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= FDOM_lab, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("Lab fDOM")+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  

ggplot(core_sites, aes(x=DOC, y = FDOM_lab, color =as.factor(Site)))+
  scale_color_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ),breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" , "Terminus", "Glacier"))+
  geom_point(size = 4, alpha = 0.7)+
  #geom_hline(yintercept=1.9, linetype="dashed", color = "#1A237E", size=1)+
 #geom_hline(yintercept=1.4, linetype="dashed", color = "#43A047", size=1)+
  theme_cust()+
  theme(legend.position = c(0.2,0.75)) +
  #ylim(1,2.7)+
  labs(color = "Site")+
  ylab("Lab fDOM")+
  xlab(bquote('DOC' (mgl^-1)))+  
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 16))

#ggsave(file ="Incub_HIX_age.pdf",width=6, height=5, units = "in" )

#ggsave(file ="FI_boxplot.pdf",width=6, height=5, units = "in" )

##### Subsetting and plotting by site #######

for (k in 1:length(site_names)) {
  temp <- full_data %>%
    filter(Site == site_names[k])
  
  print(ggplot(temp, aes(x=doy, y= DOC, group = yearS)) +
    geom_point(aes(shape = as.factor(yearS)))+
    ylab("FI")+
    labs(title=site_names[k])+
    theme_cust() +
    scale_fill_discrete(name = "Year Collected")+
    xlim(0,365)+
    ylim(0,3.2))
}

stream_gage <- full_data %>%
  filter(Site == "stream_gauge")  

ggplot(stream_gage, aes(x=doy, y= Fe, group = yearS)) +
  geom_point(aes(shape = as.factor(yearS)))+
  ylab("Fe (ppm)")+
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
      summarise(meanconc = mean(DOC_mgL),stdconc = sd(DOC_mgL),mean_FI = mean(FI),mean_HIX = mean(HIX), mean_FDOM = mean(FDOM)) 
    

     incub_summary <-incub_summary %>%
      group_by(Site) %>% 
      mutate(roll_pct_change = ((meanconc[1]-meanconc)/meanconc[1]) * 100)
    
    ggplot(incub_summary, aes(x=Time.Point, y = meanconc, group= Site, color =Site))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=meanconc-stdconc, ymax=meanconc+stdconc ), width=.6,position=position_dodge(0.05))+
    theme_cust()+
    xlim(0,30)+
    ylab("DOC (ppm)")+
    xlab("Days since incubation start") 
    ggsave(file ="Incub_TS_conc.pdf",width=6, height=5, units = "in" )
    
    ggplot(incub_summary, aes(x=Time.Point, y = roll_pct_change, group= Site, color =Site))+
      geom_line()+
      geom_point()+
      theme_cust()+
      geom_hline(yintercept = 0)+
      xlim(0,30)+
      ylab("DOC percent lost")+
      xlab("Days since incubation start") 
    ggsave(file ="Incub_TS_percentlost.pdf",width=6, height=5, units = "in" )
  
   

  DOC_lost <- rep(NA,length(incub_names))
  DOC_init <- rep(NA,length(incub_names))

  DOC_Fparams<- incub_summary %>%
    filter(Time.Point == 0) %>%
    subset(select = c(Site,mean_FI, mean_HIX, mean_FDOM ))
  
  for (k in 1:length(incub_names)) {
    temp <- incub_summary %>%
    filter(Site == incub_names[k] & (Time.Point == 0 |Time.Point == 6)) 
    
    DOC_lost[k] <- ((temp$meanconc[1] - temp$meanconc[2])/temp$meanconc[1])*100 
    DOC_init[k]<- temp$meanconc[1]
  }
  
  DOC_lost <- data.frame(DOC_lost,incub_names,DOC_init) %>%
    rename(Site = incub_names)
  
  DOC_full <- merge(DOC_lost, NOSAMS, by="Site")
  DOC_full <- merge(DOC_full, DOC_Fparams, by="Site")
  DOC_full$DOC_lost[DOC_full$Site == "Nellie_Juan"] = 0
  
  ggplot(DOC_full, aes(fill=Site, y= DOC_lost, x= reorder(Site,DOC_init)))+
    geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ),breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "Glacier"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" , "Terminus", "Glacier"))+
  theme_cust()+
  ylab("% BDOC (6 day incubation) ")+
  xlab("")+
    scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "Glacier" = "Glacier"))+
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))
  
  ggplot(DOC_full, aes(fill=Site, y= DOC_init, x= reorder(Site,DOC_init)))+
    geom_bar(stat="identity", color = "black")+
    scale_fill_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ),breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "Glacier"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" , "Terminus", "Glacier"))+
    theme_cust()+
    ylab("Initial average DOC (ppm) ")+
    xlab("")+
    scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "Glacier" = "Glacier"))+
    theme(axis.text.x=element_text(angle = -45, hjust = 0))+
    theme(legend.position = "none")+ 
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16))
  
    
  ggplot(DOC_full, aes(x=Age, y = DOC_lost, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("DOC percent lost")+
    xlab("Apparent Age (years)") 
  ggsave(file ="Incub_percent_age.pdf",width=6, height=5, units = "in" )
  
  ggplot(DOC_full, aes(x=mean_HIX, y = DOC_lost, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("DOC percent lost")+
    xlab("mean HIX") 
  ggsave(file ="Incub_percent_HIX.pdf",width=6, height=5, units = "in" )
  
  ggplot(DOC_full, aes(x=Age, y = mean_HIX, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("mean HIX")+
    xlab("Apparent Age (years)")  
  ggsave(file ="Incub_HIX_age.pdf",width=6, height=5, units = "in" )
  
  ggplot(DOC_full, aes(x=mean_FI, y = DOC_lost, color =Site))+
    geom_point(size=5)+
    scale_color_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ),breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "Glacier"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" , "Terminus", "Glacier"))+
    theme_cust()+
    ylab("% BDOC (6 day incubation) ")+
    xlab("mean FI")+
    #theme(axis.text.x=element_text(angle = -45, hjust = 0))+
    theme(legend.position = "none")+ 
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16))
  
    

  
  ggplot(DOC_full, aes(x=Age, y = mean_FI, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("mean FI")+
    xlab("Apparent Age (years)")  
  ggsave(file ="Incub_HIX_age.pdf",width=6, height=5, units = "in" )
  
  ggplot(DOC_full, aes(x=mean_FDOM, y = DOC_lost, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("DOC percent lost")+
    xlab("mean FDOM") 
  ggsave(file ="Incub_percent_HIX.pdf",width=6, height=5, units = "in" )
  
  ggplot(DOC_full, aes(x=Age, y = mean_FDOM, color =Site))+
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("mean FDOM")+
    xlab("Apparent Age (years)")  
  ggsave(file ="Incub_HIX_age.pdf",width=6, height=5, units = "in" )
  
  
  ################## Loading in FDOM timeseries data ######################
  
  # loading files and getting the right time zone set 
 FDOM21 <- read.csv('2021.combined.corrected.fDOM.csv')
 FDOM21$Forest.datetime <- mdy_hm(FDOM21$Forest.datetime, tz='America/Anchorage')
 FDOM21$Tundra.datetime <- mdy_hm(FDOM21$Tundra.datetime, tz='America/Anchorage')
 FDOM21$Shrub.datetime <- mdy_hm(FDOM21$Shrub.datetime, tz='America/Anchorage')
 FDOM21$Nellie.datetime <- mdy_hm(FDOM21$Nellie.datetime, tz='America/Anchorage')
 
 WlFDOM21 <- read.csv('wolv.2021.WT.cor.fDOM.csv')
 WlFDOM21$datetime <- mdy_hm(WlFDOM21$datetime, tz='America/Anchorage')
 
 GlFDOM21 <- read.csv('glacier.2021.wt.cor.fdom.csv')
 GlFDOM21$datetime <- mdy_hm(GlFDOM21$datetime, tz='America/Anchorage')

 # pulling out each location into it's own dataframe so it can more easily be used in the next step 
glacier <- data.frame((GlFDOM21[,3:4]))
shrub = data.frame((FDOM21[,1:2]))
forest = data.frame((FDOM21[,3:4]))
tundra = data.frame((FDOM21[,5:6]))
nellie = data.frame((FDOM21[,7:8]))


start <- FDOM21$Shrub.datetime[1] #finding the first time step with data (i.e. Jan 1 00:00)
datetime_target <- data.frame(seq(start, start + days(365), by = "15 min")) #making the 15 min timeseries all other data will be matched to. 
# changing column names in all data frames so they can be merged more easily
colnames(datetime_target)<- ('datetime') 
colnames(shrub)<- c('datetime','shrub')
colnames(forest)<- c('datetime','forest')
colnames(tundra)<- c('datetime','tundra')
colnames(nellie)<- c('datetime','nellie')

# Merging all data frames from each site so they all align with a common time step and NA anywhere there is no observation due to sonde being down or different logging interval. 
FDOM21TS <- merge(datetime_target,shrub, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,forest, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,tundra, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,nellie, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,glacier, by = 'datetime',all.x = TRUE)
FDOM21TS <- merge(FDOM21TS,WlFDOM21, by = 'datetime',all.x = TRUE)
 
# repeating all of the above steps for 2022  
 FDOM22 <- read.csv('2022.combined.corrected.fDOM.csv')
 FDOM22$Forest.datetime <- mdy_hm(FDOM22$Forest.datetime, tz='America/Anchorage')
 FDOM22$Tundra.datetime <- mdy_hm(FDOM22$Tundra.datetime, tz='America/Anchorage')
 FDOM22$Shrub.datetime <- mdy_hm(FDOM22$Shrub.datetime, tz='America/Anchorage')
 FDOM22$Nellie.datetime <- mdy_hm(FDOM22$Nellie.datetime, tz='America/Anchorage')
 
 WlFDOM22 <- read.csv('wolv.2022.WT.cor.fDOM.csv')
 WlFDOM22$datetime <- mdy_hm(WlFDOM22$datetime, tz='America/Anchorage')
 
 forest = data.frame((FDOM22[,1:2]))
 tundra= data.frame((FDOM22[,3:4]))
 shrub = data.frame((FDOM22[,5:6]))
 nellie = data.frame((FDOM22[,7:8]))
 
 start <- FDOM22$Forest.datetime[1]
 datetime_target <- data.frame(seq(start, start + days(365), by = "15 min"))
 colnames(datetime_target)<- ('datetime')
 colnames(shrub)<- c('datetime','shrub')
 colnames(forest)<- c('datetime','forest')
 colnames(tundra)<- c('datetime','tundra')
 colnames(nellie)<- c('datetime','nellie')
 
 # forest and Nellie data not started on an even time step starting on may 4 (i.e. 1:06, 1:21, 1:36 ...)
 #this is just manually adjusting times to bring it back to closest 15 min step (i.e. 1:00, 1:15, 1:30 ...)
 forest$datetime[forest$datetime > mdy_hms('05/04/2022 12:00:00', tz='America/Anchorage') & forest$datetime < mdy_hms('08/04/2022 11:00:00', tz='America/Anchorage') & !is.na(forest$datetime)] =  
   forest$datetime[forest$datetime > mdy_hms('05/04/2022 12:00:00', tz='America/Anchorage') & forest$datetime < mdy_hms('08/04/2022 11:00:00', tz='America/Anchorage') & !is.na(forest$datetime)] - minutes(6)
 
 nellie$datetime[ !is.na(nellie$datetime)] =  
   nellie$datetime[ !is.na(nellie$datetime)] - minutes(9)
 
 FDOM22TS <- merge(datetime_target,forest, by = 'datetime',all.x = TRUE)
 FDOM22TS <- merge(FDOM22TS,shrub, by = 'datetime',all.x = TRUE)
 FDOM22TS <- merge(FDOM22TS,tundra, by = 'datetime',all.x = TRUE)
 FDOM22TS <- merge(FDOM22TS,nellie, by = 'datetime',all.x = TRUE)
 FDOM22TS <- merge(FDOM22TS,WlFDOM22, by = 'datetime',all.x = TRUE)
 
 ######### loading in Wolv gage and WX990 data
 #setting time bounds to pull data
 bounds <- as.POSIXct(c('01/01/2021 00:00:00','10/01/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
 
 #pulling Q data form NWIS server
 gauge_data <- readNWISdata(sites = '15236900', service = 'iv', parameterCd = '00060', 
                              startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2])) %>%
   select(datetime = dateTime, Q = X_00060_00000) %>%
   mutate(datetime = with_tz(datetime, tz = 'America/Anchorage'),Q_m3s = Q*0.028316847)

 #pulling met data from NWIS server 
met_data <- readNWISdata(sites = '15236895', service = 'iv', parameterCd = '72194', 
                            startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2]))  %>%
    select(datetime = dateTime, Precip1 = X_..2.._72194_00000, Precip2 = X_72194_00000) %>%
    mutate(datetime = with_tz(datetime, tz = 'America/Anchorage'))
# met data from server is the raw cumulative time series which includes draining of the TPG
# the following few lines clumsily deal with that

precip2 <- diff(met_data$Precip2) #taking the difference from one time step to the next to get 15 min instantaneous data 
precip2 <- data.frame(met_data$datetime[2:length(met_data$datetime)],precip2) #pulling the time stamp along with the 15 min instantaneous data
colnames(precip2)[1] ="datetime"  #changing the column header
precip2$precip2[precip2$precip2 > 300 |precip2$precip2 < 0 ] = NA #when the gage was drained there are artificially high values, this turns those and any negative values into no data

precip_hourly <- aggregate(precip2["precip2"], list(hour=cut(as.POSIXct(precip2$datetime), "hour")),sum) 
# summing 15 min data to hourly precip totals
precip_hourly$precip2[precip_hourly$precip2 == 0 ] = NA  # turning any hour that has no precip to an NA so it doesn't display as a 0 in plots. 
colnames(precip_hourly)<- c('datetime','precip_mm')
precip_hourly$datetime <- as.POSIXct(precip_hourly$datetime, tz='America/Anchorage')

#merging to one data frame
start <- precip_hourly$datetime[1] #finding the first time step with data (i.e. Jan 1 00:00)
datetime_target <- data.frame(seq(start, start + days(730), by = "15 min")) #making the 15 min timeseries all other data will be matched to. 
# changing column names in all data frames so they can be merged more easily
colnames(datetime_target)<- ('datetime') 

Precip_q_ts <- merge(datetime_target,gauge_data, by = 'datetime',all.x = TRUE)
Precip_q_ts <- merge(Precip_q_ts,precip_hourly, by = 'datetime',all.x = TRUE)
Precip_q_ts <- merge(Precip_q_ts,precip2, by = 'datetime',all.x = TRUE)

########### Comparing sonde FDOM to sample DOC ##############
rep_str = c('stream_gauge' = 'gage','Forest'= 'forest', 'shrub_creek' = 'shrub', 'Tundra' = 'tundra', 'Nellie_Juan' = 'nellie', 'glacier_hut' = 'glacier')
core_sites$Site <- str_replace_all(core_sites$Site, rep_str)
core_sites$Datetime <- round_date(core_sites$Datetime, "15 minutes")

core_lab22 <- core_sites[core_sites$yearS == 2022,] 
core_lab21 <- core_sites[core_sites$yearS == 2021,]

rownames(core_lab22)<- seq(1,nrow(core_lab22),1)
rownames(core_lab21)<- seq(1,nrow(core_lab21),1)
core_lab22$'Sonde' <- rep(NA, nrow(core_lab22))
core_lab21$'Sonde' <- rep(NA, nrow(core_lab21))

#####----- INPUT VALUES BY LOOPING THROUGH EACH VARIABLE NAME -----#####
i <- 2
k<- 4
for (i in 2:6){
  inds <- which(core_lab22$'Site' == colnames(FDOM22TS)[i])
  times <- core_lab22$Datetime[inds]
  for (k in 1:length(times)){
  time_inds <- which(FDOM22TS$'datetime' == times[k])
  temp <- FDOM22TS[(time_inds-8):(time_inds+8),i]
  if (all(is.na(temp))){
    core_lab22$'Sonde'[inds[k]] <-NA}
  else{
    core_lab22$'Sonde'[inds[k]] <- mean(na_remove(temp))}
  }
} 

for (i in 2:6){
  inds <- which(core_lab21$'Site' == colnames(FDOM21TS)[i])
  times <- core_lab21$Datetime[inds]
  for (k in 1:length(times)){
    time_inds <- which(FDOM21TS$'datetime' == times[k])
    temp <- FDOM21TS[(time_inds-8):(time_inds+8),i]
    if (all(is.na(temp))){
      core_lab21$'Sonde'[inds[k]] <-NA}
    else{
      core_lab21$'Sonde'[inds[k]] <- mean(na_remove(temp))}
  }
} 

core_site20s <- rbind(core_lab21, core_lab22)

#plot with just sonde FDOM v.s. DOC 
ggplot(core_site20s, aes(x=Sonde, y = DOC, color =as.factor(Site), group = 1))+
  scale_color_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF",  "#0084A8", "#059E41" , "#6600CC"),breaks = c( "forest" , "nellie" , "shrub" , "tundra" , "gage" , "glacier"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" ,  "Glacier"))+
  geom_point(size = 3, alpha = 0.7)+
  geom_smooth(method = "lm",formula = y~x, color = 'black')+
  theme_cust()+
  theme(legend.position = c(0.2,0.75)) +
  labs(color = "Site")+
  xlab("Sonde fDOM (qsu)")+
  ylab(bquote('DOC' (mgl^-1)))+  
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 16))

 DOC_lm <- lm( formula= DOC ~ Sonde, data = core_site20s)
 summary(DOC_lm)

#Plot with sonde FDOM and Lab FDOM v.s. DOC data 
ggplot()+
  geom_point(size = 3, alpha = 0.7,data = core_site20s,aes(y=Sonde, x=DOC, color =as.factor(Site)))+
  geom_point(size = 3, alpha = 0.7,shape = 15, data = core_site20s, aes(y=FDOM_lab, x=DOC,color =as.factor(Site)))+
  scale_color_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF",  "#0084A8", "#059E41" , "#6600CC"),breaks = c( "forest" , "nellie" , "shrub" , "tundra" , "gage" ,  "glacier", "Terminus","lake_inlet"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" ,"Glacier","Terminus", "Lake Inlet"))+
  theme_cust()+
  theme(legend.position = c(0.2,0.75)) +
  labs(color = "Site")+
  ylab("fDOM")+
  xlab(bquote('DOC' (mgl^-1)))+  
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 16))

#write.csv(core_site20s, "Site_FDOMdata.csv")

############## Applying linear regression to convert FDOM time series to DOC
DOC21TS <- 0.069*FDOM21TS[2:7]+0.377
DOC21TS <-  cbind(FDOM21TS[1], DOC21TS)

DOC22TS <- 0.069*FDOM22TS[2:6]+0.377
DOC22TS <-  cbind(FDOM22TS[1], DOC22TS)

### Loading the corrected 2022 gage FDOM data and applying regression for new DOC TS
Gage_Cor_FDOM22 <- read.csv('wolv_gage_corr_fdom18jun23.csv')%>%
  rename( datetime = ISO.8601.UTC, FDOM = Value)

Gage_Cor_FDOM22$datetime <- as.POSIXct(Gage_Cor_FDOM22$datetime, "%Y-%m-%dT%H:%M:%S", tz="UTC")
Gage_Cor_FDOM22$DOC <-0.069* Gage_Cor_FDOM22$FDOM+0.377 


########## Creating a multi-panel plot of the full two years of data ######################

#FDOM data 
  ts1<- ggplot()+
    geom_point(data = DOC21TS, aes(x=datetime, y= forest), color = "#E2725B", size = 0.2)+
    geom_point(data = DOC21TS, aes(x=datetime, y= tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = DOC21TS, aes(x=datetime, y= shrub), color = "#FFAA00", size = 0.2)+
    geom_point(data = DOC21TS, aes(x=datetime, y= nellie), color = "#EA9DFF", size = 0.2)+
    geom_point(data = DOC21TS, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
    geom_point(data = DOC21TS, aes(x=datetime, y= glacier), color = "#0084A8", size = 0.2)+
    
    geom_point(data = DOC22TS, aes(x=datetime, y= forest), color = "#E2725B", size = 0.2)+
    geom_point(data = DOC22TS, aes(x=datetime, y= tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = DOC22TS, aes(x=datetime, y= shrub), color = "#FFAA00", size = 0.2)+
    geom_point(data = DOC22TS, aes(x=datetime, y= nellie), color = "#EA9DFF", size = 0.2)+
    geom_point(data = DOC22TS, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
    ylab(bquote('DOC' (mgl^-1)))+
    xlab('')+
    theme_cust()
    
    #theme(axis.text = element_text(size = 16))+
    #theme(axis.title = element_text(size = 16)) 
    
    
  #Precip and Q data  
  # Calculate the range needed to avoid having your hyetograph and hydrograph overlap 
  maxRange <- 1000 # set how wide of the first axis (streamflow)
  coeff <- .05 # set the shrink coeffcient of Precipitation
  # Use geom_tile to create the inverted hyetograph
  # y = the center point of each bar
  # maxRange - Precipitation/coeff/2
  ts2<- ggplot(data= Precip_q_ts, aes(x= datetime))+
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

  Rsep21 <- Precip_q_ts %>%
   filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 
  
    Rsep21DOC <- DOC21TS %>%
      filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 
    
    maxRange <- 10 # set how wide of the first axis (streamflow)
    coeff <- 1 # set the shrink coeffcient of Precipitation
  
     ggplot()+
    geom_point(data = Rsep21DOC, aes(x=datetime, y= forest), color = "#E2725B", size = 0.2)+
    geom_point(data = Rsep21DOC, aes(x=datetime, y= tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = Rsep21DOC, aes(x=datetime, y= shrub), color = "#FFAA00", size = 0.2)+
    geom_point(data = Rsep21DOC, aes(x=datetime, y= nellie), color = "#EA9DFF", size = 0.2)+
    geom_point(data = Rsep21DOC, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
    geom_point(data = Rsep21DOC, aes(x=datetime, y= glacier), color = "#0084A8", size = 0.2)+
   
    geom_line(data = Rsep21, aes(x=datetime, y= Q/100), color = 'black', size = 0.5)+
    geom_tile(data = Rsep21, aes(x=datetime, y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
    scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
    xlim(bounds_Rsep21)+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16))
  
  ########## Rain August 2022 ##########
  bounds_Rjul22<- as.POSIXct(c('08/16/2022 00:00:00','08/22/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
  
     
     Rjul22 <- Precip_q_ts %>%
       filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 
     
     Rjul22DOC <- DOC22TS %>%
       filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 
     
     maxRange <- 10 # set how wide of the first axis (streamflow)
     coeff <- 1 # set the shrink coeffcient of Precipitation
     
     ggplot()+
       geom_point(data = Rjul22DOC, aes(x=datetime, y= forest), color = "#E2725B", size = 0.2)+
       geom_point(data = Rjul22DOC, aes(x=datetime, y= tundra), color = "#A80084", size = 0.2 )+
       geom_point(data = Rjul22DOC, aes(x=datetime, y= shrub), color = "#FFAA00", size = 0.2)+
       geom_point(data = Rjul22DOC, aes(x=datetime, y= nellie), color = "#EA9DFF", size = 0.2)+
       geom_point(data = Rjul22DOC, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
       
       geom_line(data = Rjul22, aes(x=datetime, y= Q/100), color = 'black', size = 0.5)+
       geom_tile(data = Rjul22, aes(x=datetime, y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
       scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
       xlim(bounds_Rjul22)+
       
       xlab('')+
       theme_cust()+
       theme(axis.text = element_text(size = 16))+
       theme(axis.title = element_text(size = 16))
     
     
  ########## Snow melt 2022 ##########
  bounds_Mmay22<- as.POSIXct(c('05/16/2022 00:00:00','05/29/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

     Mmay22 <- Precip_q_ts %>%
       filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2]) 
     
     Mmay22DOC <- DOC22TS %>%
       filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2]) 
     
     maxRange <- 3 # set how wide of the first axis (streamflow)
     coeff <- 1 # set the shrink coeffcient of Precipitation
     
     ggplot()+
       geom_point(data = Mmay22DOC, aes(x=datetime, y= forest), color = "#E2725B", size = 0.2)+
       geom_point(data = Mmay22DOC, aes(x=datetime, y= tundra), color = "#A80084", size = 0.2 )+
       geom_point(data = Mmay22DOC, aes(x=datetime, y= shrub), color = "#FFAA00", size = 0.2)+
       geom_point(data = Mmay22DOC, aes(x=datetime, y= nellie), color = "#EA9DFF", size = 0.2)+
       geom_point(data = Mmay22DOC, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
       
       geom_line(data = Mmay22, aes(x=datetime, y= Q/100), color = 'black', size = 0.5)+
       geom_tile(data = Mmay22, aes(x=datetime, y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
       scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
       xlim(bounds_Mmay22)+
       
       xlab('')+
       theme_cust()+
       theme(axis.text = element_text(size = 16))+
       theme(axis.title = element_text(size = 16))
     
  
  ############# Making stacked bar of land cover #####################
  
  land_cover <- read.csv('land_cover.csv')
  
  ggplot(land_cover, aes(fill=cover, y=value, x=reorder(Site,DOC))) + 
    geom_bar(position="stack", stat="identity", color = "black")+
    scale_fill_manual(values = c("#C1D78D", "#FFFFFF", "#C79F89","#919191",  "#4E6E60" ), breaks= c("Tundra", "Ice", "Shrub", "Rock", "Forest"))+
    ylab(expression(paste("Percent of watershed")))+
    xlab("")+
    theme_cust()+
    theme(axis.text.x=element_text(angle = -45, hjust = 0))+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16))    

  
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
  
  # manipulating the met TS data to feed it into the API function
  #calculating daily precip totals
  day_sum <- aggregate(precip2["precip2"], list(hour=cut(as.POSIXct(precip2$datetime), "day")),sum, na.rm = TRUE) %>%
    mutate(datetime = with_tz(hour, tz = 'America/Anchorage'))
  
  API_990 <- getApi(day_sum$precip2, k = 0.9, n= 7) 
  day_sum <- data.frame(day_sum,API_990) 
  
  ans <-day_sum$API_990[day_sum$datetime == bounds_sub[1]] #Finding an API for a specific time
  
  
  ######### Calculating a running average for Q ###############
  swindow <- 192 #Setting smoothing window
  
  gauge_data<- gauge_data %>%
    mutate(datetime = with_tz(datetime, tz = 'America/Anchorage'),
         Q_mm_d = (Q_m3s/(9.4*2.59e6))*1000, #normalizing Q by wtrshd area and converting to mm 
         Q_filled = na_interpolation(Q_mm_d, option = 'linear', maxgap = Inf),
         Q_smoothed = rollapply(Q_mm_d,swindow,mean, na.rm = TRUE, fill = NA))
  
   gage_daily <- gauge_data %>%
      mutate(datetime_daily = cut(datetime, 'day')) %>%
      group_by(datetime_daily) %>% 
      summarise(Q_mean = mean(Q_filled)) %>%
      na.omit() %>%
      mutate(Q_mean = Q_mean*60*60*24,
             Rmean_07da = zoo::rollmean(Q_mean, k = 7, fill = NA),
             datetime_daily = with_tz(datetime_daily, tz = 'America/Anchorage'))
     
  #### Plotting #########
  bounds_sub<- as.POSIXct(c('05/01/2022 00:00:00','10/05/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
  bounds_sub2<- as.POSIXct(c('09/09/2021 00:00:00','09/15/2021 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
  
  ggplot()+
    geom_line(data = day_sum, aes(x=datetime, y= API_990), color = 'black', size = 0.5)+
    geom_line(data = gage_daily, aes(x=datetime_daily, y= Rmean_07da), color = 'blue', size = 0.5)+
    ylim(0,70)+
    xlim(bounds_sub)+
    #ylab("fDOM (QSU)")+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
############# Calculating DOC fluxes ##############################
  
  #2022 
  bounds_DOCFlux<- as.POSIXct(c('04/25/2022 09:00:00','10/24/2022 08:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
  
  Q_flux_short <- Precip_q_ts %>%
    filter(as.POSIXct(datetime, TZ = "America/Anchorage") >= bounds_DOCFlux[1], as.POSIXct(datetime, TZ = "America/Anchorage") <= bounds_DOCFlux[2]) 
  
  Gage_Cor_FDOM22 <- Gage_Cor_FDOM22 %>%
   mutate(gage_flux_kgha15min = DOC/0.001*3600*24*1e-6/2707/4/24*Q_flux$Q_m3s,
         cum_flux = cumsum(coalesce(gage_flux_kgha15min, 0)) + gage_flux_kgha15min*0) 
  
  #DOC22TS <- DOC22TS %>%
   # mutate(gage_flux_kgha15min = gage/0.001*3600*24*1e-6/2707/4/24*Q_flux$Q_m3s,
    #       cum_flux = cumsum(coalesce(gage_flux_kgha15min, 0)) + gage_flux_kgha15min*0)
   
  ggplot()+
    geom_line(data = DOC22TS, aes(x=datetime, y = gage), color = 'black', size = 0.5)+
    geom_point(data = DOC22TS, aes(x=datetime, y= cum_flux), color = '#006633', size = 0.5)+
    geom_line(data = Q_flux, aes(x=datetime, y= Q_m3s/10), color = 'blue', size = 0.5)+
    xlab('')+
    xlim(bounds_sub)+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
  

  