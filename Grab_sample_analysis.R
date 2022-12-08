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
  mutate(.before = "Temperature" , doy = yday(Datetime)) %>%
  mutate(.before = "Temperature" , yearS = year(Datetime))

#reordering datasets by sample collection time 
temp<- order(full_data$Datetime)
full_data <- full_data[temp,]


#################### Subsetting data to only include core sites ####################
core_sites <- full_data %>%
  filter(Site == "Forest" | Site == "Nellie_Juan" | Site == "shrub_creek" | Site == "Tundra" |Site == "stream_gauge" | Site == "Terminus" |Site == "glacier_hut")

site_names <- c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "glacier_lake")


ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= DOC, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab(bquote('DOC' (mgl^-1)))+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  


ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= FDOM_lab, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("FDOM")+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  

ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= FI, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  geom_hline(yintercept=1.9, linetype="dashed", color = "#1A237E", size=1)+
  geom_hline(yintercept=1.4, linetype="dashed", color = "#43A047", size=1)+
  ylab("Fluorescence Index")+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))

ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= HIX, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8"), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut"))+
  geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("HIX")+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")  

ggplot(core_sites, aes(x=DOC, y = FI, color =as.factor(Site)))+
  scale_color_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8" ),breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" , "Terminus", "Glacier"))+
  geom_point(size = 4, alpha = 0.7)+
  geom_hline(yintercept=1.9, linetype="dashed", color = "#1A237E", size=1)+
  geom_hline(yintercept=1.4, linetype="dashed", color = "#43A047", size=1)+
  theme_cust()+
  theme(legend.position = c(0.87,0.78)) +
  ylim(1,2.7)+
  labs(color = "Site")+
  ylab("Fluorescence Index")+
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

  DOC_Fparams<- incub_summary %>%
    filter(Time.Point == 0) %>%
    subset(select = c(Site,mean_FI, mean_HIX, mean_FDOM ))
  
  for (k in 1:length(incub_names)) {
    temp <- incub_summary %>%
    filter(Site == incub_names[k] & (Time.Point == 0 |Time.Point == 2)) 
    
    DOC_lost[k] <- ((temp$meanconc[1] - temp$meanconc[2])/temp$meanconc[1])*100  
  }
  
  DOC_lost <- data.frame(DOC_lost,incub_names) %>%
    rename(Site = incub_names)
  
  DOC_full <- merge(DOC_lost, NOSAMS, by="Site")
  DOC_full <- merge(DOC_full, DOC_Fparams, by="Site")
  
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
    geom_line()+
    geom_point()+
    theme_cust()+
    ylab("DOC percent lost")+
    xlab("mean FI") 
  ggsave(file ="Incub_percent_HIX.pdf",width=6, height=5, units = "in" )
  
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
  
 FDOM21 <- read.csv('2021.combined.corrected.fDOM.csv')
 FDOM21$Forest.datetime <- mdy_hm(FDOM21$Forest.datetime, tz='America/Anchorage')
 FDOM21$Tundra.datetime <- mdy_hm(FDOM21$Tundra.datetime, tz='America/Anchorage')
 FDOM21$Shrub.datetime <- mdy_hm(FDOM21$Shrub.datetime, tz='America/Anchorage')
 FDOM21$Nellie.datetime <- mdy_hm(FDOM21$Nellie.datetime, tz='America/Anchorage')
  
 FDOM22 <- read.csv('2022.combined.corrected.fDOM.csv')
 FDOM22$Forest.datetime <- mdy_hm(FDOM22$Forest.datetime, tz='America/Anchorage')
 FDOM22$Tundra.datetime <- mdy_hm(FDOM22$Tundra.datetime, tz='America/Anchorage')
 FDOM22$Shrub.datetime <- mdy_hm(FDOM22$Shrub.datetime, tz='America/Anchorage')
 FDOM22$Nellie.datetime <- mdy_hm(FDOM22$Nellie.datetime, tz='America/Anchorage')
 
 WlFDOM21 <- read.csv('wolv.2021.WT.cor.fDOM.csv')
 WlFDOM21$datetime <- mdy_hm(WlFDOM21$datetime, tz='America/Anchorage')
 
 WlFDOM22 <- read.csv('wolv.2022.WT.cor.fDOM.csv')
 WlFDOM22$datetime <- mdy_hm(WlFDOM22$datetime, tz='America/Anchorage')
 
 GlFDOM21 <- read.csv('glacier.2021.wt.cor.fdom.csv')
 GlFDOM21$datetime <- mdy_hm(GlFDOM21$datetime, tz='America/Anchorage')
 
 bounds <- as.POSIXct(c('01/01/2021 00:00:00','10/01/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
 
 gauge_data <- readNWISdata(sites = '15236900', service = 'iv', parameterCd = '00060', 
                              startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2])) %>%
   select(datetime = dateTime, Q = X_00060_00000) %>%
   mutate(datetime = with_tz(datetime, tz = 'America/Anchorage'),Q_m3s = Q*0.028316847)
 
met_data <- readNWISdata(sites = '15236895', service = 'iv', parameterCd = '72194', 
                            startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2]))  %>%
    select(datetime = dateTime, Precip1 = X_..2.._72194_00000, Precip2 = X_72194_00000) %>%
    mutate(datetime = with_tz(datetime, tz = 'America/Anchorage'))

precip2 <- diff(met_data$Precip2) 
precip2 <- data.frame(met_data$datetime[2:length(met_data$datetime)],precip2) 
colnames(precip2)[1] ="datetime" 
precip2$precip2[precip2$precip2 > 300 |precip2$precip2 < 0 ] = NA

  
Sum <- aggregate(precip2["precip2"], list(hour=cut(as.POSIXct(precip2$datetime), "hour")),sum) %>%
  mutate(datetime = with_tz(hour, tz = 'America/Anchorage'))
Sum$precip2[Sum$precip2 == 0 ] = NA 


  ts1<- ggplot()+
    geom_point(data = FDOM21, aes(x=Forest.datetime, y= Forest), color = "#E2725B", size = 0.2)+
    geom_point(data = FDOM21, aes(x=Tundra.datetime, y= Tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = FDOM21, aes(x=Shrub.datetime, y= shrub_creek), color = "#FFAA00", size = 0.2)+
    geom_point(data = FDOM21, aes(x=Nellie.datetime, y= Nellie_Juan), color = "#EA9DFF", size = 0.2)+
    geom_point(data = WlFDOM21, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
    geom_point(data = GlFDOM21, aes(x=datetime, y= glacier), color = "#0084A8", size = 0.2)+
    
    
    geom_point(data = FDOM22, aes(x=Forest.datetime, y= Forest), color = "#E2725B", size = 0.2)+
    geom_point(data = FDOM22, aes(x=Tundra.datetime, y= Tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = FDOM22, aes(x=Shrub.datetime, y= shrub_creek), color = "#FFAA00", size = 0.2)+
    geom_point(data = FDOM22, aes(x=Nellie.datetime, y= Nellie_Juan), color = "#EA9DFF", size = 0.2)+
    geom_point(data = WlFDOM22, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
    ylab("fDOM (QSU)")+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
 ts2<- ggplot()+
    geom_point(data = gauge_data, aes(x=datetime, y= Q), color = 'black', size = 0.2)+
    xlim(bounds)+
    ylab(expression(paste("Discharge (m"^"3","s"^"-1", ")")))+
    xlab('')+
    theme_cust()+
   theme(axis.text = element_text(size = 16))+
   theme(axis.title = element_text(size = 16))
 
 bothTS <- plot_grid(ts2, ts1, ncol=1, align = "v")
 bothTS
 ########## Rain Sep 2021 ##########
  bounds_sub<- as.POSIXct(c('09/09/2021 00:00:00','09/15/2021 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
  
   p1 <- ggplot()+
    geom_point(data = FDOM21, aes(x=Forest.datetime, y= Forest), color = "#E2725B", size = 0.2)+
    geom_point(data = FDOM21, aes(x=Tundra.datetime, y= Tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = FDOM21, aes(x=Shrub.datetime, y= shrub_creek), color = "#FFAA00", size = 0.2)+
    geom_point(data = FDOM21, aes(x=Nellie.datetime, y= Nellie_Juan), color = "#EA9DFF", size = 0.2)+
    geom_point(data = WlFDOM21, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
     geom_point(data = GlFDOM21, aes(x=datetime, y= glacier), color = "#0084A8", size = 0.2)+
    geom_line(data = gauge_data, aes(x=datetime, y= Q/10), color = 'black', size = 0.5)+
    scale_y_continuous('fDOM (QSU)', sec.axis = sec_axis(~.*10, name = expression(paste("Discharge (ft"^"3","s"^"-1", ")")))) +
    xlim(bounds_sub)+
    #ylab("fDOM (QSU)")+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16))
  
  p2<- ggplot()+
    geom_point(data = Sum, aes(x=datetime, y= (precip2)), color = 'darkslateblue')+
    xlim(bounds_sub)+
    scale_y_reverse()+
    ylim(7.5,0)+
    ylab(expression(paste("Precipitation (mm ","hr"^"-1", ")")))+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
  both <- plot_grid(p2, p1, ncol=1, align = "v")
  both
  
  ########## Rain July 2022 ##########
  bounds_sub<- as.POSIXct(c('08/16/2022 00:00:00','08/22/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
  
  p1 <- ggplot()+
    geom_point(data = FDOM22, aes(x=Forest.datetime, y= Forest), color = "#E2725B", size = 0.2)+
    geom_point(data = FDOM22, aes(x=Tundra.datetime, y= Tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = FDOM22, aes(x=Shrub.datetime, y= shrub_creek), color = "#FFAA00", size = 0.2)+
    geom_point(data = FDOM22, aes(x=Nellie.datetime, y= Nellie_Juan), color = "#EA9DFF", size = 0.2)+
    geom_point(data = WlFDOM22, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
    geom_line(data = gauge_data, aes(x=datetime, y= Q/10), color = 'black', size = 0.5)+
    scale_y_continuous('fDOM (QSU)', sec.axis = sec_axis(~.*10, name = expression(paste("Discharge (ft"^"3","s"^"-1", ")")))) +
    xlim(bounds_sub)+
    #ylab("fDOM (QSU)")+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
  p2<- ggplot()+
    geom_point(data = Sum, aes(x=datetime, y= (precip2)), color = 'darkslateblue')+
    xlim(bounds_sub)+
    scale_y_reverse()+
    ylim(7.5,0)+
    ylab(expression(paste("Precipitation (mm ","hr"^"-1", ")")))+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
  both <- plot_grid(p2, p1, ncol=1, align = "v")
  both
  
  ########## Snow melt 2022 ##########
  bounds_sub<- as.POSIXct(c('05/23/2022 00:00:00','05/29/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
  
  p1 <- ggplot()+
    geom_point(data = FDOM22, aes(x=Forest.datetime, y= Forest), color = "#E2725B", size = 0.2)+
    geom_point(data = FDOM22, aes(x=Tundra.datetime, y= Tundra), color = "#A80084", size = 0.2 )+
    geom_point(data = FDOM22, aes(x=Shrub.datetime, y= shrub_creek), color = "#FFAA00", size = 0.2)+
    geom_point(data = FDOM22, aes(x=Nellie.datetime, y= Nellie_Juan), color = "#EA9DFF", size = 0.2)+
    geom_point(data = WlFDOM22, aes(x=datetime, y= gage), color = "#73DFFF", size = 0.2)+
    geom_line(data = gauge_data, aes(x=datetime, y= Q/10), color = 'black', size = 0.5)+
    scale_y_continuous('fDOM (QSU)', sec.axis = sec_axis(~.*10, name = expression(paste("Discharge (ft"^"3","s"^"-1", ")")))) +
    xlim(bounds_sub)+
    #ylab("fDOM (QSU)")+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
  p2<- ggplot()+
    geom_point(data = Sum, aes(x=datetime, y= (precip2)), color = 'darkslateblue')+
    xlim(bounds_sub)+
    scale_y_reverse()+
    ylim(7.5,0)+
    ylab(expression(paste("Precipitation (mm ","hr"^"-1", ")")))+
    xlab('')+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
  both <- plot_grid(p2, p1, ncol=1, align = "v")
  both
  
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

  