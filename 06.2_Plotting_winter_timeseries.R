##06.2_Plotting_winter_subsets
# This brings in the DOC, Precip, EC, and Relative Stage timeseries for plotting
# Pulls out winter of 2021-22 and 2022-23 and makes plots
rm(list= ls())
source("paths+packages.R")

# load necessary data
RelST_FullTS <- read.csv('outputs/06_relative_stageTS.csv')
DOC_FullTS <- read.csv('outputs/06_DOC_FullTS.csv')
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')
#Precip_Q <- read.csv('outputs/04_Precip_q_ts.csv') 

#New met and gage data from Seward, all getting loaded in as separate files 
gage_data <- read.csv('outputs/04_gageQ_data.csv')
Wx990_temp <- read.csv('outputs/04_Wx990_temp.csv')
Seward_temp <- read.csv('outputs/04_Seward_temp.csv')
Seward_precip <- read.csv('outputs/04_Seward_precip.csv')



# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
RelST_FullTS$datetime <- strptime(RelST_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
DOC_FullTS$datetime <- strptime(DOC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS$datetime <- strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
#Precip_Q$datetime <- strptime(Precip_Q$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

gage_data$datetime <- strptime(gage_data$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Wx990_temp$datetime <- strptime(Wx990_temp$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Seward_temp$datetime <- strptime(Seward_temp$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Seward_precip$datetime <- strptime(Seward_precip$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

#Precip_Q <- Precip_Q %>%
#  mutate(sm.990T = rollapply(AirT, 32,mean, na.rm = TRUE, fill = NA) )

Wx990_temp <-  Wx990_temp %>%
  mutate(sm.990T = rollapply(AirT990, 32,mean, na.rm = TRUE, fill = NA) )
Seward_temp <- Seward_temp %>%
  mutate(sm.SewT = rollapply(Sew_AirT_C, 8,mean, na.rm = TRUE, fill = NA) )

########## Winter 2021-22 ##########
bounds_W2121<- as.POSIXct(c('11/01/2021 00:00:00','06/01/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Win21_22990T <- Wx990_temp %>%
  filter(as.POSIXct(datetime) >= bounds_W2121[1], as.POSIXct(datetime) <= bounds_W2121[2]) 

Win21_22DOC <- DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_W2121[1], as.POSIXct(datetime) <= bounds_W2121[2]) 

Win21_22SewT <- Seward_temp %>%
  filter(as.POSIXct(datetime) >= bounds_W2121[1], as.POSIXct(datetime) <= bounds_W2121[2]) 

Win21_22SewP <- Seward_precip %>%
  filter(as.POSIXct(datetime) >= bounds_W2121[1], as.POSIXct(datetime) <= bounds_W2121[2]) 


maxRange <- 10
coeff <- 2 # set the shrink coeffcient of Precipitation

DOC21 <- ggplot()+
  geom_line(data = Win21_22DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Win21_22DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Win21_22DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Win21_22DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Win21_22DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  ylim(0,5)+
  scale_x_datetime(limits = as.POSIXct(bounds_W2121),breaks = scales::date_breaks("months")  , date_labels = "%b")+
  xlab('')+
  ylab(bquote('DOC' (mgl^-1)))+
  theme_cust()
  
AirT21 <- ggplot()+ 
  geom_line(data = Win21_22SewT, aes(x=as.POSIXct(datetime), y= sm.SewT), color = '#023d1d', size = 0.5)+
  geom_line(data = Win21_22990T, aes(x=as.POSIXct(datetime), y= sm.990T), color = '#9bfac6', size = 0.5)+
  geom_hline(yintercept = 0, linetype="dashed", color = "#1A237E", size=0.5) +
  ylim(-20,20)+
  scale_x_datetime(limits = as.POSIXct(bounds_W2121),breaks = scales::date_breaks("months")  , date_labels = "%b")+
  xlab('')+
  ylab(bquote('Air Temperature (\u00B0C)'))+
  theme_cust()  

Precip21 <- ggplot()+     
  geom_tile(data = Win21_22SewP, aes(x=as.POSIXct(datetime), y = maxRange - precip/2, height = precip),  color = '#42ecf5', fill = '#42ecf5')+ 
  #scale_y_reverse()+
  #xlim(bounds_W2121)+
  scale_x_datetime(limits = as.POSIXct(bounds_W2121),breaks = scales::date_breaks("months")  , date_labels = "%b")+
  ylab(bquote('Precipiation ' (mmhr^-1)))+
  xlab('')+
  ylim(0,maxRange)+
  theme_cust()

win21_full_plot <- plot_grid(Precip21, AirT21, DOC21,  ncol=1, align = "v")
print(win21_full_plot)

########## Winter 2022-23 ##########
bounds_W2223<- as.POSIXct(c('11/01/2022 00:00:00','06/01/2023 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Win22_23990T <- Wx990_temp %>%
  filter(as.POSIXct(datetime) >= bounds_W2223[1], as.POSIXct(datetime) <= bounds_W2223[2]) 

Win22_23DOC <- DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_W2223[1], as.POSIXct(datetime) <= bounds_W2223[2]) 

Win22_23SewT <- Seward_temp %>%
  filter(as.POSIXct(datetime) >= bounds_W2223[1], as.POSIXct(datetime) <= bounds_W2223[2]) 

Win22_23SewP <- Seward_precip %>%
  filter(as.POSIXct(datetime) >= bounds_W2223[1], as.POSIXct(datetime) <= bounds_W2223[2]) 




maxRange <- 10
coeff <- 2 # set the shrink coeffcient of Precipitation

DOC22 <- ggplot()+
  geom_line(data = Win22_23DOC, aes(x=as.POSIXct(datetime), y= forest), color = col.forest, size = 0.5)+
  geom_line(data = Win22_23DOC, aes(x=as.POSIXct(datetime), y= tundra), color = col.tundra, size = 0.5 )+
  geom_line(data = Win22_23DOC, aes(x=as.POSIXct(datetime), y= shrub), color = col.shrub, size = 0.5)+
  geom_line(data = Win22_23DOC, aes(x=as.POSIXct(datetime), y= nellie), color = col.nellie, size = 0.5)+
  geom_line(data = Win22_23DOC, aes(x=as.POSIXct(datetime), y= gage), color = col.gage, size = 0.5)+
  ylim(0,5)+
  scale_x_datetime(limits = as.POSIXct(bounds_W2223),breaks = scales::date_breaks("months")  , date_labels = "%b")+
  xlab('')+
  ylab(bquote('DOC' (mgl^-1)))+
  theme_cust()

AirT22 <- ggplot()+ 
  geom_line(data = Win22_23SewT, aes(x=as.POSIXct(datetime), y= sm.SewT), color = '#023d1d', size = 0.5)+
  geom_line(data = Win22_23990T, aes(x=as.POSIXct(datetime), y= sm.990T), color = '#9bfac6', size = 0.5)+
  geom_hline(yintercept = 0, linetype="dashed", color = "#1A237E", size=0.5) +
  ylim(-20,20)+
  scale_x_datetime(limits = as.POSIXct(bounds_W2223),breaks = scales::date_breaks("months")  , date_labels = "%b")+
  xlab('')+
  ylab(bquote('Air Temperature (\u00B0C)'))+
  theme_cust()  

Precip22 <- ggplot()+     
  geom_tile(data = Win22_23SewP, aes(x=as.POSIXct(datetime), y = maxRange - precip/2, height = precip),  color = '#42ecf5', fill = '#42ecf5')+ 
  #scale_y_reverse()+
  #xlim(bounds_W2223)+
  scale_x_datetime(limits = as.POSIXct(bounds_W2223),breaks = scales::date_breaks("months")  , date_labels = "%b")+
  ylab(bquote('Precipiation ' (mmhr^-1)))+
  xlab('')+
  ylim(0,maxRange)+
  theme_cust()

win22_full_plot <- plot_grid(Precip22, AirT22, DOC22,  ncol=1, align = "v")
print(win22_full_plot)

