##06_Plotting_timeseries_subsets
# This brings in the DOC, Precip, EC, and Relative Stage timeseries for plotting
# Pulls out events as examples and makes plots
rm(list= ls())
source("paths+packages.R")

# load necessary data
RelST_FullTS <- read.csv('outputs/06_relative_stageTS.csv')
DOC_FullTS <- read.csv('outputs/06_DOC_FullTS.csv')
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')
Precip_Q <- read.csv('outputs/04_Precip_q_ts.csv') 


# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
RelST_FullTS$datetime <- strptime(RelST_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
DOC_FullTS$datetime <- strptime(DOC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS$datetime <- strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Precip_Q$datetime <- strptime(Precip_Q$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

# Setting up end points for color palettes that are a gradient as a function of time
gagePal <- colorRampPalette(c('#d2f3fc','#02c1fa'))
shrubPal <- colorRampPalette(c('#fcdb97','#a66f02'))
forestPal <- colorRampPalette(c('#deb4ab','#de2700'))
tundraPal <- colorRampPalette(c('#fcf2fa','#800064'))
nelliePal <- colorRampPalette(c('#e5c2fc','#9700fc'))

########## Snow melt 2022 ##########
bounds_Mmay22<- as.POSIXct(c('05/14/2022 00:00:00','05/16/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Mmay22 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2]) 

Mmay22DOC <- DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2]) 

Mmay22EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2])

Mmay22St <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Mmay22[1], as.POSIXct(datetime) <= bounds_Mmay22[2]) 


Mmay22TS <- merge(Mmay22EC,Mmay22DOC, by = 'datetime',all.x = TRUE)
Mmay22TS <- merge(Mmay22TS,Mmay22St, by = 'datetime',all.x = TRUE)

maxRange <- 5 # set how wide of the first axis (streamflow)
coeff <- 1 # set the shrink coeffcient of Precipitation

DOC1 <- ggplot()+
  geom_line(data = Mmay22DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Mmay22DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Mmay22DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Mmay22DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Mmay22DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  geom_line(data = Mmay22, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Mmay22, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Mmay22)+
  
  xlab('')+
  theme_cust()
#theme(axis.text = element_text(size = 16))+
#theme(axis.title = element_text(size = 16))


Mmay22TS$gage_pal <- gagePal(20)[as.numeric(cut(Mmay22TS$datetime,breaks = 20))]
Mmay22TS$shrub_pal <- shrubPal(20)[as.numeric(cut(Mmay22TS$datetime,breaks = 20))]
Mmay22TS$forest_pal <- forestPal(20)[as.numeric(cut(Mmay22TS$datetime,breaks = 20))]
Mmay22TS$tundra_pal <- tundraPal(20)[as.numeric(cut(Mmay22TS$datetime,breaks = 20))]
Mmay22TS$nellie_pal <- nelliePal(20)[as.numeric(cut(Mmay22TS$datetime,breaks = 20))]

EC1 <- ggplot(data = Mmay22TS)+ 
  geom_point(aes(x = forest.x, y = forest.y), color = Mmay22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub.x, y = shrub.y), color = Mmay22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra.x, y = tundra.y), color = Mmay22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = gage.x, y = gage.y), color = Mmay22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlim(0,60)+
  ylim(0,4)+
  theme_cust()

St1 <- ggplot(data = Mmay22TS)+ 
  geom_point(aes(x = forest, y = forest.y), color =Mmay22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.y), color = Mmay22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.y), color = Mmay22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.y), color = Mmay22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.y), color = Mmay22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES1 <- ggplot(data = Mmay22TS)+ 
  geom_point(aes(x = forest, y = forest.x), color = Mmay22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.x), color = Mmay22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.x), color = Mmay22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.x), color = Mmay22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.x), color = Mmay22TS$gage_pal, size = 0.75)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()


########## Dry July 2022 ##########
bounds_Djul22<- as.POSIXct(c('07/01/2022 00:00:00','07/02/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")


Djul22 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Djul22[1], as.POSIXct(datetime) <= bounds_Djul22[2]) 

Djul22DOC <- DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Djul22[1], as.POSIXct(datetime) <= bounds_Djul22[2]) 

Djul22EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Djul22[1], as.POSIXct(datetime) <= bounds_Djul22[2]) 

Djul22St <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Djul22[1], as.POSIXct(datetime) <= bounds_Djul22[2])

Djul22TS <- merge(Djul22EC,Djul22DOC, by = 'datetime',all.x = TRUE)
Djul22TS <- merge(Djul22TS,Djul22St, by = 'datetime',all.x = TRUE)

maxRange <- 5 # set how wide of the first axis (streamflow)
coeff <- 1 # set the shrink coeffcient of Precipitation

DOC2 <- ggplot()+
  geom_line(data = Djul22DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Djul22DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Djul22DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Djul22DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Djul22DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  geom_line(data = Djul22, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Djul22, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Djul22)+
  
  xlab('')+
  theme_cust()
#theme(axis.text = element_text(size = 16))+
#theme(axis.title = element_text(size = 16))

#Setting up color palettes for hysteresis plots
Djul22TS$gage_pal <- gagePal(20)[as.numeric(cut(Djul22TS$datetime,breaks = 20))]
Djul22TS$shrub_pal <- shrubPal(20)[as.numeric(cut(Djul22TS$datetime,breaks = 20))]
Djul22TS$forest_pal <- forestPal(20)[as.numeric(cut(Djul22TS$datetime,breaks = 20))]
Djul22TS$tundra_pal <- tundraPal(20)[as.numeric(cut(Djul22TS$datetime,breaks = 20))]
Djul22TS$nellie_pal <- nelliePal(20)[as.numeric(cut(Djul22TS$datetime,breaks = 20))]

EC2 <- ggplot(data = Djul22TS)+ 
  geom_point(aes(x = forest.x, y = forest.y), color = Djul22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub.x, y = shrub.y), color = Djul22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra.x, y = tundra.y), color = Djul22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = gage.x, y = gage.y), color = Djul22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlim(0,60)+
  ylim(0,4)+
  theme_cust()

St2 <- ggplot(data = Djul22TS)+ 
  geom_point(aes(x = forest, y = forest.y), color =Djul22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.y), color = Djul22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.y), color = Djul22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.y), color = Djul22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.y), color = Djul22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES2 <- ggplot(data = Djul22TS)+ 
  geom_point(aes(x = forest, y = forest.x), color = Djul22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.x), color = Djul22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.x), color = Djul22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.x), color = Djul22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.x), color = Djul22TS$gage_pal, size = 0.75)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()


########## Rain Sep 2021 ##########
bounds_Rsep21<- as.POSIXct(c('08/31/2022 00:00:00','09/02/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Rsep21 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 

Rsep21DOC <-DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 

Rsep21EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 

Rsep21St <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Rsep21[1], as.POSIXct(datetime) <= bounds_Rsep21[2]) 

Rsep21TS <- merge(Rsep21EC,Rsep21DOC, by = 'datetime',all.x = TRUE)
Rsep21TS <- merge(Rsep21TS,Rsep21St, by = 'datetime',all.x = TRUE)

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <-1 # set the shrink coeffcient of Precipitation

DOC3 <- ggplot()+
  geom_line(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Rsep21DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  
  geom_line(data = Rsep21, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Rsep21, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Rsep21)+
  xlab('')+
  theme_cust()

#theme(axis.text = element_text(size = 16))+
# theme(axis.title = element_text(size = 16))
#Setting up color palettes for hysteresis plots
Rsep21TS$gage_pal <- gagePal(20)[as.numeric(cut(Rsep21TS$datetime,breaks = 20))]
Rsep21TS$shrub_pal <- shrubPal(20)[as.numeric(cut(Rsep21TS$datetime,breaks = 20))]
Rsep21TS$forest_pal <- forestPal(20)[as.numeric(cut(Rsep21TS$datetime,breaks = 20))]
Rsep21TS$tundra_pal <- tundraPal(20)[as.numeric(cut(Rsep21TS$datetime,breaks = 20))]
Rsep21TS$nellie_pal <- nelliePal(20)[as.numeric(cut(Rsep21TS$datetime,breaks = 20))]

EC3 <- ggplot(data = Rsep21TS)+ 
  geom_point(aes(x = forest.x, y = forest.y), color = Rsep21TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub.x, y = shrub.y), color = Rsep21TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra.x, y = tundra.y), color = Rsep21TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = gage.x, y = gage.y), color = Rsep21TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlim(0,60)+
  ylim(0,4)+
  theme_cust()

St3 <- ggplot(data = Rsep21TS)+ 
  geom_point(aes(x = forest, y = forest.y), color =Rsep21TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.y), color = Rsep21TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.y), color = Rsep21TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.y), color = Rsep21TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.y), color = Rsep21TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES3 <- ggplot(data = Rsep21TS)+ 
  geom_point(aes(x = forest, y = forest.x), color = Rsep21TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.x), color = Rsep21TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.x), color = Rsep21TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.x), color = Rsep21TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.x), color = Rsep21TS$gage_pal, size = 0.75)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()


########## Rain Sep 2022 ##########
bounds_Rjul22<- as.POSIXct(c('09/28/2022 00:00:00','09/30/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Rjul22 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 

Rjul22DOC <- DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 

Rjul22EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 

Rjul22St <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Rjul22[1], as.POSIXct(datetime) <= bounds_Rjul22[2]) 

Rjul22TS <- merge(Rjul22EC,Rjul22DOC, by = 'datetime',all.x = TRUE)
Rjul22TS <- merge(Rjul22TS,Rjul22St, by = 'datetime',all.x = TRUE)

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <- 1 # set the shrink coeffcient of Precipitation

DOC4<- ggplot()+
  geom_line(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Rjul22DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  geom_line(data = Rjul22, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Rjul22, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Rjul22)+
  
  xlab('')+
  theme_cust()

#Setting up color palettes for hysteresis plots
Rjul22TS$gage_pal <- gagePal(20)[as.numeric(cut(Rjul22TS$datetime,breaks = 20))]
Rjul22TS$shrub_pal <- shrubPal(20)[as.numeric(cut(Rjul22TS$datetime,breaks = 20))]
Rjul22TS$forest_pal <- forestPal(20)[as.numeric(cut(Rjul22TS$datetime,breaks = 20))]
Rjul22TS$tundra_pal <- tundraPal(20)[as.numeric(cut(Rjul22TS$datetime,breaks = 20))]
Rjul22TS$nellie_pal <- nelliePal(20)[as.numeric(cut(Rjul22TS$datetime,breaks = 20))]

EC4 <- ggplot(data = Rjul22TS)+ 
  geom_point(aes(x = forest.x, y = forest.y), color = Rjul22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub.x, y = shrub.y), color = Rjul22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra.x, y = tundra.y), color = Rjul22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = gage.x, y = gage.y), color = Rjul22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlim(0,60)+
  ylim(0,4)+
  theme_cust()

St4 <- ggplot(data = Rjul22TS)+ 
  geom_point(aes(x = forest, y = forest.y), color =Rjul22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.y), color = Rjul22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.y), color = Rjul22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.y), color = Rjul22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.y), color = Rjul22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES4 <- ggplot(data = Rjul22TS)+ 
  geom_point(aes(x = forest, y = forest.x), color = Rjul22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.x), color = Rjul22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.x), color = Rjul22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.x), color = Rjul22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.x), color = Rjul22TS$gage_pal, size = 0.75)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()


########## Winter  ##########
bounds_Wdec22<- as.POSIXct(c('12/25/2022 00:00:00','12/27/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
#bounds_Rsep21<- as.POSIXct(c('09/10/2021 00:00:00','09/12/2021 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")


Wdec22 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Wdec22[1], as.POSIXct(datetime) <= bounds_Wdec22[2]) 

Wdec22DOC <-DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wdec22[1], as.POSIXct(datetime) <= bounds_Wdec22[2]) 

Wdec22EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wdec22[1], as.POSIXct(datetime) <= bounds_Wdec22[2]) 

Wdec22St <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wdec22[1], as.POSIXct(datetime) <= bounds_Wdec22[2]) 

Wdec22TS <- merge(Wdec22EC,Wdec22DOC, by = 'datetime',all.x = TRUE)
Wdec22TS <- merge(Wdec22TS,Wdec22St, by = 'datetime',all.x = TRUE)

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <-1 # set the shrink coeffcient of Precipitation

DOC5 <- ggplot()+
  geom_line(data = Wdec22DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Wdec22DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Wdec22DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Wdec22DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Wdec22DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  
  geom_line(data = Wdec22, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Wdec22, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Wdec22)+
  xlab('')+
  theme_cust()

#theme(axis.text = element_text(size = 16))+
# theme(axis.title = element_text(size = 16))

#Setting up color palettes for hysteresis plots
Wdec22TS$gage_pal <- gagePal(20)[as.numeric(cut(Wdec22TS$datetime,breaks = 20))]
Wdec22TS$shrub_pal <- shrubPal(20)[as.numeric(cut(Wdec22TS$datetime,breaks = 20))]
Wdec22TS$forest_pal <- forestPal(20)[as.numeric(cut(Wdec22TS$datetime,breaks = 20))]
Wdec22TS$tundra_pal <- tundraPal(20)[as.numeric(cut(Wdec22TS$datetime,breaks = 20))]
Wdec22TS$nellie_pal <- nelliePal(20)[as.numeric(cut(Wdec22TS$datetime,breaks = 20))]

EC5 <- ggplot(data = Wdec22TS)+ 
  geom_point(aes(x = forest.x, y = forest.y), color = Wdec22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub.x, y = shrub.y), color = Wdec22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra.x, y = tundra.y), color = Wdec22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = gage.x, y = gage.y), color = Wdec22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlim(0,60)+
  ylim(0,4)+
  theme_cust()

St5 <- ggplot(data = Wdec22TS)+ 
  geom_point(aes(x = forest, y = forest.y), color =Wdec22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.y), color = Wdec22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.y), color = Wdec22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.y), color = Wdec22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.y), color = Wdec22TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES5 <- ggplot(data = Wdec22TS)+ 
  geom_point(aes(x = forest, y = forest.x), color = Wdec22TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.x), color = Wdec22TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.x), color = Wdec22TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.x), color = Wdec22TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.x), color = Wdec22TS$gage_pal, size = 0.75)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()

########## Winter 2  ##########
bounds_Wjan23<- as.POSIXct(c('01/03/2023 00:00:00','01/05/2023 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

Wjan23 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23DOC <-DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23St <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23TS <- merge(Wjan23EC,Wjan23DOC, by = 'datetime',all.x = TRUE)
Wjan23TS <- merge(Wjan23TS,Wjan23St, by = 'datetime',all.x = TRUE)

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <-1 # set the shrink coeffcient of Precipitation

DOC6 <- ggplot()+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  
  geom_line(data = Wjan23, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Wjan23, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Wjan23)+
  xlab('')+
  theme_cust()

#theme(axis.text = element_text(size = 16))+
# theme(axis.title = element_text(size = 16))

#Setting up color palettes for hysteresis plots
Wjan23TS$gage_pal <- gagePal(20)[as.numeric(cut(Wjan23TS$datetime,breaks = 20))]
Wjan23TS$shrub_pal <- shrubPal(20)[as.numeric(cut(Wjan23TS$datetime,breaks = 20))]
Wjan23TS$forest_pal <- forestPal(20)[as.numeric(cut(Wjan23TS$datetime,breaks = 20))]
Wjan23TS$tundra_pal <- tundraPal(20)[as.numeric(cut(Wjan23TS$datetime,breaks = 20))]
Wjan23TS$nellie_pal <- nelliePal(20)[as.numeric(cut(Wjan23TS$datetime,breaks = 20))]

EC6 <- ggplot(data = Wjan23TS)+ 
  geom_point(aes(x = forest.x, y = forest.y), color = Wjan23TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub.x, y = shrub.y), color = Wjan23TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra.x, y = tundra.y), color = Wjan23TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = gage.x, y = gage.y), color = Wjan23TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlim(0,60)+
  ylim(0,4)+
  theme_cust()

St6 <- ggplot(data = Wjan23TS)+ 
  geom_point(aes(x = forest, y = forest.y), color =Wjan23TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.y), color = Wjan23TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.y), color = Wjan23TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.y), color = Wjan23TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.y), color = Wjan23TS$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES6 <- ggplot(data = Wjan23TS)+ 
  geom_point(aes(x = forest, y = forest.x), color = Wjan23TS$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.x), color = Wjan23TS$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.x), color = Wjan23TS$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.x), color = Wjan23TS$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.x), color = Wjan23TS$gage_pal, size = 0.75)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()

#DOC3
#EC3
#St3
#ES3
All_together <-plot_grid(DOC1, DOC2, DOC3, DOC4, DOC5, DOC6, St1, St2, St3, St4, St5, St6, ES1, ES2, ES3, ES4, ES5, ES6,ncol=6, align = "v")
All_together


########## Fun with plotting timeseries ############
#Extra code to examine a specific subset in time

bounds_Wjan23<-  as.POSIXct(c('05/04/2023 00:00:00','05/15/2023 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
#bounds_Rsep21<- as.POSIXct(c('09/10/2021 00:00:00','09/12/2021 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")


Wjan23 <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23DOC <-DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23EC <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23St <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds_Wjan23[1], as.POSIXct(datetime) <= bounds_Wjan23[2]) 

Wjan23TS <- merge(Wjan23EC,Wjan23DOC, by = 'datetime',all.x = TRUE)
Wjan23TS <- merge(Wjan23TS,Wjan23St, by = 'datetime',all.x = TRUE)

maxRange <- 10 # set how wide of the first axis (streamflow)
coeff <-1 # set the shrink coeffcient of Precipitation

DOC6 <- ggplot()+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = Wjan23DOC, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  
  geom_line(data = Wjan23, aes(x=as.POSIXct(datetime), y= Q/100), color = 'black', size = 0.5)+
  geom_tile(data = Wjan23, aes(x=as.POSIXct(datetime), y = maxRange - precip_mm/coeff/2, height = precip_mm/coeff),  color = 'darkslateblue', fill = 'darkslateblue')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds_Wjan23)+
  xlab('')+
  theme_cust()

ECts3<- ggplot()+
  geom_line(data = Wjan23EC,aes(x=as.POSIXct(datetime), y= tundra), color = col.tundra, size = 0.5)+
  geom_line(data = Wjan23EC,aes(x=as.POSIXct(datetime), y= forest), color = col.forest, size = 0.5)+
  geom_line(data = Wjan23EC,aes(x=as.POSIXct(datetime), y= nellie), color = col.nellie, size = 0.5)+
  geom_line(data = Wjan23EC,aes(x=as.POSIXct(datetime), y= shrub), color = col.shrub, size = 0.5)+
  geom_line(data = Wjan23EC,aes(x=as.POSIXct(datetime), y= gage), color = col.gage, size = 0.5)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlab('')+
  theme_cust()

#theme(axis.text = element_text(size = 16))+
# theme(axis.title = element_text(size = 16))

EC6 <- ggplot(data = Wjan23TS)+ 
  geom_point(aes(x = forest.x, y = forest.y), color = col.forest, size = 0.5)+
  geom_point(aes(x = shrub.x, y = shrub.y), color = col.shrub, size = 0.5)+
  geom_point(aes(x = tundra.x, y = tundra.y), color = col.tundra, size = 0.5)+
  geom_point(aes(x = nellie.x, y = nellie.y), color = col.nellie, size = 0.5)+
  geom_point(aes(x = gage.x, y = gage.y), color = col.gage, size = 0.5)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlim(0,60)+
  ylim(0,4)+
  theme_cust()


St6 <- ggplot(data = Wjan23TS)+ 
  geom_point(aes(x = forest, y = forest.y), color = col.forest, size = 0.5)+
  geom_point(aes(x = shrub, y = shrub.y), color = col.shrub, size = 0.5)+
  geom_point(aes(x = tundra, y = tundra.y), color = col.tundra, size = 0.5)+
  geom_point(aes(x = nellie, y = nellie.y), color = col.nellie, size = 0.5)+
  geom_point(aes(x = gage, y = gage.y), color = col.gage, size = 0.5)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES6 <- ggplot(data = Wjan23TS)+ 
  geom_point(aes(x = forest, y = forest.x), color = col.forest, size = 0.5)+
  geom_point(aes(x = shrub, y = shrub.x), color = col.shrub, size = 0.5)+
  geom_point(aes(x = tundra, y = tundra.x), color = col.tundra, size = 0.5)+
  geom_point(aes(x = nellie, y = nellie.x), color = col.nellie, size = 0.5)+
  geom_point(aes(x = gage, y = gage.x), color = col.gage, size = 0.5)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()



#DOC6
#EC6
#St6
#ES6

tundra_sub <- Wjan23EC[!is.na(Wjan23EC$tundra), ]
forest_sub <- Wjan23EC[!is.na(Wjan23EC$forest), ]
nellie_sub <- Wjan23EC[!is.na(Wjan23EC$nellie), ]
shrub_sub <- Wjan23EC[!is.na(Wjan23EC$shrub), ]
gage_sub <- Wjan23EC[!is.na(Wjan23EC$gage), ]
ECts3<- ggplot()+
  geom_line(data = tundra_sub,aes(x=as.POSIXct(datetime), y= tundra), color = col.tundra, size = 0.5)+
  geom_line(data = forest_sub,aes(x=as.POSIXct(datetime), y= forest), color = col.forest, size = 0.5)+
  geom_line(data = nellie_sub,aes(x=as.POSIXct(datetime), y= nellie), color = col.nellie, size = 0.5)+
  geom_line(data = shrub_sub,aes(x=as.POSIXct(datetime), y= shrub), color = col.shrub, size = 0.5)+
  geom_line(data = gage_sub,aes(x=as.POSIXct(datetime), y= gage), color = col.gage, size = 0.5)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
  xlab('')+
  theme_cust()
#ECts3

