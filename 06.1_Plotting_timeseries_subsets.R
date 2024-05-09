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

#New met and gage data from Seward, all getting loaded in as separate files 
Seward_precip <- read.csv('outputs/04_Seward_precip.csv')


# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
RelST_FullTS$datetime <- strptime(RelST_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
DOC_FullTS$datetime <- strptime(DOC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS$datetime <- strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Precip_Q$datetime <- strptime(Precip_Q$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
Seward_precip$datetime <- strptime(Seward_precip$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

# Setting up end points for color palettes that are a gradient as a function of time
gagePal <- colorRampPalette(c('#d2f3fc','#02c1fa'))
shrubPal <- colorRampPalette(c('#fcdb97','#a66f02'))
forestPal <- colorRampPalette(c('#deb4ab','#de2700'))
tundraPal <- colorRampPalette(c('#fcf2fa','#800064'))
nelliePal <- colorRampPalette(c('#e5c2fc','#9700fc'))



########## Setting up function for plotting ##########
subset_plot <- function(bounds, maxRange){
  
Q_sub <- Precip_Q %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2]) 

DOC_sub <- DOC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2]) 

EC_sub <- EC_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2])

ST_sub <- RelST_FullTS %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2]) 

Precip_sub <- Seward_precip %>%
  filter(as.POSIXct(datetime) >= bounds[1], as.POSIXct(datetime) <= bounds[2]) 

Sub_comb <- merge(EC_sub,DOC_sub, by = 'datetime',all.x = TRUE)
Sub_comb <- merge(Sub_comb,ST_sub, by = 'datetime',all.x = TRUE)

coeff <- 1 # set the shrink coefficient of Precipitation

DOC <- ggplot()+
  geom_line(data = DOC_sub, aes(x=as.POSIXct(datetime), y= forest), color = "#E2725B", size = 0.5)+
  geom_line(data = DOC_sub, aes(x=as.POSIXct(datetime), y= tundra), color = "#A80084", size = 0.5 )+
  geom_line(data = DOC_sub, aes(x=as.POSIXct(datetime), y= shrub), color = "#FFAA00", size = 0.5)+
  geom_line(data = DOC_sub, aes(x=as.POSIXct(datetime), y= nellie), color = "#EA9DFF", size = 0.5)+
  geom_line(data = DOC_sub, aes(x=as.POSIXct(datetime), y= gage), color = "#73DFFF", size = 0.5)+
  
  geom_line(data = Q_sub, aes(x=as.POSIXct(datetime), y= Q/100), color = '#182ff5', size = 1)+
  geom_tile(data = Precip_sub, aes(x=as.POSIXct(datetime), y = maxRange - precip/coeff/2, height = precip/coeff),  color = '#42ecf5', fill = '#42ecf5')+ 
  scale_y_continuous(name = 'DOC (mg l-1)',limit = c(0, maxRange),expand = c(0, 0),sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,name = "Precipitation (mm/hr)"))+
  xlim(bounds)+
  xlab('')+
  theme_cust()


Sub_comb$gage_pal <- gagePal(20)[as.numeric(cut(Sub_comb$datetime,breaks = 20))]
Sub_comb$shrub_pal <- shrubPal(20)[as.numeric(cut(Sub_comb$datetime,breaks = 20))]
Sub_comb$forest_pal <- forestPal(20)[as.numeric(cut(Sub_comb$datetime,breaks = 20))]
Sub_comb$tundra_pal <- tundraPal(20)[as.numeric(cut(Sub_comb$datetime,breaks = 20))]
Sub_comb$nellie_pal <- nelliePal(20)[as.numeric(cut(Sub_comb$datetime,breaks = 20))]


ST <- ggplot(data = Sub_comb)+ 
  geom_point(aes(x = forest, y = forest.y), color =Sub_comb$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.y), color = Sub_comb$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.y), color = Sub_comb$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.y), color = Sub_comb$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.y), color = Sub_comb$gage_pal, size = 0.75)+
  ylab(bquote('DOC' (mgl^-1)))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,4)+
  theme_cust()

ES <- ggplot(data = Sub_comb)+ 
  geom_point(aes(x = forest, y = forest.x), color = Sub_comb$forest_pal, size = 0.75)+
  geom_point(aes(x = shrub, y = shrub.x), color = Sub_comb$shrub_pal, size = 0.75)+
  geom_point(aes(x = tundra, y = tundra.x), color = Sub_comb$tundra_pal, size = 0.75)+
  geom_point(aes(x = nellie, y = nellie.x), color = Sub_comb$nellie_pal, size = 0.75)+
  geom_point(aes(x = gage, y = gage.x), color = Sub_comb$gage_pal, size = 0.75)+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+ 
  xlab("Stage") + 
  xlim(0,1)+
  ylim(0,60)+
  theme_cust()
plots <- list(DOC, ST, ES)
return(plots)
}



#maxRange <- 5 # set how wide of the first axis (streamflow), 5 for the first winter and melt, 10 for the rain events

bounds_Mmay22<- as.POSIXct(c('05/14/2022 00:00:00','05/16/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
plots_May <- subset_plot(bounds_Mmay22, 5)

bounds_Djul22<- as.POSIXct(c('07/01/2022 00:00:00','07/02/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
plots_Jul <- subset_plot(bounds_Djul22, 5)

bounds_Rsep21<- as.POSIXct(c('08/31/2022 00:00:00','09/02/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
plots_sep21 <- subset_plot(bounds_Rsep21, 10)

bounds_Rjul22<- as.POSIXct(c('09/28/2022 00:00:00','09/30/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")
plots_jul22 <- subset_plot(bounds_Rjul22, 10)




########## Winter, not in fnal plots  ##########
bounds_Wdec22<- as.POSIXct(c('12/25/2022 00:00:00','12/27/2022 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

bounds_Wjan23<- as.POSIXct(c('01/03/2023 00:00:00','01/05/2023 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")


All_together <-plot_grid(plots_May[[1]], plots_Jul[[1]], plots_sep21[[1]], plots_jul22[[1]], plots_May[[2]], plots_Jul[[2]], plots_sep21[[2]], plots_jul22[[2]], plots_May[[3]], plots_Jul[[3]], plots_sep21[[3]], plots_jul22[[3]],ncol=4, align = "v")
All_together



