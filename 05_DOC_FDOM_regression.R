##05_DOC_FDOM_regression
# This imports the DOC data and FDOM timeseries and fits a statitstical model between DOC samples and sonde FDOM 
# We can use this to convert sonde FDOM to DOC for later analyses 

source("paths+packages.R")

# load necessary data
FDOM21TS <- read.csv('outputs/04_FDOM21TS.csv')
FDOM22TS <- read.csv('outputs/04_FDOM22TS.csv')
core_sites <- read.csv('outputs/01_grabsample_core_sites.csv') 

# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
FDOM21TS$datetime <- strptime(FDOM21TS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
FDOM22TS$datetime <- strptime(FDOM22TS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
core_sites$Datetime <- strptime(core_sites$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

core_sites <- mutate(core_sites, doc_detect = if_else(DOC< 0.5, TRUE, FALSE))
rep_str = c('stream_gauge' = 'gage','Forest'= 'forest', 'shrub_creek' = 'shrub', 'Tundra' = 'tundra', 'Nellie_Juan' = 'nellie', 'glacier_hut' = 'glacier')
core_sites$Site <- str_replace_all(core_sites$Site, rep_str)
core_sites$Datetime <- round_date(as.POSIXct(core_sites$Datetime), "15 mins")

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

core_site20s <- core_site20s[!is.na(core_site20s$Sonde),] 

core_site20s <- mutate(core_site20s, FDOM_detect = if_else(Sonde > 0, FALSE, TRUE))

cen_model <- cenken(core_site20s$DOC, core_site20s$doc_detect, core_site20s$Sonde, core_site20s$FDOM_detect)

# NADA package default plotting
# Plotting data and the regression line
data(core_site20s)
# Recall x and y parameter positons are swapped in plot vs regression calls
with(core_site20s, cenxyplot(Sonde, FDOM_detect, DOC, doc_detect))    # x vs. y
reg = with(core_site20s, cenken(DOC, doc_detect, Sonde, FDOM_detect)) # y~x
lines(reg)

#Fitting regression model
cen_model <- cenken(core_site20s$DOC, core_site20s$doc_detect, core_site20s$Sonde, core_site20s$FDOM_detect)

#plot with just sonde FDOM v.s. DOC 
ggplot(core_site20s, aes(x=Sonde, y = DOC, color =as.factor(Site), group = 1))+
  scale_color_manual(values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF",  "#0084A8", "#059E41" , "#6600CC"),breaks = c( "forest" , "nellie" , "shrub" , "tundra" , "gage" , "glacier"),labels = c("Forest", "Nellie Juan" , "Shrub" , "Tundra" , "Gage" ,  "Glacier"))+
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(aes(slope=cen_model$slope,intercept=cen_model$intercept,color="black"))+
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

# Save fit model for use in TS analysis
# For some reason I need the full file path 
saveRDS(cen_model, file = "/Users/annabergstrom/BSU_drive/Projects/AK_post-doc/DOC/wolv_DOC/outputs/DOC_fdom_model.RData")


