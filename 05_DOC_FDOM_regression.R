##05_DOC_FDOM_regression
# This imports the DOC data and FDOM timeseries and fits a statitstical model between DOC samples and sonde FDOM 
# We can use this to convert sonde FDOM to DOC for later analyses 

source("paths+packages.R")

#remove all objects from the workspace
rm()

# load necessary data
FDOM_fullTS <- read.csv('outputs/04_FDOM_FullTS.csv')
core_sites <- read.csv('outputs/01_grabsample_core_sites.csv') 

# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
FDOM_fullTS$datetime <- strptime(FDOM_fullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
core_sites$Datetime <- strptime(core_sites$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

core_sites <- mutate(core_sites, doc_detect = if_else(DOC< 0.5, TRUE, FALSE))
rep_str = c('stream_gauge' = 'gage','Forest'= 'forest', 'shrub_creek' = 'shrub', 'Tundra' = 'tundra', 'Nellie_Juan' = 'nellie', 'glacier_hut' = 'glacier')
core_sites$Site <- str_replace_all(core_sites$Site, rep_str)
core_sites$Datetime <- round_date(as.POSIXct(core_sites$Datetime), "15 mins")

core_lab <- core_sites[core_sites$yearS == 2022|core_sites$yearS == 2021,] 
rownames(core_lab)<- seq(1,nrow(core_lab),1)
core_lab$'Sonde' <- rep(NA, nrow(core_lab))


#####----- INPUT VALUES BY LOOPING THROUGH EACH VARIABLE NAME -----#####
i <- 2
k<- 4
for (i in 2:6){
  inds <- which(core_lab$'Site' == colnames(FDOM_fullTS)[i])
  times <- core_lab$Datetime[inds]
  for (k in 1:length(times)){
    time_inds <- which(FDOM_fullTS$'datetime' == times[k])
    temp <- FDOM_fullTS[(time_inds-8):(time_inds+8),i]
    if (all(is.na(temp))){
      core_lab$'Sonde'[inds[k]] <-NA}
    else{
      core_lab$'Sonde'[inds[k]] <- mean(na_remove(temp))}
  }
} 


core_lab <- core_lab[!is.na(core_lab$Sonde),] 

core_lab <- mutate(core_lab, FDOM_detect = if_else(Sonde > 0, FALSE, TRUE))

cen_model <- cenken(core_lab$DOC, core_lab$doc_detect, core_lab$Sonde, core_lab$FDOM_detect)

# NADA package default plotting
# Plotting data and the regression line
data(core_lab)
# Recall x and y parameter positons are swapped in plot vs regression calls
with(core_lab, cenxyplot(Sonde, FDOM_detect, DOC, doc_detect))    # x vs. y
reg = with(core_lab, cenken(DOC, doc_detect, Sonde, FDOM_detect)) # y~x
lines(reg)

#Fitting regression model
cen_model <- cenken(core_lab$DOC, core_lab$doc_detect, core_lab$Sonde, core_lab$FDOM_detect)

#plot with just sonde FDOM v.s. DOC 
ggplot(core_lab, aes(x=Sonde, y = DOC, color =as.factor(Site), group = 1))+
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

sampls_data_export <- core_lab[ ,c("SampleID","Site","Datetime", "DOC", "doc_detect", "Sonde", "FDOM_detect")]
readr::write_csv(sampls_data_export, file = file.path("outputs", "05_grabsample_regression_data.csv"))

