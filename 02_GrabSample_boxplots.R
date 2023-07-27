## 02_GrabSample_boxplots

# This code plots the boxplots of grab sample parameters at the core sampling sites, figures will be used in paper

source("paths+packages.R")

#Loading core site data
core_sites <- read.csv('outputs/01_grabsample_full_data.csv') 

#!!!!!!!!!!! WE NEED TO DECIDE HOW TO DEAL WITH PRESENTING DATA BELOW ADVERTISED DETECTION LIMITS FOR DOC (0.5ppm), NO3 (0.02 ppm), PO4 (0.03ppm) 
core_sites <- mutate(core_sites, doc_detect = if_else(DOC< 0.5, TRUE, FALSE)) %>%
   mutate(NO3_detect = if_else(Nitrate< 0.02, TRUE, FALSE))%>%
   mutate(PO4_detect = if_else(Phosphate_P< 0.03, TRUE, FALSE))

DOC_table <- core_sites [!is.na(core_sites$DOC),]
site <- as.factor(DOC_table$Site)

cendiff(DOC_table$DOC, DOC_table$doc_detect, site)
cenfit(DOC_table$DOC, DOC_table$doc_detect, site)
cenboxplot(DOC_table$DOC, DOC_table$doc_detect, site)

no3_table <- core_sites [!is.na(core_sites$Nitrate),]
site <- as.factor(no3_table$Site)

cendiff(no3_table$Nitrate, no3_table$NO3_detect, site)
cenfit(no3_table$Nitrate, no3_table$NO3_detect, site)
cenboxplot(no3_table$Nitrate, no3_table$NO3_detect, site)

po4_table <- core_sites [!is.na(core_sites$Phosphate_P),]
site <- as.factor(po4_table$Site)

cendiff(po4_table$Phosphate_P, po4_table$PO4_detect, site)
cenfit(po4_table$Phosphate_P, po4_table$PO4_detect, site)
cenboxplot(po4_table$Phosphate_P, po4_table$PO4_detect, site)

# DOC boxplot
ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= DOC, color= as.factor(Site))) +
  scale_color_manual( values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
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

# Flourescence Index boxplot
ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= FI, color= as.factor(Site))) +
  scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in  ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
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

# Nitrate boxplot
ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= Nitrate, color= as.factor(Site))) +
  scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
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

# Phosphate boxplot 
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

