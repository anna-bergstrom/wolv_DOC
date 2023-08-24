## 02_GrabSample_boxplots

# This code plots the boxplots of grab sample parameters at the core sampling sites, figures will be used in paper

source("paths+packages.R")

#Loading core site data
core_sites <- read.csv('outputs/01_grabsample_core_sites.csv') 

#!!!!!!!!!!! WE NEED TO DECIDE HOW TO DEAL WITH PRESENTING DATA BELOW ADVERTISED DETECTION LIMITS FOR DOC (0.5ppm), NO3 (0.02 ppm), PO4 (0.03ppm) 
core_sites <- mutate(core_sites, doc_detect = if_else(DOC< 0.5, TRUE, FALSE)) %>%
  mutate(NO3_detect = if_else(Nitrate< 0.02, TRUE, FALSE))%>%
  mutate(PO4_detect = if_else(Phosphate_P< 0.03, TRUE, FALSE))

# Censored data statistics for DOC
DOC_table <- core_sites [!is.na(core_sites$DOC),]
site <- as.factor(DOC_table$Site)

cendiff(DOC_table$DOC, DOC_table$doc_detect, site)
cenfit(DOC_table$DOC, DOC_table$doc_detect, site)
cendf_DOC <- cenboxplot(DOC_table$DOC, DOC_table$doc_detect, site)

max_threshold <- DOC_table %>%
  filter(doc_detect == TRUE) %>%
  summarise(max = max(DOC))

DOC_plot = ggplot(cendf_DOC, aes(x= factor(group, level = c("Terminus" ,  "stream_gauge" ,"glacier_hut","Nellie_Juan" ,"Tundra" , "lake_inlet","shrub_creek" ,"Forest" )), y=ros.model, fill= as.factor(group))) +
  geom_boxplot(coef=1.5, outlier.shape = 19) +
  scale_fill_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_hline(yintercept = max_threshold[[1]], linetype="dashed", color = "#1A237E", size=1) +
  ylab(bquote('DOC' (mgl^-1)))+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  

print(DOC_plot)

# Censored data statistics for NO3
no3_table <- core_sites [!is.na(core_sites$Nitrate),]
site <- as.factor(no3_table$Site)

cendiff(no3_table$Nitrate, no3_table$NO3_detect, site)
cenfit(no3_table$Nitrate, no3_table$NO3_detect, site)
cendf_NO3 <- cenboxplot(no3_table$Nitrate, no3_table$NO3_detect, site)

max_threshold <- no3_table %>%
  filter(NO3_detect == TRUE) %>%
  summarise(max = max(Nitrate))

NO3_plot = ggplot(cendf_NO3, aes(x=factor(group, level = c("Terminus" ,  "stream_gauge" ,"glacier_hut","Nellie_Juan" ,"Tundra" , "lake_inlet","shrub_creek" ,"Forest" )), y=ros.model, fill= as.factor(group))) +
  geom_boxplot(coef=1.5, outlier.shape = 19) +
  scale_fill_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  geom_hline(yintercept = max_threshold[[1]], linetype="dashed", color = "#1A237E", size=1) +
  ylab(bquote('Nitrate' (mgl^-1)))+
  xlab("")+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  

print(NO3_plot)

#A series of pairwise combinations to determine differences

# make dataframe for pairwise comparisons, remove duplicates, and create columns to populate with p-values
site1 <- factor(c("Terminus" ,  "stream_gauge" ,"glacier_hut","Nellie_Juan" ,"Tundra" , "lake_inlet","shrub_creek" ,"Forest" ))
site2 <- factor(c("Terminus" ,  "stream_gauge" ,"glacier_hut","Nellie_Juan" ,"Tundra" , "lake_inlet","shrub_creek" ,"Forest" ))
data_comp <- expand.grid(site1,site2) %>%
  filter(Var1 != Var2) %>%
  mutate(DOC_p = NaN, DOC_Sig = NaN, NO3_p = NaN, NO3_Sig = NaN) 

for (i in 1:nrow(data_comp)){
  pairwise <- DOC_table %>%
    filter(Site == data_comp$Var1[i]|Site==data_comp$Var2[i])
  P_sites<- as.factor(pairwise$Site)
  temp <- cendiff(pairwise$DOC, pairwise$doc_detect, P_sites)
  p.val <- 1 - pchisq(temp$chisq, length(temp$n) - 1)
  data_comp$DOC_p[i]<-p.val
  data_comp$DOC_Sig[i]<- p.val<0.05
  
  pairwise <- no3_table %>%
    filter(Site == data_comp$Var1[i]|Site==data_comp$Var2[i])
  P_sites<- as.factor(pairwise$Site)
  temp <- cendiff(pairwise$Nitrate, pairwise$NO3_detect, P_sites)
  p.val <- 1 - pchisq(temp$chisq, length(temp$n) - 1)
  data_comp$NO3_p[i]<-p.val
  data_comp$NO3_Sig[i]<- p.val<0.05
}

data_comp<- data_comp %>% distinct(DOC_p, .keep_all = TRUE)

#Censored data stats for PO4 
# just basically to see that too many points below detection limit to do much 
po4_table <- core_sites [!is.na(core_sites$Phosphate_P),]
site <- as.factor(po4_table$Site)

cenfit(po4_table$Phosphate_P, po4_table$PO4_detect, site)

# Flourescence Index boxplot
ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= FI, fill= as.factor(Site))) +
  geom_boxplot(outlier.shape =  19) +
  scale_fill_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in  ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
  #geom_jitter(shape=16, position=position_jitter(0.2))+
  geom_hline(yintercept=1.9, linetype="dashed", color = "#000000", size=1)+
  geom_hline(yintercept=1.4, linetype="dashed", color = "#999999", size=1)+
  ylab("Fluorescence Index")+
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
  #geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.1))+
  geom_hline(yintercept = 0.03, linetype="dashed", color = "#1A237E", size=1) +
  ylab(bquote('Phosphate' (mgl^-1)))+
  xlab("")+
  ylim(0,0.06)+
  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))



########## No longer used ##############
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


