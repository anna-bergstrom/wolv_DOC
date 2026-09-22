## 02_GrabSample_boxplots

# This code plots the boxplots of grab sample parameters at the core sampling sites, figures will be used in paper
#setwd("/Users/annabergstrom/BSU_drive/Projects/AK_post-doc/DOC/wolv_DOC")
source("paths+packages.R")


#remove all objects from the workspace
rm()

#Loading core site data
core_sites <- read.csv('outputs/01_grabsample_core_sites.csv') 

core_sites$Phosphate_P[core_sites$Phosphate_P == 0]<-NA
#DATA BELOW ADVERTISED DETECTION LIMITS FOR DOC (0.5ppm 0.2?), NO3 (0.02 ppm), PO4 (0.03ppm) now dealt with using censored stats
core_sites <- mutate(core_sites, doc_detect = if_else(DOC< 0.2, TRUE, FALSE)) %>%
  mutate(NO3_detect = if_else(Nitrate< 0.02, TRUE, FALSE))%>%
  mutate(PO4_detect = if_else(P_PO4< 0.001, TRUE, FALSE)) # New, from nutrient analyzer
  #mutate(PO4_detect = if_else(Phosphate_P< 0.03, TRUE, FALSE)) #Old, from IC 

# Very lengthy way of creating a table with counts of samples above and below the detection limit by site, for DOC, NO3, and PO4. 
detect_subset <- core_sites %>%
  select(Site, doc_detect,NO3_detect, PO4_detect) 

DOC<- detect_subset %>%
  group_by(Site) %>%
  count(doc_detect)%>%
  filter(doc_detect == TRUE )%>%
  select(Site = Site,  DOC_below = n )

DOCa<- detect_subset %>%
  group_by(Site) %>%
  count(doc_detect)%>%
  filter(doc_detect == FALSE)%>%
  select(Site = Site,  DOC_above = n) 

NO3<- detect_subset %>%
  group_by(Site) %>%
  count(NO3_detect)%>%
  filter(NO3_detect == TRUE)%>%
  select(Site = Site,  NO3_below = n )

NO3a<- detect_subset %>%
  group_by(Site) %>%
  count(NO3_detect)%>%
  filter(NO3_detect == FALSE)%>%
  select(Site = Site,  NO3_above = n )

PO4 <- detect_subset %>%
  group_by(Site) %>%
  count(PO4_detect) %>%
  filter(PO4_detect == TRUE) %>%
  select(Site = Site,  PO4_below = n )

PO4_a <- detect_subset %>%
  group_by(Site) %>%
  count(PO4_detect) %>%
  filter(PO4_detect == FALSE) %>%
  select(Site = Site,  PO4_above = n )

FI_n <- core_sites %>%
  select(FInew,Site)%>%
  group_by(Site) %>%
  count(Site) %>%
  select(Site = Site,  FI_count= n )

Detect_table <- merge( PO4, PO4_a, by= 'Site', all.x =  TRUE)
Detect_table <- merge(Detect_table, NO3, by= 'Site', all.x =  TRUE)
Detect_table <- merge(Detect_table, NO3a, by= 'Site', all.x =  TRUE)
Detect_table <- merge(Detect_table, DOC , by= 'Site', all.x =  TRUE)
Detect_table <- merge(Detect_table, DOCa, by= 'Site', all.x =  TRUE)
Detect_table <- merge(Detect_table, FI_n, by= 'Site', all.x =  TRUE)

#Writing detection table:
readr::write_csv(Detect_table, file = file.path("outputs", "02_grabsample_detection_table.csv"))

######## Censored data stats for DOC, NO3, PO4 #############
# Censored data statistics for DOC
DOC_table <- core_sites [!is.na(core_sites$DOC),]
site <- as.factor(DOC_table$Site)

cendiff(DOC_table$DOC, DOC_table$doc_detect, site)
cenfit(DOC_table$DOC, DOC_table$doc_detect, site)
cendf_DOC <- cenboxplot(DOC_table$DOC, DOC_table$doc_detect, site)

max_threshold <- DOC_table %>%
  filter(doc_detect == TRUE) %>%
  summarise(max = max(DOC))

DOC_plot = ggplot(cendf_DOC, aes(x= factor(group, level = c("glacier_2","lake_inlet","tundra" , "shrub_creek" ,"forest" ,"terminus" ,  "stream_gauge" ,"nellie_juan" )), y=ros.model, fill= as.factor(group))) +
  geom_boxplot(coef=1.5, outlier.shape = 19) +
  scale_fill_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in ), breaks = c( "forest" , "nellie_juan" , "shrub_creek" , "tundra" , "stream_gauge" ,"terminus" , "glacier_2", "lake_inlet"))+
  geom_hline(yintercept = max_threshold[[1]], linetype="dashed", color = "#1A237E", size=1) +
  ylab(bquote('DOC' (mgl^-1)))+
  xlab("")+
  ylim(-0.2,3.2)+
  scale_x_discrete(labels=c("forest" = "Forest", "nellie_juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "tundra"= "Tundra" , "stream_gauge"= "Gage" ,"terminus" =  "Terminus", "glacier_2" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))  

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

NO3_plot = ggplot(cendf_NO3, aes(x=factor(group, level = c("glacier_2","lake_inlet","tundra" , "shrub_creek" ,"forest" ,"terminus" ,  "stream_gauge" ,"nellie_juan" )), y=ros.model, fill= as.factor(group))) +
  geom_boxplot(coef=1.5, outlier.shape = 19) +
  scale_fill_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in ), breaks = c( "forest" , "nellie_juan" , "shrub_creek" , "tundra" , "stream_gauge" ,"terminus" , "glacier_2", "lake_inlet"))+
  geom_hline(yintercept = max_threshold[[1]], linetype="dashed", color = "#1A237E", size=1) +
  ylab(bquote('Nitrate' (mgl^-1)))+
  xlab("")+
  ylim(-0.05,0.7)+
  scale_x_discrete(labels=c("forest" = "Forest", "nellie_juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "tundra"= "Tundra" , "stream_gauge"= "Gage" ,"terminus" =  "Terminus", "glacier_2" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  

print(NO3_plot)

#Censored data stats for PO4 
# Once this was switched to using data from the nutrient analyzer with a lower detection limit, we can now do stats with P
#po4_table <- core_sites [!is.na(core_sites$Phosphate_P),]
po4_table <- core_sites [!is.na(core_sites$P_PO4),]
site <- as.factor(po4_table$Site)

#cenfit(po4_table$Phosphate_P, po4_table$PO4_detect, site)

cendiff(po4_table$P_PO4, po4_table$PO4_detect, site)
cenfit(po4_table$P_PO4, po4_table$PO4_detect, site)

cendf_PO4 <- cenboxplot(po4_table$P_PO4, po4_table$PO4_detect, site)

max_threshold <- po4_table %>%
  filter(PO4_detect == TRUE) %>%
  summarise(max = max(P_PO4))

#Phosphate plot
PO4_plot = ggplot(cendf_PO4, aes(x=factor(group, level = c("glacier_2","lake_inlet","tundra" , "shrub_creek" ,"forest" ,"terminus" ,  "stream_gauge" ,"nellie_juan" )), y=ros.model, fill= as.factor(group))) +
  geom_boxplot(coef=1.5, outlier.shape = 19) +
  scale_fill_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in ), breaks = c( "forest" , "nellie_juan" , "shrub_creek" , "tundra" , "stream_gauge" ,"terminus" , "glacier_2", "lake_inlet"))+
  geom_hline(yintercept = max_threshold[[1]], linetype="dashed", color = "#1A237E", size=1) +
  ylab(bquote('Phosphate' (mgl^-1)))+
  xlab("")+
  ylim(-0.001,0.022)+
  scale_x_discrete(labels=c("forest" = "Forest", "nellie_juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "tundra"= "Tundra" , "stream_gauge"= "Gage" ,"terminus" =  "Terminus", "glacier_2" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))  
print(PO4_plot)

#########A series of pairwise combinations to determine significant differences among sites #################

# make dataframe for pairwise comparisons, remove duplicates, and create columns to populate with p-values
site1 <- factor(c("terminus" ,  "stream_gauge" ,"glacier_2","nellie_juan" ,"tundra" , "lake_inlet","shrub_creek" ,"forest" ))
site2 <- factor(c("terminus" ,  "stream_gauge" ,"glacier_2","nellie_juan" ,"tundra" , "lake_inlet","shrub_creek" ,"forest" ))
data_comp <- expand.grid(site1,site2) %>%
  filter(Var1 != Var2) %>%
  mutate(DOC_p = NaN, DOC_Sig = NaN, NO3_p = NaN, NO3_Sig = NaN,PO4_p = NaN, PO4_Sig = NaN, FI_p = NaN, FI_Sig = NaN) 

i = 4
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
  
  pairwise <- po4_table %>%
    filter(Site == data_comp$Var1[i]|Site==data_comp$Var2[i])
  P_sites<- as.factor(pairwise$Site)
  temp <- cendiff(pairwise$P_PO4, pairwise$PO4_detect, P_sites)
  p.val <- 1 - pchisq(temp$chisq, length(temp$n) - 1)
  data_comp$PO4_p[i]<-p.val
  data_comp$PO4_Sig[i]<- p.val<0.05
  
  temp <- t.test(core_sites$FInew[core_sites$Site == data_comp$Var1[i]], core_sites$FI[core_sites$Site == data_comp$Var2[i]])
  data_comp$FI_p[i]<-temp$p.value
  data_comp$FI_Sig[i]<- temp$p.value<0.05
}

data_comp<- data_comp %>% distinct(DOC_p, .keep_all = TRUE)


############################

# Flourescence Index boxplot
ggplot(core_sites, aes(x=factor(Site, level = c("glacier_2","lake_inlet","tundra" , "shrub_creek" ,"forest" ,"terminus" ,  "stream_gauge" ,"nellie_juan" )), y= FInew, fill= as.factor(Site))) +
  geom_boxplot(outlier.shape =  19) +
  scale_fill_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in  ), breaks = c( "forest" , "nellie_juan" , "shrub_creek" , "tundra" , "stream_gauge" ,"terminus" , "glacier_2", "lake_inlet"))+
  #geom_jitter(shape=16, position=position_jitter(0.2))+
  geom_hline(yintercept=1.9, linetype="dashed", color = "#000000", size=1)+
  geom_hline(yintercept=1.4, linetype="dashed", color = "#999999", size=1)+
  ylab("Fluorescence Index")+
  xlab("")+
  scale_x_discrete(labels=c("forest" = "Forest", "nellie_juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "tundra"= "Tundra" , "stream_gauge"= "Gage" ,"terminus" =  "Terminus", "glacier_2" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))

# Phosphate box plot 
po4_point <- ggplot(core_sites, aes(x=factor(Site, level = c("glacier_2","lake_inlet","tundra" , "shrub_creek" ,"forest" ,"terminus" ,  "stream_gauge" ,"nellie_juan" )), y= P_PO4, color= as.factor(Site))) +
  scale_color_manual( values = c("#E2725B", "#EA9DFF", "#FFAA00", "#A80084", "#73DFFF", "#059E41", "#0084A8", "#6600CC" ), breaks = c( "forest" , "nellie_juan" , "shrub_creek" , "tundra" , "stream_gauge" ,"terminus" , "glacier_2", "lake_inlet"))+
  #geom_boxplot(outlier.shape =  NA) +
  geom_jitter(shape=16, position=position_jitter(0.1))+
  geom_hline(yintercept = 0.001, linetype="dashed", color = "#1A237E", size=1) +
  ylab(bquote('Phosphate' (mgl^-1)))+
  xlab("")+
  ylim(0,0.025)+
  scale_x_discrete(labels=c("forest" = "Forest", "nellie_juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "tundra"= "Tundra" , "stream_gauge"= "Gage" ,"terminus" =  "Terminus", "glacier_2" = "Glacier", "lake_inlet" = "Upper Tundra"))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))



########## No longer used ##############
# DOC boxplot
#ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= DOC, color= as.factor(Site))) +
 # scale_color_manual( values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in ), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
#  geom_boxplot(outlier.shape =  NA) +
#  geom_jitter(shape=16, position=position_jitter(0.2))+
 # ylab(bquote('DOC' (mgl^-1)))+
#  xlab("")+
 # scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
 # theme_cust() +
 # theme(axis.text.x=element_text(angle = -45, hjust = 0))+
#  theme(legend.position = "none")+ 
  #theme(aspect.ratio = 1/1)+
 # theme(axis.text = element_text(size = 16))+
 # theme(axis.title = element_text(size = 16))  


# Nitrate boxplot
#ggplot(core_sites, aes(x=reorder(Site,DOC,na.rm = TRUE), y= Nitrate, color= as.factor(Site))) +
 # scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.gage, col.term, col.glacier, col.lake_in), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra" , "stream_gauge" ,"Terminus" , "glacier_hut", "lake_inlet"))+
 # geom_boxplot(outlier.shape =  NA) +
 # geom_jitter(shape=16, position=position_jitter(0.2))+
 # ylab(bquote('Nitrate' (mgl^-1)))+
  #xlab("")+
#  scale_x_discrete(labels=c("Forest" = "Forest", "Nellie_Juan" = "Nellie Juan" , "shrub_creek"= "Shrub" , "Tundra"= "Tundra" , "stream_gauge"= "Gage" ,"Terminus" =  "Terminus", "glacier_hut" = "Glacier", "lake_inlet" = "Upper Tundra"))+
 # theme_cust() +
 # theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  #theme(legend.position = "none")+ 
 # theme(aspect.ratio = 1/1)+
 # theme(axis.text = element_text(size = 16))+
  #theme(axis.title = element_text(size = 16))


