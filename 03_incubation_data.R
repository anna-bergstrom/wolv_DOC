## 03_incubation_data
# Processing BDOC data and comparing with other related carbon data

source("paths+packages.R")

#reading in 2022 Incubation results
incub22 <- read.csv('Data/incubation_results22.csv') 
incub17 <- read.csv("Data/INcubation2017_DOC_data.csv")
NOSAMS <- read.csv('Data/NOSAMS_results.csv') 

# pulling the names for each site 
incub_names22 <- unique(incub22$Site)
incub_names17 <- unique(incub17$location) 


# calculating the mean and standatd deviation of triplicates for each site and timepoint
incub_summary22 <- incub22 %>%
  group_by(Site,Time.Point) %>%
  summarise(meanconc = mean(DOC_mgL),stdconc = sd(DOC_mgL),mean_FI = mean(FI),mean_HIX = mean(HIX), mean_FDOM = mean(FDOM)) 

incub17 <- incub17[incub17$keep == 1, ] %>%
  mutate(DOC_mg = DOC..ug.l./1000)
incub_summary17 <- incub17 %>%
  group_by(location,time) %>%
  summarise(meanconc = mean(DOC_mg), stdconc = sd(DOC_mg))
  

# calculate a percent change from one time step to the next
incub_summary22 <-incub_summary22 %>%
  group_by(Site) %>% 
  mutate(roll_pct_change = ((meanconc[1]-meanconc)/meanconc[1]) * 100)

incub_summary17 <-incub_summary17 %>%
  group_by(location) %>% 
  mutate(roll_pct_change = ((meanconc[1]-meanconc)/meanconc[1]) * 100)

# plot of DOC concentration over time
ggplot()+
  geom_line(data = incub_summary22, aes(x=Time.Point, y = meanconc, group= Site, color =Site))+
  geom_point(data = incub_summary22, aes(x=Time.Point, y = meanconc, group= Site, color =Site), size = 3)+
  geom_line (data = incub_summary17, aes( x = time, y = meanconc, group = location, color = location))+
  geom_point(data = incub_summary17, aes( x = time, y = meanconc, group = location, color = location), shape = 15, size = 3)+
  geom_errorbar(data = incub_summary17, aes(x = time, ymin=meanconc-stdconc, ymax=meanconc+stdconc, color = location ), width=.6)+
  geom_errorbar(data = incub_summary22, aes(x=Time.Point,ymin=meanconc-stdconc, ymax=meanconc+stdconc, color = Site ), width=.6)+
  scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.term, col.glacier, col.lake_in, col.glacier, col.term, col.nellie), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra"  ,"Terminus" , "Glacier", "lake inlet", 'glacial', 'terminus', 'nellie_juan'))+
  theme_cust()+
  xlim(0,7)+
  ylab("DOC (ppm)")+
  xlab("Days since incubation start") 
#ggsave(file ="Incub_TS_conc.pdf",width=6, height=5, units = "in" )

#Plot of percent change over time
ggplot()+
  geom_line(data = incub_summary22, aes(x=Time.Point, y = roll_pct_change, group= Site, color =Site))+
  geom_point(data = incub_summary22, aes(x=Time.Point, y = roll_pct_change, group= Site, color =Site), size = 3)+
  geom_line (data = incub_summary17, aes( x = time, y = roll_pct_change, group = location, color = location))+
  geom_point(data = incub_summary17, aes( x = time, y = roll_pct_change, group = location, color = location), shape = 15, size = 3)+
  scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.term, col.glacier, col.lake_in, col.glacier, col.term, col.nellie), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra"  ,"Terminus" , "Glacier", "lake inlet", 'glacial', 'terminus', 'nellie_juan'))+
  theme_cust()+
  xlim(0,7)+
  #ylim(0,50)+
  ylab("DOC percent lost")+
  xlab("Days since incubation start") 
#ggsave(file ="Incub_TS_percentlost.pdf",width=6, height=5, units = "in" )

# Creating a dataframe/table with initial DOC concentration and the % lost after 6 days of incubation. 
DOC_lost22 <- rep(NA,length(incub_names22))
DOC_init22 <- rep(NA,length(incub_names22))

DOC_lost17 <- rep(NA,length(incub_names17))
DOC_init17 <- rep(NA,length(incub_names17))

for (k in 1:length(incub_names22)) {
  temp <- incub_summary22 %>%
    filter(Site == incub_names22[k] & (Time.Point == 0 |Time.Point == 6)) 
  
  DOC_lost22[k] <- ((temp$meanconc[1] - temp$meanconc[2])/temp$meanconc[1])*100 
  DOC_init22[k]<- temp$meanconc[1]
}

DOC_lost22 <- data.frame(DOC_lost22,incub_names22,DOC_init22) %>%
  rename(Site = incub_names22)

for (k in 1:length(incub_names17)) {
  temp <- incub_summary17 %>%
    filter(location == incub_names17[k] & (time == 0 |time == 6)) 
  
  DOC_lost17[k] <- ((temp$meanconc[1] - temp$meanconc[2])/temp$meanconc[1])*100 
  DOC_init17[k]<- temp$meanconc[1]
}

DOC_lost17 <- data.frame(DOC_lost17,incub_names17,DOC_init17) %>%
  rename(Site = incub_names17)


ggplot()+
  geom_point(data = DOC_lost17, aes(x=DOC_init17, y = DOC_lost17,  color =Site), shape = 15, size = 3)+
  geom_point(data = DOC_lost22, aes(x=DOC_init22, y = DOC_lost22,  color =Site), size = 3)+
  scale_color_manual(values = c(col.forest, col.nellie, col.shrub, col.tundra, col.term, col.glacier, col.lake_in, col.glacier, col.term, col.nellie), breaks = c( "Forest" , "Nellie_Juan" , "shrub_creek" , "Tundra"  ,"Terminus" , "Glacier", "lake inlet", 'glacial', 'terminus', 'nellie_juan'))+
  theme_cust()+
  ylim(0,75)+
  ylab("DOC percent lost")+
  xlab("Initial DOC") 

# More code for plotting 14C data against incubation data is in original Grab_sample_analysis.R script 
#- as of right now, they aren't going to be used in the paper so they weren't ported over 


