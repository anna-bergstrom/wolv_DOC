## 03_incubation_data

# Processing BDOC data and comparing with other related carbon data

#reading in 2022 Incubation results
incub22 <- read.csv('Data/incubation_results22.csv') 
NOSAMS <- read.csv('Data/NOSAMS_results.csv') 

# pulling the names for each site 
incub_names <- unique(incub22$Site)

# calculating the mean and standatd deviation of triplicates for each site and timepoint
incub_summary <- incub22 %>%
  group_by(Site,Time.Point) %>%
  summarise(meanconc = mean(DOC_mgL),stdconc = sd(DOC_mgL),mean_FI = mean(FI),mean_HIX = mean(HIX), mean_FDOM = mean(FDOM)) 

# calculate a percent change from one time step to the next
incub_summary <-incub_summary %>%
  group_by(Site) %>% 
  mutate(roll_pct_change = ((meanconc[1]-meanconc)/meanconc[1]) * 100)

# plot of DOC concentration over time
ggplot(incub_summary, aes(x=Time.Point, y = meanconc, group= Site, color =Site))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=meanconc-stdconc, ymax=meanconc+stdconc ), width=.6,position=position_dodge(0.05))+
  theme_cust()+
  xlim(0,30)+
  ylab("DOC (ppm)")+
  xlab("Days since incubation start") 
ggsave(file ="Incub_TS_conc.pdf",width=6, height=5, units = "in" )

#Plot of percent change over time
ggplot(incub_summary, aes(x=Time.Point, y = roll_pct_change, group= Site, color =Site))+
  geom_line()+
  geom_point()+
  theme_cust()+
  geom_hline(yintercept = 0)+
  xlim(0,30)+
  ylab("DOC percent lost")+
  xlab("Days since incubation start") 
ggsave(file ="Incub_TS_percentlost.pdf",width=6, height=5, units = "in" )

# Creating a dataframe/table with initial DOC concentration and the % lost after 6 days of incubation. 
DOC_lost <- rep(NA,length(incub_names))
DOC_init <- rep(NA,length(incub_names))

DOC_Fparams<- incub_summary %>%
  filter(Time.Point == 0) %>%
  subset(select = c(Site,mean_FI, mean_HIX, mean_FDOM ))

for (k in 1:length(incub_names)) {
  temp <- incub_summary %>%
    filter(Site == incub_names[k] & (Time.Point == 0 |Time.Point == 6)) 
  
  DOC_lost[k] <- ((temp$meanconc[1] - temp$meanconc[2])/temp$meanconc[1])*100 
  DOC_init[k]<- temp$meanconc[1]
}

DOC_lost <- data.frame(DOC_lost,incub_names,DOC_init) %>%
  rename(Site = incub_names)

# Full table with NOSAMS and carbon age data merged with incubation data 
DOC_full <- merge(DOC_lost, NOSAMS, by="Site")
DOC_full <- merge(DOC_full, DOC_Fparams, by="Site")
DOC_full$DOC_lost[DOC_full$Site == "Nellie_Juan"] = 0

# More code for plotting 14C data against incubation data is in original Grab_sample_analysis.R script 
#- as of right now, they aren't going to be used in the paper so they weren't ported over 


