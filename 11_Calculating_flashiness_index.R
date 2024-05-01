##11_Calculating_flashiness_index
# This brings in the DOC, Precip, EC, and Relative Stage timeseries for plotting
# Pulls out events as examples and makes plots
setwd("/Users/annabergstrom/BSU_drive/Projects/AK_post-doc/DOC/wolv_DOC")
rm(list= ls())
source("paths+packages.R")

# load necessary data
RelST_FullTS <- read.csv('outputs/06_relative_stageTS.csv')
DOC_FullTS <- read.csv('outputs/06_DOC_FullTS.csv')
EC_FullTS <- read.csv('outputs/04_EC_FullTS.csv')

# Convert ISO datestrings to datetime type 
# All times were converted to UTC in previous processing scripts - we'll just work in UTC for consistency here
RelST_FullTS$datetime <- strptime(RelST_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
DOC_FullTS$datetime <- strptime(DOC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
EC_FullTS$datetime <- strptime(EC_FullTS$datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')


# Calculating flashiness index by week  
#Function to calculate FI each week in the enitre dataset sequentially
FI_weekly <- function (dataset){
ratio_numer <- abs(dataset[2:length(dataset$forest),2:6]-dataset[1:length(dataset$forest)-1, 2:6]) %>%
  mutate(datetime = dataset$datetime[2:length(dataset$datetime)]) %>%
  mutate(week = week(datetime), year = year(datetime))

ratio_numer_week <- ratio_numer%>%
  subset(select = -datetime)%>%
  group_by(week,year)

ratio_numer_sum <- ratio_numer_week %>%
  summarize(across(everything(), ~(sum(.x,na.rm = TRUE))))
ratio_numer_sum <- ratio_numer_sum[order(ratio_numer_sum[,2], ratio_numer_sum[,1]),]

ratio_denom <- dataset[2:length(dataset$forest),1:6] %>%
  mutate(week = week(datetime), year = year(datetime)) %>%
  subset(select = -datetime) %>%
  group_by(week,year)
  
ratio_denom_sum <- ratio_denom %>%
  summarize(across(everything(), ~(sum(.,na.rm = TRUE)))) 
ratio_denom_sum <- ratio_denom_sum[order(ratio_denom_sum[,2], ratio_denom_sum[,1]),]

FI <- ratio_numer_sum [,3:7]/ratio_denom_sum[,3:7] 
FI <- FI %>% mutate(week = 1:146) %>%
  replace_with_na_all(condition = ~.x ==0)

return(FI)
}

FI_EC_week <- FI_weekly(EC_FullTS)
FI_DOC_week <- FI_weekly(DOC_FullTS)
FI_ST_week <- FI_weekly(RelST_FullTS)


#Function to calculate FI each week in aggregate across the dataset (i.e. the first week of january in all three years)
dataset <- DOC_FullTS
FI_weekly_agg <- function (dataset){
  dataset.sm <- as.data.frame(rollapply(dataset[,2:6],24,mean, na.rm = TRUE, fill = NA))%>%
    mutate(datetime = dataset$datetime, .before= forest)

  ratio_numer <- abs(dataset.sm[2:length(dataset.sm$forest),2:6]-dataset.sm[1:length(dataset.sm$forest)-1, 2:6]) %>%
    mutate(datetime = dataset.sm$datetime[2:length(dataset.sm$datetime)]) %>%
    mutate(week = week(datetime))
  
  ratio_numer_week <- ratio_numer%>%
    subset(select = -datetime)%>%
    group_by(week)
    
  
  ratio_numer_sum <- ratio_numer_week %>%
    summarize(across(everything(), ~(sum(.x,na.rm = TRUE))))

  
  ratio_denom <- dataset.sm[2:length(dataset.sm$forest),1:6] %>%
    mutate(week = week(datetime)) %>%
    subset(select = -datetime) %>%
    group_by(week)
  
  ratio_denom_sum <- ratio_denom %>%
    summarize(across(everything(), ~(sum(.,na.rm = TRUE)))) 
 
  
  FI <- ratio_numer_sum [,2:6]/ratio_denom_sum[,2:6] 
  FI <- FI %>% mutate(week = 1:53) %>%
    replace_with_na_all(condition = ~.x ==0)
  
  return(FI)
}

FI_EC_wagg <- FI_weekly_agg(EC_FullTS)
FI_DOC_wagg <- FI_weekly_agg(DOC_FullTS)
FI_ST_wagg <- FI_weekly_agg(RelST_FullTS)

  ggplot(data = FI_ST_wagg)+
    geom_point( aes(x=week, y= forest), color = col.forest, size = 3)+
    geom_smooth(aes(x=week, y= forest), color = col.forest, size = 1, se= FALSE)+
    geom_point( aes(x=week, y= shrub), color = col.shrub, size = 3)+
    geom_smooth( aes(x=week, y= shrub), color = col.shrub, size = 1, se= FALSE)+
    geom_point( aes(x=week, y= tundra), color = col.tundra, size = 3)+
    geom_smooth( aes(x=week, y= tundra), color = col.tundra, size = 1, se= FALSE)+
    geom_point( aes(x=week, y= nellie), color =  col.nellie, size = 3)+
    geom_smooth( aes(x=week, y= nellie), color =  col.nellie, size = 1, se= FALSE)+
    geom_point( aes(x=week, y= gage), color = col.gage, size = 3)+
    geom_smooth( aes(x=week, y= gage), color = col.gage, size = 1, se= FALSE)+
    xlab('Week')+
    ylab('Flashiness Index')+
    ggtitle("ST")+
    ylim(0,0.05)+
    #xlim(bounds_sub)+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 






periods <- c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)

breaks <- as.POSIXct(c('04/15/2021 00:00:00', #1
                       '05/29/2021 00:00:00', #2
                       '06/24/2021 23:45:00', #3
                       '09/09/2021 23:45:00', #4
                       '09/22/2021 23:45:00', #5
                       '04/22/2022 23:45:00', #6
                       '05/25/2022 23:45:00', #7
                       '06/23/2022 23:45:00', #8
                       '08/23/2022 23:45:00', #9
                       '10/17/2022 23:45:00', #10
                       '05/10/2023 00:00:00', #11
                       '05/21/2023 23:45:00', #12
                       '06/19/2023 23:45:00', #13
                       '08/25/2023 23:45:00', #14
                       '09/21/2023 23:45:00'
), format="%m/%d/%Y %H:%M:%S", TZ = "America/Anchorage")

###### DOC flashiness calculation ########
#subsetting out first period and interpolating data to 15 min
DOC_sub <- DOC_FullTS[DOC_FullTS$datetime <= breaks[1],] 
DOC_sub <- DOC_sub%>%
  mutate(across(2:6), na_interpolation(., option = 'linear', maxgap = 10))
# sub-sampling data to hourly
hrly_dat<- floor_date(as_datetime(DOC_sub$datetime), unit="hour")
DOC_sub <- DOC_sub[DOC_sub$datetime==as.POSIXct(hrly_dat), ]

#site by site calculating flashiness index
RB <- lapply(DOC_sub[2:6], function(x)
(sum(abs(diff(x)), na.rm = TRUE)/sum(x, na.rm = TRUE))) #R-B index calc
RB <- as.data.frame(RB) %>%
  mutate(period = 0, year = year(breaks[1])) #adding in the year and period of this first period

#now for the rest of the R-B calculations by period, we can do this in a loop, same steps as above
for (i in 1:(length(periods)-1)) {
  DOC_sub <- DOC_FullTS[DOC_FullTS$datetime > breaks[i]  & DOC_FullTS$datetime <= breaks[i+1], ] 
  DOC_sub <- DOC_sub%>%
    mutate(across(2:6), na_interpolation(., option = 'linear', maxgap = 10))
  # sub-sampling data to hourly
  hrly_dat<- floor_date(as_datetime(DOC_sub$datetime), unit="hour")
  DOC_sub <- DOC_sub[DOC_sub$datetime==as.POSIXct(hrly_dat), ]
  
  temp <- lapply(DOC_sub[2:6], function(x)
    (sum(abs(diff(x)), na.rm = TRUE)/sum(x, na.rm = TRUE)))
  temp <- as.data.frame(temp) %>%
    mutate(period = periods[i+1], year = year(breaks[i]))
  RB<- rbind(RB,temp) #adding this period to the overall RB index matrix
}

############## EC flashiness calculation #############
#same steps as above
EC_sub <- EC_FullTS[EC_FullTS$datetime <= breaks[1],] 
EC_sub <- EC_sub%>%
  mutate(across(2:6), na_interpolation(., option = 'linear', maxgap = 10))

# sub-sampling data to hourly
hrly_dat<- floor_date(as_datetime(DOC_sub$datetime), unit="hour")
DOC_sub <- DOC_sub[DOC_sub$datetime==as.POSIXct(hrly_dat), ]

RB_EC <- lapply(DOC_sub[2:6], function(x)
  (sum(abs(diff(x)), na.rm = TRUE)/sum(x, na.rm = TRUE)))
RB_EC <- as.data.frame(RB_EC) %>%
  mutate(period = 0, year = year(breaks[1]))


for (i in 1:(length(periods)-1)) {
  EC_sub <- EC_FullTS[EC_FullTS$datetime > breaks[i]  & EC_FullTS$datetime <= breaks[i+1], ] 
  EC_sub <- EC_sub%>%
    mutate(across(2:6), na_interpolation(., option = 'linear', maxgap = 10))
  temp <- lapply(EC_sub[2:6], function(x)
    (sum(abs(diff(x)), na.rm = TRUE)/sum(x, na.rm = TRUE)))
  temp <- as.data.frame(temp) %>%
    mutate(period = periods[i+1], year = year(breaks[i]))
  RB_EC<- rbind(RB_EC,temp)
}

############## Stage flashiness calculation #############
#same steps as above
ST_sub <- RelST_FullTS[RelST_FullTS$datetime <= breaks[1],] 
ST_sub <- ST_sub%>%
  mutate(across(2:6), na_interpolation(., option = 'linear', maxgap = 10))


RB_ST <- lapply(ST_sub[2:6], function(x)
  (sum(abs(diff(x)), na.rm = TRUE)/sum(x, na.rm = TRUE)))
RB_ST <- as.data.frame(RB_ST) %>%
  mutate(period = 0, year = year(breaks[1]))


for (i in 1:(length(periods)-1)) {
  ST_sub <- RelST_FullTS[RelST_FullTS$datetime > breaks[i]  & RelST_FullTS$datetime <= breaks[i+1], ] 
  ST_sub <- ST_sub%>%
    mutate(across(2:6), na_interpolation(., option = 'linear', maxgap = 10))
  temp <- lapply(ST_sub[2:6], function(x)
    (sum(abs(diff(x)), na.rm = TRUE)/sum(x, na.rm = TRUE)))
  temp <- as.data.frame(temp) %>%
    mutate(period = periods[i+1], year = year(breaks[i]))
  RB_ST<- rbind(RB_ST,temp)
}


########## Plotting #############
x_RB <- seq(1,15,1)

# hydroperiods over time
ggplot(data = RB)+
  geom_point(aes(x = x_RB, y= forest), color = col.forest, size = 2)+
  geom_point(aes(x = x_RB, y= shrub), color = col.shrub, size = 2)+
  geom_point(aes(x = x_RB, y= tundra), color = col.tundra, size = 2)+
  geom_point(aes(x = x_RB, y= nellie), color = col.nellie, size = 2)+
  geom_point(aes(x = x_RB, y= gage), color = col.gage, size = 2)+
  ylab("R-B Flashiness index")+
  ggtitle("DOC")+
  theme_cust() 

ggplot(data = RB_ST)+
  geom_point(aes(x = x_RB, y= forest), color = col.forest, size = 2)+
  geom_point(aes(x = x_RB, y= shrub), color = col.shrub, size = 2)+
  geom_point(aes(x = x_RB, y= tundra), color = col.tundra, size = 2)+
  geom_point(aes(x = x_RB, y= nellie), color = col.nellie, size = 2)+
  geom_point(aes(x = x_RB, y= gage), color = col.gage, size = 2)+
  ylab("R-B Flashiness index")+
  ggtitle("Stage")+
  theme_cust() 

ggplot(data = RB_EC)+
  geom_point(aes(x = x_RB, y= forest), color = col.forest, size = 2)+
  geom_point(aes(x = x_RB, y= shrub), color = col.shrub, size = 2)+
  geom_point(aes(x = x_RB, y= tundra), color = col.tundra, size = 2)+
  geom_point(aes(x = x_RB, y= nellie), color = col.nellie, size = 2)+
  geom_point(aes(x = x_RB, y= gage), color = col.gage, size = 2)+
  ylab("R-B Flashiness index")+
  ggtitle("EC")+
  theme_cust() 


## DOC
RB_DOC_melt <- melt(RB, id = c("period", "year"), variable = "site") #Reshape for plotting

ggplot(data = RB_DOC_melt, aes(x = factor(site, level = c("tundra" , "shrub" ,"forest" , "gage" ,"nellie" )),  y = value))+
  geom_jitter(aes(colour = factor(RB_DOC_melt$year), shape = factor(RB_DOC_melt$period)), size = 3,width = 0.2)+
  ylab("R-B Flashiness index")+
  xlab("")+
  scale_color_brewer(palette="Dark2")+
  scale_shape_manual(values = c(1,2,3,4,5))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = c(0.8, 0.8))+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))  


## EC
RB_EC_melt <- melt(RB_EC, id = c("period", "year"), variable = "site") #Reshape for plotting

ggplot(data = RB_EC_melt, aes(x = factor(site, level = c("tundra" , "shrub" ,"forest" , "gage" ,"nellie" )),  y = value))+
  geom_jitter(aes(colour = factor(RB_EC_melt$year), shape = factor(RB_EC_melt$period)), size = 3,width = 0.2)+
  ylab("R-B Flashiness index")+
  xlab("")+
  scale_color_brewer(palette="Dark2")+
  scale_shape_manual(values = c(1,2,3,4,5))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = c(0.8, 0.8))+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))  

## Stage
RB_ST_melt <- melt(RB_ST, id = c("period", "year"), variable = "site") #Reshape for plotting

ggplot(data = RB_ST_melt, aes(x = factor(site, level = c("tundra" , "shrub" ,"forest" , "gage" ,"nellie" )),  y = value))+
  geom_jitter(aes(colour = factor(RB_ST_melt$year), shape = factor(RB_ST_melt$period)), size = 3,width = 0.2)+
  ylab("R-B Flashiness index")+
  xlab("")+
  scale_color_brewer(palette="Dark2")+
  scale_shape_manual(values = c(1,2,3,4,5))+
  theme_cust() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(legend.position = "outside")+ 
  theme(aspect.ratio = 1/1)+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))  
