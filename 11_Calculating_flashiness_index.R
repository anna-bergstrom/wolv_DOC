##11_Calculating_flashiness_index
# This brings in the DOC, Precip, EC, and Relative Stage timeseries for plotting
# Pulls out events as examples and makes plots
#setwd("/Users/annabergstrom/BSU_drive/Projects/AK_post-doc/DOC/wolv_DOC")
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
    summarize(across(everything(), ~(sum(.x,na.rm = TRUE))), .groups = "drop")

  
  ratio_denom <- dataset.sm[2:length(dataset.sm$forest),1:6] %>%
    mutate(week = week(datetime)) %>%
    subset(select = -datetime) %>%
    group_by(week)
  
  ratio_denom_sum <- ratio_denom %>%
    summarize(across(everything(), ~(sum(.,na.rm = TRUE))), .groups = "drop") 
 
  
  FI <- ratio_numer_sum [,2:6]/ratio_denom_sum[,2:6] 
  FI <- FI %>% mutate(week = 1:53) %>%
    replace_with_na_all(condition = ~.x ==0)
  
  return(FI)
}

FI_EC_wagg <- FI_weekly_agg(EC_FullTS)
FI_DOC_wagg <- FI_weekly_agg(DOC_FullTS)
FI_ST_wagg <- FI_weekly_agg(RelST_FullTS)

  
############### Plotting ###################

## Stage
ggplot(data = FI_ST_wagg)+
    geom_point( aes(x=week, y= forest), color = col.forest, size = 3)+
    geom_smooth(aes(x=week, y= forest), color = col.forest, size = 1, se= FALSE, span = 0.3)+ #default currently in figure is 0.75
    geom_point( aes(x=week, y= shrub), color = col.shrub, size = 3)+
    geom_smooth( aes(x=week, y= shrub), color = col.shrub, size = 1, se= FALSE, span = 0.3)+
    geom_point( aes(x=week, y= tundra), color = col.tundra, size = 3)+
    geom_smooth( aes(x=week, y= tundra), color = col.tundra, size = 1, se= FALSE, span = 0.3)+
    geom_point( aes(x=week, y= nellie), color =  col.nellie, size = 3)+
    geom_smooth( aes(x=week, y= nellie), color =  col.nellie, size = 1, se= FALSE, span = 0.3)+
    geom_point( aes(x=week, y= gage), color = col.gage, size = 3)+
    geom_smooth( aes(x=week, y= gage), color = col.gage, size = 1, se= FALSE, span = 0.3)+
    scale_x_continuous(breaks = seq(0, 50, by = 10))+
    xlab('Week')+
    ylab('Flashiness Index')+
    ggtitle("ST")+
    ylim(0,0.008)+
    #xlim(bounds_sub)+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 


## EC 
  ggplot(data = FI_EC_wagg)+
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
    scale_x_continuous(breaks = seq(0, 50, by = 10))+
    xlab('Week')+
    ylab('Flashiness Index')+
    ggtitle("EC")+
    ylim(0,0.008)+
    #xlim(bounds_sub)+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  
  ## DOC 
  ggplot(data = FI_DOC_wagg)+
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
    scale_x_continuous(breaks = seq(0, 50, by = 10))+
    xlab('Week')+
    ylab('Flashiness Index')+
    ggtitle("DOC")+
    ylim(0,0.008)+
    #xlim(bounds_sub)+
    theme_cust()+
    theme(axis.text = element_text(size = 16))+
    theme(axis.title = element_text(size = 16)) 
  

