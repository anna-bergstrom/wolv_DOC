
############# Making stacked bar of land cover #####################

land_cover <- read.csv('data/land_cover.csv')

ggplot(land_cover, aes(fill=cover, y=value, x=reorder(Site,DOC))) + 
  geom_bar(position="stack", stat="identity", color = "black")+
  scale_fill_manual(values = c("#C1D78D", "#FFFFFF", "#C79F89","#919191",  "#4E6E60" ), breaks= c("Tundra", "Ice", "Shrub", "Rock", "Forest"))+
  ylab(expression(paste("Percent of watershed")))+
  xlab("")+
  
  theme_cust()+
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))   