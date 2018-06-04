library(ggplot2)
library(dplyr)
library(viridis)
###############################################
######CREATING A POPULATION ###################
###############################################

POP_DAT <- read.csv("pa_pop_2010.csv")
POP_DAT$County<- tolower(POP_DAT$County)



counties <- map_data("county")
pa_county <- subset(counties, region == 'pennsylvania')
colnames(pa_county)[6] <- 'County'

POP_DAT_COUNTY <- as.numeric(gsub(",","",POP_DAT$Total.Pop))

POP_SPAT<-left_join(pa_county, POP_DAT, by= 'County')
POP_SPAT$Total.Pop<- as.numeric(gsub(",","",POP_SPAT$Total.Pop))

ggplot(POP_SPAT, aes(x= long, y= lat, group=County))+
  geom_polygon(data=POP_SPAT, aes(x= long+0.05, y= lat-0.05), fill='grey')+
  geom_polygon(data=POP_SPAT, aes(x= long, y= lat, group=County,fill=(Total.Pop)),color='black')+theme_void()+
  scale_fill_viridis(option='viridis',name = 'Total Population')

#####################
#regular county map##
#######################
ggplot(pa_county , aes(x=long, y=lat, group=County))+
  geom_polygon(data=pa_county, aes(x= long+0.05, y= lat-0.05), fill='grey')+
  theme_void()+coord_map()+
  geom_polygon(data = pa_county,fill = '#e1e5be',color='black')
