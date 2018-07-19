library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)
###############################################
######CREATING A POPULATION ###################
###############################################

#POP_DAT <- read.csv("pa_pop_2010.csv")
#POP_DAT$County<- tolower(POP_DAT$County)



#counties <- map_data("county")
#pa_county <- subset(counties, region == 'pennsylvania')
#colnames(pa_county)[6] <- 'County'

#POP_DAT_COUNTY <- as.numeric(gsub(",","",POP_DAT$Total.Pop))

#POP_SPAT<-left_join(pa_county, POP_DAT, by= 'County')
#POP_SPAT$Total.Pop<- as.numeric(gsub(",","",POP_SPAT$Total.Pop))

#ggplot(POP_SPAT, aes(x= long, y= lat, group=County))+
 # geom_polygon(data=POP_SPAT, aes(x= long+0.05, y= lat-0.05), fill='grey')+
 # geom_polygon(data=POP_SPAT, aes(x= long, y= lat, group=County,fill=(Total.Pop)),color='black')+theme_void()+
  #scale_fill_viridis(option='viridis',name = 'Total Population')

#####################
#regular county map##
#######################
#ggplot(pa_county , aes(x=long, y=lat, group=County))+
#  geom_polygon(data=pa_county, aes(x= long+0.05, y= lat-0.05), fill='grey')+
 # theme_void()+coord_map()+
#  geom_polygon(data = pa_county,fill = '#e1e5be',color='black')


###OLD WAY ABOVE

###TAKING INTO ACCOUNT ALL THE 
POP_DAT <- read.csv("pa60_10.csv")


POP_DAT$County<- tolower(POP_DAT$County)
POP_DAT$County<- trimws(POP_DAT$County)



counties <- map_data("county")
pa_county <- subset(counties, region == 'pennsylvania')
colnames(pa_county)[6] <- 'County'
pa_county$County <- tolower(pa_county$County)

POP_SPAT<-left_join(pa_county, POP_DAT, by= 'County')

###Remove unnecessary columns

POP_SPAT_1960<- POP_SPAT[,c(1,2,3,6,10)]
ggplot(POP_SPAT_1960, aes(x= long, y= lat , group=group))+
  geom_polygon(color='black',aes(fill=(X1960)))+scale_fill_viridis()

POP_SPAT_1990<- POP_SPAT[,c(1,2,3,6,9)]
ggplot(POP_SPAT_1990, aes(x= long, y= lat , group=group))+
  geom_polygon(color='black',aes(fill=(X1990)))+scale_fill_viridis()






POP_SPAT_2 <- POP_SPAT[,-c(4,5)]

POP_SPAT_2_M <- melt(POP_SPAT_2, id.vars=c("long","lat","group",'County'),
                     measure.vars =c('X1960','X1990','X2000','X2010'))+theme_voi()


ggplot(POP_SPAT_2_M, aes(x= long, y= lat , group=group))+geom_polygon(color='black',
                                                                      aes(fill=value))+
  facet_wrap(~variable)+scale_fill_viridis(option='viridis')+coord_map()+theme_void()


###POPULATION SHIFT

POP_SPAT_2$DIFF<- POP_SPAT$X2010 - POP_SPAT$X1960

Pop_Difference <- ggplot(POP_SPAT_2, aes(x= long, y= lat, group=group))+geom_polygon(color='black', 
                                                                  aes(fill = DIFF))+
  scale_fill_gradient2(low='navyblue',high='darkred',limits=c(-500000, 500000))+theme_void()+ggtitle('Population difference between 1960 and 2000')
