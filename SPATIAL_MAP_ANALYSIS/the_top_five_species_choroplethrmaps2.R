###libraries required
library(ggplot2)
library(dplyr)
library(gridExtra)


###THE PA population for 1960, 1990, 2000,2010
PA_POP_60_10<- read.csv("pa60_10.csv") #GET THIS FROMT HE PA_POP_DAT DIRECTORY

###County Populuation Data
###Make everything lower case
PA_POP_60_10$County <- tolower(PA_POP_60_10$County)
###takes out the white-space
PA_POP_60_10$County<- trimws(PA_POP_60_10$County)

###County Spatial Information: spatial data of the PA counties
counties <- map_data("county")
###Only looking at Pennsylvania 
pa_county <- subset(counties, region == 'pennsylvania')
colnames(pa_county)[6] <- 'County'
pa_count$County <- as.character(pa_county$County)
##################
###THE PERIODS ###
##################
###Subsetting the data###

Ameri <- subset(PA_SUB, PA_SUB$Species == 'americanum')
Vari <- subset(PA_SUB, PA_SUB$Species== 'variabilis')
Scap <- subset(PA_SUB, PA_SUB$Species == 'scapularis')
Cook <- subset(PA_SUB, PA_SUB$Species== 'cookei')
Sang<- subset(PA_SUB, PA_SUB$Species== 'sanguineus')


###1960

###AMBLOYMMA AMERICANUM 

###A vector of the decades we want to look at
DECADES <- c("1960-1970", '1990-2000','2000-2010', '2010-2020')
###A vector of the year in the census data we want to look at
Year  <- c('X1960', 'X1990','X2000',"X2010")
###A function that combines spatial data and the individual data
Species_Decades <- function(x){
  y = NULL 
  for (i in seq(1,4)){
     Dat = x              ###Take the data (x) and put it in a new var name Dat
     Dat = subset(Dat, Dat$Decade == DECADES[i]) ###Loop around to look at data at a specific decades
     Dat2 =    Dat[,c("County",'Individuals')] ###Use Dat2 now and rename the columns
     Dat2 = subset(Dat2, Dat2$County != 'no record'& Dat2$County != '') ###Make sure that 'no-record' and '' empty is gone
    
      if((nrow(Dat2))== 0){ ###Some decades there are not going to be any submissio
       Dat2[1,]<- c('adams',as.numeric(0))   ###Just to make things work, we're going to insert an artificial data
       Dat2$Decade = DECADES[i] ###Name the decade
       ###Look at specific population data to adjust by
       pop_dat <- cbind.data.frame(County = PA_POP_60_10$County,Pop= PA_POP_60_10[,Year[i]])
       ###Join the population data and the tick data
       pop_dat_2 <- left_join(pop_dat, Dat2, by='County')
       #This is the formula for adjusting
        pop_dat_2$Incidence <- (as.numeric(pop_dat_2$Individuals)/pop_dat_2$Pop)*100000
        
        ###Now we take the data and merge it with the spatial data for mapping 
       spat_pop_dat2 <- left_join(pa_county,   pop_dat_2,by=c('County'))
       ###If it's NAin the Decades, then replace with the ith of the Vector list
       spat_pop_dat2$Decade[is.na(spat_pop_dat2$Decade)==TRUE]<- DECADES[i]
       ###Put this in a list
       y[[i]] =     spat_pop_dat2
     }
     else{ ###If there is information in this decade- then continue on this way
     Dat2 = (aggregate(Dat2$Individuals,by=list(Dat2$County),'sum')) #Sum up by counties 
     colnames(Dat2)<- c('County','Individuals')
     Dat2$Decade = DECADES[i]  # Give it the decade column 
     ###Look at specific population data to adjust by
     pop_dat <- cbind.data.frame(County = PA_POP_60_10$County,Pop= PA_POP_60_10[,Year[i]])
     ###Join the population data and the tick data
     pop_dat_2 <- left_join(pop_dat, Dat2, by='County')
     #This is the formula for adjusting
     
     pop_dat_2$Incidence <- (as.numeric(pop_dat_2$Individuals)/pop_dat_2$Pop)*100000
     ###Now we take the data and merge it with the spatial data for mapping 
     
     spat_pop_dat2 <- left_join(pa_county,   pop_dat_2,by=c('County'))
     ###If it's NAin the Decades, then replace with the ith of the Vector list
     
     spat_pop_dat2$Decade[is.na(spat_pop_dat2$Decade)==TRUE]<- DECADES[i]
     ###Put this in a list
     
     y[[i]] =     spat_pop_dat2}
  }
      
###Now bind all the elemtns ina list. 
  a<- (do.call(rbind,y))
  ###Cut at these different numbers 
  a$CUT<-cut(a$Incidence, 
                     breaks=  c(0,1,10,40,100,200,500,1000),
                     include.lowest = TRUE,dig.lab=10)
  ###If Incidence is NA, then replace with 0
  a$Incidence[is.na(a$Incidence)==TRUE]=0
  ###We are assuming that if we don't have any submission (NA), that it's 0.
  a$CUT[is.na(a$CUT)==TRUE] =  "[0,1]"
  
return(a)
}


###THIS IS THE FUNCTION FOR CUMULATIVE 

ALL<- function(x){
  Dat = x ###Call the data inserted and call it Dat
Dat = aggregate(Dat$Individuals, by=list(Dat$County),'sum') ###Aggregate the total number of individuals by County 
colnames(Dat) = c("County","Ind")
###Ensure that there is no 'No record" or "" in the data
Dat2 = subset(Dat, Dat$County != 'no record'& Dat$County != '')
###Joint he spatial data and the tick individual data
spat_pop_dat2 <- left_join(pa_county,  Dat2,by=c('County'))
###If there are no individuals in a county, make it 0 
spat_pop_dat2$Ind[is.na(spat_pop_dat2$Ind)==TRUE ]<-0
return(spat_pop_dat2)
}


Ameri_inc<-cbind(Species_Decades(Ameri),id= 'A.americanum')
Ameri_all <- cbind(ALL(Ameri), id = 'A.americanum')

Cook_inc<- cbind(Species_Decades(Cook),id='I.cookei')
Cook_all <- cbind(ALL(Cook), id = 'I.cookei')

Sang_inc <- cbind(Species_Decades(Sang),id='R.sanguineus')
Sang_all <- cbind(ALL(Sang),id='R.sanguineus')

Scap_inc <- cbind(Species_Decades(Scap),id='I.scapularis')
Scap_all <-  cbind(ALL(Scap),id='I.scapularis')

Vari_inc <- cbind(Species_Decades(Vari),id='D.variabilis')
Vari_all <-  cbind(ALL(Vari),id='D.variabilis')

ALL_RATE_INCIDENCE <- rbind.data.frame(Ameri_inc,Cook_inc, Sang_inc, Scap_inc, Vari_inc)
ALL_IND <- rbind.data.frame(Ameri_all,Cook_all, Sang_all, Scap_all, Vari_all)

all_CUT <- ggplot(ALL_RATE_INCIDENCE, aes(x = long,y = lat, group=group,fill =CUT))+
  geom_polygon(color='black')+
  scale_fill_viridis(discrete=TRUE,option='virdis')+
  facet_grid(id~Decade,switch ='y')+coord_map()+theme_void()+
  theme(legend.position="bottom")


all_IND<- ggplot(ALL_IND,aes(x=long, y= lat, group=group, fill = log(Ind+1)))+
  geom_polygon(color='black')+
  scale_fill_viridis(option='magma')+
  facet_grid(id~.)+coord_map()+theme_void()



