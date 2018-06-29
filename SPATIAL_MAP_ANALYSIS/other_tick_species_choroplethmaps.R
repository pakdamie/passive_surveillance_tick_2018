################################
#######FIGURE 5 ################
################################

library(dplyr)
library(ggplot2)
library(lemon)
###ABSENCE/OCCURENCE MAPS OF THE OTHER GENUS
###################################################################
counties <- map_data("county")
pa_county <- subset(counties, region == 'pennsylvania')
colnames(pa_county)[6] <- 'County'
###AMBLYOMMA 

AMB<- subset(PA_SUB, PA_SUB$Genus == 'Amblyomma'& PA_SUB$Species!='americanum')
AMB_PA<- aggregate(AMB$submission, by=list(AMB$County),'sum')
AMB_PA_SP <- aggregate(AMB$submission, by= list(AMB$County, AMB$Species),'sum')
colnames(AMB_PA)<- c('County','Submission')
AMB_PA$presence <- 'Yes'

AMB_PA_SPAT <- left_join(pa_county,AMB_PA, by=c('County'),copy=TRUE)
AMB_PA_SPAT$presence[is.na(AMB_PA_SPAT$presence)==TRUE]<-'No'

AMB_OCCURENCE<-
  ggplot(AMB_PA_SPAT, aes(x= long,y=lat, group=group))+
  geom_polygon(data=AMB_PA_SPAT, aes(x= long+0.05, y= lat-0.05), fill='grey')+
  geom_polygon(data=AMB_PA_SPAT, aes(x= long,y=lat, group=group,fill =presence),
               color='black')+coord_map()+theme_void()+
  scale_fill_manual(values=c("Yes"="#ffcd51","No"="#293444"),
                    name='Presence')+labs(title = "Other Amblyomma species")+
  theme(plot.title = 
          element_text(hjust = 0.5,lineheight=.8, face="bold.italic",
                       size=13))


###ARGAS

ARG<- subset(PA_SUB, PA_SUB$Genus == 'Argas')
ARG_PA<- aggregate(ARG$submission, by=list(ARG$County),'sum')
colnames(ARG_PA)<- c('County','Submission')
ARG_PA$presence <- 'Yes'

ARG_PA_SPAT <- left_join(pa_county,ARG_PA, by=c('County'),copy=TRUE)
ARG_PA_SPAT$presence[is.na(ARG_PA_SPAT$presence)==TRUE]<-'No'

ARG_OCCURENCE<- ggplot(ARG_PA_SPAT, aes(x= long,y=lat, group=group))+
  geom_polygon(data=ARG_PA_SPAT, aes(x= long+0.05, y= lat-0.05), fill='grey')+
  geom_polygon(data=ARG_PA_SPAT, aes(x= long,y=lat, group=group,fill 
                                     =presence),color='black')+
                 coord_map()+theme_void()+
  scale_fill_manual(values=c("Yes"="#ffcd51","No"="#293444"),
                    name='Presence')+labs(title = "Argas species")+
  theme(plot.title = 
          element_text(hjust = 0.5,lineheight=.8, face="bold.italic",
                       size=13))

###ARGAS

CAR<- subset(PA_SUB, PA_SUB$Genus == 'Carios' |PA_SUB$Genus ==
               'Ornithodoros')
CAR_PA<- aggregate(CAR$submission, by=list(CAR$County),'sum')
colnames(CAR_PA)<- c('County','Submission')
CAR_PA$presence <- 'Yes'

CAR_PA_SPAT <- left_join(pa_county,CAR_PA, by=c('County'),copy=TRUE)
CAR_PA_SPAT$presence[is.na(CAR_PA_SPAT$presence)==TRUE]<-'No'

CAR_OCCURENCE<- ggplot(CAR_PA_SPAT , aes(x= long,y=lat, group=group))+
  geom_polygon(data=CAR_PA_SPAT , aes(x= long+0.05, y= lat-0.05), fill='grey')+
  geom_polygon(data=CAR_PA_SPAT , aes(x= long,y=lat, group=group,fill 
                                     =presence),color='black')+coord_map()+theme_void()+
  scale_fill_manual(values=c("Yes"="#ffcd51","No"="#293444"),
                    name='Presence')+labs(title = "Carios species")+
  theme(plot.title = 
          element_text(hjust = 0.5,lineheight=.8, face="bold.italic",
                       size=13))

###DERMACENTOR

DERM<- subset(PA_SUB, PA_SUB$Genus == 'Dermacentor' &PA_SUB$Species !=
               'variabilis')
DERM_PA<- aggregate(DERM$submission, by=list(DERM$County, DERM$Species),'sum')
colnames(DERM_PA)<- c('County','Submission')
DERM_PA$presence <- 'Yes'

DERM_PA_SPAT <- left_join(pa_county,DERM_PA, by=c('County'),copy=TRUE)
DERM_PA_SPAT$presence[is.na(DERM_PA_SPAT$presence)==TRUE]<-'No'

DERM_OCCURENCE<-ggplot(DERM_PA_SPAT , aes(x= long,y=lat, group=group))+
  geom_polygon(data=DERM_PA_SPAT , aes(x= long+0.05, y= lat-0.05), fill='grey')+
  geom_polygon(data=DERM_PA_SPAT, aes(x= long,y=lat, group=group,fill 
                                      =presence),color='black')+coord_map()+theme_void()+
  scale_fill_manual(values=c("Yes"="#ffcd51","No"="#293444"),
                    name='Presence')+labs(title = "Other Dermacentor species")+
  theme(plot.title = 
          element_text(hjust = 0.5,lineheight=.8, face="bold.italic",
                       size=13))

###Haemaphysalis

HAE<- subset(PA_SUB, PA_SUB$Genus == 'Haemaphysalis')
HAE_PA<- aggregate(HAE$submission, by=list(HAE$County),'sum')
colnames(HAE_PA)<- c('County','Submission')
HAE_PA$presence <- 'Yes'

HAE_PA_SPAT <- left_join(pa_county,HAE_PA, by=c('County'),copy=TRUE)
HAE_PA_SPAT$presence[is.na(HAE_PA_SPAT$presence)==TRUE]<-'No'

HAE_OCCURENCE<-ggplot(HAE_PA_SPAT , aes(x= long,y=lat, group=group))+
  geom_polygon(data=HAE_PA_SPAT , aes(x= long+0.05, y= lat-0.05), fill='grey')+
  geom_polygon(data=HAE_PA_SPAT, aes(x= long,y=lat, group=group,fill 
                                      =presence),color='black')+coord_map()+theme_void()+
  scale_fill_manual(values=c("Yes"="#ffcd51","No"="#293444"),
                    name='Presence')+labs(title = "Haemaphysalis species")+
  theme(plot.title = 
          element_text(hjust = 0.5,lineheight=.8, face="bold.italic",
                       size=13))


###Haemaphysalis

IXODES <- subset(PA_SUB, PA_SUB$Genus == 'Ixodes' & 
                   PA_SUB$Species != 'scapularis'&
                   PA_SUB$Species != 'cookei')
IXODES_PA<- aggregate(IXODES$submission, by=list(IXODES$County, IXODES$Species),'sum')
colnames(IXODES_PA)<- c('County','Submission')
IXODES_PA$presence <- 'Yes'

IXODES_PA_SPAT <- left_join(pa_county,IXODES_PA, by=c('County'),copy=TRUE)
IXODES_PA_SPAT$presence[is.na(IXODES_PA_SPAT$presence)==TRUE]<-'No'

IXODES_OCCURENCE<-ggplot(IXODES_PA_SPAT, aes(x= long,y=lat, group=group))+
  geom_polygon(data=IXODES_PA_SPAT , aes(x= long+0.05, y= lat-0.05), fill='grey')+
  geom_polygon(data=IXODES_PA_SPAT, aes(x= long,y=lat, group=group,fill 
                                     =presence),color='black')+coord_map()+theme_void()+
  scale_fill_manual(values=c("Yes"="#ffcd51","No"="#293444"),
                    name='Presence')+labs(title = "Other Ixodes species")+
  theme(plot.title = 
          element_text(hjust = 0.5,lineheight=.8, face="bold.italic",
                       size=13))


grid_arrange_shared_legend(AMB_OCCURENCE, ARG_OCCURENCE,
                           CAR_OCCURENCE, DERM_OCCURENCE,
                           HAE_OCCURENCE, IXODES_OCCURENCE,
                           nrow=3, ncol=2)
