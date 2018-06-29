###################################
#########DOT DENSITY ############
################################
library(ggplot2)
library(plyr)
library(reshape)
library(maptools)
library(viridis)
library(ggmap)
library(data.table)

#################################
#######ALL SPECIES###############
##################################

###AGGREGATE THE NUMBER OF SUBMISSIONS BY BOTH COUNTY AND SPECIES
PA_SUB_GROUPED <- cbind.data.frame(County =PA_SUB$County,
                                   Species = PA_SUB$Species,
                                   Sub = PA_SUB$Individuals)
PA_SUB_GROUPED $Species<- revalue(PA_SUB_GROUPED$Species,
                  c("leporispalustris"="Ixodidae species",
                    "cajennense" = 'Ambloymma species',
                    'muris' = 'Ixodidae species',
                    'marxi' = 'Ixodidae species',
                    'chordeilis' = 'Ixodidae species',
                    'kelleyi' = 'Soft tick species',
                    'cooleyi' = 'Soft tick species',
                    'angustus'='Ixodidae species',
                    'persicus' = 'Soft tick species',
                    'longirostre' = 'Ambloymma species',
                    'dissimile' = 'Ambloymma species',
                    'maculatum' = 'Ambloymma species',
                    'sp.'= 'Ixodidae species',
                    'transversale' = 'Ixodidae species',
                    'Ixodidae' = 'Ixodidae species',
                    'andersoni' = 'Dermacentor species',
                    'affinis'= 'Ixodidae species'))



PA_SUB_SP<- aggregate(PA_SUB[,17],by =list(PA_SUB$County,PA_SUB$Species),'sum')
PA_SUB_GROUPED_SP <- aggregate(PA_SUB_GROUPED$Sub, by= list(PA_SUB_GROUPED$Species,
                                                        PA_SUB_GROUPED$County),
                               'sum')


PA_SUB_GROUPED_SP <- subset(PA_SUB_GROUPED_SP,
                            PA_SUB_GROUPED_SP$Group.2 !=''&
                              PA_SUB_GROUPED_SP$Group.2!= 'no record')
PA_SUB_GROUPED_SP<- subset(PA_SUB_GROUPED_SP,PA_SUB_GROUPED_SP$Group.1 !=''&
                             PA_SUB_GROUPED_SP$Group.1 != 'Not determined'&
                             PA_SUB_GROUPED_SP$Group.1 != '<NA>')


####REMOVE ANY ROWS WHERE IT HAS EITHER '' or "no record for the county
PA_SUB_SP_2 <- subset(PA_SUB_SP, PA_SUB_SP$Group.1 !=''&
                        PA_SUB_SP$Group.1 != 'no record')
###REMOVE ANY ROWS WHERE IT HAS EITHER '' OR "NOT DETERMINED 
PA_SUB_SP_3 <- subset(PA_SUB_SP_2, PA_SUB_SP_2$Group.2 !=''&
                       PA_SUB_SP_2$Group.2 != 'Not determined'&
                        PA_SUB_SP_2$Group.2 != '<NA>')
###COLLECT ANY GROUP. 2 INTO ITS OWN IXODIDAE SP. GROUP
PA_SUB_SP_3$Group.2<- revalue(PA_SUB_SP_3$Group.2, c("sp."="Ixodidae"))
###Rename columns 
###THIS IS THE FIXED DATA FRAME THAT WILL BE USED FOR THE GRAPHING
colnames(PA_SUB_SP_3)<- c('County','Species','Individuals')

colnames(PA_SUB_GROUPED_SP) <- c('Species', 'County', 'Individuals')
###SHORTENING





###################################################################
###GETTING THE SPATIAL DATA########################################
###################################################################
counties <- map_data("county")
pa_county <- subset(counties, region == 'pennsylvania')
colnames(pa_county)[6] <- 'County'
###JUST WANT THE COUNTY NAMES AND THE UNIQUE GROUP NUMBER 
PA_COUNTY_GROUP <- unique(pa_county[,c(6,3)])
#Merging both the county unique names and the actual data
PA_SUB_SP_MERGED<-merge(PA_COUNTY_GROUP, PA_SUB_SP_3, by='County')
PA_SUB_SP_MERGED_CAST<-dcast(PA_SUB_SP_MERGED[,c(2,3,4)], group ~Species,sum  )

PA_SUB_SP_GROUPEDMERGED <- merge(PA_COUNTY_GROUP,PA_SUB_GROUPED_SP, by= 'County')
PA_SUB_SP_GROUPEDMERGED_CAST <- dcast(PA_SUB_SP_GROUPEDMERGED[,c(2,3,4)],
                                      group~Species, sum)
###THIS IS THE DATA STRUCTURE NEEDED
rownames(PA_SUB_SP_MERGED_CAST)<- PA_SUB_SP_MERGED_CAST$group
rownames(PA_SUB_SP_GROUPEDMERGED_CAST) <- PA_SUB_SP_GROUPEDMERGED_CAST$group

##############################################################
###POLYGONS                                 ###################
##############################################################
PA_SUB_COORD <- pa_county[,c(1:3)]
coordinates(PA_SUB_COORD ) <- ~long+lat

#THIS SPLITS UP THE COORDINATES BY THE COUNTIES 
PA_SUB_COORD_LIST <- split(PA_SUB_COORD, PA_SUB_COORD$group)

###THIS MAKES EACH OF THE PA_SUB_COORD_LIST A POLYGON SHAPE
ps <- lapply(PA_SUB_COORD_LIST, Polygon)
###GIVES A NAME TO EACH POLGYON 
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = names(PA_SUB_COORD_LIST)[i]  ))

###CHANGES EVERYTHING TO A SPATIAL POLYGON
SPAT<-
  SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84") )
# str(df)- MAKES EVERYTHING INTO A SPATIAL DATA FRAME INCLUDING THE SPATIAL INFORMATION 
###AS WELL AS THE DATA
SPDF <- SpatialPolygonsDataFrame(SPAT, 
                                 PA_SUB_SP_MERGED_CAST )
SPDF2 <- SpatialPolygonsDataFrame(SPAT, 
                                  PA_SUB_SP_GROUPEDMERGED_CAST )
###SPECIES
COOK <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF$cookei), 
                    f='random'),species='cookei')

DENT <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $dentatus), 
                    f='random'),species='dentatus')
PERS <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $persicus), 
                    f='random'),species='persicus')
VAR<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $variabilis), 
                    f='random'),species='variabilis')
AMERI<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $americanum), 
                    f='random'),species='americanum')
DISS <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $dissimile), 
                    f='random'),species='dissimile')
KELL <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $kelleyi), 
                    f='random'),species='kelleyi')
MAC <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $maculatum), 
                    f='random'),species='maculatum')
MARX<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $marxi), 
                  f='random'),species='marxi')
SANG<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $sanguineus), 
                    f='random'),species='sanguineus')
SCAP <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $scapularis), 
                    f='random'),species='scapularis')
TEX <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $texanus), 
                   f='random'),species='texanus')
LEP<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $leporispalustris), 
                   f='random'),species='leporispalustris')
ALB<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $albipictus), 
                   f='random'),species='albipictus')

ANG<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $angustus), 
                  f='random'),species='angustus')

AFFIN<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $affinis), 
                  f='random'),species='affinis')
AND<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $andersoni), 
                  f='random'),species='andersoni')

TRANS<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $transversale), 
                  f='random'),species='transversale')

COOL<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $cooleyi), 
                   f='random'),species='cooleyi')

MUR<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $muris), 
                   f='random'),species='muris')
CHORD<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $chordeilis), 
                                   f='random'),species='chordeilis')
LONG <- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $longirostre), 
                                     f='random'),species='longirostre')


CAJEN<- cbind.data.frame(dotsInPolys(SPDF , as.integer(SPDF $cajennense), 
                  f='random'),species='cajennense')
DATA<-rbind(COOK,DENT,PERS, VAR,AMERI,DISS,KELL,MAC,MARX,
      SANG,SCAP,TEX,LEP,ALB,ANG,AFFIN,AND,TRANS,
      COOL,MUR,CHORD,LONG,CAJEN)
TOPFIVE <- rbind(COOK, SCAP,SANG,VAR,AMERI)

###GROUPED 

COOK_2 <- cbind.data.frame(dotsInPolys(SPDF2, as.integer(SPDF2$cookei),
                                       f='random'),species='cookei')
IXOD_SP <- cbind.data.frame(dotsInPolys(SPDF2, as.integer(SPDF2$`Ixodidae species`),
                                        f='random'),species='Ixodidae species')
ALB_2 <- cbind.data.frame(dotsInPolys(SPDF2, as.integer(SPDF2$albipictus),
                                    f='random'),species='albipictus')
AMER_2 <- cbind.data.frame(dotsInPolys(SPDF2, as.integer(SPDF2$americanum),
                                    f='random'),species='americanum')
DERM_SP <- cbind.data.frame(dotsInPolys(SPDF2, as.integer(SPDF2$`Dermacentor species`),
                                     f='random'),species='Dermacentor species')
AMB_SP <- cbind.data.frame(dotsInPolys(SPDF2, 
                                       as.integer(SPDF2$`Ambloymma species`),
                                        f='random')
                                      ,species='Ambloymma species')
SOFT_SP <- cbind.data.frame(dotsInPolys(SPDF2, 
                                       as.integer(SPDF2$`Soft tick species`),
                                       f='random')
                           ,species='Soft tick species')

DENT_2 <- cbind.data.frame(dotsInPolys(SPDF2, 
                                        as.integer(SPDF2$dentatus),
                                        f='random')
                            ,species='dentatus')
SANG_2 <- cbind.data.frame(dotsInPolys(SPDF2, 
                                       as.integer(SPDF2$sanguineus),
                                       f='random')
                           ,species='sanguineus')
SCAP_2 <- cbind.data.frame(dotsInPolys(SPDF2, 
                                       as.integer(SPDF2$scapularis),
                                       f='random')
                           ,species='scapularis')

TEX_2 <- cbind.data.frame(dotsInPolys(SPDF2, 
                                       as.integer(SPDF2$texanus),
                                       f='random')
                           ,species='texanus')

VAR_2 <- cbind.data.frame(dotsInPolys(SPDF2, 
                                      as.integer(SPDF2$variabilis),
                                      f='random')
                          ,species='variabilis')

DATA<-rbind(COOK_2, IXOD_SP, ALB_2, AMER_2, DERM_SP, AMB_SP,
            SOFT_SP, DENT_2, SANG_2, SCAP_2, TEX_2,VAR_2)



b = 2.5
ggplot(pa_county, aes(x= long,y=lat,group=group))+
  geom_polygon(data=pa_county, 
               aes(long+0.08,lat-0.05), fill="grey50",alpha=0.6)+
  geom_polygon(data=pa_county, 
   aes(x= long,y=lat,group=group),color='#152d32',fill='#eae2d3',size=0.7)+
  coord_map()+
geom_point(data = VAR_2, aes(x=x, y=y,group=1, fill='D.variabilis'),
           size =b,shape = 21,color='black',alpha=0.8)+
geom_point(data = SCAP_2, aes(x=x, y=y,group=1,fill ='I.scapularis' ),size = b,
           shape = 21,color='black')+
geom_point(data = COOK_2, aes(x=x, y=y, group=1, fill = 'I.cookei'),size =b, shape = 21,
           color='black')+
geom_point(data = SANG_2, aes(x=x, y=y, group=1,fill = 'R.sanguineus'),size =b, shape = 21, 
           color='black')+
geom_point(data = IXOD_SP, aes(x=x, y=y ,group=1, fill = 'Other Ixodidae species'),
           size= b,shape = 21, color='black')+
geom_point(data = AMER_2, aes(x=x, y=y, group=1, fill ='A.americanum'),
           size =b,shape = 21,  color='black')+
geom_point(data=DENT_2, aes(x=x, y=y , group=1,fill = 'I.dentatus'), 
           size = b,shape = 21,
           color='black')+
geom_point(data=ALB_2, aes(x=x, y=y , group =1, fill = 'D.albipictus'),
           size = b,shape = 21,
           color='black')+
geom_point(data = TEX_2,aes(x=x, y=y, group=1, fill = 'I.texanus'),
           size = b,shape = 21, color='black')+
geom_point(data =SOFT_SP, aes(x= x, y=y, group =1, fill = 'Soft ticks'),
           size = b,shape = 21, color='black')+
geom_point(data =AMB_SP, aes(x =x, y=y , group =1, fill = 'Other Amblyomma species'),
           size =b ,shape = 21, color='black')+
  geom_point(data =AMB_SP, aes(x =x, y=y , group =1, fill = 'Other Dermacentor species'),
             size =b ,shape = 21, color='black')+
  theme_void()+  theme(legend.position = "bottom",
        panel.background = element_rect(fill = NA, colour = "#cccccc"))+
  scale_fill_manual(values=COLOR, name = 'Species')+
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Tick submissions across Pennsylvania (1900-2017)")+
  theme(text = element_text(family = "Arial Narrow", size = 12),
        plot.title = element_text(size = 20, face = "bold"),

        legend.text = element_text(size = 12))


COLOR = c('D.variabilis'='#4D4D4D','I.scapularis'='#F15854','I.cookei'='#5DA5DA',
         'R.sanguineus'='#FAA43A', 'Other Ixodidae species'='#B276B2',
         'A.americanum'='#60BD68','I.dentatus'='#44B3C2',
         'D.albipictus'='#CCCC00','I.texanus'='#1b4b10','Soft ticks'='#A97D5D',
         'Other Amblyomma species'=	'#ccffee' , 
         'Other Dermacentor species'='#fff0f5')

           