library(dplyr)
library(SpatialEpi)
library(ggplot2)
library(viridis)
library(reshape2)

PA_HOST_DATA <-PA_SUB[,c(1, #County
                         3, #Year
                         5, #Host
                         6, #Host Species
                         7,8,9, #Host Taxon
                         10,11,17,18)]

###GETTING RID OF ANY DATA ENTRIES WHERE THE TICKS
### ARE NOT ATTACHED TO HOSTS OR No record

PA_HOST_DATA <- subset(PA_HOST_DATA, PA_HOST_DATA$Host != 
                        'not attached to host'&
                         PA_HOST_DATA$Host != 'NR'&
                       PA_HOST_DATA$Host != 'Not attached to host'&
                       PA_HOST_DATA$Host != 'Not attached'&
                         PA_HOST_DATA$Host != 'not attacked to host'&
                         PA_HOST_DATA$Host != 'No record')
unique(PA_HOST_DATA$Host_Taxon_1)

PA_HOST_DATA <- subset(PA_HOST_DATA, PA_HOST_DATA$Species != 'sp.')
###GETTING CENTROID DATA from the SpatialEpi file
data(pennLC)
PA_centroid <- pennLC$geo
colnames(PA_centroid) <- c("County","Long",'Lat')


### LAGOMORPHA

LAG <- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Lagomorpha')
sum(LAG$Individuals)
sum(LAG$submission)

LAG_TICK_CHORD <- aggregate(LAG$submission ,by=list(LAG$Species),'sum')
LAG_TICK_CHORD$ID <- 'rabbits'
##############################
#########Cervidae #############        

CERV <- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Cervidae')
sum(CERV$Individuals)
sum(CERV$submission)

CERV_TICK_CHORD <- aggregate(CERV$submission ,by=list(CERV$Species),'sum')
CERV_TICK_CHORD$ID<-"Deers"
#######Mustelidae######## 
MUST <- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Mustelidae')
sum(MUST$Individuals)
sum(MUST$submission)

MUST_TICK_CHORD <- aggregate(MUST$submission ,by=list(MUST$Species),'sum')
MUST_TICK_CHORD$ID <- "Ferrets"
#######RODENTIA######## 
ROD<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Rodentia')
sum(ROD$Individuals)
sum(ROD$submission)

ROD_TICK_CHORD <- aggregate(ROD$submission ,by=list(ROD$Species),'sum')
ROD_TICK_CHORD$ID <- "Rodents"
#######Bovidae####### 
BOV<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Bovidae')
sum(BOV$Individuals)
sum(BOV$submission)

BOV_TICK_CHORD <- aggregate(BOV$submission ,by=list(BOV$Species),'sum')
BOV_TICK_CHORD$ID<- "Cows"

#######Wild Canids ######## 
CAN<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Canidae')
sum(CAN$Individuals)
sum(CAN$submission)

CAN_TICK_CHORD <- aggregate(CAN$submission ,by=list(CAN$Species),'sum')
CAN_TICK_CHORD$ID <- "Fox"
#######Aves   ######## 
AVE<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Aves')
sum(AVE$Individuals)
sum(AVE$submission)
AVE_TICK_CHORD <- aggregate(AVE$submission ,by=list(AVE$Species),'sum')
AVE_TICK_CHORD$ID <- 'Birds'
#######CANISIS- DOMESTIC DOGS ######## 
CANIS<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Canis')

sum(CANIS$Individuals)
sum(CANIS$submission)
CANIS_TICK_CHORD <- aggregate(CANIS$submission ,by=list(CANIS$Species),'sum')
CANIS_TICK_CHORD$ID <- "Dog"

#######Marmota monax  OR 	Sciuridae ######## 
SCI<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Marmota monax')


sum(SCI$Individuals)
sum(SCI$submission)
SCI_TICK_CHORD <- aggregate(SCI$submission ,by=list(SCI$Species),'sum')
SCI_TICK_CHORD$ID <- 'Woodchuck'

############### 'Marsupialia'###############
MARS<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Marsupialia')

sum(MARS$Individuals)
sum(MARS$submission)
MARS_TICK_CHORD <- aggregate(MARS$submission ,by=list(MARS$Species),'sum')
MARS_TICK_CHORD$ID<-'opposum'

############### Chiroptera###############
CHI<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Chiroptera')

sum(CHI$Individuals)
sum(CHI$submission)
CHI_TICK_CHORD <- aggregate(CHI$submission ,by=list(CHI$Species),'sum')
CHI_TICK_CHORD$ID <- 'Bat'
############## Soricidae ###############
SOR<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Soricidae')

sum(SOR$Individuals)
sum(SOR$submission)
SOR_TICK_CHORD <- aggregate(SOR$submission ,by=list(SOR$Species),'sum')
SOR_TICK_CHORD$ID <- 'Shrew'
############## Mephitidae ###############
MEP<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Mephitidae')

sum(MEP$Individuals)
sum(MEP$submission)
MEP_TICK_CHORD <- aggregate(MEP$submission ,by=list(MEP$Species),'sum')
MEP_TICK_CHORD$ID <- "Skunks"
##############Reptilia###############
REP<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Reptilia')


sum(REP$Individuals)
sum(REP$submission)

REP_TICK_CHORD <- aggregate(REP$submission ,by=list(REP$Species),'sum')
REP_TICK_CHORD$ID <- "Reptiles"

########Procyonidae############
PRO<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Procyonidae')

sum(PRO$Individuals)
sum(PRO$submission)

PRO_TICK_CHORD <- aggregate(PRO$submission ,by=list(PRO$Species),'sum')
PRO_TICK_CHORD$ID <-'Raccoon'

########Equus############
EQU<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Equus')

sum(EQU$Individuals)
sum(EQU$submission)
EQU_TICK_CHORD <- aggregate(EQU$submission ,by=list(EQU$Species),'sum')
EQU_TICK_CHORD $ID <- "Horse"

########Felis############
FEL<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Felis')
                

sum(FEL$Individuals)
sum(FEL$submission)
FEL_TICK_CHORD <- aggregate(FEL$submission ,by=list(FEL$Species),'sum')
FEL_TICK_CHORD$ID <- "Cat"

#########Carnivora###########
CARN<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Carnivora')


sum(CARN$Individuals)
sum(CARN$submission)
CARN_TICK_CHORD <- aggregate(CARN$submission ,by=list(CARN$Species),'sum')
CARN_TICK_CHORD$ID <- 'Bears'

#########Human###########
HOMO<- subset(PA_HOST_DATA, PA_HOST_DATA$Host_Spec=='Homo sapiens'|
                    PA_HOST_DATA$Host_Spec=='Human')


sum(HOMO$Individuals)
sum(HOMO$submission)
HOMO_TICK_CHORD <- aggregate(HOMO$submission ,by=list(HOMO$Species),'sum')
HOMO_TICK_CHORD<-HOMO_TICK_CHORD[-c(1,13),]
HOMO_TICK_CHORD$ID <- "Humans"

#####################################################################
###Circilize plot                                               #####
#####################################################################
CIRC_ALL<-rbind.data.frame(MARS_TICK_CHORD, SCI_TICK_CHORD, LAG_TICK_CHORD,
                           CERV_TICK_CHORD, MUST_TICK_CHORD, ROD_TICK_CHORD,
                           CAN_TICK_CHORD, AVE_TICK_CHORD, CARN_TICK_CHORD,
                           HOMO_TICK_CHORD, EQU_TICK_CHORD,CANIS_TICK_CHORD,
                           FEL_TICK_CHORD,PRO_TICK_CHORD,CHI_TICK_CHORD,
                           SOR_TICK_CHORD,
                           BOV_TICK_CHORD,MEP_TICK_CHORD,REP_TICK_CHORD)

CIRC_ALL_DCAST<-dcast(CIRC_ALL, Group.1~ID,value.var=c('x'))
CIRC_ALL_DCAST$Group.1 <- as.character(CIRC_ALL_DCAST$Group.1)

CIRC_ALL_DCAST[is.na(CIRC_ALL_DCAST)==TRUE]<-0

###GROUPING ALL THE AMBLOYMMA TOGETHER
AMB_SPP <- subset(CIRC_ALL_DCAST,CIRC_ALL_DCAST$Group.1 == 'dissimile'|
                    CIRC_ALL_DCAST$Group.1 == 'longirostre'|
                    CIRC_ALL_DCAST$Group.1 == 'maculatum'|
                    CIRC_ALL_DCAST$Group.1 == 'transversale')

AMB_SPP[5, ] <- c('AMB_SPP',colSums(AMB_SPP[,2:20]))


#Dermacentor spp
DERM_SPP <- subset(CIRC_ALL_DCAST,CIRC_ALL_DCAST$Group.1 == 'albipictus'|
                     CIRC_ALL_DCAST$Group.1 == 'andersoni')

DERM_SPP[3, ] <- c('DERM_SPP',colSums(DERM_SPP[,2:20]))

#Ixodes spp
IXO_SPP <- subset(CIRC_ALL_DCAST, CIRC_ALL_DCAST$Group.1 =='muris'|
                    CIRC_ALL_DCAST$Group.1 =='texanus'|
                    CIRC_ALL_DCAST$Group.1 =='sp.'|
                    CIRC_ALL_DCAST$Group.1 ==''|
                    CIRC_ALL_DCAST$Group.1 == 'affinis'|
                    CIRC_ALL_DCAST$Group.1 =='angustus'|
                  CIRC_ALL_DCAST$Group.1 =='Ixodidae')

IXO_SPP[7,] <- c('SCAP_SPP', colSums(IXO_SPP[,2:20]))


#HAEMAPHYSALIS

HAEM_SPP <- subset(CIRC_ALL_DCAST, CIRC_ALL_DCAST$Group.1 =='leporispalustris'|
                     CIRC_ALL_DCAST$Group.1 =='chordeilis')

HAEM_SPP[3,] <- c('HAEM_SPP', colSums(HAEM_SPP[,2:20]))



#SOFT TICKS
SOFT_SPP <- subset(CIRC_ALL_DCAST, CIRC_ALL_DCAST$Group.1 == 'kelleyi'|
                     CIRC_ALL_DCAST$Group.1 == 'cooleyi'|
                     CIRC_ALL_DCAST$Group.1 == 'persicus')

SOFT_SPP[4,] <- c("SOFT_SPP", colSums(SOFT_SPP[,2:20]))
#######################################################################
################COMBINE EVERYTHING TOGETHER ##############################
##########################################################################

OTHER_TICKS <-rbind.data.frame(AMB_SPP[5,], HAEM_SPP[3,],
                               DERM_SPP[3,], IXO_SPP[7,],SOFT_SPP[4,])
for (i in seq(2,20)){
OTHER_TICKS[,i] <- as.numeric(OTHER_TICKS[,i])
}

OTHER_TICKS[6,]<- c("Other",colSums(OTHER_TICKS[,2:20]))

CIRCILIZE_2 <- 
  rbind.data.frame(CIRC_ALL_DCAST,OTHER_TICKS[6,])
rownames(CIRCILIZE_2)<- seq(1,nrow(CIRCILIZE_2))

CIRCILIZE_2<- CIRCILIZE_2[-c(1:2,4,5,7,9,10, 11,12,13,15,16,19,20,22,23),]

CIRCILIZE_PROP <- CIRCILIZE_2 
for (i in seq(2,20)){
  CIRCILIZE_PROP[,i] <- as.numeric(CIRCILIZE_PROP[,i])
}
CIRCILIZE_PROP$TOTAL <- rowSums(CIRCILIZE_PROP[,2:20])

CIRCILIZE_PROP[,2:20]<- CIRCILIZE_PROP[,2:20]/CIRCILIZE_PROP$TOTAL
ALL_DATA_MATRIX<- data.matrix(CIRCILIZE_PROP[,2:20])


EVERYONE_MAT_TICKS<- c(as.character(CIRCILIZE_PROP$Group.1))
EVERYONE_MAT_HOSTS <- c(as.character(colnames(CIRCILIZE_PROP[,2:20])))

dimnames(ALL_DATA_MATRIX) <- list(Tick = EVERYONE_MAT_TICKS,
                           Host =EVERYONE_MAT_HOSTS )


Species_name <- c('Amblyomma americanum', 
                  'Ixodes cookei', 'Ixodes dentatus',
                  'Ixodes marxi','Rhipicephalus sanguineus',
                  'Ixodes scapularis', 'Dermacentor variabilis',
                  'Other')
Host_name <- c('Ticks','Bats', 'Bears', 'Birds', 'Cats', 'Cows', 'Cervids', 'Dogs', 
               "Mustelids",'Wild Canids', 'Horses', 'Humans', 'Opossums',
               'Leporids',
               'Raccoons', 'Reptiles', 'Rodents','Shrews' ,'Skunks', 'Groundhogs')



CIRCILIZE_PROP$Group.1 <-  Species_name
colnames(CIRCILIZE_PROP) <- Host_name
CIRCILIZE_MAT <- data.matrix(CIRCILIZE_PROP[,2:20])


df = data.frame(from = rep(CIRCILIZE_PROP$Ticks,
                           times = ncol(CIRCILIZE_PROP[,2:20])),
                to = rep(colnames(CIRCILIZE_PROP[,2:20]), 
                                  each = nrow(CIRCILIZE_PROP)),
                value = as.vector(CIRCILIZE_MAT))
vec <- c(unique(as.character(df$from)),
                unique(as.character(df$to)))


TICK_SPECIES_ORDER <- rev(c('Amblyomma americanum', 'Dermacentor variabilis',
                        'Ixodes cookei', 'Ixodes dentatus',
                        'Ixodes marxi',  'Ixodes scapularis','Rhipicephalus sanguineus', 'Other'))

HOST_SPECIES_ORDER <- c('Bats','Bears', 'Birds',"Cervids","Mustelids", "Wild Canids",
                        "Opossums", "Leporids", "Raccoons",
                        "Rodents","Shrews", "Skunks", "Groundhogs",
                       "Cats","Cows","Dogs","Horses","Humans","Reptiles")

circos.par(start.degree = 68,gap.after = c(rep(3,18), 60,
                                           rep(3,7), 60))


chordDiagram(df,
             order = c( HOST_SPECIES_ORDER,TICK_SPECIES_ORDER),
             annotationTrack = c( "grid"),
             link.lwd = 0.08, link.lty = 1,
             link.border = "black",
              col = grid.col,transparency=0.1,
             grid.col=link_color,
      preAllocateTracks =list(track.height=0.1))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))},
              bg.border = NA) 
circos.clear()

col_mat = rand_color(length(CIRCILIZE_MAT), transparency = 0.5)
grid.col <- brewer.pal(8, "Spectral")

link_color <- c('Amblyomma americanum'='#d73027',
                'Dermacentor variabilis'='#d73027',
                'Ixodes cookei'='#d73027', 'Ixodes dentatus'='#d73027',
                'Ixodes marxi'='#d73027',  'Ixodes scapularis'='#d73027',
                'Rhipicephalus sanguineus'='#d73027', 'Other'='#d73027',
                'Bats'='#fee090',
                'Bears'='#fee090',
                'Birds'=	'#fee090',
                "Cervids"='#fee090',
                "Mustelids"='#fee090', 
                "Wild Canids"='#fee090',
                "Opossums"='#fee090', 
                "Leporids"='#fee090', 
                "Raccoons"='#fee090',
                "Reptiles"='#4575b4',
                "Rodents"='#fee090'
                ,"Shrews"='#fee090'
                , "Skunks"='#fee090'
                , "Groundhogs"='#fee090'
                , "Cats"='#4575b4'
                ,"Cows"='#4575b4'
                ,"Dogs"=	'#4575b4'
                ,"Horses"='#4575b4'
                ,"Humans"='#4575b4'
                
                )

###########################WILD#################################

WILD_CIRCILIZE  <- CIRCILIZE_2[,c(1,2:4, 7,9:10,13:15, 17:20)]
colnames(WILD_CIRCILIZE)[1]<- 'Ticks'
WILD_DATA_MATRIX<- data.matrix(WILD_CIRCILIZE[,2:14])


WILD_MAT_TICKS<- c(as.character(WILD_CIRCILIZE $Ticks))
WILD_MAT_HOSTS <- c(as.character(colnames(WILD_CIRCILIZE [,2:14])))

dimnames(WILD_DATA_MATRIX) <- list(Tick = WILD_MAT_TICKS,
                                  Host =WILD_MAT_HOSTS )


Species_name_W <- c('Amblyomma americanum', 
                  'Ixodes cookei', 'Ixodes dentatus',
                  'Ixodes marxi','Rhipicephalus sanguineus',
                  'Ixodes scapularis', 'Dermacentor variabilis',
                  'Other')
Host_name_W <- c('Ticks','Bats', 'Bears', 'Birds',  'Cervids', 
               "Mustelids",'Wild Canids', 'Opossums',
               'Leporids',
               'Raccoons', 'Rodents','Shrews' ,'Skunks', 'Groundhogs')



WILD_CIRCILIZE$Ticks <-  Species_name_W
colnames(WILD_CIRCILIZE) <- Host_name_W


df_WILD = data.frame(from = rep(WILD_CIRCILIZE $Ticks,
                           times = ncol(WILD_CIRCILIZE [,2:14])),
                to = rep(colnames(WILD_CIRCILIZE[,2:14]), 
                         each = nrow(WILD_CIRCILIZE)),
                value = as.vector(WILD_DATA_MATRIX))
vec_WILD <- c(unique(as.character(df_WILD$from)),
         unique(as.character(df_WILD$to)))


TICK_SPECIES_ORDER_W <- rev(c('Amblyomma americanum', 'Dermacentor variabilis',
                            'Ixodes cookei', 'Ixodes dentatus',
                            'Ixodes marxi',  'Ixodes scapularis','Rhipicephalus sanguineus', 'Other'))

HOST_SPECIES_ORDER_W <- c('Bats','Bears','Birds',"Cervids",
                        "Mustelids", "Wild Canids",
                        "Opossums", "Leporids", "Raccoons",
                        "Rodents","Shrews", "Skunks", "Groundhogs")

circos.par(start.degree = 69,gap.after = c(rep(3,12), 60,
                                           rep(3,6), 60))


chordDiagram(df_WILD,
             order = c( HOST_SPECIES_ORDER_W,TICK_SPECIES_ORDER_W),
             annotationTrack = c( "grid"),
             link.lwd = 0.08, link.lty = 1,
             link.border = "black",
             col = grid.col,transparency=0.1,
             grid.col=link_color,
             preAllocateTracks =list(track.height=0.4))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))},
  bg.border = NA) 
circos.clear()


###########################URBAN#################################

URBAN_CIRCILIZE  <- CIRCILIZE_2[,c(1,5:6, 8,11,12)]
colnames(URBAN_CIRCILIZE)[1] <- 'Ticks'
URB_DATA_MATRIX<- data.matrix(URBAN_CIRCILIZE[,2:6])


URB_MAT_TICKS<- c(as.character(URBAN_CIRCILIZE $Ticks))
URB_MAT_HOSTS <- c(as.character(colnames(URBAN_CIRCILIZE  [,2:6])))

dimnames(URB_DATA_MATRIX) <- list(Tick = URB_MAT_TICKS,
                                   Host =URB_MAT_HOSTS )


Species_name_U <- c('Amblyomma americanum', 
                    'Ixodes cookei', 'Ixodes dentatus',
                    'Ixodes marxi','Rhipicephalus sanguineus',
                    'Ixodes scapularis', 'Dermacentor variabilis',
                    'Other')
Host_name_U <- c('Ticks','Cats','Cows','Dogs','Horses','Humans')



URBAN_CIRCILIZE$Ticks <-  Species_name_U
colnames(URBAN_CIRCILIZE) <- Host_name_U


df_URB = data.frame(from = rep(URBAN_CIRCILIZE$Ticks,
                                times = ncol(URBAN_CIRCILIZE [,2:6])),
                     to = rep(colnames(URBAN_CIRCILIZE[,2:6]), 
                              each = nrow(URBAN_CIRCILIZE)),
                     value = as.vector(URB_DATA_MATRIX))
vec_URB <- c(unique(as.character(df_URB$from)),
              unique(as.character(df_URB$to)))


TICK_SPECIES_ORDER_U <- rev(c('Amblyomma americanum', 'Dermacentor variabilis',
                              'Ixodes cookei', 'Ixodes dentatus',
                              'Ixodes marxi',  'Ixodes scapularis','Rhipicephalus sanguineus', 'Other'))

HOST_SPECIES_ORDER_U <- c('Cats','Cows','Dogs','Horses','Humans')

circos.par(start.degree = 53,gap.after = c(rep(3,4), 90,
                                           rep(3,7), 90))


chordDiagram(df_URB,
             order = c( HOST_SPECIES_ORDER_U,TICK_SPECIES_ORDER_U),
             annotationTrack = c( "grid"),
             link.lwd = 0.08, link.lty = 1,
             link.border = "black",
             col = grid.col,transparency=0.1,
             grid.col=link_color,
             preAllocateTracks =list(track.height=0.4))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))},
  bg.border = NA) 
circos.clear()
