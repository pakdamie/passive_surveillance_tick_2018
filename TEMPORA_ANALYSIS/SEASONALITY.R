
###PACKAGES REQUIRED
library(ggridges)
library(ggplot2)
library(viridis)
library(forcats)
library(plyr)
library(tidyr)

###ALL SUBMISSIOn

PA_SUB$Date <- as.Date(PA_SUB$Date,
                                  format = '%m/%d/%Y')
PA_SUB$month <- as.numeric(format(PA_SUB$Date, '%m'))

PA_SUB_2 <- subset(PA_SUB,
                   PA_SUB$month != 'NA')

PA_SUB_AGG<- aggregate(PA_SUB_2$Individuals, by=list(PA_SUB_2$month),'sum')
PA_SUB_AGG$prop <- PA_SUB_AGG$x/sum(PA_SUB_AGG$x)

ggplot(PA_SUB_AGG, aes(x= as.factor(Group.1), fill=as.factor(Group.1), y= prop))+
  geom_bar(stat= 'identity',color='black')+  
  scale_fill_viridis(discrete=TRUE,guide=FALSE)+
  theme_classic()+ylab("Proportion")+
  scale_x_discrete(name='Month',
                    labels= c("Jan","Feb","Mar","Apr","May",
                                "Jun","Jul","Aug","Sept","Oct","Nov",
                                "Dec"))+
ggtitle("Monthly proportions of ticks submitted across 1900 to 2017")+
scale_y_continuous(expand = c(0, 0))
#########################
###NOW ACROSS DECADES###
########################
PA_2_SPLIT <- split(PA_SUB, PA_SUB$Decade)

MONTHLY_PROP <- NULL
for (i in seq(1,length(PA_2_SPLIT))){
  TMP <- PA_2_SPLIT[[i]]
  TMP_AGG <- aggregate(TMP$Individuals, by=list(TMP$month),'sum' )
  TMP_AGG$prop <-(TMP_AGG$x)/sum(TMP_AGG$x)
  TMP_AGG$Decade <- unique(TMP$Decade)
  MONTHLY_PROP[[i]] <-  TMP_AGG
}
BY_DECADES_MONTH<-na.omit(do.call(rbind,MONTHLY_PROP))

ggplot(BY_DECADES_MONTH, aes(x= as.factor(Group.1), y = prop, fill=Group.1))+
  geom_bar(stat='identity',color='black',width =1)+
  scale_fill_viridis(guide=FALSE)+
facet_wrap(~Decade)+theme_bw()+
  ylab("Proportions")+
  scale_x_discrete(name='Month',
                   labels= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+
      theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())+
      ggtitle("Monthly proportions of ticks submitted by decades (1900-2017)")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) 
############################################################################
#The five major tick species: scapularis, cookei, americanum, sangineus,####
#and variabilis                                                          ###
############################################################################
SCAPULARIS_SEASON <- subset(PA_SUB, PA_SUB$Species == 'scapularis')

SCAP_SUB<-cbind.data.frame(Ind = SCAPULARIS_SEASON$Individuals,month= SCAPULARIS_SEASON$month)

SCAP_SUB_AGG <- aggregate(SCAP_SUB$Ind, list(SCAP_SUB$month),'sum')


SCAP_SUB_AGG $prop <- SCAP_SUB_AGG$x/sum(SCAP_SUB_AGG$x)
colnames(SCAP_SUB_AGG) <- c("Month",'Inds','Prop')
###SCAPULARIS PROPORTION
SCAPULARIS_SUB_COUNT_G<-ggplot(SCAP_SUB_AGG, aes(x= as.factor(Month),y = Prop,
               fill=as.factor(Month)))+geom_bar(stat='identity',color='black')+
  
  scale_fill_viridis(discrete=TRUE,guide=FALSE)+theme_classic()+
  ylab("Proportions")+
  scale_x_discrete(name='Month',
                   labels= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+ggtitle("Ixodes scapularis ")+ylim(0,0.5)

################################################################################
#####################SEASONALITY OF LIFE_STAGES ################################
#################################################################################
LIFE_STAG_SCAP <- SCAPULARIS_SEASON[, c(13,14,15,16,17,18,19,20)]
LIFE_STAG_SCAP$ADULT <- LIFE_STAG_SCAP$Adult_FEM + LIFE_STAG_SCAP$Adult_Male


###NYMPHS
NYMPH_SCAP_DAT<- cbind.data.frame(month = LIFE_STAG_SCAP$month,sub= LIFE_STAG_SCAP$Nymph)

NYMPH_SCAP_DAT<-na.omit(NYMPH_SCAP_DAT[NYMPH_SCAP_DAT$sub!=0,])
NYMPH_SCAP_REP <- rep(NYMPH_SCAP_DAT$month, NYMPH_SCAP_DAT$sub)
NYMPH_SCAP_COUNT <- data.frame(table(NYMPH_SCAP_REP))
NYMPH_SCAP_COUNT$prop <- NYMPH_SCAP_COUNT$Freq/sum(NYMPH_SCAP_COUNT$Freq)
NYMPH_SCAP_COUNT$id <- 'Nymph'
colnames(NYMPH_SCAP_COUNT)[1] <-'x'
VEC<- data.frame(x=as.factor(seq(1,12)))
NYMPH_SCAP_COUNT_FULL<-left_join(VEC,NYMPH_SCAP_COUNT,by='x')
NYMPH_SCAP_COUNT_FULL$Freq[is.na(NYMPH_SCAP_COUNT_FULL$Freq)==TRUE]<- 0
NYMPH_SCAP_COUNT_FULL$ prop[is.na(NYMPH_SCAP_COUNT_FULL$ prop)==TRUE]<- 0
NYMPH_SCAP_COUNT_FULL$ id[is.na(NYMPH_SCAP_COUNT_FULL$ id)==TRUE]<- 'Nymph'

###LARVAE
LARV_SCAP_DAT <- cbind.data.frame(month = LIFE_STAG_SCAP$month, sub = LIFE_STAG_SCAP$Larva)
LARV_SCAP_DAT<-na.omit(LARV_SCAP_DAT[LARV_SCAP_DAT$sub!=0,])
LARV_SCAP_REP <- rep(LARV_SCAP_DAT$month, LARV_SCAP_DAT$sub)
LARV_SCAP_COUNT <- data.frame(table(LARV_SCAP_REP))
LARV_SCAP_COUNT$prop <- LARV_SCAP_COUNT$Freq/sum(LARV_SCAP_COUNT$Freq)
LARV_SCAP_COUNT$id <- 'Larvae'
colnames(LARV_SCAP_COUNT)[1] <-'x'
LARV_SCAP_COUNT_FULL<-left_join(VEC,LARV_SCAP_COUNT,by='x')
LARV_SCAP_COUNT_FULL$Freq[is.na(LARV_SCAP_COUNT_FULL$Freq)==TRUE]<- 0
LARV_SCAP_COUNT_FULL$ prop[is.na(LARV_SCAP_COUNT_FULL$ prop)==TRUE]<- 0
LARV_SCAP_COUNT_FULL$ id[is.na(LARV_SCAP_COUNT_FULL$ id)==TRUE]<- 'Larvae'

###LARVAE
###ADULT
ADULT_SCAP_DAT <- cbind.data.frame(month = LIFE_STAG_SCAP$month, 
                                   sub = LIFE_STAG_SCAP$ADULT)
ADULT_SCAP_DAT<-na.omit(ADULT_SCAP_DAT[ADULT_SCAP_DAT$sub!=0,])
ADULT_SCAP_REP <- rep(ADULT_SCAP_DAT$month, ADULT_SCAP_DAT$sub)
ADULT_SCAP_COUNT <- data.frame(table(ADULT_SCAP_REP))
ADULT_SCAP_COUNT$prop <- ADULT_SCAP_COUNT$Freq/sum(ADULT_SCAP_COUNT$Freq)
ADULT_SCAP_COUNT$id <- 'Adult'
colnames(ADULT_SCAP_COUNT)[1] <-'x'

ADULT_SCAP_COUNT<- data.frame(complete(ADULT_SCAP_COUNT, 
                                      x, fill = list(freq=0, prop = 0,id='Adult')))
ADULT_SCAP_COUNT$x <- as.factor(ADULT_SCAP_COUNT$x)
ADULT_SCAP_COUNT_FULL<-left_join(VEC,ADULT_SCAP_COUNT,by='x')
ADULT_SCAP_COUNT_FULL$Freq[is.na(ADULT_SCAP_COUNT_FULL$Freq)==TRUE]<- 0
ADULT_SCAP_COUNT_FULL$ prop[is.na(ADULT_SCAP_COUNT_FULL$ prop)==TRUE]<- 0
ADULT_SCAP_COUNT_FULL$ id[is.na(ADULT_SCAP_COUNT_FULL$ id)==TRUE]<- 'Adult'


SCAP_SEASON <- rbind.data.frame(NYMPH_SCAP_COUNT_FULL, LARV_SCAP_COUNT_FULL,
                                ADULT_SCAP_COUNT_FULL)
SCAP_SEASON$x <- as.numeric(SCAP_SEASON$x)
SCAP_SEASON$id= factor(SCAP_SEASON$id, levels=c('Nymph', 'Larvae','Adult'))


SCAP_GGPLOT <-ggplot(SCAP_SEASON,aes(x= x, y= prop, fill=x))+scale_fill_viridis(guide=FALSE)+
  geom_bar(color='black',
          stat= 'identity')+ scale_x_discrete(
                   limits= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+theme_bw()+facet_grid(id~.,switch = 'both')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Month')+ylab('Proportion')+ggtitle("Ixodes scapularis")+
  ylim(0,1)


##############################################
##############COOOKEI ########################
###########################################
COOKEI_SEASON <- subset(PA_SUB, PA_SUB$Species == 'cookei')

COOKEI_SUB<-cbind.data.frame(Ind= COOKEI_SEASON$Individuals,
                             month= COOKEI_SEASON$month)

COOK_SUB_AGG <- aggregate(COOKEI_SUB$Ind, list(COOKEI_SUB$month),'sum')


COOK_SUB_AGG $prop <- COOK_SUB_AGG $x/sum(COOK_SUB_AGG $x)
colnames(COOK_SUB_AGG ) <- c("Month",'Inds','Prop')
###SCAPULARIS PROPORTION
COOKEI_SUB_COUNT_G<- ggplot(COOK_SUB_AGG, aes(x= as.factor(Month),y = Prop,
                           fill=as.factor(Month)))+geom_bar(stat='identity',color='black')+
  
  scale_fill_viridis(discrete=TRUE,guide=FALSE)+theme_classic()+
  ylab("Proportions")+
  scale_x_discrete(name='Month',
                   labels= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+ggtitle("Ixodes cookei")+ylim(0,0.5)

#################################################
#################################################
#########BY DECADES ###############################
#########################################

COOKEI_SEASON_DEC <- split(COOKEI_SEASON, COOKEI_SEASON$Decade)

DEC_SEASON_COOK<- NULL
for (i in seq(1, length(COOKEI_SEASON_DEC))){
  TMP <- COOKEI_SEASON_DEC[[i]]
  TMP_REP <-na.omit(rep(  TMP $month,   TMP $sub))
  TMP_REP_COUNT<- data.frame(FREQ= count( TMP_REP))
  TMP_REP_COUNT$Prop <- TMP_REP_COUNT$FREQ.freq/(sum(TMP_REP_COUNT$FREQ.freq))
  TMP_REP_COUNT$Decade <- unique(TMP$Decade)
  DEC_SEASON_COOK[[i]]<- TMP_REP_COUNT
}

DEC_SEASON_COOK_FINAL <- do.call(rbind, DEC_SEASON_COOK)

ggplot(subset(DEC_SEASON_COOK_FINAL,DEC_SEASON_COOK_FINAL$Decade
              == '1980-1990'|
                DEC_SEASON_COOK_FINAL$Decade
             == '1990-2000'| 
                DEC_SEASON_COOK_FINAL$Decade
              == '2000-2010'), 
       aes(x=FREQ.x, y= Prop))+
  geom_bar(stat='identity',aes(fill=FREQ.x),color='black')+
  facet_grid(Decade~.,switch='both')+
  scale_fill_viridis(guide=FALSE)+
  scale_x_discrete(name='Month',
                   limits= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#####################SEASONALITY OF LIFE_STAGES ################################

LIFE_STAG_COOK <- COOKEI_SEASON[, c(13,14,15,16,17,18,19)]
LIFE_STAG_COOK$ADULT <- LIFE_STAG_COOK$Adult_FEM + LIFE_STAG_COOK$Adult_Male


###NYMPHS
NYMPH_COOK_DAT<- cbind.data.frame(month = LIFE_STAG_COOK$month,
                                  sub= LIFE_STAG_COOK$Nymph)

NYMPH_COOK_DAT<-na.omit(NYMPH_COOK_DAT[NYMPH_COOK_DAT$sub!=0,])
NYMPH_COOK_REP <- rep(NYMPH_COOK_DAT$month, NYMPH_COOK_DAT$sub)
NYMPH_COOK_COUNT <- data.frame(table(NYMPH_COOK_REP))
colnames(NYMPH_COOK_COUNT)[1]<-'x'
NYMPH_COOK_COUNT$prop <- NYMPH_COOK_COUNT$Freq/sum(NYMPH_COOK_COUNT$Freq)
NYMPH_COOK_COUNT$id <- 'Nymph'
NYMPH_COOK_COUNT$x <- as.factor(NYMPH_COOK_COUNT$x)
NYMPH_COOK_COUNT_FULL<-left_join(VEC,NYMPH_COOK_COUNT,by='x')
NYMPH_COOK_COUNT_FULL$Freq[is.na(NYMPH_COOK_COUNT_FULL$Freq)==TRUE]<- 0
NYMPH_COOK_COUNT_FULL$ prop[is.na(NYMPH_COOK_COUNT_FULL$ prop)==TRUE]<- 0
NYMPH_COOK_COUNT_FULL$ id[is.na(NYMPH_COOK_COUNT_FULL$ id)==TRUE]<- 'Nymph'

###LARVAE
LARV_COOK_DAT <- cbind.data.frame(month = LIFE_STAG_COOK$month, sub = LIFE_STAG_COOK$Larva)
LARV_COOK_DAT<-na.omit(LARV_COOK_DAT[LARV_COOK_DAT$sub!=0,])
LARV_COOK_REP <- rep(LARV_COOK_DAT$month, LARV_COOK_DAT$sub)
LARV_COOK_COUNT <- data.frame(table(LARV_COOK_REP))
colnames(LARV_COOK_COUNT)[1]<- 'x'
LARV_COOK_COUNT$prop <- LARV_COOK_COUNT$Freq/sum(LARV_COOK_COUNT$Freq)
LARV_COOK_COUNT$id <- 'Larvae'
LARV_COOK_COUNT_FULL<-left_join(VEC,LARV_COOK_COUNT,by='x')
LARV_COOK_COUNT_FULL$Freq[is.na(LARV_COOK_COUNT_FULL$Freq)==TRUE]<- 0
LARV_COOK_COUNT_FULL$ prop[is.na(LARV_COOK_COUNT_FULL$ prop)==TRUE]<- 0
LARV_COOK_COUNT_FULL$ id[is.na(LARV_COOK_COUNT_FULL$ id)==TRUE]<- 'Larvae'

###LARVAE
###ADULT
ADULT_COOK_DAT <- cbind.data.frame(month = LIFE_STAG_COOK$month, 
                                   sub = LIFE_STAG_COOK$ADULT)
ADULT_COOK_DAT<-na.omit(ADULT_COOK_DAT[ADULT_COOK_DAT$sub!=0,])
ADULT_COOK_REP <- rep(ADULT_COOK_DAT$month, ADULT_COOK_DAT$sub)
ADULT_COOK_COUNT <-data.frame(table(ADULT_COOK_REP))
colnames(ADULT_COOK_COUNT)[1]<-'x'
ADULT_COOK_COUNT$prop <- ADULT_COOK_COUNT$Freq/sum(ADULT_COOK_COUNT$Freq)
ADULT_COOK_COUNT$id <- 'Adult'
ADULT_COOK_COUNT<- data.frame(complete(ADULT_COOK_COUNT, 
                                       x, fill = list(freq=0, prop = 0,id='Adult')))
ADULT_COOK_COUNT$x <- as.factor(ADULT_COOK_COUNT$x)
ADULT_COOK_COUNT_FULL<-left_join(VEC,ADULT_COOK_COUNT,by='x')
ADULT_COOK_COUNT_FULL$Freq[is.na(ADULT_COOK_COUNT_FULL$Freq)==TRUE]<- 0
ADULT_COOK_COUNT_FULL$ prop[is.na(ADULT_COOK_COUNT_FULL$ prop)==TRUE]<- 0
ADULT_COOK_COUNT_FULL$ id[is.na(ADULT_COOK_COUNT_FULL$ id)==TRUE]<- 'Adult'


COOK_SEASON <- rbind.data.frame(NYMPH_COOK_COUNT_FULL, LARV_COOK_COUNT_FULL,
                                ADULT_COOK_COUNT_FULL)
COOK_SEASON$x <- as.numeric(COOK_SEASON$x)
COOK_SEASON$id= factor(COOK_SEASON$id, levels=c('Nymph', 'Larvae','Adult'))

COOK_GGPLOT <- ggplot(COOK_SEASON,aes(x= x, y= prop, fill=x))+scale_fill_viridis(guide=FALSE)+
  geom_bar(color='black',
           stat= 'identity')+ scale_x_discrete(
             limits= c("Jan","Feb","Mar","Apr","May",
                       "Jun","Jul","Aug","Sept","Oct","Nov",
                       "Dec"))+theme_bw()+facet_grid(id~.,switch = 'both')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Month')+ylab('')+ylim(0,1)+ggtitle("Ixodes cookei")


##############################################
##############VARIABILIS########################
###########################################
VARIABILIS_SEASON <- subset(PA_SUB, PA_SUB$Species == 'variabilis')

VARI_SUB<-cbind.data.frame(Ind = VARIABILIS_SEASON $Individuals,
                             month= VARIABILIS_SEASON$month)

FULL_DATE <- data.frame(Group.1 =seq(1,12))
VARI_SUB_AGG <- aggregate(VARI_SUB$Ind, list(VARI_SUB$month),'sum')
VARI_SUB_AGG <- left_join(FULL_DATE, VARI_SUB_AGG)
VARI_SUB_AGG [is.na(VARI_SUB_AGG)==TRUE]<-0
VARI_SUB_AGG$prop <- VARI_SUB_AGG$x/sum(VARI_SUB_AGG$x)
colnames(VARI_SUB_AGG) <- c("Month",'Inds','Prop')
###SCAPULARIS PROPORTION
VARI_SUB_COUNT_G<- ggplot(VARI_SUB_AGG, aes(x= as.factor(Month),y = Prop,
                                  fill=as.factor(Month)))+geom_bar(stat='identity',color='black')+
  
  scale_fill_viridis(discrete=TRUE,guide=FALSE)+theme_classic()+
  ylab("Proportions")+
  scale_x_discrete(name='Month',
                   labels= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+ggtitle("Dermacentor variabilis")+ylim(0,0.5)

#################################################
#################################################
#########BY DECADES ###############################
#########################################

#####################SEASONALITY OF LIFE_STAGES ################################

LIFE_STAG_VARI <- VARIABILIS_SEASON [, c(13,14,15,16,17,18,19)]
LIFE_STAG_VARI$ADULT <- LIFE_STAG_VARI$Adult_FEM + LIFE_STAG_VARI$Adult_Male


###NYMPHS
NYMPH_VARI_DAT<- cbind.data.frame(month = LIFE_STAG_VARI$month,
                                  sub= LIFE_STAG_VARI$Nymph)

NYMPH_VARI_DAT<-na.omit(NYMPH_VARI_DAT[NYMPH_VARI_DAT$sub!=0,])
NYMPH_VARI_REP <- rep(NYMPH_VARI_DAT$month, NYMPH_VARI_DAT$sub)
NYMPH_VARI_COUNT <- data.frame(table(NYMPH_VARI_REP))
colnames(NYMPH_VARI_COUNT)[1]<-'x'
NYMPH_VARI_COUNT$prop <- NYMPH_VARI_COUNT$Freq/sum(NYMPH_VARI_COUNT$Freq)
NYMPH_VARI_COUNT$id <- 'Nymph'
NYMPH_VARI_COUNT$x <- as.factor(NYMPH_VARI_COUNT$x)
NYMPH_VARI_COUNT_FULL<-left_join(VEC,NYMPH_VARI_COUNT,by='x')
NYMPH_VARI_COUNT_FULL$Freq[is.na(NYMPH_VARI_COUNT_FULL$Freq)==TRUE]<- 0
NYMPH_VARI_COUNT_FULL$ prop[is.na(NYMPH_VARI_COUNT_FULL$ prop)==TRUE]<- 0
NYMPH_VARI_COUNT_FULL$ id[is.na(NYMPH_VARI_COUNT_FULL$ id)==TRUE]<- 'Nymph'

###LARVAE
LARV_VARI_DAT <- cbind.data.frame(month = LIFE_STAG_VARI$month, sub = LIFE_STAG_VARI$Larva)
LARV_VARI_DAT<-na.omit(LARV_VARI_DAT[LARV_VARI_DAT$sub!=0,])
LARV_VARI_REP <- rep(LARV_VARI_DAT$month, LARV_VARI_DAT$sub)
LARV_VARI_COUNT <- data.frame(table(LARV_VARI_REP))
colnames(LARV_VARI_COUNT)[1]<- 'x'
LARV_VARI_COUNT$prop <- LARV_VARI_COUNT$Freq/sum(LARV_VARI_COUNT$Freq)
LARV_VARI_COUNT$id <- 'Larvae'
LARV_VARI_COUNT_FULL<-left_join(VEC,LARV_VARI_COUNT,by='x')
LARV_VARI_COUNT_FULL$Freq[is.na(LARV_VARI_COUNT_FULL$Freq)==TRUE]<- 0
LARV_VARI_COUNT_FULL$ prop[is.na(LARV_VARI_COUNT_FULL$ prop)==TRUE]<- 0
LARV_VARI_COUNT_FULL$ id[is.na(LARV_VARI_COUNT_FULL$ id)==TRUE]<- 'Larvae'

###LARVAE
###ADULT
ADULT_VARI_DAT <- cbind.data.frame(month = LIFE_STAG_VARI$month, 
                                   sub = LIFE_STAG_VARI$ADULT)
ADULT_VARI_DAT<-na.omit(ADULT_VARI_DAT[ADULT_VARI_DAT$sub!=0,])
ADULT_VARI_REP <- rep(ADULT_VARI_DAT$month, ADULT_VARI_DAT$sub)
ADULT_VARI_COUNT <-data.frame(table(ADULT_VARI_REP))
colnames(ADULT_VARI_COUNT)[1]<-'x'
ADULT_VARI_COUNT$prop <- ADULT_VARI_COUNT$Freq/sum(ADULT_VARI_COUNT$Freq)
ADULT_VARI_COUNT$id <- 'Adult'
ADULT_VARI_COUNT<- data.frame(complete(ADULT_VARI_COUNT, 
                                       x, fill = list(freq=0, prop = 0,id='Adult')))
ADULT_VARI_COUNT$x <- as.factor(ADULT_VARI_COUNT$x)
ADULT_VARI_COUNT_FULL<-left_join(VEC,ADULT_VARI_COUNT,by='x')
ADULT_VARI_COUNT_FULL$Freq[is.na(ADULT_VARI_COUNT_FULL$Freq)==TRUE]<- 0
ADULT_VARI_COUNT_FULL$ prop[is.na(ADULT_VARI_COUNT_FULL$ prop)==TRUE]<- 0
ADULT_VARI_COUNT_FULL$ id[is.na(ADULT_VARI_COUNT_FULL$ id)==TRUE]<- 'Adult'


VARI_SEASON <- rbind.data.frame(NYMPH_VARI_COUNT_FULL, LARV_VARI_COUNT_FULL,
                                ADULT_VARI_COUNT_FULL)
VARI_SEASON$x <- as.numeric(VARI_SEASON$x)
VARI_SEASON$id= factor(VARI_SEASON$id, levels=c('Nymph', 'Larvae','Adult'))


VARI_GGPLOT <- ggplot(VARI_SEASON,aes(x= x, y= prop, fill=x))+scale_fill_viridis(guide=FALSE)+
  geom_bar(color='black',
           stat= 'identity')+ scale_x_discrete(
             limits= c("Jan","Feb","Mar","Apr","May",
                       "Jun","Jul","Aug","Sept","Oct","Nov",
                       "Dec"))+theme_bw()+facet_grid(id~.,switch = 'both')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Month')+ylab('')+ggtitle("Dermacentor variabilis")+ylim(0,1)


##############################################
##############AMERICANUM########################
###########################################
AMERICANUM_SEASON <- subset(PA_SUB, PA_SUB$Species == 'americanum')

AMERI_SUB<-cbind.data.frame(Ind = AMERICANUM_SEASON $Individuals,
                           month=AMERICANUM_SEASON$month)
AMERI_SUB_AGG <- aggregate(AMERI_SUB$Ind, by=list(AMERI_SUB$month),'sum')
AMERI_SUB_AGG <- left_join(FULL_DATE, AMERI_SUB_AGG)
AMERI_SUB_AGG [is.na(AMERI_SUB_AGG)==TRUE]<-0


AMERI_SUB_AGG$prop <- AMERI_SUB_AGG$x/sum(AMERI_SUB_AGG$x)
colnames(AMERI_SUB_AGG) <- c("Month",'Freq','Prop')


###SCAPULARIS PROPORTION
AMERI_SUB_COUNT_G<- ggplot(AMERI_SUB_AGG, aes(x= as.factor(Month),y = Prop,
                                              fill=as.factor(Month)))+geom_bar(stat='identity',color='black')+
  
  scale_fill_viridis(discrete=TRUE,guide=FALSE)+theme_classic()+
  ylab("Proportions")+
  scale_x_discrete(name='Month',
                   labels= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+ggtitle("Amblyomma americanum")+ylim(0,0.5)

#################################################
#################################################
#########BY DECADES ###############################
#########################################

#####################SEASONALITY OF LIFE_STAGES ################################

LIFE_STAG_AMERI <- AMERICANUM_SEASON [, c(13,14,15,16,17,18,19)]
LIFE_STAG_AMERI$ADULT <- LIFE_STAG_AMERI$Adult_FEM + LIFE_STAG_AMERI$Adult_Male


###NYMPHS
NYMPH_AMERI_DAT<- cbind.data.frame(month = LIFE_STAG_AMERI$month,
                                  sub= LIFE_STAG_AMERI$Nymph)

NYMPH_AMERI_DAT<-na.omit(NYMPH_AMERI_DAT[NYMPH_AMERI_DAT$sub!=0,])
NYMPH_AMERI_REP <- rep(NYMPH_AMERI_DAT$month, NYMPH_AMERI_DAT$sub)
NYMPH_AMERI_COUNT <- data.frame(table(NYMPH_AMERI_REP))
colnames(NYMPH_AMERI_COUNT)[1]<-'x'
NYMPH_AMERI_COUNT$prop <- NYMPH_AMERI_COUNT$Freq/sum(NYMPH_AMERI_COUNT$Freq)
NYMPH_AMERI_COUNT$id <- 'Nymph'
NYMPH_AMERI_COUNT$x <- as.factor(NYMPH_AMERI_COUNT$x)
NYMPH_AMERI_COUNT_FULL<-left_join(VEC,NYMPH_AMERI_COUNT,by='x')
NYMPH_AMERI_COUNT_FULL$Freq[is.na(NYMPH_AMERI_COUNT_FULL$Freq)==TRUE]<- 0
NYMPH_AMERI_COUNT_FULL$ prop[is.na(NYMPH_AMERI_COUNT_FULL$ prop)==TRUE]<- 0
NYMPH_AMERI_COUNT_FULL$ id[is.na(NYMPH_AMERI_COUNT_FULL$ id)==TRUE]<- 'Nymph'

###LARVAE
LARV_AMERI_DAT <- cbind.data.frame(month = LIFE_STAG_AMERI$month, sub = LIFE_STAG_AMERI$Larva)
LARV_AMERI_DAT<-na.omit(LARV_AMERI_DAT[LARV_AMERI_DAT$sub!=0,])
LARV_AMERI_REP <- rep(LARV_AMERI_DAT$month, LARV_AMERI_DAT$sub)
LARV_AMERI_COUNT <- data.frame(table(LARV_AMERI_REP))
colnames(LARV_AMERI_COUNT)[1]<- 'x'
LARV_AMERI_COUNT$prop <- LARV_AMERI_COUNT$Freq/sum(LARV_AMERI_COUNT$Freq)
LARV_AMERI_COUNT$id <- 'Larvae'
LARV_AMERI_COUNT_FULL<-left_join(VEC,LARV_AMERI_COUNT,by='x')
LARV_AMERI_COUNT_FULL$Freq[is.na(LARV_AMERI_COUNT_FULL$Freq)==TRUE]<- 0
LARV_AMERI_COUNT_FULL$ prop[is.na(LARV_AMERI_COUNT_FULL$ prop)==TRUE]<- 0
LARV_AMERI_COUNT_FULL$ id[is.na(LARV_AMERI_COUNT_FULL$ id)==TRUE]<- 'Larvae'

###LARVAE
###ADULT
ADULT_AMERI_DAT <- cbind.data.frame(month = LIFE_STAG_AMERI$month, 
                                   sub = LIFE_STAG_AMERI$ADULT)
ADULT_AMERI_DAT<-na.omit(ADULT_AMERI_DAT[ADULT_AMERI_DAT$sub!=0,])
ADULT_AMERI_REP <- rep(ADULT_AMERI_DAT$month, ADULT_AMERI_DAT$sub)
ADULT_AMERI_COUNT <-data.frame(table(ADULT_AMERI_REP))
colnames(ADULT_AMERI_COUNT)[1]<-'x'
ADULT_AMERI_COUNT$prop <- ADULT_AMERI_COUNT$Freq/sum(ADULT_AMERI_COUNT$Freq)
ADULT_AMERI_COUNT$id <- 'Adult'
ADULT_AMERI_COUNT<- data.frame(complete(ADULT_AMERI_COUNT, 
                                       x, fill = list(freq=0, prop = 0,id='Adult')))
ADULT_AMERI_COUNT$x <- as.factor(ADULT_AMERI_COUNT$x)
ADULT_AMERI_COUNT_FULL<-left_join(VEC,ADULT_AMERI_COUNT,by='x')
ADULT_AMERI_COUNT_FULL$Freq[is.na(ADULT_AMERI_COUNT_FULL$Freq)==TRUE]<- 0
ADULT_AMERI_COUNT_FULL$ prop[is.na(ADULT_AMERI_COUNT_FULL$ prop)==TRUE]<- 0
ADULT_AMERI_COUNT_FULL$ id[is.na(ADULT_AMERI_COUNT_FULL$ id)==TRUE]<- 'Adult'


AMERI_SEASON <- rbind.data.frame(NYMPH_AMERI_COUNT_FULL, LARV_AMERI_COUNT_FULL,
                                ADULT_AMERI_COUNT_FULL)
AMERI_SEASON$x <- as.numeric(AMERI_SEASON$x)
AMERI_SEASON$id= factor(AMERI_SEASON$id, levels=c('Nymph', 'Larvae','Adult'))


AMERICANUM_GGPLOT <-ggplot(AMERI_SEASON,aes(x= x, y= prop, fill=x))+scale_fill_viridis(guide=FALSE)+
  geom_bar(color='black',
           stat= 'identity')+ scale_x_discrete(
             limits= c("Jan","Feb","Mar","Apr","May",
                       "Jun","Jul","Aug","Sept","Oct","Nov",
                       "Dec"))+theme_bw()+facet_grid(id~.,switch = 'both')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Month')+ylab('')+ggtitle("Amblyomma americanum")+ylim(0,1)

###################################
####RHICEPHALUS SANGUINEUS #####################
#####################################


SANG_SEASON <- subset(PA_SUB, PA_SUB$Species == 'sanguineus')

SANG_SUB<-cbind.data.frame(Ind = SANG_SEASON $Individuals,
                            month=
                             SANG_SEASON$month)

SANG_SUB_AGG <- aggregate(SANG_SUB$Ind, by=list(SANG_SUB$month),'sum')
SANG_SUB_AGG <- left_join(FULL_DATE, SANG_SUB_AGG)
SANG_SUB_AGG [is.na(SANG_SUB_AGG)==TRUE]<-0
SANG_SUB_AGG$prop <- SANG_SUB_AGG$x/sum(SANG_SUB_AGG$x)
colnames(SANG_SUB_AGG) <- c("Month",'Freq','Prop')



SANG_SUB_COUNT_G<- ggplot(SANG_SUB_AGG, aes(x= as.factor(Month),y = Prop,
                                                fill=as.factor(Month)))+
                                  geom_bar(stat='identity',color='black')+
  
  scale_fill_viridis(discrete=TRUE,guide=FALSE)+theme_classic()+
  ylab("Proportions")+
  scale_x_discrete(name='Month',
                   labels= c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sept","Oct","Nov",
                             "Dec"))+ggtitle("Rhipicephalus sanguineus")+ylim(0,0.5)


grid.arrange(SCAPULARIS_SUB_COUNT_G,COOKEI_SUB_COUNT_G, VARI_SUB_COUNT_G,
             AMERI_SUB_COUNT_G, SANG_SUB_COUNT_G)


#################################################
#################################################
#########BY DECADES ###############################
#########################################

#####################SEASONALITY OF LIFE_STAGES ################################

LIFE_STAG_SANG <- SANG_SEASON [, c(13,14,15,16,17,18,19)]
LIFE_STAG_SANG$ADULT <- LIFE_STAG_SANG$Adult_FEM + LIFE_STAG_SANG$Adult_Male


###NYMPHS
NYMPH_SANG_DAT<- cbind.data.frame(month = LIFE_STAG_SANG$month,
                                   sub= LIFE_STAG_SANG$Nymph)

NYMPH_SANG_DAT<-na.omit(NYMPH_SANG_DAT[NYMPH_SANG_DAT$sub!=0,])
NYMPH_SANG_REP <- rep(NYMPH_SANG_DAT$month, NYMPH_SANG_DAT$sub)
NYMPH_SANG_COUNT <- data.frame(table(NYMPH_SANG_REP))
colnames(NYMPH_SANG_COUNT)[1]<-'x'
NYMPH_SANG_COUNT$prop <- NYMPH_SANG_COUNT$Freq/sum(NYMPH_SANG_COUNT$Freq)
NYMPH_SANG_COUNT$id <- 'Nymph'
NYMPH_SANG_COUNT$x <- as.factor(NYMPH_SANG_COUNT$x)
NYMPH_SANG_COUNT_FULL<-left_join(VEC,NYMPH_SANG_COUNT,by='x')
NYMPH_SANG_COUNT_FULL$Freq[is.na(NYMPH_SANG_COUNT_FULL$Freq)==TRUE]<- 0
NYMPH_SANG_COUNT_FULL$ prop[is.na(NYMPH_SANG_COUNT_FULL$ prop)==TRUE]<- 0
NYMPH_SANG_COUNT_FULL$ id[is.na(NYMPH_SANG_COUNT_FULL$ id)==TRUE]<- 'Nymph'

###LARVAE
LARV_SANG_DAT <- cbind.data.frame(month = LIFE_STAG_SANG$month, sub = LIFE_STAG_SANG$Larva)
LARV_SANG_DAT<-na.omit(LARV_SANG_DAT[LARV_SANG_DAT$sub!=0,])
LARV_SANG_REP <- rep(LARV_SANG_DAT$month, LARV_SANG_DAT$sub)
LARV_SANG_COUNT <- data.frame(table(LARV_SANG_REP))
colnames(LARV_SANG_COUNT)[1]<- 'x'
LARV_SANG_COUNT$prop <- LARV_SANG_COUNT$Freq/sum(LARV_SANG_COUNT$Freq)
LARV_SANG_COUNT$id <- 'Larvae'
LARV_SANG_COUNT_FULL<-left_join(VEC,LARV_SANG_COUNT,by='x')
LARV_SANG_COUNT_FULL$Freq[is.na(LARV_SANG_COUNT_FULL$Freq)==TRUE]<- 0
LARV_SANG_COUNT_FULL$ prop[is.na(LARV_SANG_COUNT_FULL$ prop)==TRUE]<- 0
LARV_SANG_COUNT_FULL$ id[is.na(LARV_SANG_COUNT_FULL$ id)==TRUE]<- 'Larvae'

###LARVAE
###ADULT
ADULT_SANG_DAT <- cbind.data.frame(month = LIFE_STAG_SANG$month, 
                                    sub = LIFE_STAG_SANG$ADULT)
ADULT_SANG_DAT<-na.omit(ADULT_SANG_DAT[ADULT_SANG_DAT$sub!=0,])
ADULT_SANG_REP <- rep(ADULT_SANG_DAT$month, ADULT_SANG_DAT$sub)
ADULT_SANG_COUNT <-data.frame(table(ADULT_SANG_REP))
colnames(ADULT_SANG_COUNT)[1]<-'x'
ADULT_SANG_COUNT$prop <- ADULT_SANG_COUNT$Freq/sum(ADULT_SANG_COUNT$Freq)
ADULT_SANG_COUNT$id <- 'Adult'
ADULT_SANG_COUNT<- data.frame(complete(ADULT_SANG_COUNT, 
                                        x, fill = list(freq=0, prop = 0,id='Adult')))
ADULT_SANG_COUNT$x <- as.factor(ADULT_SANG_COUNT$x)
ADULT_SANG_COUNT_FULL<-left_join(VEC,ADULT_SANG_COUNT,by='x')
ADULT_SANG_COUNT_FULL$Freq[is.na(ADULT_SANG_COUNT_FULL$Freq)==TRUE]<- 0
ADULT_SANG_COUNT_FULL$ prop[is.na(ADULT_SANG_COUNT_FULL$ prop)==TRUE]<- 0
ADULT_SANG_COUNT_FULL$ id[is.na(ADULT_SANG_COUNT_FULL$ id)==TRUE]<- 'Adult'


SANG_SEASON <- rbind.data.frame(NYMPH_SANG_COUNT_FULL, LARV_SANG_COUNT_FULL,
                                 ADULT_SANG_COUNT_FULL)
SANG_SEASON$x <- as.numeric(SANG_SEASON$x)
SANG_SEASON$id= factor(SANG_SEASON$id, levels=c('Nymph', 'Larvae','Adult'))


SANGCANUM_GGPLOT <-ggplot(SANG_SEASON,aes(x= x, y= prop, fill=x))+scale_fill_viridis(guide=FALSE)+
  geom_bar(color='black',
           stat= 'identity')+ scale_x_discrete(
             limits= c("Jan","Feb","Mar","Apr","May",
                       "Jun","Jul","Aug","Sept","Oct","Nov",
                       "Dec"))+theme_bw()+facet_grid(id~.,switch = 'both')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Month')+ylab('')+ggtitle("Rhipicephalus sanguineus")+ylim(0,1)

