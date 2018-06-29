library(dplyr)
library(plyr)
library(gridExtra)
library(cowplot)

###FIGURE 7###


###THE PROPORTION OF THE SUBMISSIONS 
###LOOKING AT THE TOTAL NUMBER OF Individuals
PA_SUB_AGG <-  aggregate(PA_SUB$Individuals, list(PA_SUB$Year),'sum')
colnames(PA_SUB_AGG) <- c("Year", "All_Individuals")
PA_FIVE <- subset(PA_SUB, PA_SUB$Species == 'variabilis'|
                  PA_SUB$Species == 'scapularis' |
                    PA_SUB$Species == 'americanum'|
                    PA_SUB$Species == 'sanguineus'|
                    PA_SUB$Species== 'cookei')


PA_FIVE_AGG <- aggregate(PA_FIVE$Individuals, list(PA_FIVE$Year,
                                                  PA_FIVE$Species),'sum')
colnames(PA_FIVE_AGG)  <- c("Year", "Species","Individuals")
PA_FIVE_AGG_PROP <- left_join(PA_FIVE_AGG, PA_SUB_AGG, by= c("Year"))

PA_FIVE_AGG_PROP$prop <- c(PA_FIVE_AGG_PROP$Individuals/ 
                             PA_FIVE_AGG_PROP$All_Individuals)


PA_FIVE_AGG_PROP$Species<- 
  revalue(PA_FIVE_AGG_PROP$Species, c("scapularis"="I.scapularis", 
                                   "variabilis"="D.variabilis",
                                   "americanum" = 'A.americanum',
                                   "sanguineus" = 'R.sanguineus',
                                   'cookei'= 'I.cookei'))

 ggplot(PA_FIVE_AGG_PROP, aes(x= Year, y= prop))+
  geom_bar(stat= 'identity', aes(fill= Species),color='black')+
   annotate("rect",
     fill="lightgrey",alpha=0.3, 
     xmin=1970.5,
     xmax=1986.5,
     ymin=0,
     ymax=1.10
   )+
  facet_grid(Species~.,switch='both')+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = seq(1900,2020,by=10))+
   scale_y_continuous(expand=c(0,0),limits=c(0,1.10))+
  guides(fill=FALSE)+ylab("Proportion")+
  ggtitle('Proportion of total submissions for the major tick species')+
scale_fill_viridis(discrete=TRUE)
   

 ###MANN-KENDALL TEST
 summary(MannKendall(subset(PA_FIVE_AGG_PROP, PA_FIVE_AGG_PROP$Species=='I.scapularis')$prop))
 
 MannKendall(subset(PA_FIVE_AGG_PROP, PA_FIVE_AGG_PROP$Species=='I.cookei')$prop)
 MannKendall(subset(PA_FIVE_AGG_PROP, PA_FIVE_AGG_PROP$Species=='D.variabilis')$prop)
 MannKendall(subset(PA_FIVE_AGG_PROP, PA_FIVE_AGG_PROP$Species=='A.americanum')$prop)
 MannKendall(subset(PA_FIVE_AGG_PROP, PA_FIVE_AGG_PROP$Species=='R.sanguineus')$prop)
 