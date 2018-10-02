###WRANGLED script needs to be ran first for all the other scripts to work
### first. This ensures that the original data frame is fixed 
### SO RUN WRANGLED FIRST!


###Packages needed

library(ggplot2) 
library(maps)

#####################
### Data wrangling###
###########################################################
###This is the main data file that has from 1900 - 2016####
###########################################################
main.dat <- read.csv("main_dat_tick_FINAL.csv")

###################################################
###THIS IS THE MAP DATA- the spatial data that one#
###gets from the ggplot2                      #####
###################################################
map <- map_data("county", "pennsylvania") 
colnames(map)[6] <- 'County'



###########################################################
### Instead of using the whole data, we're only getting ###
###data that has Pennsylvania in it and cleaning it up ####
############################################################
SUB_DAT<- main.dat[,c(3:14,16,17,18,19,
                      20,21)]
colnames(SUB_DAT)<- c("County","State",
                      "Year","Date","Host",
                      "Host_Spec","Host_Taxon_1",
                      "Host_Taxon_2","Host_Taxon_3",
                      "Genus","Species","Tick_Taxon","Adult_FEM",
                      "Adult_Male", "Nymph", "Larva", 
                      "submission",'Individuals')
###CLEANING UP THE STATES
factor(SUB_DAT$State)
SUB_DAT$State[SUB_DAT$State == 'pA'] <- 'PA'
SUB_DAT$State[SUB_DAT$State == 'PA`'] <- 'PA'


###ONLY TAKING OUT DATA WHERE THE STATE IS IN PENNSYLVNAIA 

PA_SUB <- subset(SUB_DAT,SUB_DAT$State == "PA")

unique(factor(PA_SUB$County))
PA_SUB$County[PA_SUB$County == 'Huntington'] <- 'Huntingdon'
PA_SUB$County[PA_SUB$County == 'Schuykill'] <- 'Schuylkill'
PA_SUB$County[PA_SUB$County == 'Elk/ Cameron'] <- 'Elk'
PA_SUB$County[PA_SUB$County == 'Not Indicated'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'not listed'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'N/A'] <- 'No record'

PA_SUB$County[PA_SUB$County == 'Palmyra Township'] <- 'Wayne'
PA_SUB$County[PA_SUB$County == 'Chester or Virginia '] <- 'Chester'
PA_SUB$County[PA_SUB$County == 'Monroe or Pike'] <- 'Monroe'
PA_SUB$County[PA_SUB$County == 'Northhampton or Lehigh'] <- 'Northampton'
PA_SUB$County[PA_SUB$County == 'Northhampton'] <- 'Northampton'

PA_SUB$County[PA_SUB$County == 'Potter or York'] <- 'Potter'
PA_SUB$County[PA_SUB$County == 'Delaware\vMedia\v'] <- 'Delaware'
PA_SUB$County[PA_SUB$County == 'Cameron or Mckean'] <- 'Cameron'
PA_SUB$County[PA_SUB$County == 'Elk or Butler'] <- 'Elk'
PA_SUB$County[PA_SUB$County == 'Centre County'] <- 'Centre'
PA_SUB$County[PA_SUB$County == ' Indiana'] <- 'Indiana'
PA_SUB$County[PA_SUB$County == 'Indiana Co.'] <- 'Indiana'
PA_SUB$County[PA_SUB$County == 'Crawford or Venango'] <- 'Crawford'
PA_SUB$County[PA_SUB$County == 'Wayne County'] <- 'Wayne'
PA_SUB$County[PA_SUB$County == 'Warren County'] <- 'Warren'
PA_SUB$County[PA_SUB$County == 'Snyder?'] <- 'Snyder'
PA_SUB$County[PA_SUB$County == 'Snyder '] <- 'Snyder'

PA_SUB$County[PA_SUB$County == 'Curwensville'] <- 'Clearfield'
PA_SUB$County[PA_SUB$County == 'Near Ronova (Sinnema Honing State Park'] <- 'Clinton'
PA_SUB$County[PA_SUB$County == 'Northumberland ?'] <- 'Northumberland'
PA_SUB$County[PA_SUB$County == 'Northemberland'] <- 'Northumberland'
PA_SUB$County[PA_SUB$County == 'McKean'] <- 'Mckean'
PA_SUB$County[PA_SUB$County == 'Snyder.'] <- 'Snyder'
PA_SUB$County[PA_SUB$County == 'Ohio'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'Washington Township'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'Northid'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'Atlantic City'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'New Jersey'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'Canada'] <- 'No record'
PA_SUB$County[PA_SUB$County == 'Venengo'] <- 'Venango'
PA_SUB$County[PA_SUB$County == '<NA>' ] <- 'No record'
PA_SUB$County[PA_SUB$County == 'delaware ' ] <- 'delaware'

PA_SUB$County <- tolower(PA_SUB$County)
PA_SUB<- subset(PA_SUB, PA_SUB$County != "No record")
PA_SUB$Year <- as.numeric(as.character(PA_SUB$Year))
PA_SUB<- PA_SUB[order(PA_SUB$Year),] 
PA_SUB<-PA_SUB[!is.na(PA_SUB$Year),]

###Cleaning up the species 
unique(factor(PA_SUB$Species))
PA_SUB$Species <- factor(PA_SUB$Species,
                         levels=c(levels(PA_SUB$Species), 'leporispalustris'))
PA_SUB$Species <- factor(PA_SUB$Species,
                         levels=c(levels(PA_SUB$Species), 'chordeilis'))

PA_SUB$Species[PA_SUB$Species == 'leporis-palustris'] <- 'leporispalustris'

PA_SUB$Species[PA_SUB$Species == 'chordeilis\v'] <- 'chordeilis'
PA_SUB$Species[PA_SUB$Species == 'scapularius'] <- 'scapularis'
PA_SUB$Species[PA_SUB$Species == 'Kelleyi'] <- 'kelleyi'

#Creating the decade
###I want decades that start from 1900 and go to 2020, 
v <- seq(1900,2020,10) #

year1<- PA_SUB$Year
indx<-findInterval(year1,seq(1900,2020,by=10))
group<-seq(1900,2020,by=10)
ind<-seq(1,length(group),by=1)
labl1<-paste(group[ind],group[ind+1],sep="-")[-42]
dat1<- data.frame(year=year1,decade=labl1[indx],stringsAsFactors=FALSE)
PA_SUB$Decade <- dat1[,2]


PA_SUB$Date <- as.character(PA_SUB$Date)
PA_SUB$Date <- as.Date(PA_SUB$Date,format = '%m/%d/%Y' )

PA_SUB$month <- as.numeric(format(PA_SUB$Date, '%m'))

PA_SUB$submission <-1

##########################################################################
###END- NOW YOU CAN START 
##########################################################################
##########################################################################

OUT_PA <- subset(SUB_DAT, SUB_DAT$State != 'PA')

aggregate(OUT_PA$submission, by=list(OUT_PA$State, OUT_PA$Species),'sum')
