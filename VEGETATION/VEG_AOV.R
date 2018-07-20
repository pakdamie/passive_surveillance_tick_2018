###VEGETATION

VEG <- read.csv('1989_1990_counts_knownveg.csv')
                
VEG_AOV <- aov(Counts_submit~Vegtype + Species+Host, data = VEG)
summary(VEG_AOV)
TukeyHSD(x=VEG_AOV,  conf.level=0.95)



VEG_AOV <- aggregate(VEG$Counts, by=list(VEG$Species, VEG$Vegtype),'sum')

