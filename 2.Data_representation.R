library (rfishbase)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(rpart)
library(GGally)
library(lsr)
library(purrr)
library(ggcorrplot)
library(mgcv)
library(plyr)
library(car)
library(MASS)
library(lmtest)
library(pscl)
library(boot)
library(purrr)
library(scales)
library(rlist)
library(PCAmixdata)
library(ellipse)
library(reshape)

rm(list = ls())
#####Load data#####
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")
INTRO<-read.csv2("D:/these/Axe_2/outputs/INTRO_all_good_selectedregdiet.csv")
INTRO$Introduced<-as.factor(INTRO$Introduced)
levels(INTRO$Introduced)[levels(INTRO$Introduced)=="Yes"]<-1
levels(INTRO$Introduced)[levels(INTRO$Introduced)=="No"]<-0
INTRO$Aquaculture<-as.factor(INTRO$Aquaculture)
INTRO$fisheries<-as.factor(INTRO$fisheries)
INTRO$Species.control<-as.factor(INTRO$Species.control)
INTRO$Accidental<-as.factor(INTRO$Accidental)
INTRO$Diffusion<-as.factor(INTRO$Diffusion)
INTRO$Sport.Angling<-as.factor(INTRO$Sport.Angling)
INTRO$ornamental<-as.factor(INTRO$ornamental)




##effet géographique de l'ordre?##############
geordre<-INTRO[, c("Order", "Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                   "Neotropical", "Australian" , "Madasgascan")]

geordre<-na.omit(geordre)
datafish2<-aggregate(geordre[,c("Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                                "Neotropical", "Australian" , "Madasgascan")], by=list(Category=geordre$Order), FUN=sum)

dat2 <- datafish2 %>%
   gather(Region, Nb_species, -Category)

#x11()
ggplot(data.frame(dat2), aes(x= Region,  y=Nb_species , fill=Category)) +
  geom_bar(position = "dodge", stat="identity") +
  # scale_y_log10( breaks = plotbreaks, labels = scales::comma(plotbreaks, accuracy = 1), expand = c(0, 0)) +  #scales::comma  scales::comma(plotbreaks, accuracy = 1) #trans_format("log10", math_format(10^.x))
  theme_bw() +
  # labs(y="Costs (2017 US$)")+
  theme(axis.line = element_line(colour = "black"), axis.title.x=element_blank(), axis.text.x=element_text(angle = 60, hjust = 1),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())
  #scale_fill_manual(values=c('black','grey'))
###



###graph nb diet vs nb countries for each step#####




########INTRODUCTION########
##### introduction 0-1######GLOBAL#######################
INTRO$Introduced<-as.factor(INTRO$Introduced)
levels(INTRO$Introduced)[levels(INTRO$Introduced)=="Yes"]<-1
levels(INTRO$Introduced)[levels(INTRO$Introduced)=="No"]<-0



no.na.data <- na.omit(INTRO[c("Species","Introduced","TL" , 
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "RepGuild1",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                              "Order", "Used_by_humans", "BlBd", "PFiBd")])

colnames(INTRO)

#####number of native region : why more than one?
nbregion<-no.na.data[,c("Species", "Nb.Native.Region")]
table(nbregion$Nb.Native.Region)


#####################représenter les données
# plot(log10(no.na.data$Introduced), no.na.data$TL)
# plot(log10(no.na.data$Introduced), no.na.data$Amplitudetemp)
# plot(log10(no.na.data$Introduced), no.na.data$Area.Bassins)
# plot(log10(no.na.data$Introduced), no.na.data$Nb.Diet)

no<-no.na.data[c("Introduced", "RepGuild1")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = RepGuild1), position = "dodge")


no<-no.na.data[c("Introduced", "Order")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = Order), position = "dodge")

no<-no.na.data[c("Introduced", "Used_by_humans")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = Used_by_humans), position = "dodge")


#x11()
ggplot(no.na.data, aes(x=log10( PFiBd +1), y=Introduced)) +
  geom_boxplot(notch=F)

#x11()
ggplot(no.na.data, aes(x=log10(BlBd+1), y=Introduced)) +
  geom_boxplot(notch=F)


#x11()
ggplot(no.na.data, aes(x=log10( Nb.Diet +1), y=Introduced)) +
  geom_boxplot(notch=F)

#x11()
ggplot(no.na.data, aes(x=( Nb.Diet), y=Introduced)) +
  geom_boxplot(notch=F)

#x11()
ggplot(no.na.data, aes(x=log10( TL +1), y=Introduced)) +
  geom_boxplot(notch=TRUE)


#x11()
ggplot(no.na.data, aes(x=log10( Amplitudetemp +1), y=Introduced)) +
  geom_boxplot(notch=TRUE)


#x11()
ggplot(no.na.data, aes(x=log10( Area.Bassins +1), y=Introduced)) +
  geom_boxplot(notch=TRUE)


#x11()
ggplot(no.na.data, aes(x=log10( Nb.Diet +1), y=Introduced)) +
  geom_boxplot(notch=F)

#x11()
ggplot(no.na.data, aes(x=log10( MaxBio5 +1), y=Introduced)) +
  geom_boxplot(notch=F)

no<-no.na.data[c("Introduced", "zoobenthos")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = zoobenthos), position = "dodge")

no<-no.na.data[c("Introduced", "nekton")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = nekton), position = "dodge")

no<-no.na.data[c("Introduced", "plants")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = plants), position = "dodge")

no<-no.na.data[c("Introduced", "detritus")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = detritus), position = "dodge")

no<-no.na.data[c("Introduced", "zooplankton")]
#x11()
ggplot(no, aes(Introduced, ..count..)) + geom_bar(aes(fill = zooplankton), position = "dodge")




geordre<-no.na.data[, c("Introduced", "Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                          "Neotropical", "Australian" , "Madasgascan")]
geordre$Paleartic<-as.integer(as.character(geordre$Paleartic))
geordre$Sino.oriental<-as.integer(as.character(geordre$Sino.oriental))
geordre$Ethiopian<-as.integer(as.character(geordre$Ethiopian))
geordre$Neartic<-as.integer(as.character(geordre$Neartic))
geordre$Neotropical<-as.integer(as.character(geordre$Neotropical))
geordre$Australian<-as.integer(as.character(geordre$Australian))
geordre$Madasgascan<-as.integer(as.character(geordre$Madasgascan))
geordre<-na.omit(geordre)
datafish2<-aggregate(geordre[,c("Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                                "Neotropical", "Australian" , "Madasgascan")], by=list(Category=geordre$Introduced), FUN=sum)

dat2 <- datafish2 %>%
  gather(Region, Nb_species, -Category)

#x11()
ggplot(data.frame(dat2), aes(x= Region,  y=Nb_species , fill=Category)) +
  geom_bar(position = "dodge", stat="identity") +
  # scale_y_log10( breaks = plotbreaks, labels = scales::comma(plotbreaks, accuracy = 1), expand = c(0, 0)) +  #scales::comma  scales::comma(plotbreaks, accuracy = 1) #trans_format("log10", math_format(10^.x))
  theme_bw() +
  # labs(y="Costs (2017 US$)")+
  theme(axis.line = element_line(colour = "black"), axis.title.x=element_blank(), axis.text.x=element_text(angle = 60, hjust = 1),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())


####vérifier l'effet géographique de l'ordre
geordre<-no.na.data[, c("Order", "Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                          "Neotropical", "Australian" , "Madasgascan")]
geordre$Paleartic<-as.integer(as.character(geordre$Paleartic))
geordre$Sino.oriental<-as.integer(as.character(geordre$Sino.oriental))
geordre$Ethiopian<-as.integer(as.character(geordre$Ethiopian))
geordre$Neartic<-as.integer(as.character(geordre$Neartic))
geordre$Neotropical<-as.integer(as.character(geordre$Neotropical))
geordre$Australian<-as.integer(as.character(geordre$Australian))
geordre$Madasgascan<-as.integer(as.character(geordre$Madasgascan))

geordre<-na.omit(geordre)
datafish2<-aggregate(geordre[,c("Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                                "Neotropical", "Australian" , "Madasgascan")], by=list(Category=geordre$Order), FUN=sum)

dat2 <- datafish2 %>%
  gather(Region, Nb_species, -Category)

#x11()
ggplot(data.frame(dat2), aes(x= Region,  y=Nb_species , fill=Category)) +
  geom_bar(position = "dodge", stat="identity") +
  # scale_y_log10( breaks = plotbreaks, labels = scales::comma(plotbreaks, accuracy = 1), expand = c(0, 0)) +  #scales::comma  scales::comma(plotbreaks, accuracy = 1) #trans_format("log10", math_format(10^.x))
  theme_bw() +
  # labs(y="Costs (2017 US$)")+
  theme(axis.line = element_line(colour = "black"), axis.title.x=element_blank(), axis.text.x=element_text(angle = 60, hjust = 1),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

##idem mais pour repguild
geordre<-no.na.data[, c("RepGuild1", "Order")]
#x11()
ggplot(geordre, aes(Order, ..count..)) + geom_bar(aes(fill = RepGuild1), position = "dodge")


geordre<-no.na.data[, c("RepGuild1", "Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                        "Neotropical", "Australian" , "Madasgascan")]
geordre$Paleartic<-as.integer(as.character(geordre$Paleartic))
geordre$Sino.oriental<-as.integer(as.character(geordre$Sino.oriental))
geordre$Ethiopian<-as.integer(as.character(geordre$Ethiopian))
geordre$Neartic<-as.integer(as.character(geordre$Neartic))
geordre$Neotropical<-as.integer(as.character(geordre$Neotropical))
geordre$Australian<-as.integer(as.character(geordre$Australian))
geordre$Madasgascan<-as.integer(as.character(geordre$Madasgascan))

geordre<-na.omit(geordre)
datafish2<-aggregate(geordre[,c("Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                                "Neotropical", "Australian" , "Madasgascan")], by=list(Category=geordre$RepGuild1), FUN=sum)

dat2 <- datafish2 %>%
  gather(Region, Nb_species, -Category)

#x11()
ggplot(data.frame(dat2), aes(x= Region,  y=Nb_species , fill=Category)) +
  geom_bar(position = "dodge", stat="identity") +
  # scale_y_log10( breaks = plotbreaks, labels = scales::comma(plotbreaks, accuracy = 1), expand = c(0, 0)) +  #scales::comma  scales::comma(plotbreaks, accuracy = 1) #trans_format("log10", math_format(10^.x))
  theme_bw() +
  # labs(y="Costs (2017 US$)")+
  theme(axis.line = element_line(colour = "black"), axis.title.x=element_blank(), axis.text.x=element_text(angle = 60, hjust = 1),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())


subset(no.na.data, RepGuild1 == "bearers")

scale_fill_manual(values=c('black','grey'))







#madagascan region : only 14 species with 1


###pb with ecoregions? ==> see if one region per species
regionnative<-read.csv2("./regionnative.csv")
regions<-regionnative[,c(2,4)]
colnames(regions)<-c("Species", "Native_regionLeroy")

regions2<-merge(x = no.na.data, y = regions, by = "Species", all.x = TRUE)
regions2<-unique(regions2)
blip<-as.data.frame(table(regions2$Species))
rowSums(no.na.data[,c("Paleartic", "Sino.oriental",
                      "Ethiopian" , "Neartic" ,
                      "Neotropical", "Australian" ,
                      "Madasgascan")])














##### introduction 0-1###################SEPARATED PATHWAYS#######
getwd()
pdf(file = "representationdesdonnées.pdf")

###taille#######
nonadataTL<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                            "RepGuild1" , "Amplitudetemp" ,
                            "MaxBio5", "RepGuild1",
                            "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                             "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataTL<-nonadataTL[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]
nonadataTL2 <- melt(nonadataTL, id = "TL")  

#x11()
ggplot(nonadataTL2, aes(x=log(TL +1), y=variable, color=value)) +
  geom_boxplot(notch=TRUE)+theme_bw()






##blbd####
nonadataBlBd<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                            "RepGuild1" , "Amplitudetemp" ,
                            "MaxBio5", "RepGuild1",
                            "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                             "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataBlBd<-nonadataBlBd[c("Introduced","BlBd", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]
nonadataBlBd2 <- melt(nonadataBlBd, id = "BlBd")  

#x11()
ggplot(nonadataBlBd2, aes(x=log10(BlBd +1), y=variable, color=value)) +
  geom_boxplot(notch=TRUE)+theme_bw()


##PFiBd#####
nonadataPFiBd<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "RepGuild1",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                               "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataPFiBd<-nonadataPFiBd[c("Introduced","PFiBd", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]
nonadataPFiBd2 <- melt(nonadataPFiBd, id = "PFiBd")  

#x11()
ggplot(nonadataPFiBd2, aes(x=log10(PFiBd +1), y=variable, color=value)) +
  geom_boxplot(notch=TRUE)+theme_bw()




#########Nombre de diet#####
nonadataNb.Diet<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                               "RepGuild1" , "Amplitudetemp" ,
                               "MaxBio5", "RepGuild1",
                               "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataNb.Diet<-nonadataNb.Diet[c("Introduced","Nb.Diet", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]
nonadataNb.Diet2 <- melt(nonadataNb.Diet, id = "Nb.Diet")  

#x11()
ggplot(nonadataNb.Diet2, aes(x=log(Nb.Diet +1), y=variable, color=value, notch=FALSE)) +
  geom_boxplot(notch=TRUE)+theme_bw()




#########aMPLI TEMP####
nonadataAmplitudetemp<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                 "RepGuild1" , "Amplitudetemp" ,
                                 "MaxBio5", "RepGuild1",
                                 "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                  "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataAmplitudetemp<-nonadataAmplitudetemp[c("Introduced","Amplitudetemp", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]
nonadataAmplitudetemp2 <- melt(nonadataAmplitudetemp, id = "Amplitudetemp")  

#x11()
ggplot(nonadataAmplitudetemp2, aes(x=log10(Amplitudetemp +1), y=variable, color=value)) +
  geom_boxplot(notch=TRUE)

#x11()
ggplot(nonadataAmplitudetemp2, aes(x=log(Amplitudetemp +1), y=variable, color=value)) +
  geom_boxplot(notch=TRUE)

#########Nb.Native.Region######
nonadataNb.Native.Region<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                       "RepGuild1" , "Amplitudetemp" ,
                                       "MaxBio5", "RepGuild1",
                                       "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                        "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataNb.Native.Region<-nonadataNb.Native.Region[c("Introduced","Nb.Native.Region", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]
nonadataNb.Native.Region2 <- melt(nonadataNb.Native.Region, id = "Nb.Native.Region")  

#x11()
ggplot(nonadataNb.Native.Region2, aes(x=log(Nb.Native.Region +1), y=variable, color=value)) +
  geom_boxplot(notch=TRUE)

int<-subset(nonadataNb.Native.Region2, variable == "Introduced")
intint<-subset(nonadataNb.Native.Region2, value == "1")
mean(intint$Nb.Native.Region)
intnotint<-subset(nonadataNb.Native.Region2, value == "0")
mean(intnotint$Nb.Native.Region)

dev.off()

####Main native region#####
#proportions
nonadataREG<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                             "RepGuild1" , "Amplitudetemp" ,
                             "MaxBio5", "RepGuild1",
                             "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                             "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataREG<-nonadataREG[c("Introduced","Maj.Region", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]

region.prop <- nonadataREG%>%
  group_by(Introduced, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
introducedregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = Introduced)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#introducedregion



region.prop <- nonadataREG%>%
  group_by(Accidental, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
accidentalregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = Accidental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#accidentalregion


region.prop <- nonadataREG%>%
  group_by(Sport.Angling, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Sport.Anglingregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = Sport.Angling)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Sport.Anglingregion




region.prop <- nonadataREG%>%
  group_by(Aquaculture, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Aquacultureregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = Aquaculture)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Aquacultureregion


region.prop <- nonadataREG%>%
  group_by(Diffusion, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Diffusionregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = Diffusion)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Diffusionregion


region.prop <- nonadataREG%>%
  group_by(fisheries, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
fisheriesregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = fisheries)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#fisheriesregion


region.prop <- nonadataREG%>%
  group_by(Species.control, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Species.controlregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = Species.control)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Species.controlregion


region.prop <- nonadataREG%>%
  group_by(ornamental, Maj.Region) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
ornamentalregion<-ggplot(region.prop, aes(x=Maj.Region, y=prop, fill = ornamental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#ornamentalregion

pdf("representationdesdonnees-region.pdf", width=13, height=13)
ggarrange(introducedregion, Aquacultureregion, ornamentalregion, fisheriesregion, accidentalregion, Sport.Anglingregion,Diffusionregion,Species.controlregion + rremove("x.text"), 
          labels = c("Global", "Aquaculture", "Ornamental", "Fisheries", "Accidental","Sport/Angling","Diffusion","Species control"),
          ncol = 2, nrow = 4)
dev.off()


########Order####
nonadataOrder<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                             "RepGuild1" , "Amplitudetemp" ,
                             "MaxBio5", "RepGuild1",
                             "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                              "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataOrder<-nonadataOrder[c("Introduced","Order", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]

#proportions

Order.prop <- nonadataOrder%>%
  group_by(Introduced, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
introducedOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = Introduced)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#introducedOrder



Order.prop <- nonadataOrder%>%
  group_by(Accidental, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
accidentalOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = Accidental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#accidentalOrder


Order.prop <- nonadataOrder%>%
  group_by(Sport.Angling, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Sport.AnglingOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = Sport.Angling)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Sport.AnglingOrder




Order.prop <- nonadataOrder%>%
  group_by(Aquaculture, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
AquacultureOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = Aquaculture)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#AquacultureOrder


Order.prop <- nonadataOrder%>%
  group_by(Diffusion, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
DiffusionOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = Diffusion)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#DiffusionOrder


Order.prop <- nonadataOrder%>%
  group_by(fisheries, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
fisheriesOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = fisheries)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#fisheriesOrder


Order.prop <- nonadataOrder%>%
  group_by(Species.control, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Species.controlOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = Species.control)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Species.controlOrder


Order.prop <- nonadataOrder%>%
  group_by(ornamental, Order) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
ornamentalOrder<-ggplot(Order.prop, aes(x=Order, y=prop, fill = ornamental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#ornamentalOrder


pdf("representationdesdonnees-Order.pdf", width=13, height=13)
ggarrange(introducedOrder, AquacultureOrder, ornamentalOrder, fisheriesOrder, accidentalOrder, Sport.AnglingOrder,DiffusionOrder,Species.controlOrder + rremove("x.text"), 
          labels = c("Global", "Aquaculture", "Ornamental", "Fisheries", "Accidental","Sport/Angling","Diffusion","Species control"),
          ncol = 2, nrow = 4)
dev.off()

# 
# ##number
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = Introduced), position = "dodge")
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = Accidental), position = "dodge")
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = Sport.Angling), position = "dodge")
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = Aquaculture), position = "dodge")
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = Diffusion), position = "dodge")
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = fisheries), position = "dodge")
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = Species.control), position = "dodge")
# #x11()
# ggplot(nonadataOrder, aes(Order, ..count..)) + geom_bar(aes(fill = ornamental), position = "dodge")
# 
# 
# 

########Used_by_humans####
nonadataUsed_by_humans<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                               "RepGuild1" , "Amplitudetemp" ,
                               "MaxBio5", "RepGuild1",
                               "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataUsed_by_humans<-nonadataUsed_by_humans[c("Introduced","Used_by_humans", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]

#proportions

Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(Introduced, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
introducedUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = Introduced)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#introducedUsed_by_humans



Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(Accidental, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
accidentalUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = Accidental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#accidentalUsed_by_humans


Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(Sport.Angling, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Sport.AnglingUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = Sport.Angling)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Sport.AnglingUsed_by_humans




Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(Aquaculture, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
AquacultureUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = Aquaculture)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#AquacultureUsed_by_humans


Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(Diffusion, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
DiffusionUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = Diffusion)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#DiffusionUsed_by_humans


Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(fisheries, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
fisheriesUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = fisheries)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#fisheriesUsed_by_humans


Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(Species.control, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Species.controlUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = Species.control)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Species.controlUsed_by_humans


Used_by_humans.prop <- nonadataUsed_by_humans%>%
  group_by(ornamental, Used_by_humans) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
ornamentalUsed_by_humans<-ggplot(Used_by_humans.prop, aes(x=Used_by_humans, y=prop, fill = ornamental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#ornamentalUsed_by_humans

pdf("representationdesdonnees-Usedbyhumans.pdf", width=13, height=13)
ggarrange(introducedUsed_by_humans, AquacultureUsed_by_humans, ornamentalUsed_by_humans, fisheriesUsed_by_humans, accidentalUsed_by_humans, Sport.AnglingUsed_by_humans,DiffusionUsed_by_humans,Species.controlUsed_by_humans + rremove("x.text"), 
          labels = c("Global", "Aquaculture", "Ornamental", "Fisheries", "Accidental","Sport/Angling","Diffusion","Species control"),
          ncol = 2, nrow = 4)
dev.off()

# #number
# ####x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = Introduced), position = "dodge")
# ####x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = Accidental), position = "dodge")
# ####x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = Sport.Angling), position = "dodge")
# ####x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = Aquaculture), position = "dodge")
# ####x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = Diffusion), position = "dodge")
# ####x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = fisheries), position = "dodge")
# ####x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = Species.control), position = "dodge")
# ###x11()
# ggplot(nonadataUsed_by_humans, aes(Used_by_humans, ..count..)) + geom_bar(aes(fill = ornamental), position = "dodge")
# 

#####diet majoritaires en fonction pathways######
nonadataDIET<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "RepGuild1",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                              "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataDIET<-nonadataDIET[c("Introduced","Maj.Diet", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]



##proportions

diet.prop <- nonadataDIET%>%
  group_by(Introduced, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
introduceddiet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = Introduced)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#introduceddiet



diet.prop <- nonadataDIET%>%
  group_by(Accidental, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
accidentaldiet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = Accidental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#accidentaldiet


diet.prop <- nonadataDIET%>%
  group_by(Sport.Angling, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Sport.Anglingdiet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = Sport.Angling)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Sport.Anglingdiet




diet.prop <- nonadataDIET%>%
  group_by(Aquaculture, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Aquaculturediet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = Aquaculture)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Aquaculturediet


diet.prop <- nonadataDIET%>%
  group_by(Diffusion, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Diffusiondiet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = Diffusion)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Diffusiondiet


diet.prop <- nonadataDIET%>%
  group_by(fisheries, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
fisheriesdiet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = fisheries)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#fisheriesdiet


diet.prop <- nonadataDIET%>%
  group_by(Species.control, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Species.controldiet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = Species.control)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Species.controldiet


diet.prop <- nonadataDIET%>%
  group_by(ornamental, Maj.Diet) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
ornamentaldiet<-ggplot(diet.prop, aes(x=Maj.Diet, y=prop, fill = ornamental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#ornamentaldiet

pdf("representationdesdonnees-diet.pdf", width=13, height=13)
library(ggpubr)
ggarrange(introduceddiet, Aquaculturediet, ornamentaldiet, fisheriesdiet, accidentaldiet, Sport.Anglingdiet,Diffusiondiet,Species.controldiet + rremove("x.text"), 
          labels = c("Global", "Aquaculture", "Ornamental", "Fisheries", "Accidental","Sport/Angling","Diffusion","Species control"),
          ncol = 2, nrow = 4)

dev.off()



# 
# 
# ###number
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = Introduced), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, (..count..)/sum(..count..))) + geom_bar(aes(fill = Introduced), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = Accidental), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = Sport.Angling), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = Aquaculture), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = Diffusion), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = fisheries), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = Species.control), position = "dodge")
# #x11()
# ggplot(nonadataDIET, aes(Maj.Diet, ..count..)) + geom_bar(aes(fill = ornamental), position = "dodge")
# 



#####Repguild1#####
nonadataREP<-na.omit(INTRO[c("Introduced","TL", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental", "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                             "RepGuild1" , "Amplitudetemp" ,
                             "MaxBio5", "RepGuild1",
                             "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                             "Order", "Used_by_humans", "BlBd", "PFiBd") ])
nonadataREP<-nonadataREP[c("Introduced","RepGuild1", "Accidental", "Sport.Angling", "Aquaculture", "Diffusion", "fisheries", "Species.control", "ornamental")]

##proportions

parent.prop <- nonadataREP%>%
  group_by(Introduced, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
introducedparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = Introduced)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#introducedparent



parent.prop <- nonadataREP%>%
  group_by(Accidental, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
accidentalparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = Accidental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#accidentalparent


parent.prop <- nonadataREP%>%
  group_by(Sport.Angling, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Sport.Anglingparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = Sport.Angling)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Sport.Anglingparent




parent.prop <- nonadataREP%>%
  group_by(Aquaculture, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Aquacultureparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = Aquaculture)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Aquacultureparent


parent.prop <- nonadataREP%>%
  group_by(Diffusion, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Diffusionparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = Diffusion)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Diffusionparent


parent.prop <- nonadataREP%>%
  group_by(fisheries, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
fisheriesparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = fisheries)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#fisheriesparent


parent.prop <- nonadataREP%>%
  group_by(Species.control, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
Species.controlparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = Species.control)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#Species.controlparent


parent.prop <- nonadataREP%>%
  group_by(ornamental, RepGuild1) %>%
  dplyr::summarise(n = n())%>%
  dplyr::mutate(prop = n/sum(n))

#x11()
ornamentalparent<-ggplot(parent.prop, aes(x=RepGuild1, y=prop, fill = ornamental)) + geom_bar(stat="identity", position = "dodge")+
  theme_bw()
#ornamentalparent



pdf("representationdesdonnees-parent.pdf", width=13, height=13)
ggarrange(introducedparent, Aquacultureparent, ornamentalparent, fisheriesparent, accidentalparent, Sport.Anglingparent,Diffusionparent,Species.controlparent + rremove("x.text"), 
          labels = c("Global", "Aquaculture", "Ornamental", "Fisheries", "Accidental","Sport/Angling","Diffusion","Species control"),
          ncol = 2, nrow = 4)


dev.off()


# 
# ###nombre
# 
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = Introduced), position = "dodge")
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = Accidental), position = "dodge")
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = Sport.Angling), position = "dodge")
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = Aquaculture), position = "dodge")
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = Diffusion), position = "dodge")
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = fisheries), position = "dodge")
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = Species.control), position = "dodge")
# #x11()
# ggplot(nonadataREP, aes(RepGuild1, ..count..)) + geom_bar(aes(fill = ornamental), position = "dodge")
# 
# 






























#####Number of country without 0 _ GLM############

no.na.data <- na.omit(INTRO[c("Species","nb.country.intro","TL" ,
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "Accidental" ,
                              "Sport.Angling","Aquaculture","Diffusion","fisheries",
                              "Species.control","ornamental", "research",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",
                               "Order", "Used_by_humans", "Nb_reasons"
)])

table(no.na.data$nb.country.intro)
no.na.data.1<-subset(no.na.data, nb.country.intro > 0)

table(no.na.data.1$nb.country.intro) #--> too many zeros!!!! pas une loi de poisson
plot(table(no.na.data.1$nb.country.intro))

######représenter les données #####
#x11()
plot(log10(no.na.data.1$nb.country.intro), no.na.data.1$TL)
#x11()
plot(log10(no.na.data.1$nb.country.intro), no.na.data.1$Amplitudetemp)
#x11()
plot(log10(no.na.data.1$nb.country.intro), no.na.data.1$Area.Bassins)
#x11()
plot(log10(no.na.data.1$nb.country.intro), no.na.data.1$Nb.Diet)
#x11()
plot(log10(no.na.data.1$nb.country.intro), no.na.data.1$MaxBio5)


#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro +1), y=Order)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro +1), y=Used_by_humans)) +
  geom_boxplot(notch=F)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro +1), y=RepGuild1)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro +1), y=zoobenthos)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=nekton)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro +1), y=plants)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=detritus)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=zooplankton)) +
  geom_boxplot(notch=TRUE)



#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Ethiopian)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Neartic)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Neotropical)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Australian)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Madasgascan)) +
  geom_boxplot(notch=TRUE)


#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Accidental)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Sport.Angling)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Aquaculture)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Diffusion)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=fisheries)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=Species.control)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=ornamental)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data.1, aes(x=log10(nb.country.intro + 1), y=research)) +
  geom_boxplot(notch=TRUE)

####vérifier l'effet géographique de l'ordre
geordre<-no.na.data.1[, c("Order", "Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                        "Neotropical", "Australian" , "Madasgascan")]

geordre<-na.omit(geordre)
datafish2<-aggregate(geordre[,c("Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                                "Neotropical", "Australian" , "Madasgascan")], by=list(Category=geordre$Order), FUN=sum)

dat2 <- datafish2 %>%
  gather(Region, Nb_species, -Category)

#x11()
ggplot(data.frame(dat2), aes(x= Region,  y=Nb_species , fill=Category)) +
  geom_bar(position = "dodge", stat="identity") +
  # scale_y_log10( breaks = plotbreaks, labels = scales::comma(plotbreaks, accuracy = 1), expand = c(0, 0)) +  #scales::comma  scales::comma(plotbreaks, accuracy = 1) #trans_format("log10", math_format(10^.x))
  theme_bw() +
  # labs(y="Costs (2017 US$)")+
  theme(axis.line = element_line(colour = "black"), axis.title.x=element_blank(), axis.text.x=element_text(angle = 60, hjust = 1),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())
#







##idem mais pour repguild
geordre<-no.na.data.1[, c("RepGuild1", "Order")]
library(plyr)
dat2<-ddply(geordre, c("RepGuild1", "Order"), summarise )

geordre<-na.omit(geordre)
datafish2<-aggregate(geordre[,c("Paleartic", "Sino.oriental", "Ethiopian" , "Neartic" ,
                                "Neotropical", "Australian" , "Madasgascan")], by=list(Category=geordre$"RepGuild1"), FUN=sum)

dat2 <- datafish2 %>%
  gather(Region, Nb_species, -Category)

#x11()
ggplot(data.frame(dat2), aes(x= Region,  y=Nb_species , fill=Category)) +
  geom_bar(position = "dodge", stat="identity") +
  # scale_y_log10( breaks = plotbreaks, labels = scales::comma(plotbreaks, accuracy = 1), expand = c(0, 0)) +  #scales::comma  scales::comma(plotbreaks, accuracy = 1) #trans_format("log10", math_format(10^.x))
  theme_bw() +
  # labs(y="Costs (2017 US$)")+
  theme(axis.line = element_line(colour = "black"), axis.title.x=element_blank(), axis.text.x=element_text(angle = 60, hjust = 1),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())



###ESTABLISHMENT#######
####estabwild 1-0#####
levels(INTRO$Estabwild)[levels(INTRO$Estabwild)=="Yes"]<-1
levels(INTRO$Estabwild)[levels(INTRO$Estabwild)=="No"]<-0

###BIEN METTRE NBDIET ET NBREASON POUR ENLEVER LES ESPECES SANS DIET OU SANS REASONS DE RENSEIGNE
####nb of country######### SEULEMENT AVEC 1######
no.na.data <- na.omit(INTRO[c("Species","nb.country.establish","TL" ,
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "Accidental" ,
                              "Sport.Angling","Aquaculture","Diffusion","fisheries",
                              "Species.control","ornamental", "research",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",
                               "Order", "Used_by_humans", "Nb_reasons"
)])






######################représenter les données#####
#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$TL)
#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$Amplitudetemp)
#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$Area.Bassins)
#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$Nb.Diet)
#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$MaxBio5)

#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$NbDietMax)
#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$Nb.Diet)

#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$NbregionMax)
#x11()
plot(log10(no.na.data$nb.country.establish), no.na.data$Nb.Native.Region)



#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish +1), y=Order)) +
  geom_boxplot(notch=F)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish +1), y=Used_by_humans)) +
  geom_boxplot(notch=F)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish +1), y=Maj.Region)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish +1), y=Maj.Diet)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish +1), y=RepGuild1)) +
  geom_boxplot(notch=TRUE)







#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=Accidental)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=Sport.Angling)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=Aquaculture)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=Diffusion)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=fisheries)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=Species.control)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=ornamental)) +
  geom_boxplot(notch=TRUE)
#x11()
ggplot(no.na.data, aes(x=log10(nb.country.establish + 1), y=research)) +
  geom_boxplot(notch=TRUE)





