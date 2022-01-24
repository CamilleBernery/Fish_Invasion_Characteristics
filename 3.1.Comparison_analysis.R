library (rfishbase)
library(dplyr)
library(ggplot2)
library(ggpubr)

library(tidyverse)
library(caret)
library(rpart)
library(randomForest)
library(GGally)

library(tidyverse)
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








#####Load data#####
rm(list = ls())
setwd("D:/these/Axe_2")
INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")






########INTRODUCTION -- KHI2######
#########DATABASE CONSTRUCTION##########

alldatabase<-list()
alldatabasesample<-list()

AllIntro<-list()
AllNotIntro<-list()


#####GLOBAL_________________________________________________________##

INTRO$Introduced<-as.factor(INTRO$Introduced)
levels(INTRO$Introduced)[levels(INTRO$Introduced)=="Yes"]<-1
levels(INTRO$Introduced)[levels(INTRO$Introduced)=="No"]<-0

no.na.data <- na.omit(INTRO[c("Species","Introduced","TL" , 
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "RepGuild1",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                               "Order", "Used_by_humans", "BlBd", "PFiBd")])



###Khi2
nb<-rep(1, length(no.na.data[,1]))
nonadata<-cbind(no.na.data, nb)
alldatabase[[1]]<-nonadata

#999 samples
set.seed(123) 
NotIntro<-subset(nonadata, Introduced==0)
AllNotIntro[[1]]<-NotIntro
Intro<-subset(nonadata, Introduced==1)
AllIntro[[1]]<-Intro
nb2<-rep(1, length(NotIntro[,1])) 
NotIntro1<-cbind(NotIntro, nb2)
listofNotIntro <-replicate(999, NotIntro1 %>% sample_n(length(Intro[,1])), simplify=F)

alldatabasesample[[1]]<-listofNotIntro 


#######Aquaculture________________________________________________________##

INTRO$Aquaculture<-as.factor(INTRO$Aquaculture)
levels(INTRO$Aquaculture)[levels(INTRO$Aquaculture)=="Yes"]<-1
levels(INTRO$Aquaculture)[levels(INTRO$Aquaculture)=="No"]<-0

no.na.data.aqua <- na.omit(INTRO[c("Species","Aquaculture","TL" , 
                                   "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                   "RepGuild1" , "Amplitudetemp" ,
                                   "MaxBio5", "RepGuild1",
                                   "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                   "Order", "Used_by_humans", "BlBd", "PFiBd")])


###Khi2
nb<-rep(1, length(no.na.data.aqua[,1]))
nonadataaqua<-cbind(no.na.data.aqua, nb)
alldatabase[[2]]<-nonadataaqua

#999 samples
set.seed(123) 
NotIntroaqua<-subset(nonadataaqua, Aquaculture==0)
Introaqua<-subset(nonadataaqua, Aquaculture==1)
AllNotIntro[[2]]<-NotIntroaqua
AllIntro[[2]]<-Introaqua
nb2<-rep(1, length(NotIntroaqua[,1])) 
NotIntro1aqua<-cbind(NotIntroaqua, nb2)
listofNotIntroaqua <-replicate(999, NotIntro1aqua %>% sample_n(length(Introaqua[,1])), simplify=F)

alldatabasesample[[2]]<-listofNotIntroaqua 


#####################Species.control________________________________________________________##

INTRO$Species.control<-as.factor(INTRO$Species.control)
levels(INTRO$Species.control)[levels(INTRO$Species.control)=="Yes"]<-1
levels(INTRO$Species.control)[levels(INTRO$Species.control)=="No"]<-0

no.na.data.cont <- na.omit(INTRO[c("Species","Species.control","TL" , 
                                   "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                   "RepGuild1" , "Amplitudetemp" ,
                                   "MaxBio5", "RepGuild1",
                                   "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                   "Order", "Used_by_humans", "BlBd", "PFiBd")])


###Khi2
nb<-rep(1, length(no.na.data.cont[,1]))
nonadatacont<-cbind(no.na.data.cont, nb)
alldatabase[[3]]<-nonadatacont

#999 samples
set.seed(123) 
NotIntrocont<-subset(nonadatacont, Species.control==0)
Introcont<-subset(nonadatacont, Species.control==1)
AllNotIntro[[3]]<-NotIntrocont
AllIntro[[3]]<-Introcont
nb2<-rep(1, length(NotIntrocont[,1])) 
NotIntro1cont<-cbind(NotIntrocont, nb2)
listofNotIntrocont <-replicate(999, NotIntro1cont %>% sample_n(length(Introcont[,1])), simplify=F)

alldatabasesample[[3]]<-listofNotIntrocont

###ORNAMENTAL________________________________________________________##

INTRO$ornamental<-as.factor(INTRO$ornamental)
levels(INTRO$ornamental)[levels(INTRO$ornamental)=="Yes"]<-1
levels(INTRO$ornamental)[levels(INTRO$ornamental)=="No"]<-0

no.na.data.orna <- na.omit(INTRO[c("Species","ornamental","TL" , 
                                   "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                   "RepGuild1" , "Amplitudetemp" ,
                                   "MaxBio5", "RepGuild1",
                                   "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                   "Order", "Used_by_humans", "BlBd", "PFiBd")])

# INTRObrosse<-merge(no.na.data.orna, brosse, by="Species")
# INTRObrosse<-na.omit(INTRObrosse)


colnames(INTRO)




###Khi2
nb<-rep(1, length(no.na.data.orna[,1]))
nonadataorna<-cbind(no.na.data.orna, nb)
alldatabase[[4]]<-nonadataorna


#999 samples
set.seed(123) 
NotIntroorna<-subset(nonadataorna, ornamental==0)
Introorna<-subset(nonadataorna, ornamental==1)
AllNotIntro[[4]]<-NotIntroorna
AllIntro[[4]]<-Introorna

nb2<-rep(1, length(NotIntroorna[,1])) 
NotIntro1orna<-cbind(NotIntroorna, nb2)
listofNotIntroorna <-replicate(999, NotIntro1orna %>% sample_n(length(Introorna[,1])), simplify=F)
alldatabasesample[[4]]<-listofNotIntroorna



#####FISHERIES________________________________________________________###


INTRO$fisheries<-as.factor(INTRO$fisheries)
levels(INTRO$fisheries)[levels(INTRO$fisheries)=="Yes"]<-1
levels(INTRO$fisheries)[levels(INTRO$fisheries)=="No"]<-0

no.na.data.fish <- na.omit(INTRO[c("Species","fisheries","TL" , 
                                   "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                   "RepGuild1" , "Amplitudetemp" ,
                                   "MaxBio5", "RepGuild1",
                                   "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                   "Order", "Used_by_humans", "BlBd", "PFiBd")])


###Khi2
nb<-rep(1, length(no.na.data.fish[,1]))
nonadatafish<-cbind(no.na.data.fish, nb)
alldatabase[[5]]<-nonadatafish

#999 samples
set.seed(123) 
NotIntrofish<-subset(nonadatafish, fisheries==0)
Introfish<-subset(nonadatafish, fisheries==1)
AllNotIntro[[5]]<-NotIntrofish
AllIntro[[5]]<-Introfish
nb2<-rep(1, length(NotIntrofish[,1]))
NotIntro1fish<-cbind(NotIntrofish, nb2)
listofNotIntrofish <-replicate(999, NotIntro1fish %>% sample_n(length(Introfish[,1])), simplify=F)
alldatabasesample[[5]]<-listofNotIntrofish


#####################ACCIDENTAL________________________________________________________###
INTRO$Accidental<-as.factor(INTRO$Accidental)
levels(INTRO$Accidental)[levels(INTRO$Accidental)=="Yes"]<-1
levels(INTRO$Accidental)[levels(INTRO$Accidental)=="No"]<-0

no.na.data.acci <- na.omit(INTRO[c("Species","Accidental","TL" , 
                                   "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                   "RepGuild1" , "Amplitudetemp" ,
                                   "MaxBio5", "RepGuild1",
                                   "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                   "Order", "Used_by_humans", "BlBd", "PFiBd")])



###Khi2
nb<-rep(1, length(no.na.data.acci[,1]))
nonadataacci<-cbind(no.na.data.acci, nb)
alldatabase[[6]]<-nonadataacci

#999 samples
set.seed(123) 
NotIntroacci<-subset(nonadataacci, Accidental==0)
Introacci<-subset(nonadataacci, Accidental==1)
AllNotIntro[[6]]<-NotIntroacci
AllIntro[[6]]<-Introacci
nb2<-rep(1, length(NotIntroacci[,1])) 
NotIntro1acci<-cbind(NotIntroacci, nb2)
listofNotIntroacci <-replicate(999, NotIntro1acci %>% sample_n(length(Introacci[,1])), simplify=F)
alldatabasesample[[6]]<-listofNotIntroacci


####SPORT ANGLING________________________________________________________##
INTRO$Sport.Angling<-as.factor(INTRO$Sport.Angling)
levels(INTRO$Sport.Angling)[levels(INTRO$Sport.Angling)=="Yes"]<-1
levels(INTRO$Sport.Angling)[levels(INTRO$Sport.Angling)=="No"]<-0

no.na.data.spor <- na.omit(INTRO[c("Species","Sport.Angling","TL" , 
                                   "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                   "RepGuild1" , "Amplitudetemp" ,
                                   "MaxBio5", "RepGuild1",
                                   "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                   "Order", "Used_by_humans", "BlBd", "PFiBd")])



###Khi2
nb<-rep(1, length(no.na.data.spor[,1]))
nonadataspor<-cbind(no.na.data.spor, nb)
alldatabase[[7]]<-nonadataspor

#999 samples
set.seed(123)
NotIntrospor<-subset(nonadataspor, Sport.Angling==0)
Introspor<-subset(nonadataspor, Sport.Angling==1)
AllNotIntro[[7]]<-NotIntrospor
AllIntro[[7]]<-Introspor
nb2<-rep(1, length(NotIntrospor[,1]))
NotIntro1spor<-cbind(NotIntrospor, nb2)
listofNotIntrospor <-replicate(999, NotIntro1spor %>% sample_n(length(Introspor[,1])), simplify=F)
alldatabasesample[[7]]<-listofNotIntrospor


######DIFFUSION________________________________________________________##

INTRO$Diffusion<-as.factor(INTRO$Diffusion)
levels(INTRO$Diffusion)[levels(INTRO$Diffusion)=="Yes"]<-1
levels(INTRO$Diffusion)[levels(INTRO$Diffusion)=="No"]<-0

no.na.data.diff <- na.omit(INTRO[c("Species","Diffusion","TL" , 
                                   "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                                   "RepGuild1" , "Amplitudetemp" ,
                                   "MaxBio5", "RepGuild1",
                                   "Maj.Region",  "NbregionMax", "Nb.Native.Region",                          
                                   "Order", "Used_by_humans", "BlBd", "PFiBd")])



###Khi2
nb<-rep(1, length(no.na.data.diff[,1]))
nonadatadiff<-cbind(no.na.data.diff, nb)
alldatabase[[8]]<-nonadatadiff

#999 samples
set.seed(123) 
NotIntrodiff<-subset(nonadatadiff, Diffusion==0)
Introdiff<-subset(nonadatadiff, Diffusion==1)
AllNotIntro[[8]]<-NotIntrodiff
AllIntro[[8]]<-Introdiff
nb2<-rep(1, length(NotIntrodiff[,1])) 
NotIntro1diff<-cbind(NotIntrodiff, nb2)
listofNotIntrodiff <-replicate(999, NotIntro1diff %>% sample_n(length(Introdiff[,1])), simplify=F)
alldatabasesample[[8]]<-listofNotIntrodiff



namecolumn<-c("Introduced", "Aquaculture", "Species.control", "ornamental", "fisheries", "Accidental", "Sport.Angling", "Diffusion")



####DATAFRAMES PREPARATION #####

CHITEST<-data.frame(matrix(ncol = 22, nrow = 8))
rownames(CHITEST)<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(CHITEST)<-c("bearers","guarders","nonguarders",       
                     "detritus","nekton", "plants",
                     "zoobenthos","detritus" ,"Australian",      
                     "Ethiopian","Nearctic","Neotropical",       
                     "Palearctic","Sino-Oriental","0",                 
                     "1","Characiformes","Cypriniformes",     
                     "Cyprinodontiformes","Other","Perciformes",      
                     "Siluriformes")
FTEST<-data.frame(matrix(ncol = 22, nrow = 8))
rownames(FTEST)<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(FTEST)<-c("bearers","guarders","nonguarders",       
                     "detritus","nekton", "plants",
                     "zoobenthos","detritus" ,"Australian",      
                     "Ethiopian","Nearctic","Neotropical",       
                     "Palearctic","Sino-Oriental","0",                 
                     "1","Characiformes","Cypriniformes",     
                     "Cyprinodontiformes","Other","Perciformes",      
                     "Siluriformes")

CHITESTpercent<-data.frame(matrix(ncol = 22, nrow = 8))
rownames(CHITESTpercent)<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(CHITESTpercent)<-c("bearers","guarders","nonguarders",       
                     "detritus","nekton", "plants",
                     "zoobenthos","detritus" ,"Australian",      
                     "Ethiopian","Nearctic","Neotropical",       
                     "Palearctic","Sino-Oriental","0",                 
                     "1","Characiformes","Cypriniformes",     
                     "Cyprinodontiformes","Other","Perciformes",      
                     "Siluriformes")

FTESTpercent<-data.frame(matrix(ncol = 22, nrow = 8))
rownames(FTESTpercent)<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(FTESTpercent)<-c("bearers","guarders","nonguarders",       
                     "detritus","nekton", "plants",
                     "zoobenthos","detritus" ,"Australian",      
                     "Ethiopian","Nearctic","Neotropical",       
                     "Palearctic","Sino-Oriental","0",                 
                     "1","Characiformes","Cypriniformes",     
                     "Cyprinodontiformes","Other","Perciformes",      
                     "Siluriformes")

#######KHI 2 LOOP####
for (y in 1:length(alldatabase)){
  
  
  

########tests per modality parental care##################
dataPA<-ddply(alldatabase[[y]], c("RepGuild1", namecolumn[y]), summarise, nb=sum(nb))
dataTPA<-subset(dataPA, dataPA[,2]==1) #threatened
class(dataTPA$nb)<-"integer"
dataNTPA<-subset(dataPA, dataPA[,2]==0) #non-threatened



#Proportions
b<-(dataTPA$nb)/sum(dataTPA$nb)
dataTPA2<-cbind(dataTPA,b)
dataTPA2


b<-dataNTPA$nb/sum(dataNTPA$nb)
dataNTPA2<-cbind(dataNTPA, b)
dataNTPA2



#Contingence table
dataTPA2<-dataTPA[, c(1,3)]
dataTPAcontin2 <- data.frame(t(dataTPA2[-1]))
colnames(dataTPAcontin2) <- dataTPA2[, 1]
val <- colnames(dataTPAcontin2)
dataTPAcontin <- dataTPAcontin2[,order(val)]


dataNTPA2<-dataNTPA[, c(1,3)]
dataNTPAcontin2 <- data.frame(t(dataNTPA2[-1]))
colnames(dataNTPAcontin2) <- dataNTPA2[, 1]
Nval <- colnames(dataNTPAcontin2)
dataNTPAcontin <- dataNTPAcontin2[,order(val)]



ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()

for (j in 1:length(dataTPAcontin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAcontin[,j], (sum(dataTPAcontin)-dataTPAcontin[,j]))

    dataNTcontin <- dataNTPAcontin
    
    tableNT<-cbind(dataNTPAcontin[,j], (sum(dataNTPAcontin)-dataNTPAcontin[,j]))
    
    CONTIN<-rbind(table,tableNT)
    row.names(CONTIN)<-c("T", "NT")
    
    p<-chisq.test(CONTIN)
    pvaluesPA<-c(pvaluesPA, p$p.value )
    p2<- fisher.test(CONTIN) 
    pvaluesPA2<-c(pvaluesPA2, p2$p.value)
    print(j)
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
}


#without resample
Resultschitest<-as.data.frame(ALLpvaluesPA) #chi.test
colnames(Resultschitest)<-names(dataNTPAcontin)
Resultschitest
CHITEST[y, c(1,2,3)]<-Resultschitest
names(CHITEST)[c(1,2,3)]<-colnames(Resultschitest)


ResultsFtest<-as.data.frame(ALLpvaluesPA2) #fiher test
colnames(ResultsFtest)<-names(dataNTPAcontin)
FTEST[y, c(1,2,3)]<-ResultsFtest
colnames(FTEST)[c(1,2,3)]<-colnames(ResultsFtest)
ResultsFtest



##With resample

ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()
percentpvalue<-c()
percentpvalue2<-c()


for (j in 1:length(dataTPAcontin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAcontin[,j], (sum(dataTPAcontin)-dataTPAcontin[,j]))
  
  for (i in 1:length(alldatabasesample[[y]])){
    dataNT<-ddply(as.data.frame(alldatabasesample[[y]][i]), c("RepGuild1"), summarise, nb=sum(nb2)) #pourchaque ?chantillon, on 'arrange en tableau de contingence
    dataNTcontin <- data.frame(t(dataNT[-1]))
    colnames(dataNTcontin) <- dataNT[, 1]
    
    if (is.null(dataNTcontin$bearers)==T){  ##if no column, put a zero
      dataNTcontin$bearers=0
    }
    if (is.null(dataNTcontin$guarders)==T){
      dataNTcontin$guarders=0
    }
    if (is.null(dataNTcontin$nonguarders)==T){
      dataNTcontin$nonguarders=0
    }
    
    val2 <- colnames(dataNTcontin)
    dataNTcontin <- dataNTcontin[,order(val2)]
    
    tableNT<-cbind(dataNTcontin[,j], (sum(dataNTcontin)-dataNTcontin[,j]))
    
    CONTIN<-rbind(table,tableNT)
    row.names(CONTIN)<-c("T", "NT")
    
    p<-chisq.test(CONTIN)
    pvaluesPA<-c(pvaluesPA, p$p.value )
    p2<- fisher.test(CONTIN) 
    pvaluesPA2<-c(pvaluesPA2, p2$p.value)
    print(i)
  }
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
  percentpvalue<-c(percentpvalue, sum(pvaluesPA < 0.05)/length(alldatabasesample[[y]]))
  percentpvalue2<-c(percentpvalue2, sum(pvaluesPA2 < 0.05)/length(alldatabasesample[[y]]))
  print(j)
}

warnings() 

#Percentage of positive tests per modality
Resultschitest<-as.data.frame(percentpvalue) #chi.test
row.names(Resultschitest)<-names(dataNTcontin)
Resultschitest
CHITESTpercent[y, c(1,2,3)]<-Resultschitest$percentpvalue
colnames(CHITESTpercent)[c(1,2,3)]<-rownames(Resultschitest)

ResultsFtest<-as.data.frame(percentpvalue2) #fiher test
row.names(ResultsFtest)<-names(dataNTcontin)
ResultsFtest
FTESTpercent[y, c(1,2,3)]<-ResultsFtest$percentpvalue2
names(FTESTpercent)[c(1,2,3)]<-rownames(ResultsFtest)



########tests per modality Maj.Diet##################
dataPA<-ddply(alldatabase[[y]], c("Maj.Diet", namecolumn[y]), summarise, nb=sum(nb))
dataTPA<-subset(dataPA, dataPA[,2]==1) #threatened
class(dataTPA$nb)<-"integer"
dataNTPA<-subset(dataPA, dataPA[,2]==0) #non-threatened



#Proportions
b<-(dataTPA$nb)/sum(dataTPA$nb)
dataTPA2<-cbind(dataTPA,b)
if (namecolumn[y]=="fisheries") {
  dataTPA2<-rbind(c("dedritus", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("dedritus", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
if (namecolumn[y]=="Accidental") {
  dataTPA2<-rbind(c("dedritus", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("dedritus", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
if (namecolumn[y]=="Diffusion") {
  dataTPA2<-rbind(c("dedritus", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("dedritus", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
# if (namecolumn[y]=="Sport.Angling") {
#   dataTPA2<-rbind(c("dedritus", 1, 0,0), dataTPA2)
#   dataTPA<-rbind( c("dedritus", 1, 0),dataTPA)
#   class(dataTPA$nb)<-"numeric" 
# }

#dataTPA2<-rbind(dataTPA2, c("detritus", 1, 0,0))
#dataTPA<-rbind(c("detritus", 1, 0), dataTPA)
#class(dataTPA$nb)<-"numeric"
dataTPA2



b<-dataNTPA$nb/sum(dataNTPA$nb)
dataNTPA2<-cbind(dataNTPA, b)
dataNTPA2


#Contingence table
dataTPA2<-dataTPA[, c(1,3)]
dataTPAin2 <- data.frame(t(dataTPA2[-1]))
colnames(dataTPAin2) <- dataTPA2[, 1]
val <- colnames(dataTPAin2)
dataTPAin <- dataTPAin2[,order(val)]

dataNTPA2<-dataNTPA2[, c(1,3)]
dataNTPAcontin2 <- data.frame(t(dataNTPA2[-1]))
colnames(dataNTPAcontin2) <- dataNTPA2[, 1]
Nval <- colnames(dataNTPAcontin2)
dataNTPAcontin <- dataNTPAcontin2[,order(val)]




ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()

for (j in 1:length(dataTPAin)) {
  pvaluesPA<-c() 
  pvaluesPA2<-c() 
  table<-cbind(dataTPAin[,j], (sum(dataTPAin)-dataTPAin[,j]))
  
  dataNTcontin <- dataNTPAcontin
  
  tableNT<-cbind(dataNTPAcontin[,j], (sum(dataNTPAcontin)-dataNTPAcontin[,j]))
  
  CONTIN<-rbind(table,tableNT)
  row.names(CONTIN)<-c("T", "NT")
  
  p<-chisq.test(CONTIN)
  pvaluesPA<-c(pvaluesPA, p$p.value )
  p2<- fisher.test(CONTIN) 
  pvaluesPA2<-c(pvaluesPA2, p2$p.value)
  print(j)
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
}


#Percentage of positive tests per modality
Resultschitest<-as.data.frame(ALLpvaluesPA) #chi.test
colnames(Resultschitest)<-names(dataNTPAcontin)
Resultschitest
CHITEST[y, c(4,5,6,7,8)]<-Resultschitest
names(CHITEST)[c(4,5,6,7,8)]<-colnames(Resultschitest)

ResultsFtest<-as.data.frame(ALLpvaluesPA2) #fiher test
colnames(ResultsFtest)<-names(dataNTPAcontin)
ResultsFtest
FTEST[y, c(4,5,6,7,8)]<-ResultsFtest
names(FTEST)[c(4,5,6,7,8)]<-colnames(ResultsFtest)


ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()
percentpvalue<-c()
percentpvalue2<-c()


for (j in 1:length(dataTPAin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAin[,j], (sum(dataTPAin)-dataTPAin[,j]))
  
  for (i in 1:length(alldatabasesample[[y]])){
    dataNT<-ddply(as.data.frame(alldatabasesample[[y]][i]), c("Maj.Diet"), summarise, nb=sum(nb2)) #pourchaque ?chantillon, on 'arrange en tableau de ingence
    dataNTin <- data.frame(t(dataNT[-1]))
    colnames(dataNTin) <- dataNT[, 1]
    
    if (is.null(dataNTin$detritus)==T){  ##if no column, put a zero
      dataNTin$detritus=0
    }
    if (is.null(dataNTin$nekton)==T){
      dataNTin$nekton=0
    }
    
    if (is.null(dataNTin$plants)==T){  #if no column, put a zero
      dataNTin$plants=0
    }
    if (is.null(dataNTin$zoobenthos)==T){
      dataNTin$zoobenthos=0
    }
    if (is.null(dataNTin$zooplankton )==T){
      dataNTin$zooplankton=0
    }
    
    val2 <- colnames(dataNTin)
    dataNTin <- dataNTin[,order(val2)]
    
    tableNT<-cbind(dataNTin[,j], (sum(dataNTin)-dataNTin[,j]))
    
    IN<-rbind(table,tableNT)
    row.names(IN)<-c("T", "NT")
    
    p<-chisq.test(IN)
    pvaluesPA<-c(pvaluesPA, p$p.value )
    p2<- fisher.test(IN) 
    pvaluesPA2<-c(pvaluesPA2, p2$p.value)
    print(i)
    
  }
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
  percentpvalue<-c(percentpvalue, sum(pvaluesPA < 0.05)/length(alldatabasesample[[y]]))
  percentpvalue2<-c(percentpvalue2, sum(pvaluesPA2 < 0.05)/length(alldatabasesample[[y]]))
  print(j)
}

warnings() 
#Percentage of positive tests per modality
Resultschitest<-as.data.frame(percentpvalue) #chi.test
row.names(Resultschitest)<-names(dataNTin)
Resultschitest
CHITESTpercent[y, c(4,5,6,7,8)]<-Resultschitest$percentpvalue
names(CHITESTpercent)[c(4,5,6,7,8)]<-rownames(Resultschitest)


ResultsFtest<-as.data.frame(percentpvalue2) #fiher test
row.names(ResultsFtest)<-names(dataNTin)
ResultsFtest
FTESTpercent[y, c(4,5,6,7,8)]<-ResultsFtest$percentpvalue2
names(FTESTpercent)[c(4,5,6,7,8)]<-rownames(ResultsFtest)





########tests per modality region majoritaire##################
dataPA<-ddply(alldatabase[[y]], c("Maj.Region", namecolumn[y]), summarise, nb=sum(nb))
dataTPA<-subset(dataPA, dataPA[,2]==1) #threatened
class(dataTPA$nb)<-"integer"
dataNTPA<-subset(dataPA, dataPA[,2]==0) #non-threatened



#Proportions
b<-(dataTPA$nb)/sum(dataTPA$nb)
dataTPA2<-cbind(dataTPA,b)
if (namecolumn[y]=="Species.control") {
  dataTPA2<-rbind(c("Australian", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("Australian", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
if (namecolumn[y]=="Accidental") {
  dataTPA2<-rbind(c("Australian", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("Australian", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}


dataTPA2


b<-dataNTPA$nb/sum(dataNTPA$nb)
dataNTPA2<-cbind(dataNTPA, b)
dataNTPA2



#Contingence table
dataTPA2<-dataTPA[, c(1,3)]
dataTPAcontin2 <- data.frame(t(dataTPA2[-1]))
colnames(dataTPAcontin2) <- dataTPA2[, 1]
val <- colnames(dataTPAcontin2)
dataTPAcontin <- dataTPAcontin2[,order(val)]

dataNTPA2<-dataNTPA[, c(1,3)]
dataNTPAcontin2 <- data.frame(t(dataNTPA2[-1]))
colnames(dataNTPAcontin2) <- dataNTPA2[, 1]
val <- colnames(dataNTPAcontin2)
dataNTPAcontin <- dataNTPAcontin2[,order(val)]


# table<-cbind(dataTPAcontin[,"Ethiopian"], (sum(dataTPAcontin)-dataTPAcontin[,"Ethiopian"]))
# table2<-cbind(dataNTPAcontin[,"Ethiopian"], (sum(dataNTPAcontin)-dataNTPAcontin[,"Ethiopian"]))
# CONTIN2<-rbind(table,table2)
# chisq.test(CONTIN2)
# fisher.test(CONTIN2)







ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()

for (j in 1:length(dataTPAcontin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAcontin[,j], (sum(dataTPAcontin)-dataTPAcontin[,j]))
  
  dataNTcontin <- dataNTPAcontin
  
  tableNT<-cbind(dataNTPAcontin[,j], (sum(dataNTPAcontin)-dataNTPAcontin[,j]))
  
  CONTIN<-rbind(table,tableNT)
  row.names(CONTIN)<-c("T", "NT")
  
  p<-chisq.test(CONTIN)
  pvaluesPA<-c(pvaluesPA, p$p.value )
  p2<- fisher.test(CONTIN) 
  pvaluesPA2<-c(pvaluesPA2, p2$p.value)
  print(j)
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
}


#Percentage of positive tests per modality
Resultschitest<-as.data.frame(ALLpvaluesPA) #chi.test
colnames(Resultschitest)<-names(dataNTPAcontin)
Resultschitest
# 
#   if (is.null(dataNTPAcontin$Australian)==T){  #if no column, put a zero
#     Resultschitest<- rbind(c(1,NA), Resultschitest)
#   }



CHITEST[y, c(9,10,11,12,13,14)]<-Resultschitest
names(CHITEST)[c(9,10,11,12,13,14)]<-colnames(Resultschitest)

ResultsFtest<-as.data.frame(ALLpvaluesPA2) #fiher test
colnames(ResultsFtest)<-names(dataNTPAcontin)
ResultsFtest
# if (is.null(dataNTPAcontin$Australian)==T){  #if no column, put a zero
#   ResultsFtest<- rbind(c(1,NA), ResultsFtest)
# }

FTEST[y, c(9,10,11,12,13,14)]<-ResultsFtest
names(FTEST)[c(9,10,11,12,13,14)]<-colnames(ResultsFtest)








ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()
percentpvalue<-c()
percentpvalue2<-c()


for (j in 1:length(dataTPAcontin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAcontin[,j], (sum(dataTPAcontin)-dataTPAcontin[,j]))
  
  for (i in 1:length(alldatabasesample[[y]])){
    dataNT<-ddply(as.data.frame(alldatabasesample[[y]][i]), c("Maj.Region"), summarise, nb=sum(nb2)) #pourchaque ?chantillon, on 'arrange en tableau de contingence
    dataNTcontin <- data.frame(t(dataNT[-1]))
    colnames(dataNTcontin) <- dataNT[, 1]
    
    if (is.null(dataNTcontin$Australian)==T){  #if no column, put a zero
      dataNTcontin$Australian=0
    }
    if (is.null(dataNTcontin$Ethiopian)==T){
      dataNTcontin$Ethiopian=0
    }
    # if (is.null(dataNTcontin$Madagascan)==T){
    #   dataNTcontin$Madagascan=0
    # }
    if (is.null(dataNTcontin$Nearctic)==T){  #if no column, put a zero
      dataNTcontin$Nearctic=0
    }
    if (is.null(dataNTcontin$Neotropical)==T){
      dataNTcontin$Neotropical=0
    }
    if (is.null(dataNTcontin$Palearctic)==T){
      dataNTcontin$Palearctic=0
    }
    if (is.null(dataNTcontin$`Sino-Oriental` )==T){
      dataNTcontin$`Sino-Oriental`=0
    }
    
    val2 <- colnames(dataNTcontin)
    dataNTcontin <- dataNTcontin[,order(val2)]
    
    tableNT<-cbind(dataNTcontin[,j], (sum(dataNTcontin)-dataNTcontin[,j]))
    
    CONTIN<-rbind(table,tableNT)
    row.names(CONTIN)<-c("T", "NT")
    
    p<-chisq.test(CONTIN)
    pvaluesPA<-c(pvaluesPA, p$p.value )
    p2<- fisher.test(CONTIN) 
    pvaluesPA2<-c(pvaluesPA2, p2$p.value)
    print(i)
    
  }
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
  percentpvalue<-c(percentpvalue, sum(pvaluesPA < 0.05)/length(alldatabasesample[[y]]))
  percentpvalue2<-c(percentpvalue2, sum(pvaluesPA2 < 0.05)/length(alldatabasesample[[y]]))
  print(j)
}

warnings() 
#Percentage of positive tests per modality
Resultschitest<-as.data.frame(percentpvalue) #chi.test
row.names(Resultschitest)<-names(dataNTcontin)
Resultschitest
CHITESTpercent[y, c(9,10,11,12,13,14)]<-Resultschitest$percentpvalue
names(CHITESTpercent)[c(9,10,11,12,13,14)]<-rownames(Resultschitest)

ResultsFtest<-as.data.frame(percentpvalue2) #fiher test
row.names(ResultsFtest)<-names(dataNTcontin)
ResultsFtest
FTESTpercent[y, c(9,10,11,12,13,14)]<-ResultsFtest$percentpvalue2
names(FTESTpercent)[c(9,10,11,12,13,14)]<-rownames(ResultsFtest)










########tests per modality human use##################
head(alldatabase[[y]])
dataFNPC<-ddply(alldatabase[[y]], c("Used_by_humans", namecolumn[y]), summarise, nb=sum(nb))
dataTPC<-subset(dataFNPC, dataPA[,2]==1) #threatened
dataNTPC<-subset(dataFNPC, dataPA[,2]==0) #non-threatened



#Contingence table (threatened)
dataTPC2<-dataTPC[, c(1,3)]
dataTPCcontin <- data.frame(t(dataTPC2[-1]))
colnames(dataTPCcontin) <- dataTPC2[, 1]




dataPA<-ddply(alldatabase[[y]], c("Used_by_humans", namecolumn[y]), summarise, nb=sum(nb))
dataTPA<-subset(dataPA, dataPA[,2]==1) #threatened
class(dataTPA$nb)<-"integer"
dataNTPA<-subset(dataPA, dataPA[,2]==0) # non threatened



#Proportions
b<-(dataTPA$nb)/sum(dataTPA$nb)
dataTPA2<-cbind(dataTPA,b)
dataTPA2<-cbind(dataTPA,b)
if (namecolumn[y]=="Species.control") {
  dataTPA2<-rbind(c("0", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("0", 1, 0), dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
if (namecolumn[y]=="fisheries") {
  dataTPA2<-rbind(c("0", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("0", 1, 0), dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
if (namecolumn[y]=="Sport.Angling") {
  dataTPA2<-rbind(c("0", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("0", 1, 0), dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
dataTPA2

# > dataTPA2
# Used_by_humans Introduced  nb               b
# 2              0          1  14 0.0401146131805
# 4              1          1 335 0.9598853868195


b<-dataNTPA$nb/sum(dataNTPA$nb)
dataNTPA2<-cbind(dataNTPA, b)
dataNTPA2

# > dataNTPA2
# 1              0          0 177 0.332082551595
# 3              1          0 356 0.667917448405


#Contingence table
dataTPA2<-dataTPA[, c(1,3)]
dataTPAcontin2 <- data.frame(t(dataTPA2[-1]))
colnames(dataTPAcontin2) <- dataTPA2[, 1]
val <- colnames(dataTPAcontin2)
dataTPAcontin <- dataTPAcontin2[,order(val)]

dataNTPA2<-dataNTPA[, c(1,3)]
dataNTPAcontin2 <- data.frame(t(dataNTPA2[-1]))
colnames(dataNTPAcontin2) <- dataNTPA2[, 1]
Nval <- colnames(dataNTPAcontin2)
dataNTPAcontin <- dataNTPAcontin2[,order(val)]



ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()

for (j in 1:length(dataTPAcontin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAcontin[,j], (sum(dataTPAcontin)-dataTPAcontin[,j]))
  
  dataNTcontin <- dataNTPAcontin
  
  tableNT<-cbind(dataNTPAcontin[,j], (sum(dataNTPAcontin)-dataNTPAcontin[,j]))
  
  CONTIN<-rbind(table,tableNT)
  row.names(CONTIN)<-c("T", "NT")
  
  p<-chisq.test(CONTIN)
  pvaluesPA<-c(pvaluesPA, p$p.value )
  p2<- fisher.test(CONTIN) 
  pvaluesPA2<-c(pvaluesPA2, p2$p.value)
  print(j)
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
}


#Percentage of positive tests per modality
Resultschitest<-as.data.frame(ALLpvaluesPA) #chi.test
colnames(Resultschitest)<-names(dataNTPAcontin)
Resultschitest
CHITEST[y, c(15,16)]<-Resultschitest
names(CHITEST)[c(15,16)]<-colnames(Resultschitest)

ResultsFtest<-as.data.frame(ALLpvaluesPA2) #fiher test
colnames(ResultsFtest)<-names(dataNTPAcontin)
ResultsFtest
FTEST[y, c(15,16)]<-ResultsFtest
names(FTEST)[c(15,16)]<-colnames(ResultsFtest)



ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()
percentpvalue<-c()
percentpvalue2<-c()


for (j in 1:length(dataTPAcontin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAcontin[,j], (sum(dataTPAcontin)-dataTPAcontin[,j]))
  
  for (i in 1:length(alldatabasesample[[y]])){
    dataNT<-ddply(as.data.frame(alldatabasesample[[y]][i]), c("Used_by_humans"), summarise, nb=sum(nb2)) #pourchaque ?chantillon, on 'arrange en tableau de contingence
    dataNTcontin <- data.frame(t(dataNT[-1]))
    colnames(dataNTcontin) <- dataNT[, 1]
    
    if (is.null(dataNTcontin$'1')==T){  #if no column, put a zero
      dataNTcontin$'1'=0
    }
    if (is.null(dataNTcontin$'0')==T){
      dataNTcontin$'0'=0
    }
    
    val2 <- colnames(dataNTcontin)
    dataNTcontin <- dataNTcontin[,order(val2)]
    
    tableNT<-cbind(dataNTcontin[,j], (sum(dataNTcontin)-dataNTcontin[,j]))
    
    CONTIN<-rbind(table,tableNT)
    row.names(CONTIN)<-c("T", "NT")
    
    p<-chisq.test(CONTIN)
    pvaluesPA<-c(pvaluesPA, p$p.value )
    p2<- fisher.test(CONTIN) 
    pvaluesPA2<-c(pvaluesPA2, p2$p.value)
    print(i)
    
  }
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
  percentpvalue<-c(percentpvalue, sum(pvaluesPA < 0.05)/length(alldatabasesample[[y]]))
  percentpvalue2<-c(percentpvalue2, sum(pvaluesPA2 < 0.05)/length(alldatabasesample[[y]]))
  print(j)
}

warnings() 

#Percentage of positive tests per modality
Resultschitest<-as.data.frame(percentpvalue) #chi.test
row.names(Resultschitest)<-names(dataNTcontin)
Resultschitest
CHITESTpercent[y, c(15,16)]<-Resultschitest$percentpvalue
names(CHITESTpercent)[c(15,16)]<-rownames(Resultschitest)


ResultsFtest<-as.data.frame(percentpvalue2) #fiher test
row.names(ResultsFtest)<-names(dataNTcontin)
ResultsFtest
FTESTpercent[y, c(15,16)]<-ResultsFtest$percentpvalue2
names(FTESTpercent)[c(15,16)]<-rownames(ResultsFtest)





########tests per modality Order##################
dataPA<-ddply(alldatabase[[y]], c("Order", namecolumn[y]), summarise, nb=sum(nb))
dataTPA<-subset(dataPA, dataPA[,2]==1) #threatened
class(dataTPA$nb)<-"integer"
dataNTPA<-subset(dataPA, dataPA[,2]==0) #non-threatened



#Proportions
b<-(dataTPA$nb)/sum(dataTPA$nb)
dataTPA2<-cbind(dataTPA,b)
if (namecolumn[y]=="Species.control") {
  dataTPA2<-rbind(c("Characiformes", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("Characiformes", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
if (namecolumn[y]=="Sport.Angling") {
  dataTPA2<-rbind(c("Characiformes", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("Characiformes", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
if (namecolumn[y]=="Diffusion") {
  dataTPA2<-rbind(c("Characiformes", 1, 0,0), dataTPA2)
  dataTPA<-rbind( c("Characiformes", 1, 0),dataTPA)
  class(dataTPA$nb)<-"numeric" 
}
dataTPA2


b<-dataNTPA$nb/sum(dataNTPA$nb)
dataNTPA2<-cbind(dataNTPA, b)
dataNTPA2


#Contingence table
dataTPA2<-dataTPA[, c(1,3)]
dataTPAin2 <- data.frame(t(dataTPA2[-1]))
colnames(dataTPAin2) <- dataTPA2[, 1]
val <- colnames(dataTPAin2)
dataTPAin <- dataTPAin2[,order(val)]

dataNTPA2<-dataNTPA2[, c(1,3)]
dataNTPAcontin2 <- data.frame(t(dataNTPA2[-1]))
colnames(dataNTPAcontin2) <- dataNTPA2[, 1]
Nval <- colnames(dataNTPAcontin2)
dataNTPAcontin <- dataNTPAcontin2[,order(val)]




ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()

for (j in 1:length(dataTPAin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAin[,j], (sum(dataTPAin)-dataTPAin[,j]))
  
  dataNTcontin <- dataNTPAcontin
  
  tableNT<-cbind(dataNTPAcontin[,j], (sum(dataNTPAcontin)-dataNTPAcontin[,j]))
  
  CONTIN<-rbind(table,tableNT)
  row.names(CONTIN)<-c("T", "NT")
  
  p<-chisq.test(CONTIN)
  pvaluesPA<-c(pvaluesPA, p$p.value )
  p2<- fisher.test(CONTIN) 
  pvaluesPA2<-c(pvaluesPA2, p2$p.value)
  print(j)
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
}


#Percentage of positive tests per modality
Resultschitest<-as.data.frame(ALLpvaluesPA) #chi.test
colnames(Resultschitest)<-names(dataNTPAcontin)
Resultschitest
CHITEST[y, c(17,18,19,20,21,22)]<-Resultschitest
names(CHITEST)[c(17,18,19,20,21,22)]<-colnames(Resultschitest)

ResultsFtest<-as.data.frame(ALLpvaluesPA2) #fiher test
colnames(ResultsFtest)<-names(dataNTPAcontin)
ResultsFtest
FTEST[y, c(17,18,19,20,21,22)]<-ResultsFtest
names(FTEST)[c(17,18,19,20,21,22)]<-colnames(ResultsFtest)


##pour chaque ?chantillon, tableau de ingence puis test de chi2
#Dans certaines cat?giries on a des 0 parfois : emp?che de faire un chi2!! ==> test exact de fisher ? la place?

ALLpvaluesPA<-list()
ALLpvaluesPA2<-list()
percentpvalue<-c()
percentpvalue2<-c()


for (j in 1:length(dataTPAin)) {
  pvaluesPA<-c() #for khi2 test
  pvaluesPA2<-c() #for fisher test
  table<-cbind(dataTPAin[,j], (sum(dataTPAin)-dataTPAin[,j]))
  
  for (i in 1:length(alldatabasesample[[y]])){
    dataNT<-ddply(as.data.frame(alldatabasesample[[y]][i]), c("Order"), summarise, nb=sum(nb2)) #pourchaque ?chantillon, on 'arrange en tableau de ingence
    dataNTin <- data.frame(t(dataNT[-1]))
    colnames(dataNTin) <- dataNT[, 1]
    
    if (is.null(dataNTin$Characiformes)==T){  #if no column, put a zero
      dataNTin$Characiformes=0
    }
    if (is.null(dataNTin$Cypriniformes)==T){
      dataNTin$Cypriniformes=0
    }
    
    if (is.null(dataNTin$Cyprinodontiformes)==T){  ##if no column, put a zero
      dataNTin$Cyprinodontiformes=0
    }
    if (is.null(dataNTin$Other)==T){
      dataNTin$Other=0
    }
    if (is.null(dataNTin$Perciformes)==T){
      dataNTin$Perciformes=0
    }
    if (is.null(dataNTin$Siluriformes )==T){
      dataNTin$Siluriformes=0
    }
    
    val2 <- colnames(dataNTin)
    dataNTin <- dataNTin[,order(val2)]
    
    tableNT<-cbind(dataNTin[,j], (sum(dataNTin)-dataNTin[,j]))
    
    IN<-rbind(table,tableNT)
    row.names(IN)<-c("T", "NT")
    
    p<-chisq.test(IN)
    pvaluesPA<-c(pvaluesPA, p$p.value )
    p2<- fisher.test(IN) 
    pvaluesPA2<-c(pvaluesPA2, p2$p.value)
    print(i)
    
  }
  ALLpvaluesPA<-append(ALLpvaluesPA, list(pvaluesPA))
  ALLpvaluesPA2<-append(ALLpvaluesPA2, list(pvaluesPA2))
  percentpvalue<-c(percentpvalue, sum(pvaluesPA < 0.05)/length(alldatabasesample[[y]]))
  percentpvalue2<-c(percentpvalue2, sum(pvaluesPA2 < 0.05)/length(alldatabasesample[[y]]))
  print(j)
}

warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
#les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif

#Percentage of positive tests per modality
Resultschitest<-as.data.frame(percentpvalue) #chi.test
row.names(Resultschitest)<-names(dataNTin)
Resultschitest
CHITESTpercent[y, c(17,18,19,20,21,22)]<-Resultschitest$percentpvalue
names(CHITESTpercent)[c(17,18,19,20,21,22)]<-rownames(Resultschitest)

ResultsFtest<-as.data.frame(percentpvalue2) #fiher test
row.names(ResultsFtest)<-names(dataNTin)
ResultsFtest
FTESTpercent[y, c(17,18,19,20,21,22)]<-ResultsFtest$percentpvalue2
names(FTESTpercent)[c(17,18,19,20,21,22)]<-rownames(ResultsFtest)



}


# ###saved data ###
# write.csv2(as.data.frame(CHITEST), "./outputs/CHITEST.csv")
# write.csv2(as.data.frame(FTEST), "./outputs/FTEST.csv")
# 
# write.csv2(as.data.frame(CHITESTpercent), "./outputs/CHITESTpercent.csv")
# write.csv2(as.data.frame(FTESTpercent), "./outputs/FTESTpercent.csv")
# 





























#####WILCOXON TESTS#####
######DATAFRAME PREPARATION####

WTEST<-data.frame(matrix(ncol = 12, nrow = 8))
rownames(WTEST)<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(WTEST)<-c("Amplitudetemp","Amplitudetemp_W", "BlBd", "BlBd_W", "TL", "TL_W","NbDiet","NbDiet_W", "NbNativeRegion","NbNativeRegion_W", "PFiBd", "PFiBd_W")
WTESTpercent<-data.frame(matrix(ncol = 6, nrow = 8))
rownames(WTESTpercent)<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(WTESTpercent)<-c("Amplitudetemp","BlBd","TL", "NbDiet","NbNativeRegion","PFiBd")
shapirotest<-c()
shapirotestnotintro<-c()
#######Wilcoxon LOOP####


  

for (y in 1:length(rownames(WTEST))){

################Amplitude de temperature##########
#Normality test for each modality to choose the test
s<-shapiro.test(log(AllIntro[[y]]$Amplitudetemp+1))
shapirotest<-c(shapirotest, s$p.value)

ggqqplot(log(AllIntro[[y]]$Amplitudetemp+1))
hist(log(AllIntro[[y]]$Amplitudetemp+1))
s2<-shapiro.test(log(AllNotIntro[[y]]$Amplitudetemp+1))
shapirotestnotintro<-c(shapirotestnotintro, s2$p.value)

ggqqplot(log(AllNotIntro[[y]]$Amplitudetemp+1))#####none of the modalities are normaly distributed --> Mann whitney test
hist(log(AllNotIntro[[y]]$Amplitudetemp+1))
hist(AllNotIntro[[y]]$Amplitudetemp+1)

Amptem<-wilcox.test(log(AllIntro[[y]]$Amplitudetemp+1), 
                       log(AllNotIntro[[y]]$Amplitudetemp+1))
Amptem


WTEST[y, c(1,2)]<-c(Amptem$p.value, Amptem$statistic)



####en faisant avec les samples
ALLpvaluesPA<-list()
percentpvalue<-c()

for (i in 1:length(alldatabasesample[[y]])){
  dataNT<-as.data.frame(alldatabasesample[[y]][i])$Amplitudetemp #pourchaque ?chantillon, on 'arrange en tableau de contingence
  
  p<-wilcox.test(log(AllIntro[[y]]$Amplitudetemp+1), 
                 log(dataNT+1))
  ALLpvaluesPA<-append(ALLpvaluesPA, list( p$p.value))#c(ALLpvaluesPA, p$p.value )
  print(i)
  
}

percentpvalue<-c(percentpvalue, sum(ALLpvaluesPA< 0.05)/length(alldatabasesample[[y]]))


warnings() 

#Percentage of positive tests per modality
Resultswilcox<-as.data.frame(percentpvalue) #chi.test
Resultswilcox
WTESTpercent[y, c(1)]<-c(Resultswilcox)

# x11()
# plot(log10(no.na.data$Introduced), no.na.data$Amplitudetemp)





############BlBd##########################
#Normality test for each modality to choose the test
s<-shapiro.test(log(AllIntro[[y]]$BlBd+1))
shapirotest<-c(shapirotest, s$p.value)
ggqqplot(log(AllIntro[[y]]$BlBd+1))
hist(log(AllIntro[[y]]$BlBd+1))
s2<-shapiro.test(log(AllNotIntro[[y]]$BlBd+1))
shapirotestnotintro<-c(shapirotestnotintro, s2$p.value)
ggqqplot(log(AllNotIntro[[y]]$BlBd+1))#####none of the modalities are normaly distributed --> Mann whitney test
hist(log(AllNotIntro[[y]]$BlBd+1))
hist(AllNotIntro[[y]]$BlBd+1)

blbd1<-wilcox.test(log(AllIntro[[y]]$BlBd+1), 
                      log(AllNotIntro[[y]]$BlBd+1))
blbd1
WTEST[y, c(3,4)]<-c(blbd1$p.value, blbd1$statistic)


####en faisant avec les samples : 
ALLpvaluesPA<-list()
percentpvalue<-c()

  for (i in 1:length(alldatabasesample[[y]])){
    dataNT<-as.data.frame(alldatabasesample[[y]][i])$BlBd #pourchaque ?chantillon, on 'arrange en tableau de contingence
    
    p<-wilcox.test(log(AllIntro[[y]]$BlBd+1), 
                    log(dataNT+1))
    ALLpvaluesPA<-append(ALLpvaluesPA, list( p$p.value))#c(ALLpvaluesPA, p$p.value )
    print(i)
    
  }

percentpvalue<-c(percentpvalue, sum(ALLpvaluesPA< 0.05)/length(alldatabasesample[[y]]))


warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
#les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif

#Percentage of positive tests per modality
Resultswilcox<-as.data.frame(percentpvalue) #chi.test
Resultswilcox
WTESTpercent[y, c(2)]<-c(Resultswilcox)










############Total length##########################
#Normality test for each modality to choose the test
s<-shapiro.test(log(AllIntro[[y]]$TL +1))
shapirotest<-c(shapirotest, s$p.value)
ggqqplot(log(AllIntro[[y]]$TL+1))
hist(log(AllIntro[[y]]$TL+1))
s2<-shapiro.test(log(AllNotIntro[[y]]$TL +1))
shapirotestnotintro<-c(shapirotestnotintro, s2$p.value)
ggqqplot(log(AllNotIntro[[y]]$TL+1))#####none of the modalities are normaly distributed --> Mann whitney test
hist(log(AllNotIntro[[y]]$TL+1))
hist(AllNotIntro[[y]]$TL+1)

TL1<-wilcox.test(log(AllIntro[[y]]$TL +1),
                 log(AllNotIntro[[y]]$TL +1))
TL1
WTEST[y, c(5,6)]<-c(TL1$p.value, TL1$statistic)

####en faisant avec les samples :
ALLpvaluesPA<-list()
percentpvalue<-c()

for (i in 1:length(alldatabasesample[[y]])){
  dataNT<-as.data.frame(alldatabasesample[[y]][i])$TL #pourchaque ?chantillon, on 'arrange en tableau de contingence
  
  p<-wilcox.test(log(AllIntro[[y]]$TL +1),
                 log(dataNT +1))
  ALLpvaluesPA<-append(ALLpvaluesPA, list( p$p.value))#c(ALLpvaluesPA, p$p.value )
  print(i)
  
}

percentpvalue<-c(percentpvalue, sum(ALLpvaluesPA< 0.05)/length(alldatabasesample[[y]]))


warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
#les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif

#Percentage of positive tests per modality
Resultswilcox<-as.data.frame(percentpvalue) #chi.test
Resultswilcox
WTESTpercent[y, c(3)]<-c(Resultswilcox)






###########Nb.Diet##########################
#Normality test for each modality to choose the test
s<-shapiro.test(log(AllIntro[[y]]$Nb.Diet +1))
shapirotest<-c(shapirotest, s$p.value)
ggqqplot(log(AllIntro[[y]]$Nb.Diet+1))
hist(log(AllIntro[[y]]$Nb.Diet+1))
s2<-shapiro.test(log(AllNotIntro[[y]]$Nb.Diet +1))
shapirotestnotintro<-c(shapirotestnotintro, s2$p.value)
ggqqplot(log(AllNotIntro[[y]]$Nb.Diet+1))#####none of the modalities are normaly distributed --> Mann whitney test
hist(log(AllNotIntro[[y]]$Nb.Diet+1))
hist(AllNotIntro[[y]]$Nb.Diet+1)

Nb.Diet1<-wilcox.test(log(AllIntro[[y]]$Nb.Diet +1),
                      log(AllNotIntro[[y]]$Nb.Diet +1))
Nb.Diet1
WTEST[y, c(7,8)]<-c(Nb.Diet1$p.value, Nb.Diet1$statistic)
####en faisant avec les samples :
ALLpvaluesPA<-list()
percentpvalue<-c()

for (i in 1:length(alldatabasesample[[y]])){
  dataNT<-as.data.frame(alldatabasesample[[y]][i])$Nb.Diet #pourchaque ?chantillon, on 'arrange en tableau de ingence
  
  p<-wilcox.test(log(AllIntro[[y]]$Nb.Diet +1),
                 log(dataNT +1))
  ALLpvaluesPA<-append(ALLpvaluesPA, list( p$p.value))#c(ALLpvaluesPA, p$p.value )
  print(i)
  
}

percentpvalue<-c(percentpvalue, sum(ALLpvaluesPA< 0.05)/length(alldatabasesample[[y]]))


warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
#les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif

#Percentage of positive tests per modality
Resultswilcox<-as.data.frame(percentpvalue) #chi.test
Resultswilcox
WTESTpercent[y, c(4)]<-c(Resultswilcox)





############Nb.Native.Region##########################
#Normality test for each modality to choose the test
s<-shapiro.test(log(AllIntro[[y]]$Nb.Native.Region +1))
shapirotest<-c(shapirotest, s$p.value)
ggqqplot(log(AllIntro[[y]]$Nb.Native.Region+1))
hist(log(AllIntro[[y]]$Nb.Native.Region+1))
s2<-shapiro.test(log(AllNotIntro[[y]]$Nb.Native.Region +1))
shapirotestnotintro<-c(shapirotestnotintro, s2$p.value)
ggqqplot(log(AllNotIntro[[y]]$Nb.Native.Region+1))#####none of the modalities are normaly distributed --> Mann whitney test
hist(log(AllNotIntro[[y]]$Nb.Native.Region+1))
hist(AllNotIntro[[y]]$Nb.Native.Region+1)

Nb.Native.Region1<-wilcox.test(log(AllIntro[[y]]$Nb.Native.Region +1),
                               log(AllNotIntro[[y]]$Nb.Native.Region +1))
Nb.Native.Region1
WTEST[y, c(9,10)]<-c(Nb.Native.Region1$p.value, Nb.Native.Region1$statistic)
####en faisant avec les samples :
ALLpvaluesPA<-list()
percentpvalue<-c()

for (i in 1:length(alldatabasesample[[y]])){
  dataNT<-as.data.frame(alldatabasesample[[y]][i])$Nb.Native.Region #pourchaque ?chantillon, on 'arrange en tableau de ingence
  
  p<-wilcox.test(log(AllIntro[[y]]$Nb.Native.Region +1),
                 log(dataNT +1))
  ALLpvaluesPA<-append(ALLpvaluesPA, list( p$p.value))#c(ALLpvaluesPA, p$p.value )
  print(i)
  
}

percentpvalue<-c(percentpvalue, sum(ALLpvaluesPA< 0.05)/length(alldatabasesample[[y]]))


warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
#les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif

#Percentage of positive tests per modality
Resultswilcox<-as.data.frame(percentpvalue) #chi.test
Resultswilcox
WTESTpercent[y, c(5)]<-c(Resultswilcox)










# ############PFiBd##########################
#Normality test for each modality to choose the test
s<-shapiro.test(log(AllIntro[[y]]$PFiBd+1))

shapirotest<-c(shapirotest, s$p.value)
ggqqplot(log(AllIntro[[y]]$PFiBd+1))
hist(log(AllIntro[[y]]$PFiBd+1))
s2<-shapiro.test(log(AllNotIntro[[y]]$PFiBd+1))
shapirotestnotintro<-c(shapirotestnotintro, s2$p.value)

ggqqplot(log(AllNotIntro[[y]]$PFiBd+1))#####none of the modalities are normaly distributed --> Mann whitney test
hist(log(AllNotIntro[[y]]$PFiBd+1))
hist(AllNotIntro[[y]]$PFiBd+1)

PFiBd1<-wilcox.test(log(AllIntro[[y]]$PFiBd+1),
                   log(AllNotIntro[[y]]$PFiBd+1))
PFiBd1
WTEST[y, c(11,12)]<-c(PFiBd1$p.value, PFiBd1$statistic)

####en faisant avec les samples :
ALLpvaluesPA<-list()
percentpvalue<-c()

for (i in 1:length(alldatabasesample[[y]])){
  dataNT<-as.data.frame(alldatabasesample[[y]][i])$PFiBd #pourchaque ?chantillon, on 'arrange en tableau de contingence

  p<-wilcox.test(log(AllIntro[[y]]$PFiBd+1),
                 log(dataNT+1))
  ALLpvaluesPA<-append(ALLpvaluesPA, list( p$p.value))#c(ALLpvaluesPA, p$p.value )
  print(i)

}

percentpvalue<-c(percentpvalue, sum(ALLpvaluesPA< 0.05)/length(alldatabasesample[[y]]))


warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
#les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif

#Percentage of positive tests per modality
Resultswilcox<-as.data.frame(percentpvalue) #chi.test
Resultswilcox
WTESTpercent[y, c(6)]<-c(Resultswilcox)



}


####save data#####
shapirotest
shapirotestnotintro

write.csv2(as.data.frame(WTESTpercent), "./outputs/WTESTpercent.csv")
write.csv2(as.data.frame(WTEST), "./outputs/WTEST.csv")










####figure
x11()
ggplot(no.na.data, aes(x=log10(BlBd+1), y=Introduced)) +
  geom_boxplot(notch=F)

x11()
ggplot(no.na.data, aes(x=log10( Nb.Diet +1), y=Introduced)) +
  geom_boxplot(notch=F)

x11()
ggplot(no.na.data, aes(x=log10( Amplitudetemp +1), y=Introduced)) +
  geom_boxplot(notch=TRUE)

x11()
ggplot(no.na.data, aes(x=log10( Amplitudetemp +1), y=Introduced)) +
  geom_boxplot(notch=TRUE)


x11()
ggplot(no.na.data, aes(x=Nb.Native.Region +1, y=Introduced)) +
  geom_boxplot(notch=TRUE)
mean(no.na.data$Nb.Native.Region)
mean(Intro$Nb.Native.Region)
mean(NotIntro$Nb.Native.Region)







































































































