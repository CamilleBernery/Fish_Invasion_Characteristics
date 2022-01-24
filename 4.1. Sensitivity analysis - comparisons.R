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
INTRO3<-read.csv2("./outputs/INTRO_all_good2.csv")
SENSDIET<-readRDS("./outputs/sensitivity-analysis/SENSDIET.rds")
identical(SENSDIET[[1]], SENSDIET[[2]]) ###verify that database are different
setdiff(SENSDIET[[1]], SENSDIET[[100]])

SENSREG<-readRDS("./outputs/sensitivity-analysis/SENSREG.rds")

INTRO3<-subset(INTRO3, select=-c(Maj.Diet, Maj.Region))


CHITEST<-list()
CHITESTpercent<-list()
FTEST<-list()
FTESTpercent<-list()
for (sd in 1:100) {
  


########INTRODUCTION -- KHI2######
  
  INTRO2<-merge(INTRO3, SENSDIET[[sd]], by="Species", all.x=T)
  INTRO<-merge(INTRO2, SENSREG[[sd]], by="Species", all.x=T)
  
  
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntro<-subset(nonadata, Introduced==0)
AllNotIntro[[1]]<-NotIntro
Intro<-subset(nonadata, Introduced==1)
AllIntro[[1]]<-Intro
nb2<-rep(1, length(NotIntro[,1])) #pour pouvoir faire le tableau de contingence des NT (non threatened) après
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntroaqua<-subset(nonadataaqua, Aquaculture==0)
Introaqua<-subset(nonadataaqua, Aquaculture==1)
AllNotIntro[[2]]<-NotIntroaqua
AllIntro[[2]]<-Introaqua
nb2<-rep(1, length(NotIntroaqua[,1])) #pour pouvoir faire le tableau de aquaingence des NT (non threatened) après
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntrocont<-subset(nonadatacont, Species.control==0)
Introcont<-subset(nonadatacont, Species.control==1)
AllNotIntro[[3]]<-NotIntrocont
AllIntro[[3]]<-Introcont
nb2<-rep(1, length(NotIntrocont[,1])) #pour pouvoir faire le tableau de contingence des NT (non threatened) après
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntroorna<-subset(nonadataorna, ornamental==0)
Introorna<-subset(nonadataorna, ornamental==1)
AllNotIntro[[4]]<-NotIntroorna
AllIntro[[4]]<-Introorna

nb2<-rep(1, length(NotIntroorna[,1])) #pour pouvoir faire le tableau de contingence des NT (non threatened) après
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntrofish<-subset(nonadatafish, fisheries==0)
Introfish<-subset(nonadatafish, fisheries==1)
AllNotIntro[[5]]<-NotIntrofish
AllIntro[[5]]<-Introfish
nb2<-rep(1, length(NotIntrofish[,1])) #pour pouvoir faire le tableau de contingence des NT (non threatened) après
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntroacci<-subset(nonadataacci, Accidental==0)
Introacci<-subset(nonadataacci, Accidental==1)
AllNotIntro[[6]]<-NotIntroacci
AllIntro[[6]]<-Introacci
nb2<-rep(1, length(NotIntroacci[,1])) #pour pouvoir faire le tableau de contingence des NT (non threatened) après
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntrospor<-subset(nonadataspor, Sport.Angling==0)
Introspor<-subset(nonadataspor, Sport.Angling==1)
AllNotIntro[[7]]<-NotIntrospor
AllIntro[[7]]<-Introspor
nb2<-rep(1, length(NotIntrospor[,1])) #pour pouvoir faire le tableau de contingence des NT (non threatened) après
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
set.seed(123) #pour avoir toujours le même aléatoire
NotIntrodiff<-subset(nonadatadiff, Diffusion==0)
Introdiff<-subset(nonadatadiff, Diffusion==1)
AllNotIntro[[8]]<-NotIntrodiff
AllIntro[[8]]<-Introdiff
nb2<-rep(1, length(NotIntrodiff[,1])) #pour pouvoir faire le tableau de contingence des NT (non threatened) après
NotIntro1diff<-cbind(NotIntrodiff, nb2)
listofNotIntrodiff <-replicate(999, NotIntro1diff %>% sample_n(length(Introdiff[,1])), simplify=F)
alldatabasesample[[8]]<-listofNotIntrodiff



namecolumn<-c("Introduced", "Aquaculture", "Species.control", "ornamental", "fisheries", "Accidental", "Sport.Angling", "Diffusion")



####DATAFRAMES PREPARATION #####

CHITEST[[sd]]<-data.frame(matrix(ncol = 11, nrow = 8))
rownames(CHITEST[[sd]])<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(CHITEST[[sd]])<-c("detritus","nekton", "plants",
                     "zoobenthos","detritus" ,"Australian",      
                     "Ethiopian","Nearctic","Neotropical",       
                     "Palearctic","Sino-Oriental")
FTEST[[sd]]<-data.frame(matrix(ncol = 11, nrow = 8))
rownames(FTEST[[sd]])<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(FTEST[[sd]])<-c("detritus","nekton", "plants",
                   "zoobenthos","detritus" ,"Australian",      
                   "Ethiopian","Nearctic","Neotropical",       
                   "Palearctic","Sino-Oriental")

CHITESTpercent[[sd]]<-data.frame(matrix(ncol = 11, nrow = 8))
rownames(CHITESTpercent[[sd]])<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(CHITESTpercent[[sd]])<-c("detritus","nekton", "plants",
                            "zoobenthos","detritus" ,"Australian",      
                            "Ethiopian","Nearctic","Neotropical",       
                            "Palearctic","Sino-Oriental")

FTESTpercent[[sd]]<-data.frame(matrix(ncol = 11, nrow = 8))
rownames(FTESTpercent[[sd]])<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")
colnames(FTESTpercent[[sd]])<-c("detritus","nekton", "plants",
                          "zoobenthos","detritus" ,"Australian",      
                          "Ethiopian","Nearctic","Neotropical",       
                          "Palearctic","Sino-Oriental")

#######KHI 2 LOOP####
for (y in 1:length(alldatabase)){
  
    ########tests par modalité Maj.Diet##################
  dataPA<-ddply(alldatabase[[y]], c("Maj.Diet", namecolumn[y]), summarise, nb=sum(nb))
  dataTPA<-subset(dataPA, dataPA[,2]==1) #juste les threatened
  class(dataTPA$nb)<-"integer"
  dataNTPA<-subset(dataPA, dataPA[,2]==0) #juste les threatened
  
  
  
  #Proportions
  b<-(dataTPA$nb)/sum(dataTPA$nb)
  dataTPA2<-cbind(dataTPA,b)
  if (dataTPA$Maj.Diet[1]=="nekton") {
    dataTPA2<-rbind(c("detritus", 1, 0,0), dataTPA2)
    dataTPA<-rbind( c("detritus", 1, 0),dataTPA)
    class(dataTPA$nb)<-"numeric" 
  }
  # if (namecolumn[y]=="Accidental") {
  #   dataTPA2<-rbind(c("dedritus", 1, 0,0), dataTPA2)
  #   dataTPA<-rbind( c("dedritus", 1, 0),dataTPA)
  #   class(dataTPA$nb)<-"numeric" 
  # }
  # if (namecolumn[y]=="Diffusion") {
  #   dataTPA2<-rbind(c("dedritus", 1, 0,0), dataTPA2)
  #   dataTPA<-rbind( c("dedritus", 1, 0),dataTPA)
  #   class(dataTPA$nb)<-"numeric" 
  # }
  # if (namecolumn[y]=="Aquaculture") {
  #   dataTPA2<-rbind(c("dedritus", 1, 0,0), dataTPA2)
  #   dataTPA<-rbind( c("dedritus", 1, 0),dataTPA)
  #   class(dataTPA$nb)<-"numeric" 
  # }
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
  
  
  #faire les tableau de ingence pour T
  dataTPA2<-dataTPA[, c(1,3)]
  dataTPAin2 <- data.frame(t(dataTPA2[-1]))
  colnames(dataTPAin2) <- dataTPA2[, 1]
  val <- colnames(dataTPAin2)#######le mettre ? la bonne place
  dataTPAin <- dataTPAin2[,order(val)]
  
  dataNTPA2<-dataNTPA2[, c(1,3)]
  dataNTPAcontin2 <- data.frame(t(dataNTPA2[-1]))
  colnames(dataNTPAcontin2) <- dataNTPA2[, 1]
  Nval <- colnames(dataNTPAcontin2)#######le mettre ? la bonne place
  dataNTPAcontin <- dataNTPAcontin2[,order(val)]
  
  
  
  
  ALLpvaluesPA<-list()
  ALLpvaluesPA2<-list()
  
  for (j in 1:length(dataTPAin)) {
    pvaluesPA<-c() #pour le test su chi2
    pvaluesPA2<-c() #pour le test de fisher
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
  
  
  #Pourcentage de test positifs pour chaque modalit?s
  Resultschitest<-as.data.frame(ALLpvaluesPA) #chi.test
  colnames(Resultschitest)<-names(dataNTPAcontin)
  Resultschitest
  CHITEST[[sd]][y, c(1,2,3,4,5)]<-Resultschitest
  names(CHITEST[[sd]])[c(1,2,3,4,5)]<-colnames(Resultschitest)
  
  ResultsFtest<-as.data.frame(ALLpvaluesPA2) #fiher test
  colnames(ResultsFtest)<-names(dataNTPAcontin)
  ResultsFtest
  FTEST[[sd]][y, c(1,2,3,4,5)]<-ResultsFtest
  names(FTEST[[sd]])[c(1,2,3,4,5)]<-colnames(ResultsFtest)
  
  ##pour chaque ?chantillon, tableau de ingence puis test de chi2
  #Dans certaines cat?giries on a des 0 parfois : emp?che de faire un chi2!! ==> test exact de fisher ? la place?
  
  ALLpvaluesPA<-list()
  ALLpvaluesPA2<-list()
  percentpvalue<-c()
  percentpvalue2<-c()
  
  
  for (j in 1:length(dataTPAin)) {
    pvaluesPA<-c() #pour le test su chi2
    pvaluesPA2<-c() #pour le test de fisher
    table<-cbind(dataTPAin[,j], (sum(dataTPAin)-dataTPAin[,j]))
    
    for (i in 1:length(alldatabasesample[[y]])){
      dataNT<-ddply(as.data.frame(alldatabasesample[[y]][i]), c("Maj.Diet"), summarise, nb=sum(nb2)) #pourchaque ?chantillon, on 'arrange en tableau de ingence
      dataNTin <- data.frame(t(dataNT[-1]))
      colnames(dataNTin) <- dataNT[, 1]
      
      if (is.null(dataNTin$detritus)==T){  #si jamais il manque des colonnes du fait qu'une modalité n'apparait pas ==> mettre un zéro
        dataNTin$detritus=0
      }
      if (is.null(dataNTin$nekton)==T){
        dataNTin$nekton=0
      }
      
      if (is.null(dataNTin$plants)==T){  #si jamais il manque des colonnes du fait qu'une modalité n'apparait pas ==> mettre un zéro
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
  
  warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
  #les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif
  
  #Pourcentage de test positifs pour chaque modalit?s
  Resultschitest<-as.data.frame(percentpvalue) #chi.test
  row.names(Resultschitest)<-names(dataNTin)
  Resultschitest
  CHITESTpercent[[sd]][y, c(1,2,3,4,5)]<-Resultschitest$percentpvalue
  names(CHITESTpercent[[sd]])[c(1,2,3,4,5)]<-rownames(Resultschitest)
  
  
  ResultsFtest<-as.data.frame(percentpvalue2) #fiher test
  row.names(ResultsFtest)<-names(dataNTin)
  ResultsFtest
  FTESTpercent[[sd]][y, c(1,2,3,4,5)]<-ResultsFtest$percentpvalue2
  names(FTESTpercent[[sd]])[c(1,2,3,4,5)]<-rownames(ResultsFtest)
  
  
  
  
  
  ########tests par modalité region majoritaire##################
  dataPA<-ddply(alldatabase[[y]], c("Maj.Region", namecolumn[y]), summarise, nb=sum(nb))
  dataTPA<-subset(dataPA, dataPA[,2]==1) #juste les threatened
  class(dataTPA$nb)<-"integer"
  dataNTPA<-subset(dataPA, dataPA[,2]==0) #juste les threatened
  
  
  
  #Proportions
  b<-(dataTPA$nb)/sum(dataTPA$nb)
  dataTPA2<-cbind(dataTPA,b)
  if (dataTPA$Maj.Region[1]=="Ethiopian"){
      dataTPA2<-rbind(c("Australian", 1, 0,0), dataTPA2)
      dataTPA<-rbind( c("Australian", 1, 0),dataTPA)
      class(dataTPA$nb)<-"numeric"
    }
    
  # if (namecolumn[y]=="Species.control") {
  #   dataTPA2<-rbind(c("Australian", 1, 0,0), dataTPA2)
  #   dataTPA<-rbind( c("Australian", 1, 0),dataTPA)
  #   class(dataTPA$nb)<-"numeric" 
  # }
  # if (namecolumn[y]=="Accidental") {
  #   dataTPA2<-rbind(c("Australian", 1, 0,0), dataTPA2)
  #   dataTPA<-rbind( c("Australian", 1, 0),dataTPA)
  #   class(dataTPA$nb)<-"numeric" 
  # }
  # 
  
  dataTPA2
  
  
  b<-dataNTPA$nb/sum(dataNTPA$nb)
  dataNTPA2<-cbind(dataNTPA, b)
  dataNTPA2
  
  
  
  #faire les tableau de contingence pour T
  dataTPA2<-dataTPA[, c(1,3)]
  dataTPAcontin2 <- data.frame(t(dataTPA2[-1]))
  colnames(dataTPAcontin2) <- dataTPA2[, 1]
  val <- colnames(dataTPAcontin2)#######le mettre ? la bonne place
  dataTPAcontin <- dataTPAcontin2[,order(val)]
  
  dataNTPA2<-dataNTPA[, c(1,3)]
  dataNTPAcontin2 <- data.frame(t(dataNTPA2[-1]))
  colnames(dataNTPAcontin2) <- dataNTPA2[, 1]
  val <- colnames(dataNTPAcontin2)#######le mettre ? la bonne place
  dataNTPAcontin <- dataNTPAcontin2[,order(val)]
  
  
  # table<-cbind(dataTPAcontin[,"Ethiopian"], (sum(dataTPAcontin)-dataTPAcontin[,"Ethiopian"]))
  # table2<-cbind(dataNTPAcontin[,"Ethiopian"], (sum(dataNTPAcontin)-dataNTPAcontin[,"Ethiopian"]))
  # CONTIN2<-rbind(table,table2)
  # chisq.test(CONTIN2)
  # fisher.test(CONTIN2)
  
  
  
  
  
  
  
  ALLpvaluesPA<-list()
  ALLpvaluesPA2<-list()
  
  for (j in 1:length(dataTPAcontin)) {
    pvaluesPA<-c() #pour le test su chi2
    pvaluesPA2<-c() #pour le test de fisher
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
  
  
  #Pourcentage de test positifs pour chaque modalit?s
  Resultschitest<-as.data.frame(ALLpvaluesPA) #chi.test
  colnames(Resultschitest)<-names(dataNTPAcontin)
  Resultschitest
  # 
  #   if (is.null(dataNTPAcontin$Australian)==T){  #si jamais il manque des colonnes du fait qu'une modalité n'apparait pas ==> mettre un zéro
  #     Resultschitest<- rbind(c(1,NA), Resultschitest)
  #   }
  
  
  
  CHITEST[[sd]][y, c(6,7,8,9,10,11)]<-Resultschitest
  names(CHITEST[[sd]])[c(6,7,8,9,10,11)]<-colnames(Resultschitest)
  
  ResultsFtest<-as.data.frame(ALLpvaluesPA2) #fiher test
  colnames(ResultsFtest)<-names(dataNTPAcontin)
  ResultsFtest
  # if (is.null(dataNTPAcontin$Australian)==T){  #si jamais il manque des colonnes du fait qu'une modalité n'apparait pas ==> mettre un zéro
  #   ResultsFtest<- rbind(c(1,NA), ResultsFtest)
  # }
  
  FTEST[[sd]][y, c(6,7,8,9,10,11)]<-ResultsFtest
  names(FTEST[[sd]])[c(6,7,8,9,10,11)]<-colnames(ResultsFtest)
  
  
  
  
  
  
  
  ##pour chaque ?chantillon, tableau de contingence puis test de chi2
  #Dans certaines cat?giries on a des 0 parfois : emp?che de faire un chi2!! ==> test exact de fisher ? la place?
  
  ALLpvaluesPA<-list()
  ALLpvaluesPA2<-list()
  percentpvalue<-c()
  percentpvalue2<-c()
  
  
  for (j in 1:length(dataTPAcontin)) {
    pvaluesPA<-c() #pour le test su chi2
    pvaluesPA2<-c() #pour le test de fisher
    table<-cbind(dataTPAcontin[,j], (sum(dataTPAcontin)-dataTPAcontin[,j]))
    
    for (i in 1:length(alldatabasesample[[y]])){
      dataNT<-ddply(as.data.frame(alldatabasesample[[y]][i]), c("Maj.Region"), summarise, nb=sum(nb2)) #pourchaque ?chantillon, on 'arrange en tableau de contingence
      dataNTcontin <- data.frame(t(dataNT[-1]))
      colnames(dataNTcontin) <- dataNT[, 1]
      
      if (is.null(dataNTcontin$Australian)==T){  #si jamais il manque des colonnes du fait qu'une modalité n'apparait pas ==> mettre un zéro
        dataNTcontin$Australian=0
      }
      if (is.null(dataNTcontin$Ethiopian)==T){
        dataNTcontin$Ethiopian=0
      }
      # if (is.null(dataNTcontin$Madagascan)==T){
      #   dataNTcontin$Madagascan=0
      # }
      if (is.null(dataNTcontin$Nearctic)==T){  #si jamais il manque des colonnes du fait qu'une modalité n'apparait pas ==> mettre un zéro
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
  
  warnings() ##Est ce que les chi.test ont ?t? correctement ex?cut?s, si non (= NA ou valeur approximative du chi) , on garde les r?sultats des tests de fisher
  #les r?sultats du chi.test et du test de fisher sont sensiblement les m?mes, m?me lorsque le chi.test est approximatif
  
  #Pourcentage de test positifs pour chaque modalit?s
  Resultschitest<-as.data.frame(percentpvalue) #chi.test
  row.names(Resultschitest)<-names(dataNTcontin)
  Resultschitest
  CHITESTpercent[[sd]][y, c(6,7,8,9,10,11)]<-Resultschitest$percentpvalue
  names(CHITESTpercent[[sd]])[c(6,7,8,9,10,11)]<-rownames(Resultschitest)
  
  ResultsFtest<-as.data.frame(percentpvalue2) #fiher test
  row.names(ResultsFtest)<-names(dataNTcontin)
  ResultsFtest
  FTESTpercent[[sd]][y, c(6,7,8,9,10,11)]<-ResultsFtest$percentpvalue2
  names(FTESTpercent[[sd]])[c(6,7,8,9,10,11)]<-rownames(ResultsFtest)
  
  
  
}
}

###saved data - # Save an object to a file
# saveRDS(CHITEST, file = "./outputs/CHITESTsensitivity.rds")###
# saveRDS(FTEST, file = "./outputs/FTESTsensitivity.rds")
# 
# saveRDS(FTESTpercent, file = "./outputs/FTESTPERCENTsensitivity.rds")
# saveRDS(CHITESTpercent, file = "./outputs/CHITESTPERCENTsensitivity.rds")



#####FIGURE#####


##GLOBAL

alldataF<-list()
alldatapercentF<-list()
listerow<-c("GLOBAL", "Aquaculture", "Species control", "Ornamental", "Fisheries", "Accidental", "Sport angling", "Diffusion")

for (y in 1:length(listerow)) {
  GLOBALF<-data.frame()
  GLOBALpercentF<-data.frame()
for (i in 1:100) {
  GLOBALF<-rbind(GLOBALF,FTEST[[i]][listerow[y],])
  GLOBALpercentF<-rbind(GLOBALpercentF,FTESTpercent[[i]][listerow[y],])
  alldataF[[listerow[y]]]<-apply(GLOBALF,2,median) ##median of all pvalues
  alldatapercentF[[listerow[y]]]<-apply(GLOBALpercentF,2,median)##median of eprcent of significant test
  }
}


##make clean table to use


Tablefmedian<-as.data.frame(bind_rows(alldataF, .id = "column_label"))
Tablefpercentmedian<-as.data.frame(bind_rows(alldatapercentF, .id = "column_label"))

df2 <- data.frame(t(Tablefmedian[-1]))
colnames(df2) <- Tablefmedian[, 1]

df3 <- data.frame(t(Tablefpercentmedian[-1]))
colnames(df3) <- Tablefpercentmedian[, 1]

# write.csv2(df2, "./outputs/Sensitivity-analysis-comparison-F.csv")
# write.csv2(df3, "./outputs/Sensitivity-analysis-comparison-Fpercent.csv")
