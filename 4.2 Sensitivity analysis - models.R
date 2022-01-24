
#####Load data#####
library(MuMIn)
library(MASS)
library(AER)
library(dplyr)
library(caret)
library(ggcorrplot)
library(ggplot2)
library(mgcv)
library(reshape2)
library(GGally)

library(DHARMa)

####/////////////////////////////////////////////////INTRODUCTION /////////////////////////////////////////////////////////////###########
rm(list = ls())
setwd("D:/these/Axe_2")
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")
INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")
SENSDIET<-readRDS("./outputs/sensitivity-analysis/SENSDIET.rds")
identical(SENSDIET[[1]], SENSDIET[[2]]) ###verify that database are different
setdiff(SENSDIET[[1]], SENSDIET[[100]])

SENSREG<-readRDS("./outputs/sensitivity-analysis/SENSREG.rds")


INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))
class(INTRO$RepGuild1)

#data
INTRO<-subset(INTRO, select=-c(Maj.Diet, Maj.Region))
infovariableintroSENS<-list()
infopvalueintroSENS<-list()

for (sd in 1:100) {
INTRO2<-merge(INTRO, SENSDIET[[sd]], by="Species", all.x=T)
INTRO2<-merge(INTRO2, SENSREG[[sd]], by="Species", all.x=T)
  



no.na.data <- na.omit(INTRO2[c("Species","nb.country.intro","TL" ,
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "Accidental" ,
                              "Sport.Angling","Aquaculture","Diffusion","fisheries",
                              "Species.control","ornamental", #"research",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",
                              "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd"
)])



##only keep values >0 
table(no.na.data$nb.country.intro) 
sum(table(no.na.data$nb.country.intro))
no.na.data.1<-NULL
no.na.data.1<-subset(no.na.data, nb.country.intro > 0)
#plot(table(no.na.data.1$nb.country.intro))


infomodelglobalintro<-data.frame(matrix(ncol = 6, nrow = 1))
rownames(infomodelglobalintro)<-c("globalintro")
colnames(infomodelglobalintro)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")
infovariableglobalintro<-list()
infopvalueglobalintro<-list()


###GLOBAL#######
mod<-glm(nb.country.intro~TL + Maj.Diet + Nb.Diet+
           MaxBio5 + Order + 
           RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
           Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
           BlBd + PFiBd+
           Order:Nb.Diet+Order:TL+
           Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL+
           Diffusion + fisheries+Species.control, 
         data = no.na.data.1, na.action = na.omit, family="poisson" ) 



##Overdispersion?? 
summary(mod)
sim <- simulateResiduals(mod, n=99)
dis<-dispersiontest(mod) ##yes
testDispersion(sim)
infomodelglobalintro[1,2]<-dis$p.value

#AIC
infomodelglobalintro[1,1]<-AIC(mod)



###Multicolinearity
#car::vif(mod) ##si très sup à 1 alors multicol(à partir de 3)
no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)

###Residus
res1<-resid(mod)
res1<-as.data.frame(res1)


########Overdispersion : Negative binomial#
modnb<-NULL
modnb<-glm.nb(nb.country.intro~TL + Maj.Diet + Nb.Diet+
                + MaxBio5 + Order + 
                RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
                BlBd + PFiBd+
                Order:Nb.Diet+Order:TL+
                Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL+
                Diffusion + fisheries+Species.control, 
              data = no.na.data.1, link = log, control = glm.control(maxit = 50))




##still overdispersion? :
sim <- simulateResiduals(modnb, n=99)
testDispersion(sim)
shapiro.test(residuals(modnb))

hist(residuals(modnb), breaks = 20)
AIC(modnb)
infomodelglobalintro[1,3]<-AIC(modnb)


###step AIC
modnbfiltre<-NULL
modnbfiltre<- stepAIC(modnb, direction = "both")
modnbfiltre$theta
modnb$theta

infomodelglobalintro[1,4]<-AIC(modnbfiltre)

#shaoptest
shap<-shapiro.test(residuals(modnbfiltre))
infomodelglobalintro[1,6]<-shap$p.value


###Residus
res<-resid(modnbfiltre)
res<-as.data.frame(res)




######pvalues
summary(modnbfiltre)
anov<-Anova(modnbfiltre, test='F')
infopvalueglobalintro[["globalintro"]]<-anov


######estimates
est<-summary(modnbfiltre)
str(est)
est$coefficients
estimates<-est$coefficients[,c("Estimate","Std. Error")]

##variable importance
imp<-varImp(modnbfiltre)
infovariableglobalintro[["globalintro"]]<-merge(estimates, imp,by="row.names",all.x=TRUE)


#####pseudo r2
R2<-1-(modnbfiltre$deviance/modnbfiltre$null.deviance)
R2
infomodelglobalintro[1,5]<-R2






###########################PER REASON################################
#setwd("D:/these/Axe_2")
# 
# INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
#                         levels = c("nonguarders", "guarders", "bearers"))
# class(INTRO$RepGuild1)


variable<-c("nb.country.intro.aqua", "nb.country.intro.orna", "nb.country.intro.fish", "nb.country.intro.acci", "nb.country.intro.angl")
infomodelintro<-data.frame(matrix(ncol = 6, nrow = 5))
rownames(infomodelintro)<-c("Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") #"Species control", , "Diffusion"
colnames(infomodelintro)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")


infovariableintro<-list()
infopvalueintro<-list()



for (i in 1:2) {
  
  
  
  no.na.data <- na.omit(INTRO2[c("Species",variable[i],"TL" ,
                                "Maj.Diet" ,"Nb.Diet",
                                "RepGuild1" , "Amplitudetemp" ,
                                "MaxBio5",
                                "Maj.Region", "Nb.Native.Region",
                                "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd"
                                
                                
                                
  )])
  
  names(no.na.data)[2]<-"nb.country.esta"
  ###garder que les valeurs sup à 0
  no.na.data.1<-subset(no.na.data,nb.country.esta> 0) ##ne garder les valeurs que au dessus de 0
  
  ###classe des variables
  no.na.data.1[c("Maj.Diet",
                 "RepGuild1", "Maj.Region", "Used_by_humans" )]<-
    lapply(no.na.data.1[c("Maj.Diet",
                          "RepGuild1", "Maj.Region", "Used_by_humans" )], factor)
  
  
  
  
  
  
  #####modele de poisson
  
  
  mod<-glm(nb.country.esta~TL + Maj.Diet + Nb.Diet+
             + MaxBio5 + Order + 
             RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
             Maj.Region + Nb.Native.Region + #Used_by_humans + 
             Order:TL+Order:Nb.Diet+
             BlBd + PFiBd,#+
           data = no.na.data.1, na.action = na.fail, family="poisson")
  
  
  ##checker l'overdispersion :
  aicmod<-AIC(mod)
  infomodelintro[i, "AIC mod. 1"]<-aicmod
  summary(mod)
  dis<-dispersiontest(mod) ##ouiiiii --> negative binomiale
  infomodelintro[i, "p-value overdispersion"]<-dis$p.value
  
  sim <- simulateResiduals(mod, n=99)
  #plot(sim)
  #plot(mod)
  #hist(residuals(mod), breaks = 50)
  testDispersion(sim)
  testDispersion(mod)
  
  
  ###multicolinéarité? --> si oui centering
  #car::vif(mod) ##si très sup à 1 alors multicol(à partir de 3)
  no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
  no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
  no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)
  
  
  ########Overdispersion : Negative binomial#
  ###use the negbino
  if (dis$p.value <0.05) {
    modnb<-glm.nb(nb.country.esta~TL + Maj.Diet + Nb.Diet +
                    MaxBio5 + Order + 
                    RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                    Maj.Region + Nb.Native.Region + #Used_by_humans +
                    Order:TL+ Order:Nb.Diet+
                    BlBd + PFiBd, ##-1 to suppress the intercept 
                  data = no.na.data.1, link = log, maxit = 50)
    
  }else{modnb<-glm(nb.country.esta~TL + Maj.Diet + Nb.Diet+
                     + MaxBio5 + Order + 
                     RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                     Maj.Region + Nb.Native.Region + #Used_by_humans + 
                     Order:TL+Order:Nb.Diet+
                     BlBd + PFiBd,#+
                   data = no.na.data.1, na.action = na.fail, family="poisson")
  }
  ##checker l'overdispersion : 
  sim_fmnb <- simulateResiduals(modnb, n=999)
  #plot(sim_fmnb)
  testDispersion(sim_fmnb) 
  testDispersion(modnb)
  shapiro.test(residuals(modnb))
  #plot(modnb)
  hist(residuals(modnb), breaks = 20)
  aicmod2<-AIC(modnb)
  infomodelintro[i, "AIC mod. 2"]<-aicmod2
  summary(modnb)
  
  ###step AIC
  modnbfiltre<- stepAIC(modnb, direction = "both")
  modnbfiltre$theta
  modnb$theta
  
  ##checker l'overdispersion : 
  testDispersion(modnbfiltre)
  shap<-shapiro.test(residuals(modnbfiltre))
  infomodelintro[i,6]<-shap$p.value
  
  #plot(modnbfiltre)
  hist(residuals(modnbfiltre), breaks = 20)
  aicmod3<-AIC(modnbfiltre)
  infomodelintro[i, "AIC mod. 3"]<-aicmod3
  
  summary(modnbfiltre)
  
  ###Residus
  res<-resid(modnbfiltre)
  res<-as.data.frame(res)

  
  
  #####Pvalue
  anov<-Anova(modnbfiltre, test='F')
  infopvalueintro[[rownames(infomodelintro)[i]]]<-anov
  
  ######estimates
  est<-summary(modnbfiltre)
  str(est)
  est$coefficients
  estimates<-est$coefficients[,c("Estimate","Std. Error")]
  
  ##variable importance
  imp<-varImp(modnbfiltre)
  infovariableintro2<-merge(estimates, imp,by="row.names",all.x=TRUE)
  infovariableintro[[rownames(infomodelintro)[i]]]<-infovariableintro2
  
  
  
  ###pseudo r2
  R2<-1-(modnbfiltre$deviance/modnbfiltre$null.deviance)
  R2
  infomodelintro[i, "Pseudo-R2"]<-R2
  
  
  
}


#format data
infomodelintro<-rbind(infomodelglobalintro, infomodelintro)
infomodelintro<-na.omit(infomodelintro)
infovariableintro<-c(infovariableglobalintro, infovariableintro)
infopvalueintro<-c(infopvalueglobalintro, infopvalueintro)
names(infovariableintro)

infopvalueintroSENS[[sd]]<-infopvalueintro



bloup<-c("GLOBAL","Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") 

infovariableintroALL<-NULL
for (x in 1:3) {
  infovariableintro2<-cbind(infovariableintro[[x]], rep(bloup[x], length(infovariableintro[[x]][,1])))
  infovariableintroALL<-rbind(infovariableintroALL, infovariableintro2)
}
names(infovariableintroALL)<-c("Variable","Estimate","Se","VarImp","Pathway")

infovariableintroSENS[[sd]]<-infovariableintro

#infomodelintroALL<-rbind(infomodelintro, infomodelglobalintro)

}

infovariableintroSENS
infopvalueintroSENS

setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Introduction")
saveRDS(infovariableintroSENS, "infovariableintroSENSDIETREG.rds")##
saveRDS(infopvalueintroSENS, "infopvalueintroSENSDIETREG.rds")##
# setwd("D:/these/Axe_2")

setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Introduction")
infovariableintroSENS<-readRDS("infovariableintroSENSDIETREG.rds")##
infopvalueintroSENS<-readRDS("infopvalueintroSENSDIETREG.rds")##
setwd("D:/these/Axe_2")

infopvalueintroSENS$globalintro

####################save the all_good table###########
# setwd("D:/these/Axe_2")
# write.csv2(as.data.frame(infomodelintroALL), "./Figures_results/Introduction/infomodelintro.csv")
# write.csv2(as.data.frame(infovariableintroALL), "./Figures_results/Introduction/infovariableintro.csv")
# 
# setwd("D:/these/Axe_2/Figures_results/Introduction")
# lapply(1:length(infopvalueintro), function(i) write.csv(infopvalueintro[[i]], 
#                                                         file = paste0(names(infopvalueintro[i]), "INTRO-pvalue.csv"),
#                                                         row.names = T))
# 
#####FIGURE INTRODUCTION#####

##for each pathway, only keep the 10 most important variables (variable importance)
#infovariableALLtop<-infovariableALL %>% group_by(Pathway) %>% slice_max(order_by = VarImp, n = 10)

#infovariableALLtop<-infovariableALL
minfoSENS<-list()
minfofigureSENS<-list()
for (sd in 1:100) {
  

###what are the significant variable?
library(tidyr)
namevariable<-c("GLOBAL", "Aquaculture", "Ornamental")
minfo<-list()
minfofigure<-list()


for (y in 1:3) {
  colnames(infopvalueintroSENS[[sd]][[y]])[4]<-"pvalue"
  rownames(infovariableintroSENS[[sd]][[y]])<-infovariableintroSENS[[sd]][[y]]$Row.names
  namepv<-subset(infopvalueintroSENS[[sd]][[y]], infopvalueintroSENS[[sd]][[y]]$pvalue <0.05)
  name<-namevariable[y]
  minfo[[name]]<-merge(infovariableintroSENS[[sd]][[y]], namepv, by="row.names", all.x=T )
  rownames(minfo[[name]])<-minfo[[name]]$Row.names
  minfo[[name]]["(Intercept)","pvalue"]<-0
  if ((length(table(rownames(infopvalueintroSENS[[sd]][[y]])=="Order"))==2)& (infopvalueintroSENS[[sd]][[y]]["Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names
    minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0

  }

  if ((length(table(rownames(infopvalueintroSENS[[sd]][[y]])=="Nb.Diet:Order"))==2)& (infopvalueintroSENS[[sd]][[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names
    minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0

  }
  if ((length(table(rownames(infopvalueintroSENS[[sd]][[y]])=="TL:Order"))==2)& (infopvalueintroSENS[[sd]][[y]]["TL:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names
    minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0

  }
  if ((length(table(rownames(infopvalueintroSENS[[sd]][[y]])=="Maj.Diet"))==2)& (infopvalueintroSENS[[sd]][[y]]["Maj.Diet",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names
    minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0

  }

  if ((length(table(rownames(infopvalueintroSENS[[sd]][[y]])=="Maj.Region"))==2)& (infopvalueintroSENS[[sd]][[y]]["Maj.Region",]$pvalue<0.05)) {
    minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0

  }

  if ((length(table(rownames(infopvalueintroSENS[[sd]][[y]])=="RepGuild1"))==2)& (infopvalueintroSENS[[sd]][[y]]["RepGuild1",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names
    minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0

  }
  minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
  minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
}

minfofigureALL<-list()
for (x in 1:3) {
  minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
  minfofigureALL[[namevariable[x]]]<- minfofigure2
}

minfofigureSENS[[sd]]<-minfofigureALL
}

####save
setwd("D:/these/Axe_2/outputs/sensitivity-analysis")
saveRDS(minfofigureSENS, "minfofigureintroSENSDIETREG.rds")##
setwd("D:/these/Axe_2")

###mean + standard err of estimates for significative variables
library(data.table)

##global
figinfoSENS<-NULL
figinfoSENS$Row.names<-row.names(minfofigureSENS[[sd]]$GLOBAL) ##to initiate the variable

# for (sd in 1:5) {
# bloup<-minfofigureSENS[[sd]]$GLOBAL["Estimate"]
# bloup$Row.names<-row.names(bloup)
# figinfoSENS<-merge(figinfoSENS, bloup, by="Row.names", all=T)
# }

for (sd in 1:100) {
  bloup<-minfofigureSENS[[sd]]$GLOBAL["Estimate"]
  bloup$Row.names<-row.names(bloup)
  #bloupp<-infopvalueintroSENS[[sd]]$globalintro["pvalue"]
  #bloupp$Row.names<-row.names(bloupp)
  figinfoSENS<-merge(figinfoSENS, bloup, by="Row.names", all=T)
  #figinfoSENS<-merge(figinfoSENS, bloupp, by="Row.names", all.x=T)
}


##Aquaculture
figinfoSENSaqua<-NULL
figinfoSENSaqua$Row.names<-row.names(minfofigureSENS[[sd]]$Aquaculture) ##to initiate the variable


for (sd in 1:100) {
  bloup<-minfofigureSENS[[sd]]$Aquaculture["Estimate"]
  bloup$Row.names<-row.names(bloup)
  #bloupp<-infopvalueintroSENS[[sd]]$Aquaculture["pvalue"]
  #bloupp$Row.names<-row.names(bloupp)
  figinfoSENSaqua<-merge(figinfoSENSaqua, bloup, by="Row.names", all=T)
  #figinfoSENSaqua<-merge(figinfoSENSaqua, bloupp, by="Row.names", all.x=T)
}


##Ornamental
figinfoSENSorna<-NULL
figinfoSENSorna$Row.names<-row.names(minfofigureSENS[[sd]]$Ornamental) ##to initiate the variable


for (sd in 1:100) {
  bloup<-minfofigureSENS[[sd]]$Ornamental["Estimate"]
  bloup$Row.names<-row.names(bloup)
  figinfoSENSorna<-merge(figinfoSENSorna, bloup, by="Row.names", all=T)
}


####save
setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Introduction")
saveRDS(figinfoSENSorna, "figinfoSENSornaDIETREG.rds")
saveRDS(figinfoSENSaqua, "figinfoSENSaquaDIETREG.rds")
saveRDS(figinfoSENS, "figinfoSENSDIETREG.rds")
figinfoSENSglo<-readRDS("figinfoSENSDIETREG.rds")
figinfoSENSorna<-readRDS("figinfoSENSornaDIETREG.rds")
figinfoSENSaqua<-readRDS("figinfoSENSaquaDIETREG.rds")
setwd("D:/these/Axe_2")

figinfoSENSglo$type<-"GLOBAL"
figinfoSENSorna$type<-"Ornamental"
figinfoSENSaqua$type<-"Aquaculture"


#####compter les NA

rowSums(is.na(figinfoSENSorna["RepGuild1.L",]))

ornaa<-figinfoSENSorna
rownames(ornaa)<-figinfoSENSorna$Row.names
aquaa<-figinfoSENSaqua
rownames(aquaa)<-figinfoSENSaqua$Row.names

######







figinfoSENS<-rbind(figinfoSENSglo,figinfoSENSaqua,figinfoSENSorna )

##count number of time variable are significant
na_count <- apply(figinfoSENS, 1, function(x) sum(is.na(x)))
figinfoSENS$signipvalue <- 100-na_count

##order variables

colnames(figinfoSENS)<-c("Row.names",sprintf(paste0("Estimate", 1:100)),"type","signipvalue")

intervariable<-c("Nb.Diet:OrderCypriniformes", "Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther",
                 "Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes", 
                 "TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes",
                 "TL:Amplitudetemp")

intervariable<-as.data.frame(cbind(intervariable,rep("Interaction", length(intervariable))))
colnames(intervariable)<-c("Variable", "category")



intervariable<-c("Nb.Diet:OrderCypriniformes", "Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther",
                 "Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes",
                 "TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes",
                 "TL:Amplitudetemp")

intervariable<-as.data.frame(cbind(intervariable,rep("Interaction", length(intervariable))))
colnames(intervariable)<-c("Variable", "category")

ecovariable<-c("Amplitudetemp", "MaxBio5", 
               "Nb.Native.Region",
               "Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental")


ecovariable<-as.data.frame(cbind(ecovariable,rep("Ecological", length(ecovariable))))
colnames(ecovariable)<-c("Variable", "category")

traitvariable<-c("Nb.Diet","Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton",
                 "BlBd", "PFiBd", "RepGuild1.L", "RepGuild1.Q", "TL", "OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes")

traitvariable<-as.data.frame(cbind(traitvariable,rep("Traits", length(traitvariable))))
colnames(traitvariable)<-c("Variable", "category")

sociovariable<-c("Accidental" ,"Sport.Angling","Aquaculture","Diffusion","fisheries",
                 "Species.control","ornamental", "Nb_reasons")

sociovariable<-as.data.frame(cbind(sociovariable,rep("Socio-economic", length(sociovariable))))
colnames(sociovariable)<-c("Variable", "category")


####table attributing categories
categories<-rbind(intervariable, ecovariable, traitvariable, sociovariable)
colnames(categories)[1]<-"Row.names"
figinfoSENSorder<-merge(figinfoSENS, categories, by="Row.names", all.x=T)
figinfoSENSorder$Row.names <- factor(figinfoSENSorder$Row.names,      # Reordering variable factor levels for facet wrap
                                     levels = c(categories$Row.names, "(Intercept)"))





colnames(figinfoSENSorder)<-c("Row.names",sprintf(paste0("Estimate", 1:100)),"type","signipvalue","category")

dat.m <- melt(figinfoSENSorder, id.vars = c("Row.names", "signipvalue", "type","category"))
data<-na.omit(dat.m)





##figure
data2<-readRDS("D:/these/Axe_2/Figures_results/Introduction/SignificantvariablepvalueINTRO.rds")
colnames(data2)[6]<-"type"
data2[6]


levels(data2$type)[levels(data2$type)=="Global - 307 species "]<-"GLOBAL"
levels(data2$type)[levels(data2$type)=="Aquaculture - 154 species "]<-"Aquaculture"
levels(data2$type)[levels(data2$type)=="Ornamental - 126 species "]<-"Ornamental"

 data2$type <- factor(data2$type,      # Reordering group factor levels for facet wrap
                      levels = c("GLOBAL", "Aquaculture", "Ornamental"))

 data$type <- factor(data$type,      # Reordering group factor levels for facet wrap
                      levels = c("GLOBAL", "Aquaculture", "Ornamental"))
 
 data2$Variable <- factor(data2$Variable ,      # Reordering variable factor levels for facet wrap
                          levels = c(categories$Row.names, "(Intercept)"))
 
 
 
library(RColorBrewer)
myPalette <- colorRampPalette(c("blue", "red"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,100))


introsens<-ggplot() +
  geom_boxplot(data=data, aes(x=value, y=Row.names, col=signipvalue))+
  #geom_point(data=data2, aes(x=Estimate, y=Variable), color="blue")+
  geom_vline(xintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  scale_x_continuous() +
  sc+
  facet_wrap(~type,nrow=1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour="grey95"),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        axis.text.y =element_text(size = 20, color = "black"),
        axis.text.x=element_text(hjust = 1, size = 20, color = "black"),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 20, color = "black"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"))

x11()
introsens

setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Introduction")
pdf("intro-Graph-DIETREG.pdf", 14,14)
# 2. Create a plot
introsens
# Close the pdf file
dev.off()




png("intro-Graph-DIETREG.png", width = 1200, height = 1500, units = "px")
# 2. Create a plot
introsens
# Close the pdf file
dev.off()

####/////////////////////////////////////////////////ESTABLISHMENT /////////////////////////////////////////////////////////////###########
rm(list = ls())
setwd("D:/these/Axe_2")
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")
INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")
SENSDIET<-readRDS("./outputs/sensitivity-analysis/SENSDIET.rds")
identical(SENSDIET[[1]], SENSDIET[[2]]) ###verify that database are different
ss<-setdiff(SENSDIET[[1]], SENSDIET[[2]])
anti_join(SENSDIET[[1]],SENSDIET[[2]])

SENSREG<-readRDS("./outputs/sensitivity-analysis/SENSREG.rds")
identical(SENSREG[[1]], SENSREG[[2]]) ###verify that database are different
ss<-setdiff(SENSREG[[1]], SENSREG[[2]])
anti_join(SENSREG[[1]],SENSREG[[2]])

INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))
class(INTRO$RepGuild1)

#data
INTRO<-subset(INTRO, select=-c(Maj.Diet, Maj.Region))
infovariableSENS<-list()
infopvalueSENS<-list()

for (sd in 1:100) {
INTRO2<-merge(INTRO, SENSDIET[[sd]], by="Species", all.x=T)
INTRO2<-merge(INTRO2, SENSREG[[sd]], by="Species", all.x=T)
no.na.data <- na.omit(INTRO2[c("Species","nb.country.establish","TL" ,
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "Accidental" ,
                              "Sport.Angling","Aquaculture","Diffusion","fisheries",
                              "Species.control","ornamental", #"research",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",
                              "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd"
)])



##only keep values >0 
table(no.na.data$nb.country.establish) 
sum(table(no.na.data$nb.country.establish))
no.na.data.1<-subset(no.na.data, nb.country.establish > 0)
#plot(table(no.na.data.1$nb.country.establish))


infomodelglobal<-data.frame(matrix(ncol = 6, nrow = 1))
rownames(infomodelglobal)<-c("GLOBAL")
colnames(infomodelglobal)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")
infovariableglobal<-list()
infopvalueglobal<-list()


###GLOBAL#######
mod<-glm(nb.country.establish~TL + Maj.Diet + Nb.Diet+
           MaxBio5 + Order + 
           RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
           Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
           BlBd + PFiBd+
           Order:Nb.Diet+Order:TL+
           Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL+
           Diffusion + fisheries+Species.control, 
         data = no.na.data.1, na.action = na.omit, family="poisson"  ) 



##Overdispersion?? 
summary(mod)
sim <- simulateResiduals(mod, n=99)
dis<-dispersiontest(mod) ##yes
testDispersion(sim)
infomodelglobal[1,2]<-dis$p.value

#AIC
infomodelglobal[1,1]<-AIC(mod)



###Multicolinearity
#car::vif(mod) ##si très sup à 1 alors multicol(à partir de 3)
no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)


########Overdispersion : Negative binomial#
modnb<-glm.nb(nb.country.establish~TL + Maj.Diet + Nb.Diet+
                + MaxBio5 + Order + 
                RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
                BlBd + PFiBd+
                Order:Nb.Diet+Order:TL+
                Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL+
                Diffusion + fisheries+Species.control, 
              data = no.na.data.1, link = log, control = glm.control(maxit = 50))

summary(modnb)

##still overdispersion? :
sim <- simulateResiduals(modnb, n=99)
testDispersion(sim)
shapiro.test(residuals(modnb))

hist(residuals(modnb), breaks = 20)
AIC(modnb)
infomodelglobal[1,3]<-AIC(modnb)


###step AIC
modnbfiltre<- stepAIC(modnb, direction = "both")
modnbfiltre$theta
modnb$theta
infomodelglobal[1,4]<-AIC(modnbfiltre)

##shap test
shap<-shapiro.test(residuals(modnbfiltre))
infomodelglobal[1,6]<-shap$p.value

###Residus
res<-resid(modnbfiltre)
res<-as.data.frame(res)


######pvalues
summary(modnbfiltre)
anov<-Anova(modnbfiltre, test='F')
infopvalueglobal[["GLOBAL"]]<-anov


######estimates
est<-summary(modnbfiltre)
str(est)
est$coefficients
estimates<-est$coefficients[,c("Estimate","Std. Error")]

##variable importance
imp<-varImp(modnbfiltre)
infovariableglobal[["GLOBAL"]]<-merge(estimates, imp,by="row.names",all.x=TRUE)


#####pseudo r2
R2<-1-(modnbfiltre$deviance/modnbfiltre$null.deviance)
R2
infomodelglobal[1,5]<-R2


###########################PER REASON################################

variable<-c("nb.country.esta.aqua", "nb.country.esta.orna", "nb.country.esta.fish", "nb.country.esta.acci", "nb.country.esta.angl")
infomodel<-data.frame(matrix(ncol = 6, nrow = 5))
rownames(infomodel)<-c("Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") #"Species control", , "Diffusion"
colnames(infomodel)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")

infovariable<-list()
infopvalue<-list()




for (i in 1:2) {
  
  
  
  no.na.data <- na.omit(INTRO2[c("Species",variable[i],"TL" ,
                                "Maj.Diet" ,"Nb.Diet",
                                "RepGuild1" , "Amplitudetemp" ,
                                "MaxBio5",
                                "Maj.Region", "Nb.Native.Region",
                                "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd"
                                
                                
                                
  )])
  
  names(no.na.data)[2]<-"nb.country.esta"
  ###garder que les valeurs sup à 0
  no.na.data.1<-subset(no.na.data,nb.country.esta> 0) ##ne garder les valeurs que au dessus de 0
  
  ###classe des variables
  no.na.data.1[c("Maj.Diet",
                 "RepGuild1", "Maj.Region", "Used_by_humans" )]<-
    lapply(no.na.data.1[c("Maj.Diet",
                          "RepGuild1", "Maj.Region", "Used_by_humans" )], factor)
  
  
  
  
  
  
  #####modele de poisson
  
  
  mod<-glm(nb.country.esta~TL + Maj.Diet + Nb.Diet+
             + MaxBio5 + Order + 
             RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
             Maj.Region + Nb.Native.Region + #Used_by_humans + 
             Order:TL+Order:Nb.Diet+
             BlBd + PFiBd,#+
           data = no.na.data.1, na.action = na.fail, family="poisson")
  
  
  ##checker l'overdispersion :
  aicmod<-AIC(mod)
  infomodel[i, "AIC mod. 1"]<-aicmod
  summary(mod)
  dis<-dispersiontest(mod) ##ouiiiii --> negative binomiale
  infomodel[i, "p-value overdispersion"]<-dis$p.value
  
  sim <- simulateResiduals(mod, n=99)
  #plot(sim)
  #plot(mod)
  hist(residuals(mod), breaks = 50)
  testDispersion(sim)
  testDispersion(mod)
  
  
  ###multicolinéarité? --> si oui centering
  #car::vif(mod) ##si très sup à 1 alors multicol(à partir de 3)
  no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
  no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
  no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)
  
  
  ########Overdispersion : Negative binomial#
  ###use the negbino
  if (dis$p.value <0.05) {
    modnb<-glm.nb(nb.country.esta~TL + Maj.Diet + Nb.Diet +
                    MaxBio5 + Order + 
                    RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                    Maj.Region + Nb.Native.Region + #Used_by_humans +
                    Order:TL+ Order:Nb.Diet+
                    BlBd + PFiBd, ##-1 to suppress the intercept 
                  data = no.na.data.1, link = log, maxit = 50)
    
  }else{modnb<-glm(nb.country.esta~TL + Maj.Diet + Nb.Diet+
                     + MaxBio5 + Order + 
                     RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                     Maj.Region + Nb.Native.Region + #Used_by_humans + 
                     Order:TL+Order:Nb.Diet+
                     BlBd + PFiBd,#+
                   data = no.na.data.1, na.action = na.fail, family="poisson")
  }
  ##checker l'overdispersion : 
  sim_fmnb <- simulateResiduals(modnb, n=999)
  #plot(sim_fmnb)
  testDispersion(sim_fmnb) 
  testDispersion(modnb)
  shapiro.test(residuals(modnb))
  #plot(modnb)
  hist(residuals(modnb), breaks = 20)
  aicmod2<-AIC(modnb)
  infomodel[i, "AIC mod. 2"]<-aicmod2
  summary(modnb)
  
  ###step AIC
  modnbfiltre<- stepAIC(modnb, direction = "both")
  modnbfiltre$theta
  modnb$theta
  
  ##checker l'overdispersion : 
  testDispersion(modnbfiltre)
  
  #shaptest
  shap<-shapiro.test(residuals(modnbfiltre))
  infomodel[i,6]<-shap$p.value
  
  #AIC
  hist(residuals(modnbfiltre), breaks = 20)
  aicmod3<-AIC(modnbfiltre)
  infomodel[i, "AIC mod. 3"]<-aicmod3
  
  summary(modnbfiltre)
  
  ###Residus
  res<-resid(modnbfiltre)
  res<-as.data.frame(res)
  

  
  
  #####Pvalue
  anov<-Anova(modnbfiltre, test='F')
  infopvalue[[rownames(infomodel)[i]]]<-anov
  
  ######estimates
  est<-summary(modnbfiltre)
  str(est)
  est$coefficients
  estimates<-est$coefficients[,c("Estimate","Std. Error")]
  
  ##variable importance
  imp<-varImp(modnbfiltre)
  infovariable2<-merge(estimates, imp,by="row.names",all.x=TRUE)
  infovariable[[rownames(infomodel)[i]]]<-infovariable2
  
  
  
  ###pseudo r2
  R2<-1-(modnbfiltre$deviance/modnbfiltre$null.deviance)
  R2
  infomodel[i, "Pseudo-R2"]<-R2
  
  
  
}


#format data
infomodel<-rbind(infomodelglobal, infomodel)
infomodel<-na.omit(infomodel)
infovariable<-c(infovariableglobal, infovariable)
infopvalue<-c(infopvalueglobal, infopvalue)
names(infovariable)


infopvalueSENS[[sd]]<-infopvalue

bloup<-c("GLOBAL","Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") 

infovariableALL<-NULL
for (x in 1:3) {
  infovariable2<-cbind(infovariable[[x]], rep(bloup[x], length(infovariable[[x]][,1])))
  infovariableALL<-rbind(infovariableALL, infovariable2)
}
names(infovariableALL)<-c("Variable","Estimate","Se","VarImp","Pathway")
infovariableSENS[[sd]]<-infovariable
infomodelALL<-rbind(infomodelglobal,infomodel)

}

infovariableSENS
infopvalueSENS

setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Establishment")
saveRDS(infovariableSENS, "infovariableSENSDIETREG.rds")##
saveRDS(infopvalueSENS, "infopvalueSENSDIETREG.rds")##
infovariableSENS<-readRDS("infovariableSENSDIETREG.rds")
infopvalueSENS<-readRDS("infopvalueSENSDIETREG.rds")
setwd("D:/these/Axe_2")



####################save the all_good table###########
# setwd("D:/these/Axe_2")
# write.csv2(as.data.frame(infomodelALL), "./Figures_results/Establishment/infomodel.csv")
# write.csv2(as.data.frame(infovariableALL), "./Figures_results/Establishment/infovariable.csv")
# write.csv2(as.data.frame(infopvalue), "./Figures_results/Establishment/infopvalue.csv")
# setwd("D:/these/Axe_2/Figures_results/Establishment")
# lapply(1:length(infopvalue), function(i) write.csv(infopvalue[[i]], 
#                                                    file = paste0(names(infopvalue[i]), "ESTA-pvalue.csv"),
#                                                    row.names = T))



#####FIGURE ESTABLISHEMENT#####

##for each pathway, only keep the 10 most important variables (variable importance)
#infovariableALLtop<-infovariableALL %>% group_by(Pathway) %>% slice_max(order_by = VarImp, n = 10)

#infovariableALLtop<-infovariableALL



#infovariableALLtop<-infovariableALL
minfoSENS<-list()
minfofigureSENS<-list()
for (sd in 1:100) {
  
  
  ###what are the significant variable?
  library(tidyr)
  namevariable<-c("GLOBAL", "Aquaculture", "Ornamental")
  minfo<-list()
  minfofigure<-list()
  
  
  for (y in 1:3) {
    colnames(infopvalueSENS[[sd]][[y]])[4]<-"pvalue"
    rownames(infovariableSENS[[sd]][[y]])<-infovariableSENS[[sd]][[y]]$Row.names
    namepv<-subset(infopvalueSENS[[sd]][[y]], infopvalueSENS[[sd]][[y]]$pvalue <0.05)
    name<-namevariable[y]
    minfo[[name]]<-merge(infovariableSENS[[sd]][[y]], namepv, by="row.names", all.x=T )
    rownames(minfo[[name]])<-minfo[[name]]$Row.names
    minfo[[name]]["(Intercept)","pvalue"]<-0
    if ((length(table(rownames(infopvalueSENS[[sd]][[y]])=="Order"))==2)& (infopvalueSENS[[sd]][[y]]["Order",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0
      
    }
    
    if ((length(table(rownames(infopvalueSENS[[sd]][[y]])=="Nb.Diet:Order"))==2)& (infopvalueSENS[[sd]][[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0
      
    }
    if ((length(table(rownames(infopvalueSENS[[sd]][[y]])=="TL:Order"))==2)& (infopvalueSENS[[sd]][[y]]["TL:Order",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0
      
    }
    if ((length(table(rownames(infopvalueSENS[[sd]][[y]])=="Maj.Diet"))==2)& (infopvalueSENS[[sd]][[y]]["Maj.Diet",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0
      
    }
    
    if ((length(table(rownames(infopvalueSENS[[sd]][[y]])=="Maj.Region"))==2)& (infopvalueSENS[[sd]][[y]]["Maj.Region",]$pvalue<0.05)) {
      minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0
      
    }
    
    if ((length(table(rownames(infopvalueSENS[[sd]][[y]])=="RepGuild1"))==2)& (infopvalueSENS[[sd]][[y]]["RepGuild1",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0
      
    }
    minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
    minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
  }
  
  minfofigureALL<-list()
  for (x in 1:3) {
    minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
    minfofigureALL[[namevariable[x]]]<- minfofigure2
  }
  
  minfofigureSENS[[sd]]<-minfofigureALL
}

####save
setwd("D:/these/Axe_2/outputs/sensitivity-analysis")
saveRDS(minfofigureSENS, "minfofigureintroSENSDIETREG.rds")##
setwd("D:/these/Axe_2")

###mean + standard err of estimates for significative variables
library(data.table)

##global
figinfoSENS<-NULL
figinfoSENS$Row.names<-row.names(minfofigureSENS[[sd]]$GLOBAL) ##to initiate the variable

# for (sd in 1:5) {
# bloup<-minfofigureSENS[[sd]]$GLOBAL["Estimate"]
# bloup$Row.names<-row.names(bloup)
# figinfoSENS<-merge(figinfoSENS, bloup, by="Row.names", all=T)
# }

for (sd in 1:100) {
  bloup<-minfofigureSENS[[sd]]$GLOBAL["Estimate"]
  bloup$Row.names<-row.names(bloup)
  #bloupp<-infopvalueSENS[[sd]]$globalintro["pvalue"]
  #bloupp$Row.names<-row.names(bloupp)
  figinfoSENS<-merge(figinfoSENS, bloup, by="Row.names", all=T)
  #figinfoSENS<-merge(figinfoSENS, bloupp, by="Row.names", all.x=T)
}


##Aquaculture
figinfoSENSaqua<-NULL
figinfoSENSaqua$Row.names<-row.names(minfofigureSENS[[sd]]$Aquaculture) ##to initiate the variable


for (sd in 1:100) {
  bloup<-minfofigureSENS[[sd]]$Aquaculture["Estimate"]
  bloup$Row.names<-row.names(bloup)
  #bloupp<-infopvalueSENS[[sd]]$Aquaculture["pvalue"]
  #bloupp$Row.names<-row.names(bloupp)
  figinfoSENSaqua<-merge(figinfoSENSaqua, bloup, by="Row.names", all=T)
  #figinfoSENSaqua<-merge(figinfoSENSaqua, bloupp, by="Row.names", all.x=T)
}


##Ornamental
figinfoSENSorna<-NULL
figinfoSENSorna$Row.names<-row.names(minfofigureSENS[[sd]]$Ornamental) ##to initiate the variable


for (sd in 1:100) {
  bloup<-minfofigureSENS[[sd]]$Ornamental["Estimate"]
  bloup$Row.names<-row.names(bloup)
  figinfoSENSorna<-merge(figinfoSENSorna, bloup, by="Row.names", all=T)
}


####save###
setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Establishment")
saveRDS(figinfoSENSorna, "figinfoestaSENSornaDIETREG.rds")
saveRDS(figinfoSENSaqua, "figinfoestaSENSaquaDIETREG.rds")
saveRDS(figinfoSENS, "figinfoestaSENSDIETREG.rds")
figinfoSENSglo<-readRDS("figinfoestaSENSDIETREG.rds")
figinfoSENSorna<-readRDS("figinfoestaSENSornaDIETREG.rds")
figinfoSENSaqua<-readRDS("figinfoestaSENSaquaDIETREG.rds")
setwd("D:/these/Axe_2")

figinfoSENSglo$type<-"GLOBAL"
figinfoSENSorna$type<-"Ornamental"
figinfoSENSaqua$type<-"Aquaculture"

figinfoSENS<-rbind(figinfoSENSglo,figinfoSENSaqua,figinfoSENSorna )

##count number of time variable are significant
na_count <- apply(figinfoSENS, 1, function(x) sum(is.na(x)))
figinfoSENS$signipvalue <- 100-na_count

##order variables

colnames(figinfoSENS)<-c("Row.names",sprintf(paste0("Estimate", 1:100)),"type","signipvalue")

intervariable<-c("Nb.Diet:OrderCypriniformes", "Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther",
                 "Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes", 
                 "TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes",
                 "TL:Amplitudetemp")

intervariable<-as.data.frame(cbind(intervariable,rep("Interaction", length(intervariable))))
colnames(intervariable)<-c("Variable", "category")



intervariable<-c("Nb.Diet:OrderCypriniformes", "Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther",
                 "Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes",
                 "TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes",
                 "TL:Amplitudetemp")

intervariable<-as.data.frame(cbind(intervariable,rep("Interaction", length(intervariable))))
colnames(intervariable)<-c("Variable", "category")

ecovariable<-c("Amplitudetemp", "MaxBio5", 
               "Nb.Native.Region",
               "Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental")


ecovariable<-as.data.frame(cbind(ecovariable,rep("Ecological", length(ecovariable))))
colnames(ecovariable)<-c("Variable", "category")

traitvariable<-c("Nb.Diet","Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton",
                 "BlBd", "PFiBd", "RepGuild1.L", "RepGuild1.Q", "TL", "OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes")

traitvariable<-as.data.frame(cbind(traitvariable,rep("Traits", length(traitvariable))))
colnames(traitvariable)<-c("Variable", "category")

sociovariable<-c("Accidental" ,"Sport.Angling","Aquaculture","Diffusion","fisheries",
                 "Species.control","ornamental", "Nb_reasons")

sociovariable<-as.data.frame(cbind(sociovariable,rep("Socio-economic", length(sociovariable))))
colnames(sociovariable)<-c("Variable", "category")


####table attributing categories
categories<-rbind(intervariable, ecovariable, traitvariable, sociovariable)
colnames(categories)[1]<-"Row.names"
figinfoSENSorder<-merge(figinfoSENS, categories, by="Row.names", all.x=T)
figinfoSENSorder$Row.names <- factor(figinfoSENSorder$Row.names,      # Reordering variable factor levels for facet wrap
                                     levels = c(categories$Row.names, "(Intercept)"))





colnames(figinfoSENSorder)<-c("Row.names",sprintf(paste0("Estimate", 1:100)),"type","signipvalue","category")

dat.m <- melt(figinfoSENSorder, id.vars = c("Row.names", "signipvalue", "type","category"))
data<-na.omit(dat.m)





##figure
###get my observed value
data2<-readRDS("D:/these/Axe_2/Figures_results/Establishment/SignificantvariablepvalueESTAESSAICHANGE.rds")
colnames(data2)[6]<-"type"
data2[6]
levels(data2$type)[levels(data2$type)=="Global - 213 species "]<-"GLOBAL"
levels(data2$type)[levels(data2$type)=="Aquaculture - 85 species "]<-"Aquaculture"
levels(data2$type)[levels(data2$type)=="Ornamental - 69 species "]<-"Ornamental"

data2$type <- factor(data2$type,      # Reordering group factor levels for facet wrap
                     levels = c("GLOBAL", "Aquaculture", "Ornamental"))

data$type <- factor(data$type,      # Reordering group factor levels for facet wrap
                    levels = c("GLOBAL", "Aquaculture", "Ornamental"))

library(RColorBrewer)
myPalette <- colorRampPalette(c("blue", "red"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,100))

# library(RColorBrewer)
# myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
# sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,100))


estasens<-ggplot() +
  geom_boxplot(data=data, aes(x=value, y=Row.names, col=signipvalue))+
 # geom_point(data=data2, aes(x=Estimate, y=Variable), color="blue")+
  geom_vline(xintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  scale_x_continuous() +
  sc+
  facet_wrap(~type,nrow=1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour="grey95"),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        axis.text.y =element_text(size = 20, color = "black"),
        axis.text.x=element_text(hjust = 1, size = 20, color = "black"),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 20, color = "black"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"))

x11()
estasens

setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Establishment")
pdf("Esta-Graph-DIETREG.pdf", 14,14)
# 2. Create a plot
estasens
# Close the pdf file
dev.off()




png("Esta-Graph-DIETREG.png", width = 1200, height = 1500, units = "px")
# 2. Create a plot
estasens
# Close the pdf file
dev.off()

# setwd("D:/these/Axe_2")
# 
# figinfoSENS$type<-"GLOBAL"
# figinfoSENSorna$type<-"Ornamental"
# figinfoSENSaqua$type<-"Aquaculture"
# 
# figinfoSENSall<-rbind(figinfoSENS,figinfoSENSaqua,figinfoSENSorna )
# 
# 
# na_count <- apply(figinfoSENSall, 1, function(x) sum(is.na(x)))
# figinfoSENSall$signipvalue <- 100-na_count
# 
# colnames(figinfoSENSall)<-c("Row.names","Estimate.1","Estimate.2","Estimate.3","Estimate.4","Estimate.5","type","signipvalue")
# 
# dat.m <- melt(figinfoSENSall, id.vars = c("Row.names", "signipvalue", "type"))
# data<-na.omit(dat.m)
# ##figure
# 
# ggplot(data, aes(x=value, y=Row.names, col=as.factor(signipvalue))) +
#   geom_vline(xintercept=0, linetype="dashed",
#              color = "darkgrey", size=0.2) +
#   geom_boxplot()+
#   scale_color_manual(breaks=c("5","4","3","2","1"),values=c("darkred", "red", "darkorange", "orange", "yellow")) +
#   facet_wrap(~type,nrow=1)+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major.y = element_line(colour="grey95"),
#         panel.border = element_blank(),
#         axis.title.x = element_text(size = 20, color = "black"),
#         axis.title.y = element_text(size = 20, color = "black"),
#         axis.text.y =element_text(size = 20, color = "black"),
#         axis.text.x=element_text(hjust = 1, size = 20, color = "black"),
#         panel.background = element_blank(),
#         strip.text.x = element_text(size = 20, color = "black"),
#         strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"))
# 
# EstagraphSENS


####################///////////////////////////////////////////////IMPACTS//////////////////////////////////////////////////////####################
#####GLOBAL#####
rm(list = ls())
setwd("D:/these/Axe_2")
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")
#INTROgood<-read.csv2("./outputs/INTRO_all_good_selectedregdiet.csv")
INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")
SENSDIET<-readRDS("./outputs/sensitivity-analysis/SENSDIET.rds")
identical(SENSDIET[[100]], SENSDIET[[2]]) ###verify that database are different
setdiff(SENSDIET[[1]], SENSDIET[[2]])
SENSREG<-readRDS("./outputs/sensitivity-analysis/SENSREG.rds")
INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))




class(INTRO$RepGuild1)

#data
INTRO<-subset(INTRO, select=-c(Maj.Diet, Maj.Region))
infovariableimpactSENS<-list()
infopvalueimpactSENS<-list()

for (sd in 1:100) {
  INTRO2<-merge(INTRO, SENSDIET[[sd]], by="Species", all.x=T)
  INTRO2<-merge(INTRO2, SENSREG[[sd]], by="Species", all.x=T)
no.na.data <- na.omit(INTRO2[c("Species","nb.country.impeco", "TL" ,
                              "Maj.Diet", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "Accidental" ,
                              "Sport.Angling","Aquaculture","Diffusion","fisheries",
                              "Species.control","ornamental", #"research",
                              "Maj.Region", "Nb.Native.Region",
                              "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd"
)])


###garder que les valeurs sup à 0
table(no.na.data$nb.country.impeco)
no.na.data.1<-subset(no.na.data, nb.country.impeco > 0) ##ne garder les valeurs que au dessus de 0

table(no.na.data.1$nb.country.impeco)


###classe des variables
no.na.data.1[c("Maj.Diet",
               "RepGuild1", "Maj.Region", "Used_by_humans" )]<-
  lapply(no.na.data.1[c("Maj.Diet",
                        "RepGuild1", "Maj.Region", "Used_by_humans" )], factor)

infomodelimpact<-data.frame(matrix(ncol = 6, nrow = 1))
rownames(infomodelimpact)<-c("GLOBAL")
colnames(infomodelimpact)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")
infovariableimpact<-list()
infopvalueimpact<-list()




##############Modèle de poisson --> overdispersion?#
mod<-glm(nb.country.impeco~TL + Maj.Diet + Nb.Diet+
           MaxBio5 + Order + 
           RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
           Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
           BlBd + PFiBd+
           Order:Nb.Diet+Order:TL+
           Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL+
           Diffusion + fisheries+Species.control, 
         data = no.na.data.1, na.action = na.omit, family="poisson"  )




##checker l'overdispersion : 
summary(mod)
dis<-dispersiontest(mod)
infomodelimpact[1,2]<-dis$p.value
AIC(mod)
infomodelimpact[1,1]<-AIC(mod)

###multicolinéarité? --> si oui centering
#car::vif(mod) ##si très sup à 1 alors multicol(à partir de 3)
no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)

###Residus
res1<-resid(mod)
res1<-as.data.frame(res1)


if (dis$p.value <0.05) {
  modnb<-glm.nb(nb.country.impeco~TL + Maj.Diet + Nb.Diet+
                  MaxBio5 + Order + 
                  RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                  Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
                  BlBd + PFiBd+
                  Order:Nb.Diet+Order:TL+
                  Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL+
                  Diffusion + fisheries+Species.control,
                data = no.na.data.1, link = log, maxit = 50)
  
}else{modnb<-glm(nb.country.impeco~TL + Maj.Diet + Nb.Diet+
                   MaxBio5 + Order + 
                   RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                   Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
                   BlBd + PFiBd+
                   Order:Nb.Diet+Order:TL+
                   Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL+
                   Diffusion + fisheries+Species.control,
                 data = no.na.data.1, na.action = na.fail, family="poisson")
}
# ########Overdispersion : Negative binomial#
# modnb<-glm.nb(nb.country.impeco~TL + Maj.Diet + Nb.Diet+
#                 + MaxBio5 + Order + 
#                 RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
#                 Maj.Region + Nb.Native.Region + Used_by_humans+ Nb_reasons + 
#                 BlBd + PFiBd+
#                 Order:Nb.Diet+Order:TL+
#                 Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL, 
#               data = no.na.data.1, link = log, control = glm.control(maxit = 50))
# 
# summary(modnb)

##checker l'overdispersion : 
sim_fmnb <- simulateResiduals(modnb, n=999)
#plot(sim_fmnb)
testDispersion(sim_fmnb) 
shapiro.test(residuals(modnb))
#plot(modnb)
hist(residuals(modnb), breaks = 100)
AIC(modnb) #
infomodelimpact[1,3]<-AIC(modnb)


###step AIC
modnbfiltre<- stepAIC(modnb, direction = "both")
modnbfiltre$theta
modnb$theta

##checker l'overdispersion : 
shap<-shapiro.test(residuals(modnbfiltre))
infomodelimpact[1,6]<-shap$p.value
dispersiontest(modnbfiltre)
#plot(modnbfiltre)
hist(residuals(modnbfiltre), breaks = 20)


AIC(modnbfiltre)
infomodelimpact[1,4]<-AIC(modnbfiltre)



###Residus
res<-resid(modnbfiltre)
res<-as.data.frame(res)

#estimates
est<-summary(modnbfiltre)
str(est)
est$coefficients
estimates<-est$coefficients[,c("Estimate","Std. Error")]

###pvalues
anov<-Anova(modnbfiltre, test='F')
infopvalueimpact[["GLOBAL"]]<-anov

###variable importance
imp<-varImp(modnbfiltre)
infovariable2<-merge(estimates, imp,by="row.names",all.x=TRUE)
infovariableimpact[["GLOBAL"]]<-infovariable2

#####pseudo r2
R2<-1-(modnbfiltre$deviance/modnbfiltre$null.deviance)
R2
infomodelimpact[1,5]<-R2

infopvalueimpactSENS[[sd]]<-infopvalueimpact
infovariableimpactSENS[[sd]]<-infovariableimpact


}
####################save the all_good table###########
# setwd("D:/these/Axe_2")
# write.csv2(as.data.frame(infomodelimpact), "./Figures_results/Impact/infomodelimpact.csv")
# write.csv2(as.data.frame(infovariableimpact), "./Figures_results/Impact/infovariableimpact.csv")
# #write.csv2(as.data.frame(infopvalueimpact), "./Figures_result/infopvalueimpact.csv")
# setwd("D:/these/Axe_2/Figures_results/Impact")
# lapply(1:length(infopvalueimpact), function(i) write.csv(infopvalueimpact[[i]], 
#                                                          file = paste0(names(infopvalueimpact[i]), "IMPACT-pvalue.csv"),
#                                                          row.names = T))


infovariableimpactSENS
infopvalueimpactSENS

setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Impact")
saveRDS(infovariableimpactSENS, "infovariableimpactSENSDIETREG.rds")##
saveRDS(infopvalueimpactSENS, "infopvalueimpactSENSDIETREG.rds")##
infovariableimpactSENS<-readRDS("infovariableimpactSENSDIETREG.rds")
infopvalueimpactSENS<-readRDS("infopvalueimpactSENSDIETREG.rds")
setwd("D:/these/Axe_2")


#####FIGURE IMPACT#####

#infovariableALLtop<-infovariableALL
minfoSENS<-list()
minfofigureSENS<-list()
for (sd in 1:100) {
  
  
  ###what are the significant variable?
  library(tidyr)
  namevariable<-c("GLOBAL", "Aquaculture", "Ornamental")
  minfo<-list()
  minfofigure<-list()
  
  
  for (y in 1) {
    colnames(infopvalueimpactSENS[[sd]][[y]])[4]<-"pvalue"
    rownames(infovariableimpactSENS[[sd]][[y]])<-infovariableimpactSENS[[sd]][[y]]$Row.names
    namepv<-subset(infopvalueimpactSENS[[sd]][[y]], infopvalueimpactSENS[[sd]][[y]]$pvalue <0.05)
    name<-namevariable[y]
    minfo[[name]]<-merge(infovariableimpactSENS[[sd]][[y]], namepv, by="row.names", all.x=T )
    rownames(minfo[[name]])<-minfo[[name]]$Row.names
    minfo[[name]]["(Intercept)","pvalue"]<-0
    if ((length(table(rownames(infopvalueimpactSENS[[sd]][[y]])=="Order"))==2)& (infopvalueimpactSENS[[sd]][[y]]["Order",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0
      
    }
    
    if ((length(table(rownames(infopvalueimpactSENS[[sd]][[y]])=="Nb.Diet:Order"))==2)& (infopvalueimpactSENS[[sd]][[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0
      
    }
    if ((length(table(rownames(infopvalueimpactSENS[[sd]][[y]])=="TL:Order"))==2)& (infopvalueimpactSENS[[sd]][[y]]["TL:Order",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0
      
    }
    if ((length(table(rownames(infopvalueimpactSENS[[sd]][[y]])=="Maj.Diet"))==2)& (infopvalueimpactSENS[[sd]][[y]]["Maj.Diet",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0
      
    }
    
    if ((length(table(rownames(infopvalueimpactSENS[[sd]][[y]])=="Maj.Region"))==2)& (infopvalueimpactSENS[[sd]][[y]]["Maj.Region",]$pvalue<0.05)) {
      minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0
      
    }
    
    if ((length(table(rownames(infopvalueimpactSENS[[sd]][[y]])=="RepGuild1"))==2)& (infopvalueimpactSENS[[sd]][[y]]["RepGuild1",]$pvalue<0.05)) {
      rownames(minfo[[name]])<-minfo[[name]]$Row.names
      minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0
      
    }
    minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
    minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
  }
  
  minfofigureALL<-list()
  for (x in 1:1) {
    minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
    minfofigureALL[[namevariable[x]]]<- minfofigure2
  }
  
  minfofigureSENS[[sd]]<-minfofigureALL
}

####save
setwd("D:/these/Axe_2/outputs/sensitivity-analysis")
saveRDS(minfofigureSENS, "minfofigureimpactSENSDIETREG.rds")##
setwd("D:/these/Axe_2")

###mean + standard err of estimates for significative variables
library(data.table)

##global
figinfoSENS<-NULL
figinfoSENS$Row.names<-row.names(minfofigureSENS[[sd]]$GLOBAL) ##to initiate the variable

# for (sd in 1:5) {
# bloup<-minfofigureSENS[[sd]]$GLOBAL["Estimate"]
# bloup$Row.names<-row.names(bloup)
# figinfoSENS<-merge(figinfoSENS, bloup, by="Row.names", all=T)
# }

for (sd in 1:100) {
  bloup<-minfofigureSENS[[sd]]$GLOBAL["Estimate"]
  bloup$Row.names<-row.names(bloup)
  #bloupp<-infopvalueimpactSENS[[sd]]$globalintro["pvalue"]
  #bloupp$Row.names<-row.names(bloupp)
  figinfoSENS<-merge(figinfoSENS, bloup, by="Row.names", all=T)
  #figinfoSENS<-merge(figinfoSENS, bloupp, by="Row.names", all.x=T)
}


####save###
setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Impact")
saveRDS(figinfoSENS, "figinfoSENSimpactDIETREG.rds")
figinfoSENS<-readRDS("figinfoSENSimpactDIETREG.rds")
setwd("D:/these/Axe_2")

#####Figures sensitivity analysis

##Count number of time variables are significative
na_count <- apply(figinfoSENS, 1, function(x) sum(is.na(x)))
figinfoSENS$signipvalue <- 100-na_count

##order variables

colnames(figinfoSENS)<-c("Row.names",sprintf(paste0("Estimate", 1:100)),"signipvalue")

intervariable<-c("Nb.Diet:OrderCypriniformes", "Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther",
                 "Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes", 
                 "TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes",
                 "TL:Amplitudetemp")

intervariable<-as.data.frame(cbind(intervariable,rep("Interaction", length(intervariable))))
colnames(intervariable)<-c("Variable", "category")



intervariable<-c("Nb.Diet:OrderCypriniformes", "Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther",
                 "Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes",
                 "TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes",
                 "TL:Amplitudetemp")

intervariable<-as.data.frame(cbind(intervariable,rep("Interaction", length(intervariable))))
colnames(intervariable)<-c("Variable", "category")

ecovariable<-c("Amplitudetemp", "MaxBio5", 
               "Nb.Native.Region",
               "Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental")


ecovariable<-as.data.frame(cbind(ecovariable,rep("Ecological", length(ecovariable))))
colnames(ecovariable)<-c("Variable", "category")

traitvariable<-c("Nb.Diet","Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton",
                 "BlBd", "PFiBd", "RepGuild1.L", "RepGuild1.Q", "TL", "OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes")

traitvariable<-as.data.frame(cbind(traitvariable,rep("Traits", length(traitvariable))))
colnames(traitvariable)<-c("Variable", "category")

sociovariable<-c("Accidental" ,"Sport.Angling","Aquaculture","Diffusion","fisheries",
                 "Species.control","ornamental", "Nb_reasons")

sociovariable<-as.data.frame(cbind(sociovariable,rep("Socio-economic", length(sociovariable))))
colnames(sociovariable)<-c("Variable", "category")


####table attributing categories
categories<-rbind(intervariable, ecovariable, traitvariable, sociovariable)
colnames(categories)[1]<-"Row.names"
figinfoSENSorder<-merge(figinfoSENS, categories, by="Row.names", all.x=T)
figinfoSENSorder$Row.names <- factor(figinfoSENSorder$Row.names,      # Reordering variable factor levels for facet wrap
                                            levels = c(categories$Row.names, "(Intercept)"))


dat.m <- melt(figinfoSENSorder, id.vars = c("Row.names", "signipvalue", "category"))
data<-na.omit(dat.m)
##figure
###get my observed value
#data2<-readRDS("D:/these/Axe_2/Figures_results/Impact/SignificantvariablepvalueIMPACTESSAICHANGE.rds")


library(RColorBrewer)
myPalette <- colorRampPalette(c("blue", "red"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,100))

# library(RColorBrewer)
# myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
# sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,100))


impactsens<-ggplot() +
  geom_boxplot(data=data, aes(x=value, y=Row.names, col=signipvalue))+
  sc+
  #geom_point(data=data2, aes(x=Estimate, y=Variable), color="blue")+
  geom_vline(xintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  scale_x_continuous() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour="grey95"),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        axis.text.y =element_text(size = 20, color = "black"),
        axis.text.x=element_text(hjust = 1, size = 20, color = "black"),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 20, color = "black"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"))


x11()
impactsens

setwd("D:/these/Axe_2/outputs/sensitivity-analysis/Impact")
pdf("impact-Graph-DIETREG.pdf", 14,14)
# 2. Create a plot
impactsens
# Close the pdf file
dev.off()




png("impact-Graph-DIETREG.png", width = 1200, height = 1500, units = "px")
# 2. Create a plot
impactsens
# Close the pdf file
dev.off()

#figinfoSENS$mean<-rowMeans(as.data.frame(figinfoSENS[,-1]), na.rm = T)


##for each pathway, only keep the 10 most important variables (variable importance)
#infovariableALLtop<-infovariableALL %>% group_by(Pathway) %>% slice_max(order_by = VarImp, n = 10)

#infovariableALLtop<-infovariableALL
# 
# 
# ###what are the significant variable?
# library(tidyr)
# namevariable<-c("GLOBAL")
# minfo<-list()
# minfofigure<-list()
# 
# for (y in 1:1) {
#   colnames(infopvalueimpact[[y]])[4]<-"pvalue"
#   rownames(infovariableimpact[[y]])<-infovariableimpact[[y]]$Row.names
#   namepv<-subset(infopvalueimpact[[y]], infopvalueimpact[[y]]$pvalue <0.05)
#   name<-namevariable[y]
#   minfo[[name]]<-merge(infovariableimpact[[y]], namepv, by="row.names", all.x=T )
#   rownames(minfo[[name]])<-minfo[[name]]$Row.names
#   minfo[[name]]["(Intercept)","pvalue"]<-0
#   if ((length(table(rownames(infopvalueimpact[[y]])=="Order"))==2)& (infopvalueimpact[[y]]["Order",]$pvalue<0.05)) {
#     rownames(minfo[[name]])<-minfo[[name]]$Row.names 
#     minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0
#     
#   }
#   
#   if ((length(table(rownames(infopvalueimpact[[y]])=="Nb.Diet:Order"))==2)& (infopvalueimpact[[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
#     rownames(minfo[[name]])<-minfo[[name]]$Row.names 
#     minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0
#     
#   }
#   if ((length(table(rownames(infopvalueimpact[[y]])=="TL:Order"))==2)& (infopvalueimpact[[y]]["TL:Order",]$pvalue<0.05)) {
#     rownames(minfo[[name]])<-minfo[[name]]$Row.names 
#     minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0
#     
#   }
#   if ((length(table(rownames(infopvalueimpact[[y]])=="Maj.Diet"))==2)& (infopvalueimpact[[y]]["Maj.Diet",]$pvalue<0.05)) {
#     rownames(minfo[[name]])<-minfo[[name]]$Row.names 
#     minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0
#     
#   }
#   
#   if ((length(table(rownames(infopvalueimpact[[y]])=="Maj.Region"))==2)& (infopvalueimpact[[y]]["Maj.Region",]$pvalue<0.05)) {
#     minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0
#     
#   }
#   
#   if ((length(table(rownames(infopvalueimpact[[y]])=="RepGuild1"))==2)& (infopvalueimpact[[y]]["RepGuild1",]$pvalue<0.05)) {
#     rownames(minfo[[name]])<-minfo[[name]]$Row.names 
#     minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0
#     
#   }
#   minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
#   minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
# }
# 
# minfofigureALL<-NULL
# for (x in 1:1) {
#   minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
#   minfofigureALL<-rbind(minfofigureALL, minfofigure2)
# }
# 
# 
# #minfofigureALL$Variable<-rownames(minfofigureALL)
# names(minfofigureALL)<-c("Variable","Estimate","Se","VarImp","pvalue","Pathway")
# 
# ##order variable in categories
# intervariable<-c("Nb.Diet:OrderCypriniformes", "Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther",
#                  "Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes", 
#                  "TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes",
#                  "TL:Amplitudetemp")
# 
# intervariable<-as.data.frame(cbind(intervariable,rep("Interaction", length(intervariable))))
# colnames(intervariable)<-c("Variable", "category")
# 
# ecovariable<-c("Amplitudetemp", "MaxBio5", 
#                "Nb.Native.Region",
#                "Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental")
# 
# 
# ecovariable<-as.data.frame(cbind(ecovariable,rep("Ecological", length(ecovariable))))
# colnames(ecovariable)<-c("Variable", "category")
# 
# traitvariable<-c("Nb.Diet","Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton",
#                  "BlBd", "PFiBd", "RepGuild1.L", "RepGuild1.Q", "TL", "OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes")
# 
# traitvariable<-as.data.frame(cbind(traitvariable,rep("Traits", length(traitvariable))))
# colnames(traitvariable)<-c("Variable", "category")
# 
# sociovariable<-c("Accidental" ,"Sport.Angling","Aquaculture","Diffusion","fisheries",
#                  "Species.control","ornamental", "Nb_reasons")
# 
# sociovariable<-as.data.frame(cbind(sociovariable,rep("Socio-economic", length(sociovariable))))
# colnames(sociovariable)<-c("Variable", "category")
# 
# ####table attributing categories
# categories<-rbind(intervariable, ecovariable, traitvariable, sociovariable)
# infovariableimpactALLtop<-merge(minfofigureALL, categories, by="Variable", all.x=T)
# 
# 
# 
# 
# 
# infovariableimpactALLtop$Pathway <- factor(infovariableimpactALLtop$Pathway,      # Reordering group factor levels for facet wrap
#                                            levels = c("GLOBAL", "Aquaculture", "Ornamental"))
# 
# # infovariableimpactALLtop$category <- factor(infovariableimpactALLtop$category,      # Reordering group factor levels for facet wrap
# #                                       levels = c("Ecological", "Socio-economic", "Traits", "Interaction"))
# 
# 
# infovariableimpactALLtop$Variable <- factor(infovariableimpactALLtop$Variable,      # Reordering variable factor levels for facet wrap
#                                             levels = c(categories$Variable, "(Intercept)"))
# 
# levels(infovariableimpactALLtop$Pathway)[levels(infovariableimpactALLtop$Pathway)=="GLOBAL"]<-"Global - 109 species "
# # levels(infovariableimpactALLtop$Pathway)[levels(infovariableimpactALLtop$Pathway)=="Aquaculture"]<-"Aquaculture - 154 species "
# # levels(infovariableimpactALLtop$Pathway)[levels(infovariableimpactALLtop$Pathway)=="Ornamental"]<-"Ornamental - 126 species "
# 
# #my_breaks = c(0.5,1,2,3,4,10,16)##to color varimp
# impactgraph<-ggplot(as.data.frame(infovariableimpactALLtop), aes(x=factor(Variable))) +
#   geom_hline(yintercept=0, linetype="dashed", 
#              color = "darkgrey", size=0.2) +
#   geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-2*Se,ymax=Estimate+2*Se, fill=category),stat="identity")+
#   facet_wrap(~Pathway,nrow=1)+
#   coord_flip()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major.y = element_line(colour="grey95"),
#         panel.border = element_blank(),
#         axis.title.x = element_text(size = 20, color = "black"),
#         axis.title.y = element_text(size = 20, color = "black"),
#         axis.text.y =element_text(size = 20, color = "black"),
#         axis.text.x=element_text(hjust = 1, size = 20, color = "black"),
#         panel.background = element_blank(),
#         strip.text.x = element_text(size = 20, color = "black"),
#         strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"))
# 
# #scale_fill_gradientn(colours = c("blue", "red"), name = "Variable Importance", trans = "log",breaks = my_breaks, labels = my_breaks)
# 
# pdf("impact-Graph.pdf", 14,14) 
# # 2. Create a plot
# impactgraph
# # Close the pdf file
# dev.off() 
# 
# 
# 
# 
# png("impact-Graph.png", width = 1200, height = 1500, units = "px") 
# # 2. Create a plot
# impactgraph
# # Close the pdf file
# dev.off() 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
