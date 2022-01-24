
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
INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")

INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))
class(INTRO$RepGuild1)

#data
no.na.data <- na.omit(INTRO[c("Species","nb.country.intro","TL" ,
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "Accidental" ,
                              "Sport.Angling","Aquaculture","Diffusion","fisheries",
                              "Species.control","ornamental", #"research",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",
                              "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd"
)])


#####how many species with several diet or region taken hasard?

manydiet<-subset(no.na.data, NbDietMax>1) ##37 (2 with several region)
saveRDS(manydiet, "D:/these/Axe_2/outputs/Specieswithseveraldiet_poolused.rds")
manyregion<- subset(no.na.data, NbregionMax>1) ##2 
saveRDS(manyregion, "D:/these/Axe_2/outputs/Specieswithseveralregion_poolused.rds")

##only keep values >0 
table(no.na.data$nb.country.intro) 
sum(table(no.na.data$nb.country.intro))
no.na.data.1<-subset(no.na.data, nb.country.intro > 0)
#plot(table(no.na.data.1$nb.country.intro))




infomodelglobalintro<-data.frame(matrix(ncol = 6, nrow = 1))
rownames(infomodelglobalintro)<-c("globalintro")
colnames(infomodelglobalintro)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")
infovariableglobalintro<-list()
infopvalueglobalintro<-list()

# #####Verify correlations - to do with each data (per pathways)#########
# ##quanti variables 
# Introquanti<-na.omit(no.na.data.1[,c("TL" ,
#                                    "NbDietMax", "Nb.Diet",
#                                    "Amplitudetemp" ,
#                                    "MaxBio5",
#                                    "NbregionMax", "Nb.Native.Region",
#                                    "Nb_reasons", "BlBd", "PFiBd")])
# 
# 
# corr <- data.frame(lapply(Introquanti, as.numeric))
# 
# Correlations<-ggcorr(corr,
#                      method = c("pairwise", "spearman"),
#                      label_size = 4,
#                      label = T,
#                      hjust = 1,
#                      label_color = "grey50",
#                      layout.exp = 6,
#                      color = "black")
# x11()
# Correlations
# 
# ##quali variables
# Introquali<-no.na.data.1[,c("Order",
#                           "RepGuild1", "Maj.Region", "Used_by_humans")]
# 

# 
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# 
# Introquali<-no.na.data.1[,c("Maj.Diet" ,
#                           "RepGuild1" ,
#                           "Accidental" ,
#                           "Sport.Angling","Aquaculture","Diffusion","fisheries",
#                           "Species.control","ornamental",
#                           "Maj.Region",
#                           "Order", "Used_by_humans")]
# x11()
# model.matrix(~0+., data=Introqualiquanti) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
#
#####quali-quanti variables 
# Introqualiquanti<-no.na.data.1[,c("Maj.Diet" ,
#                           "RepGuild1" , "Nb.Diet", "TL",
#                           "Maj.Region","BlBd", "PFiBd",
#                           "Order", "Used_by_humans")]
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# x11()
# model.matrix(~0+., data=Introqualiquanti) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))

##prop pressure corrélée avec nombre de raison d'intro
##amplitude de température inversement corrélée avec min6



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
car::vif(mod) ##si très sup à 1 alors multicol(à partir de 3)
no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)

###Residus
res1<-resid(mod)
res1<-as.data.frame(res1)


########Overdispersion : Negative binomial#
modnb<-glm.nb(nb.country.intro~TL + Maj.Diet + Nb.Diet+
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
infomodelglobalintro[1,3]<-AIC(modnb)


###step AIC
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

png("./Figures_results/Introduction/HistoRes-globalintro.png", width = 1000, height = 1000, units = "px") 
# 2. Create a plot
hist(res$res, breaks = 15, main = paste("Introduction - " , rownames(infomodelglobalintro)))
# Close the pdf file
dev.off() 



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
setwd("D:/these/Axe_2")
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")

INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))
class(INTRO$RepGuild1)


variable<-c("nb.country.intro.aqua", "nb.country.intro.orna", "nb.country.intro.fish", "nb.country.intro.acci", "nb.country.intro.angl")
infomodelintro<-data.frame(matrix(ncol = 6, nrow = 5))
rownames(infomodelintro)<-c("Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") #"Species control", , "Diffusion"
colnames(infomodelintro)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")
# 
# infomodelintro<-data.frame(matrix(ncol = 5, nrow = 6))
# rownames(infomodelintro)<-c("Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling")
# colnames(infomodelintro)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2")


# infovariableintro<-data.frame(matrix(ncol = 24, nrow = 13))
# rownames(infovariableintro)<-c("TL" ,"Maj.Diet" , "Nb.Diet",
#                             "RepGuild1" , "Amplitudetemp" ,"MaxBio5",
#                             "Maj.Region", "Nb.Native.Region",
#                             "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd", "Order:Nb.Diet", "Order:Maj.Diet", "Order:TL")
# 
# 
# # colnames(infovariableintro)<-c("Aquaculture-es","Aquaculture-er","Aquaculture-vi",  
# #                        "Ornamental-es", "Ornamental-er","Ornamental-vi", "Fisheries-es", "Fisheries-er","Fisheries-vi", "Accidental-es","Accidental-er","Accidental-vi", 
#                        "Sport angling-es","Sport angling-er","Sport angling-vi") #, "Diffusion-es","Diffusion-er","Diffusion-vi""Species control-es","Species control-er","Species control-vi",
# 


infovariableintro<-list()
infopvalueintro<-list()



# colnames(infovariableintro)<-c("TL-estimates", "TL-error", "TL-varimp", "Maj.Diet-estimates" , "Maj.Diet-error" ,"Maj.Diet-varimp" , "Nb.Diet-estimates","Nb.Diet-error","Nb.Diet-varimp",
#                           "RepGuild1-estimates" ,"RepGuild1-error" ,"RepGuild1-varimp" , "Amplitudetemp-estimates" ,"Amplitudetemp-error" ,"Amplitudetemp-varimp" ,"MaxBio5-estimates",
#                           "MaxBio5-error","MaxBio5-varimp","Maj.Region-estimates","Maj.Region-error","Maj.Region-varimp", "Nb.Native.Region-estimates","Nb.Native.Region-error","Nb.Native.Region-varimp",
#                           "Order-estimates","Order-error","Order-varimp", "Used_by_humans-estimates","Used_by_humans-error","Used_by_humans-varimp",
#                           "Nb_reasons-estimates","Nb_reasons-error","Nb_reasons-varimp", "BlBd-estimates","BlBd-error","BlBd-varimp", "PFiBd-estimates","PFiBd-error","PFiBd-varimp")



for (i in 1:2) {
  
  
  
  no.na.data <- na.omit(INTRO[c("Species",variable[i],"TL" ,
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
  
  png(paste("./Figures_results/Introduction/HistoRes-Introduction",rownames(infomodelintro[i,]),".png"), width = 1000, height = 1000, units = "px") 
  # 2. Create a plot
  hist(res$res, breaks = 15, main = paste("Introduction - " , rownames(infomodelintro[i,])))
  # Close the pdf file
  dev.off() 
  
  
  
  
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



bloup<-c("GLOBAL","Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") 

infovariableintroALL<-NULL
for (x in 1:3) {
  infovariableintro2<-cbind(infovariableintro[[x]], rep(bloup[x], length(infovariableintro[[x]][,1])))
  infovariableintroALL<-rbind(infovariableintroALL, infovariableintro2)
}
names(infovariableintroALL)<-c("Variable","Estimate","Se","VarImp","Pathway")

infomodelintroALL<-rbind(infomodelintro, infomodelglobalintro)

####################save the all_good table###########
setwd("D:/these/Axe_2")
write.csv2(as.data.frame(infomodelintroALL), "./Figures_results/Introduction/infomodelintro.csv")
write.csv2(as.data.frame(infovariableintroALL), "./Figures_results/Introduction/infovariableintro.csv")

setwd("D:/these/Axe_2/Figures_results/Introduction")
lapply(1:length(infopvalueintro), function(i) write.csv(infopvalueintro[[i]], 
                                                file = paste0(names(infopvalueintro[i]), "INTRO-pvalue.csv"),
                                                row.names = T))

#####FIGURE INTRODUCTION#####

##for each pathway, only keep the 10 most important variables (variable importance)
#infovariableALLtop<-infovariableALL %>% group_by(Pathway) %>% slice_max(order_by = VarImp, n = 10)

#infovariableALLtop<-infovariableALL


###what are the significant variable?
library(tidyr)
namevariable<-c("GLOBAL", "Aquaculture", "Ornamental")
minfo<-list()
minfofigure<-list()

for (y in 1:3) {
  colnames(infopvalueintro[[y]])[4]<-"pvalue"
  rownames(infovariableintro[[y]])<-infovariableintro[[y]]$Row.names                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
  namepv<-subset(infopvalueintro[[y]], infopvalueintro[[y]]$pvalue <0.05)
  name<-namevariable[y]
  minfo[[name]]<-merge(infovariableintro[[y]], namepv, by="row.names", all.x=T )
  rownames(minfo[[name]])<-minfo[[name]]$Row.names
  minfo[[name]]["(Intercept)","pvalue"]<-0
  if ((length(table(rownames(infopvalueintro[[y]])=="Order"))==2)& (infopvalueintro[[y]]["Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueintro[[y]])=="Nb.Diet:Order"))==2)& (infopvalueintro[[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalueintro[[y]])=="TL:Order"))==2)& (infopvalueintro[[y]]["TL:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalueintro[[y]])=="Maj.Diet"))==2)& (infopvalueintro[[y]]["Maj.Diet",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueintro[[y]])=="Maj.Region"))==2)& (infopvalueintro[[y]]["Maj.Region",]$pvalue<0.05)) {
    minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueintro[[y]])=="RepGuild1"))==2)& (infopvalueintro[[y]]["RepGuild1",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0
    
  }
  minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
  minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
}

minfofigureALL<-NULL
for (x in 1:3) {
  minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
  minfofigureALL<-rbind(minfofigureALL, minfofigure2)
}


#minfofigureALL$Variable<-rownames(minfofigureALL)
names(minfofigureALL)<-c("Variable","Estimate","Se","VarImp","pvalue","Pathway")

##order variable in categories
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
infovariableintroALLtop<-merge(minfofigureALL, categories, by="Variable", all.x=T)





infovariableintroALLtop$Pathway <- factor(infovariableintroALLtop$Pathway,      # Reordering group factor levels for facet wrap
                                     levels = c("GLOBAL", "Aquaculture", "Ornamental"))

# infovariableintroALLtop$category <- factor(infovariableintroALLtop$category,      # Reordering group factor levels for facet wrap
#                                       levels = c("Ecological", "Socio-economic", "Traits", "Interaction"))


infovariableintroALLtop$Variable <- factor(infovariableintroALLtop$Variable,      # Reordering variable factor levels for facet wrap
                                      levels = c(categories$Variable, "(Intercept)"))

levels(infovariableintroALLtop$Pathway)[levels(infovariableintroALLtop$Pathway)=="GLOBAL"]<-"Global - 307 species "
levels(infovariableintroALLtop$Pathway)[levels(infovariableintroALLtop$Pathway)=="Aquaculture"]<-"Aquaculture - 154 species "
levels(infovariableintroALLtop$Pathway)[levels(infovariableintroALLtop$Pathway)=="Ornamental"]<-"Ornamental - 126 species "


###save
saveRDS(infovariableintroALLtop, "SignificantvariablepvalueINTRO.rds")
#readRDS("SignificantvariablepvalueINTRO.rds")
# ##rename all variable for the figure
# infovariableintroALLtop$Variable <- as.character(infovariableintroALLtop$Variable)
# 
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'Nb.Diet:OrderCypriniformes'] <- 'Nb Diet/Order (Cypriniformes)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'Nb.Diet:OrderCyprinodontiformes'] <- 'Nb Diet/Order (Cyprinodontiformes)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'Nb.Diet:OrderOther'] <- 'Nb Diet/Order (Other)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'Nb.Diet:OrderPerciformes'] <- 'Nb Diet/Order (Perciformes)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'Nb.Diet:OrderSiluriformes'] <- 'Nb Diet/Order (Siluriformes)'
# 
# 
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'TL:OrderCypriniformes'] <- 'TL/Order (Cypriniformes)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'TL:OrderCyprinodontiformes'] <- 'TL/Order (Cyprinodontiformes)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'TL:OrderOther'] <- 'TL/Order (Other)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'TL:OrderPerciformes'] <- 'TL/Order (Perciformes)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'TL:OrderSiluriformes'] <- 'TL/Order (Siluriformes)'
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'TL:AmplitudeTemp'] <- 'TL/Temperature amplitude'
# 
# infovariableintroALLtop$Variable[infovariableintroALLtop$Variable == 'Amplitudetemp'] <- 'Temperature amplitude'
# 
# ##reordering
# infovariableintroALLtop$Variable <- factor(infovariableintroALLtop$Variable,      # Reordering variable factor levels for facet wrap
#                                            levels = c(categories$Variable, "(Intercept)"))


#my_breaks = c(0.5,1,2,3,4,10,16)##to color varimp
intrograph<-ggplot(as.data.frame(infovariableintroALLtop), aes(x=factor(Variable))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-2*Se,ymax=Estimate+2*Se, fill=category),stat="identity")+
  facet_wrap(~Pathway,nrow=1)+
  coord_flip()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour="grey95"),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 15, color = "black"),
        axis.title.y = element_text(size = 15, color = "black"),
        axis.text.y =element_text(size = 15, color = "black"),
        axis.text.x=element_text(hjust = 1, size = 15, color = "black"),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 15, color = "black"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"))

#scale_fill_gradientn(colours = c("blue", "red"), name = "Variable Importance", trans = "log",breaks = my_breaks, labels = my_breaks)


setwd("D:/these/Axe_2/Figures_results/Introduction")
pdf("Intro-Graph.pdf", 17,14) 
# 2. Create a plot
intrograph
# Close the pdf file
dev.off() 




png("Intro-Graph.png", width = 1900, height = 1500, units = "px") 
# 2. Create a plot
intrograph
# Close the pdf file
dev.off() 

















####/////////////////////////////////////////////////ESTABLISHMENT /////////////////////////////////////////////////////////////###########
rm(list = ls())
setwd("D:/these/Axe_2")
#INTRO55<-read.csv2("./outputs/INTRO_all_good2.csv")
#INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdiet.csv")
INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")
#INTROtest<-subset(INTRO, select=-c(maxdiet.status, maxregion.status))
#blblblbl<-setdiff(INTRO55,INTROtest)
#setdiff(INTRO55$Maj.Region,INTRO$Maj.Region)


INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))
class(INTRO$RepGuild1)

#data
no.na.data <- na.omit(INTRO[c("Species","nb.country.establish","TL" ,
                              "Maj.Diet" ,"NbDietMax", "Nb.Diet",
                              "RepGuild1" , "Amplitudetemp" ,
                              "MaxBio5", "Accidental" ,
                              "Sport.Angling","Aquaculture","Diffusion","fisheries",
                              "Species.control","ornamental", #"research",
                              "Maj.Region",  "NbregionMax", "Nb.Native.Region",
                               "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd"
)])

#setdiff(no.na.data, no.na.data55)

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

# #####Verify correlations - to do with each data (per pathways)#########
# ##quanti variables 
# Introquanti<-na.omit(no.na.data.1[,c("TL" ,
#                                    "NbDietMax", "Nb.Diet",
#                                    "Amplitudetemp" ,
#                                    "MaxBio5",
#                                    "NbregionMax", "Nb.Native.Region",
#                                    "Nb_reasons", "BlBd", "PFiBd")])
# 
# 
# corr <- data.frame(lapply(Introquanti, as.numeric))
# 
# Correlations<-ggcorr(corr,
#                      method = c("pairwise", "spearman"),
#                      label_size = 4,
#                      label = T,
#                      hjust = 1,
#                      label_color = "grey50",
#                      layout.exp = 6,
#                      color = "black")
# x11()
# Correlations
# 
# ##quali variables
# Introquali<-no.na.data.1[,c("Order",
#                           "RepGuild1", "Maj.Region", "Used_by_humans")]
# 

# 
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# 
# Introquali<-no.na.data.1[,c("Maj.Diet" ,
#                           "RepGuild1" ,
#                           "Accidental" ,
#                           "Sport.Angling","Aquaculture","Diffusion","fisheries",
#                           "Species.control","ornamental",
#                           "Maj.Region",
#                           "Order", "Used_by_humans")]
# x11()
# model.matrix(~0+., data=Introqualiquanti) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
#
#####quali-quanti variables 
# Introqualiquanti<-no.na.data.1[,c("Maj.Diet" ,
#                           "RepGuild1" , "Nb.Diet", "TL",
#                           "Maj.Region","BlBd", "PFiBd",
#                           "Order", "Used_by_humans")]
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# x11()
# model.matrix(~0+., data=Introqualiquanti) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))

##prop pressure corrélée avec nombre de raison d'intro
##amplitude de température inversement corrélée avec min6



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

png("./Figures_results/Establishment/HistoRes-globalesta.png", width = 1000, height = 1000, units = "px") 
# 2. Create a plot
hist(res$res, breaks = 15, main = paste("Establishment - " , rownames(infomodelglobal)))
# Close the pdf file
dev.off() 


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
setwd("D:/these/Axe_2")
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")
INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")

INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))
class(INTRO$RepGuild1)


variable<-c("nb.country.esta.aqua", "nb.country.esta.orna", "nb.country.esta.fish", "nb.country.esta.acci", "nb.country.esta.angl")
infomodel<-data.frame(matrix(ncol = 6, nrow = 5))
rownames(infomodel)<-c("Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") #"Species control", , "Diffusion"
colnames(infomodel)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")
# 
# infomodel<-data.frame(matrix(ncol = 5, nrow = 6))
# rownames(infomodel)<-c("Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling")
# colnames(infomodel)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2")


# infovariable<-data.frame(matrix(ncol = 24, nrow = 13))
# rownames(infovariable)<-c("TL" ,"Maj.Diet" , "Nb.Diet",
#                             "RepGuild1" , "Amplitudetemp" ,"MaxBio5",
#                             "Maj.Region", "Nb.Native.Region",
#                             "Order", "Used_by_humans", "Nb_reasons", "BlBd", "PFiBd", "Order:Nb.Diet", "Order:Maj.Diet", "Order:TL")
# 
# 
# # colnames(infovariable)<-c("Aquaculture-es","Aquaculture-er","Aquaculture-vi",  
# #                        "Ornamental-es", "Ornamental-er","Ornamental-vi", "Fisheries-es", "Fisheries-er","Fisheries-vi", "Accidental-es","Accidental-er","Accidental-vi", 
#                        "Sport angling-es","Sport angling-er","Sport angling-vi") #, "Diffusion-es","Diffusion-er","Diffusion-vi""Species control-es","Species control-er","Species control-vi",
# 


infovariable<-list()
infopvalue<-list()



# colnames(infovariable)<-c("TL-estimates", "TL-error", "TL-varimp", "Maj.Diet-estimates" , "Maj.Diet-error" ,"Maj.Diet-varimp" , "Nb.Diet-estimates","Nb.Diet-error","Nb.Diet-varimp",
#                           "RepGuild1-estimates" ,"RepGuild1-error" ,"RepGuild1-varimp" , "Amplitudetemp-estimates" ,"Amplitudetemp-error" ,"Amplitudetemp-varimp" ,"MaxBio5-estimates",
#                           "MaxBio5-error","MaxBio5-varimp","Maj.Region-estimates","Maj.Region-error","Maj.Region-varimp", "Nb.Native.Region-estimates","Nb.Native.Region-error","Nb.Native.Region-varimp",
#                           "Order-estimates","Order-error","Order-varimp", "Used_by_humans-estimates","Used_by_humans-error","Used_by_humans-varimp",
#                           "Nb_reasons-estimates","Nb_reasons-error","Nb_reasons-varimp", "BlBd-estimates","BlBd-error","BlBd-varimp", "PFiBd-estimates","PFiBd-error","PFiBd-varimp")



for (i in 1:2) {
  


no.na.data <- na.omit(INTRO[c("Species",variable[i],"TL" ,
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

png(paste("./Figures_results/Establishment/HistoRes-Establishment",rownames(infomodel[i,]),".png"), width = 1000, height = 1000, units = "px") 
# 2. Create a plot
hist(res$res, breaks = 15, main = paste("Establishment - " , rownames(infomodel[i,])))
# Close the pdf file
dev.off() 



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



bloup<-c("GLOBAL","Aquaculture", "Ornamental", "Fisheries", "Accidental", "Sport angling") 

infovariableALL<-NULL
for (x in 1:3) {
  infovariable2<-cbind(infovariable[[x]], rep(bloup[x], length(infovariable[[x]][,1])))
  infovariableALL<-rbind(infovariableALL, infovariable2)
}
names(infovariableALL)<-c("Variable","Estimate","Se","VarImp","Pathway")

infomodelALL<-rbind(infomodelglobal,infomodel)

####################save the all_good table###########
setwd("D:/these/Axe_2")
write.csv2(as.data.frame(infomodelALL), "./Figures_results/Establishment/infomodelESSAICHANGE.csv")
write.csv2(as.data.frame(infovariableALL), "./Figures_results/Establishment/infovariableESSAICHANGE.csv")
#write.csv2(as.data.frame(infopvalue), "./Figures_results/Establishment/infopvalueESSAICHANGE.csv")
setwd("D:/these/Axe_2/Figures_results/Establishment")
lapply(1:length(infopvalue), function(i) write.csv(infopvalue[[i]], 
                                                        file = paste0(names(infopvalue[i]), "ESTA-pvalueESSAICHANGE.csv"),
                                                        row.names = T))



#####FIGURE ESTABLISHEMENT#####

##for each pathway, only keep the 10 most important variables (variable importance)
#infovariableALLtop<-infovariableALL %>% group_by(Pathway) %>% slice_max(order_by = VarImp, n = 10)

#infovariableALLtop<-infovariableALL


###what are the significant variable?
library(tidyr)
infopvalue$GLOBAL[,"pvalue"]<0.05
namevariable<-c("GLOBAL", "Aquaculture", "Ornamental")
minfo<-list()
minfofigure<-list()

for (y in 1:3) {
  colnames(infopvalue[[y]])[4]<-"pvalue"
  rownames(infovariable[[y]])<-infovariable[[y]]$Row.names                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
  namepv<-subset(infopvalue[[y]], infopvalue[[y]]$pvalue <0.05)
  name<-namevariable[y]
  minfo[[name]]<-merge(infovariable[[y]], namepv, by="row.names", all.x=T )
  rownames(minfo[[name]])<-minfo[[name]]$Row.names
  minfo[[name]]["(Intercept)","pvalue"]<-0
  if ((length(table(rownames(infopvalue[[y]])=="Order"))==2)& (infopvalue[[y]]["Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalue[[y]])=="Nb.Diet:Order"))==2)& (infopvalue[[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalue[[y]])=="TL:Order"))==2)& (infopvalue[[y]]["TL:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalue[[y]])=="Maj.Diet"))==2)& (infopvalue[[y]]["Maj.Diet",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalue[[y]])=="Maj.Region"))==2)& (infopvalue[[y]]["Maj.Region",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalue[[y]])=="RepGuild1"))==2)& (infopvalue[[y]]["RepGuild1",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0
    
  }
  minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
  minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
}

minfofigureALL<-NULL
for (x in 1:3) {
  minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
  minfofigureALL<-rbind(minfofigureALL, minfofigure2)
}


#minfofigureALL$Variable<-rownames(minfofigureALL)
names(minfofigureALL)<-c("Variable","Estimate","Se","VarImp","pvalue","Pathway")

##order variable in categories
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
infovariableALLtop<-merge(minfofigureALL, categories, by="Variable", all.x=T)





infovariableALLtop$Pathway <- factor(infovariableALLtop$Pathway,      # Reordering group factor levels for facet wrap
                         levels = c("GLOBAL", "Aquaculture", "Ornamental"))

# infovariableALLtop$category <- factor(infovariableALLtop$category,      # Reordering group factor levels for facet wrap
#                                       levels = c("Ecological", "Socio-economic", "Traits", "Interaction"))


infovariableALLtop$Variable <- factor(infovariableALLtop$Variable,      # Reordering variable factor levels for facet wrap
                                     levels = c(categories$Variable, "(Intercept)"))

levels(infovariableALLtop$Pathway)[levels(infovariableALLtop$Pathway)=="GLOBAL"]<-"Global - 213 species "
levels(infovariableALLtop$Pathway)[levels(infovariableALLtop$Pathway)=="Aquaculture"]<-"Aquaculture - 85 species "
levels(infovariableALLtop$Pathway)[levels(infovariableALLtop$Pathway)=="Ornamental"]<-"Ornamental - 69 species "


###save
saveRDS(infovariableALLtop, "SignificantvariablepvalueESTAESSAICHANGE.rds")
####


#my_breaks = c(0.5,1,2,3,4,10,16)##to color varimp
estagraph<-ggplot(as.data.frame(infovariableALLtop), aes(x=factor(Variable))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-2*Se,ymax=Estimate+2*Se, fill=category),stat="identity")+
  facet_wrap(~Pathway,nrow=1)+
   coord_flip()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour="grey95"),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.text.y =element_text(size = 11, color = "black"),
        axis.text.x=element_text(hjust = 1, size = 11, color = "black"),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 12, color = "black"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"))
 
  #scale_fill_gradientn(colours = c("blue", "red"), name = "Variable Importance", trans = "log",breaks = my_breaks, labels = my_breaks)

pdf("Esta-Graph.pdf", 17,14) 
# 2. Create a plot
estagraph
# Close the pdf file
dev.off() 




png("Esta-Graph.png", width = 1200, height = 1500, units = "px") 
# 2. Create a plot
estagraph
# Close the pdf file
dev.off() 




####################///////////////////////////////////////////////IMPACTS//////////////////////////////////////////////////////####################
#####GLOBAL#####
rm(list = ls())
setwd("D:/these/Axe_2")
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")
INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")

INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))




class(INTRO$RepGuild1)
no.na.data <- na.omit(INTRO[c("Species","nb.country.impeco", "TL" ,
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



# #####Verify correlations - to do with each data (per pathways)#
# ##quanti variables
# Introquanti<-na.omit(no.na.data.1[,c("TL" ,
#                                    "Nb.Diet",
#                                    "Amplitudetemp" ,
#                                    "MaxBio5",
#                                     "Nb.Native.Region",
#                                    "Nb_reasons", "BlBd", "PFiBd")])
# 
# 
# corr <- data.frame(lapply(Introquanti, as.numeric))
# 
# Correlations<-ggcorr(corr,
#                      method = c("pairwise", "spearman"),
#                      label_size = 4,
#                      label = T,
#                      hjust = 1,
#                      label_color = "grey50",
#                      layout.exp = 6,
#                      color = "black")
# x11()
# Correlations
# 
# ##quali variables
# Introquali<-no.na.data.1[,c("Order",
#                           "RepGuild1", "Maj.Region", "Used_by_humans")]
# 
# 
# 
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# 
# Introquali<-no.na.data.1[,c("Maj.Diet" ,
#                           "RepGuild1" ,
#                           "Accidental" ,
#                           "Sport.Angling","Aquaculture","Diffusion","fisheries",
#                           "Species.control","ornamental",
#                           "Maj.Region",
#                           "Order", "Used_by_humans")]
# x11()
# model.matrix(~0+., data=Introqualiquanti) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# 
# ####quali-quanti variables
# Introqualiquanti<-no.na.data.1[,c("Maj.Diet" ,
#                           "RepGuild1" , "Nb.Diet", "TL",
#                           "Maj.Region","BlBd", "PFiBd",
#                           "Order", "Used_by_humans")]
# model.matrix(~0+., data=Introquali) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# x11()
# model.matrix(~0+., data=Introqualiquanti) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_col = "black", lab_size=2)+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10))
# 
# #prop pressure corrélée avec nombre de raison d'intro
# #amplitude de température inversement corrélée avec min6



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
# sim <- simulateResiduals(mod, n=99)
# #plot(sim)
# #plot(mod)
# hist(residuals(mod), breaks = 50)
# testDispersion(sim)
# testDispersion(mod)
AIC(mod)
infomodelimpact[1,1]<-AIC(mod)

###multicolinéarité? --> si oui centering
no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)

###Residus
res1<-resid(mod)
res1<-as.data.frame(res1)

####remove les points avec des res1 sup à 2 (ou inf à -2)
# outliers<-subset(res1, res1>4 | res1<(-4))
# no.na.data.1<-no.na.data.1[!(rownames(no.na.data.1) %in% rownames(outliers)),]


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

png("./Figures_results/Impact/HistoRes-globalimpact.png", width = 1000, height = 1000, units = "px") 
# 2. Create a plot
hist(res$res, breaks = 15, main = paste("Impact - " , rownames(infomodelimpact)))
# Close the pdf file
dev.off() 

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




####################save the all_good table###########
setwd("D:/these/Axe_2")
write.csv2(as.data.frame(infomodelimpact), "./Figures_results/Impact/infomodelimpactESSAICHANGE.csv")
write.csv2(as.data.frame(infovariableimpact), "./Figures_results/Impact/infovariableimpactESSAICHANGE.csv")
#write.csv2(as.data.frame(infopvalueimpact), "./Figures_result/infopvalueimpact.csv")
setwd("D:/these/Axe_2/Figures_results/Impact")
lapply(1:length(infopvalueimpact), function(i) write.csv(infopvalueimpact[[i]], 
                                                        file = paste0(names(infopvalueimpact[i]), "IMPACT-pvalueESSAICHANGE.csv"),
                                                        row.names = T))


#####FIGURE IMPACT#####

##for each pathway, only keep the 10 most important variables (variable importance)
#infovariableALLtop<-infovariableALL %>% group_by(Pathway) %>% slice_max(order_by = VarImp, n = 10)

#infovariableALLtop<-infovariableALL


###what are the significant variable?
library(tidyr)
namevariable<-c("GLOBAL")
minfo<-list()
minfofigure<-list()

for (y in 1:1) {
  colnames(infopvalueimpact[[y]])[4]<-"pvalue"
  rownames(infovariableimpact[[y]])<-infovariableimpact[[y]]$Row.names
  namepv<-subset(infopvalueimpact[[y]], infopvalueimpact[[y]]$pvalue <0.05)
  name<-namevariable[y]
  minfo[[name]]<-merge(infovariableimpact[[y]], namepv, by="row.names", all.x=T )
  rownames(minfo[[name]])<-minfo[[name]]$Row.names
  minfo[[name]]["(Intercept)","pvalue"]<-0
  if ((length(table(rownames(infopvalueimpact[[y]])=="Order"))==2)& (infopvalueimpact[[y]]["Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueimpact[[y]])=="Nb.Diet:Order"))==2)& (infopvalueimpact[[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalueimpact[[y]])=="TL:Order"))==2)& (infopvalueimpact[[y]]["TL:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalueimpact[[y]])=="Maj.Diet"))==2)& (infopvalueimpact[[y]]["Maj.Diet",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueimpact[[y]])=="Maj.Region"))==2)& (infopvalueimpact[[y]]["Maj.Region",]$pvalue<0.05)) {
    minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueimpact[[y]])=="RepGuild1"))==2)& (infopvalueimpact[[y]]["RepGuild1",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0
    
  }
  minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
  minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
}

minfofigureALL<-NULL
for (x in 1:1) {
  minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
  minfofigureALL<-rbind(minfofigureALL, minfofigure2)
}


#minfofigureALL$Variable<-rownames(minfofigureALL)
names(minfofigureALL)<-c("Variable","Estimate","Se","VarImp","pvalue","Pathway")

##order variable in categories
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
infovariableimpactALLtop<-merge(minfofigureALL, categories, by="Variable", all.x=T)





infovariableimpactALLtop$Pathway <- factor(infovariableimpactALLtop$Pathway,      # Reordering group factor levels for facet wrap
                                          levels = c("GLOBAL", "Aquaculture", "Ornamental"))

# infovariableimpactALLtop$category <- factor(infovariableimpactALLtop$category,      # Reordering group factor levels for facet wrap
#                                       levels = c("Ecological", "Socio-economic", "Traits", "Interaction"))


infovariableimpactALLtop$Variable <- factor(infovariableimpactALLtop$Variable,      # Reordering variable factor levels for facet wrap
                                           levels = c(categories$Variable, "(Intercept)"))

levels(infovariableimpactALLtop$Pathway)[levels(infovariableimpactALLtop$Pathway)=="GLOBAL"]<-"Global - 109 species "
# levels(infovariableimpactALLtop$Pathway)[levels(infovariableimpactALLtop$Pathway)=="Aquaculture"]<-"Aquaculture - 154 species "
# levels(infovariableimpactALLtop$Pathway)[levels(infovariableimpactALLtop$Pathway)=="Ornamental"]<-"Ornamental - 126 species "



###save
saveRDS(infovariableimpactALLtop, "SignificantvariablepvalueIMPACTESSAICHANGE.rds")


#my_breaks = c(0.5,1,2,3,4,10,16)##to color varimp
impactgraph<-ggplot(as.data.frame(infovariableimpactALLtop), aes(x=factor(Variable))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-2*Se,ymax=Estimate+2*Se, fill=category),stat="identity")+
  facet_wrap(~Pathway,nrow=1)+
  coord_flip()+
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

#scale_fill_gradientn(colours = c("blue", "red"), name = "Variable Importance", trans = "log",breaks = my_breaks, labels = my_breaks)

pdf("impact-Graph.pdf", 17,14) 
# 2. Create a plot
impactgraph
# Close the pdf file
dev.off() 




png("impact-Graph.png", width = 1200, height = 1500, units = "px") 
# 2. Create a plot
impactgraph
# Close the pdf file
dev.off() 


####///////////////////////FIGURE ALL STEPS//////////////////////////////###################################################################

setwd("D:/these/Axe_2/Figures_results/Introduction")
Intro<-readRDS("SignificantvariablepvalueINTRO.rds")

setwd("D:/these/Axe_2/Figures_results/Establishment")
Esta<-readRDS("SignificantvariablepvalueESTAESSAICHANGE.rds")

setwd("D:/these/Axe_2/Figures_results/Impact")
Impact<-readRDS("SignificantvariablepvalueIMPACTESSAICHANGE.rds")

##global#############
Globalintro<-Intro %>% filter(Pathway == "Global - 307 species ")
Globalesta<-Esta %>% filter(Pathway == "Global - 213 species ")

globaldata<-rbind(Globalintro, Globalesta, Impact)
#remove the intercept
globaldata<-globaldata[globaldata$Variable !="(Intercept)",]

globalgraph<-ggplot(as.data.frame(globaldata), aes(x=factor(Variable), y=Estimate)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  geom_point(size=3, aes(color=category))+
  geom_errorbar(aes(ymin=Estimate-Se, ymax=Estimate+Se, color=category), 
                width=.7) +
  ylim(-1.5,1.7) +
  #geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-Se,ymax=Estimate+Se, fill=category),stat="identity")+
  facet_wrap(~Pathway,nrow=1)+
  coord_flip()+
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
globalgraph

setwd("D:/these/Axe_2/Figures_results")

pdf("global-GraphESSAICHANGE.pdf", 14,10) 
# 2. Create a plot
globalgraph
# Close the pdf file
dev.off() 




png("global-GraphESSAICHANGE.png", width = 1200, height = 1500, units = "px") 
# 2. Create a plot
globalgraph
# Close the pdf file
dev.off() 

####aquaculture####
aquaintro<-Intro %>% filter(Pathway == "Aquaculture - 154 species ")
aquaesta<-Esta %>% filter(Pathway == "Aquaculture - 85 species ")

aquadata<-rbind(aquaintro, aquaesta)
#remove the intercept
aquadata<-aquadata[aquadata$Variable !="(Intercept)",]

aquagraph<-ggplot(as.data.frame(aquadata), aes(x=factor(Variable), y=Estimate)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  geom_point(size=3, aes(color=category))+
  geom_errorbar(aes(ymin=Estimate-Se, ymax=Estimate+Se, color=category), 
                width=.7) +
  #geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-Se,ymax=Estimate+Se, fill=category),stat="identity")+
  facet_wrap(~Pathway,nrow=1)+
  coord_flip()+
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
aquagraph

setwd("D:/these/Axe_2/Figures_results")

pdf("aqua-GraphESSAICHANGE.pdf", 12,8) 
# 2. Create a plot
aquagraph
# Close the pdf file
dev.off() 




png("aqua-GraphESSAICHANGE.png", width = 1200, height = 1500, units = "px") 
# 2. Create a plot
aquagraph
# Close the pdf file
dev.off() 



#Ornamental##############
ornaintro<-Intro %>% filter(Pathway == "Ornamental - 126 species ")
ornaesta<-Esta %>% filter(Pathway == "Ornamental - 69 species ")

ornadata<-rbind(ornaintro, ornaesta)
#remove the intercept
ornadata<-ornadata[ornadata$Variable !="(Intercept)",]

ornagraph<-ggplot(as.data.frame(ornadata), aes(x=factor(Variable), y=Estimate)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  geom_point(size=3, aes(color=category))+
  geom_errorbar(aes(ymin=Estimate-Se, ymax=Estimate+Se, color=category), 
                width=.7) +
  #geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-Se,ymax=Estimate+Se, fill=category),stat="identity")+
  facet_wrap(~Pathway,nrow=1)+
  coord_flip()+
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


setwd("D:/these/Axe_2/Figures_results")

pdf("orna-GraphESSAICHANGE.pdf", 12,8) 
# 2. Create a plot
ornagraph
# Close the pdf file
dev.off() 




png("orna-GraphESSAICHANGE.png", width = 1200, height = 1500, units = "px") 
# 2. Create a plot
ornagraph
# Close the pdf file
dev.off() 
















####//////////////////////////////////////////////////////////////////////////////////////////////////////APPENDIX/////////////////////////////////////////////////////////////////////////////////////////////////////######
###ESTABLISHEMENT - GLOBAL but without the number of reason #####

rm(list = ls())
setwd("D:/these/Axe_2")
#INTRO<-read.csv2("./outputs/INTRO_all_good2.csv")


INTRO$RepGuild1<-factor(INTRO$RepGuild1, order = TRUE, 
                        levels = c("nonguarders", "guarders", "bearers"))
class(INTRO$RepGuild1)

#data
no.na.data <- na.omit(INTRO[c("Species","nb.country.establish","TL" ,
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


infomodelglobalappendix<-data.frame(matrix(ncol = 5, nrow = 1))
rownames(infomodelglobalappendix)<-c("GLOBAL")
colnames(infomodelglobalappendix)<-c("AIC mod. 1", "p-value overdispersion" ,"AIC mod. 2", "AIC mod. 3", "Pseudo-R2", "Shapiro-test")
infovariableglobalappendix<-list()
infopvalueglobalappendix<-list()

#models
mod<-glm(nb.country.establish~TL + Maj.Diet + Nb.Diet+
           MaxBio5 + Order + 
           RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
           Maj.Region + Nb.Native.Region + Used_by_humans+ #Nb_reasons + 
           BlBd + PFiBd+
           Order:Nb.Diet+Order:TL+
           Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL, 
         data = no.na.data.1, na.action = na.omit, family="poisson"  ) 



##Overdispersion?? 
summary(mod)
sim <- simulateResiduals(mod, n=99)
dis<-dispersiontest(mod) ##yes
testDispersion(sim)
infomodelglobalappendix[1,2]<-dis$p.value

#AIC
infomodelglobalappendix[1,1]<-AIC(mod)



###Multicolinearity
car::vif(mod) ##si très sup à 1 alors multicol(à partir de 3)
no.na.data.1$TL<-scale(no.na.data.1$TL, center=TRUE, scale=FALSE)  
no.na.data.1$Amplitudetemp<-scale(no.na.data.1$Amplitudetemp, center=TRUE, scale=FALSE)
no.na.data.1$Nb.Diet<-scale(no.na.data.1$Nb.Diet, center=TRUE, scale=FALSE)

###Residus
res1<-resid(mod)
res1<-as.data.frame(res1)


########Overdispersion : Negative binomial#
modnb<-glm.nb(nb.country.establish~TL + Maj.Diet + Nb.Diet+
                + MaxBio5 + Order + 
                RepGuild1  +  Amplitudetemp  + Amplitudetemp:TL +
                Maj.Region + Nb.Native.Region + Used_by_humans+ #Nb_reasons + 
                BlBd + PFiBd+
                Order:Nb.Diet+Order:TL+
                Accidental + ornamental + ornamental:TL + Sport.Angling +Sport.Angling:TL + Aquaculture +  Aquaculture:TL, 
              data = no.na.data.1, link = log, control = glm.control(maxit = 50))

summary(modnb)

##still overdispersion? :
sim <- simulateResiduals(modnb, n=99)
testDispersion(sim)
shapiro.test(residuals(modnb))

hist(residuals(modnb), breaks = 20)
AIC(modnb)
infomodelglobalappendix[1,3]<-AIC(modnb)


###step AIC
modnbfiltre<- stepAIC(modnb, direction = "both")
modnbfiltre$theta
modnb$theta
infomodelglobalappendix[1,4]<-AIC(modnbfiltre)

##shap test
shap<-shapiro.test(residuals(modnbfiltre))
infomodelimpact[1,6]<-shap$p.value

###Residus
res<-resid(modnbfiltre)
res<-as.data.frame(res)

######pvalues
summary(modnbfiltre)
anov<-Anova(modnbfiltre, test='F')
infopvalueglobalappendix[["GLOBAL"]]<-anov


######estimates
est<-summary(modnbfiltre)
str(est)
est$coefficients
estimates<-est$coefficients[,c("Estimate","Std. Error")]

##variable importance
imp<-varImp(modnbfiltre)
infovariableglobalappendix[["GLOBAL"]]<-merge(estimates, imp,by="row.names",all.x=TRUE)


#####pseudo r2
R2<-1-(modnbfiltre$deviance/modnbfiltre$null.deviance)
R2
infomodelglobalappendix[1,5]<-R2


###################save the all_good table###########
write.csv2(as.data.frame(infomodelglobalappendix), "./Figures_result/infomodelglobalappendix.csv")
write.csv2(as.data.frame(infovariableglobalappendix), "./Figures_result/infovariableglobalappendix.csv")
#write.csv2(as.data.frame(infopvalueglobalappendix), "./Figures_result/infopvalueglobalappendix.csv")
setwd("D:/these/Axe_2/Figures_results")
lapply(1:length(infopvalueintro), function(i) write.csv(infopvalueglobalappendix[[i]], 
                                                        file = paste0(names(infopvalueglobalappendix[i]), "esta-aPPENDIX-pvalue.csv"),
                                                        row.names = T))


#####FIGURE ESTA#####

##for each pathway, only keep the 10 most important variables (variable importance)
#infovariableALLtop<-infovariableALL %>% group_by(Pathway) %>% slice_max(order_by = VarImp, n = 10)

#infovariableALLtop<-infovariableALL


###what are the significant variable?
library(tidyr)
namevariable<-c("GLOBAL")
minfo<-list()
minfofigure<-list()

for (y in 1:1) {
  colnames(infopvalueglobalappendix[[y]])[4]<-"pvalue"
  rownames(infovariableglobalappendix[[y]])<-infovariableglobalappendix[[y]]$Row.names
  namepv<-subset(infopvalueglobalappendix[[y]], infopvalueglobalappendix[[y]]$pvalue <0.05)
  name<-namevariable[y]
  minfo[[name]]<-merge(infovariableglobalappendix[[y]], namepv, by="row.names", all.x=T )
  rownames(minfo[[name]])<-minfo[[name]]$Row.names
  minfo[[name]]["(Intercept)","pvalue"]<-0
  if ((length(table(rownames(infopvalueglobalappendix[[y]])=="Order"))==2)& (infopvalueglobalappendix[[y]]["Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("OrderCypriniformes","OrderCyprinodontiformes","OrderOther","OrderPerciformes","OrderSiluriformes"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueglobalappendix[[y]])=="Nb.Diet:Order"))==2)& (infopvalueglobalappendix[[y]]["Nb.Diet:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Nb.Diet:OrderCypriniformes","Nb.Diet:OrderCyprinodontiformes","Nb.Diet:OrderOther","Nb.Diet:OrderPerciformes","Nb.Diet:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalueglobalappendix[[y]])=="TL:Order"))==2)& (infopvalueglobalappendix[[y]]["TL:Order",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("TL:OrderCypriniformes","TL:OrderCyprinodontiformes","TL:OrderOther","TL:OrderPerciformes","TL:OrderSiluriformes"),"pvalue"]<-0
    
  }
  if ((length(table(rownames(infopvalueglobalappendix[[y]])=="Maj.Diet"))==2)& (infopvalueglobalappendix[[y]]["Maj.Diet",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("Maj.Dietnekton","Maj.Dietplants","Maj.Dietzoobenthos","Maj.Dietzooplankton"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueglobalappendix[[y]])=="Maj.Region"))==2)& (infopvalueglobalappendix[[y]]["Maj.Region",]$pvalue<0.05)) {
    minfo[[name]][c("Maj.RegionEthiopian","Maj.RegionNearctic","Maj.RegionNeotropical","Maj.RegionPalearctic", "Maj.RegionSino-Oriental"),"pvalue"]<-0
    
  }
  
  if ((length(table(rownames(infopvalueglobalappendix[[y]])=="RepGuild1"))==2)& (infopvalueglobalappendix[[y]]["RepGuild1",]$pvalue<0.05)) {
    rownames(minfo[[name]])<-minfo[[name]]$Row.names 
    minfo[[name]][c("RepGuild1.Q","RepGuild1.L"),"pvalue"]<-0
    
  }
  minfofigure[[name]]<-minfo[[name]][,c("Row.names","Estimate","Std. Error", "Overall", "pvalue"),]
  minfofigure[[name]]<-minfofigure[[name]] %>% drop_na(pvalue)
}

minfofigureALL<-NULL
for (x in 1:1) {
  minfofigure2<-cbind(minfofigure[[x]], rep(namevariable[x], length(minfofigure[[x]][,1])))
  minfofigureALL<-rbind(minfofigureALL, minfofigure2)
}


#minfofigureALL$Variable<-rownames(minfofigureALL)
names(minfofigureALL)<-c("Variable","Estimate","Se","VarImp","pvalue","Pathway")

##order variable in categories
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
infovariableglobalappendixALLtop<-merge(minfofigureALL, categories, by="Variable", all.x=T)





infovariableglobalappendixALLtop$Pathway <- factor(infovariableglobalappendixALLtop$Pathway,      # Reordering group factor levels for facet wrap
                                           levels = c("GLOBAL", "Aquaculture", "Ornamental"))

# infovariableimpactALLtop$category <- factor(infovariableimpactALLtop$category,      # Reordering group factor levels for facet wrap
#                                       levels = c("Ecological", "Socio-economic", "Traits", "Interaction"))


infovariableglobalappendixALLtop$Variable <- factor(infovariableglobalappendixALLtop$Variable,      # Reordering variable factor levels for facet wrap
                                            levels = c(categories$Variable, "(Intercept)"))

levels(infovariableglobalappendixALLtop$Pathway)[levels(infovariableglobalappendixALLtop$Pathway)=="GLOBAL"]<-"Global - 307 species "
levels(infovariableglobalappendixALLtop$Pathway)[levels(infovariableglobalappendixALLtop$Pathway)=="Aquaculture"]<-"Aquaculture - 154 species "
levels(infovariableglobalappendixALLtop$Pathway)[levels(infovariableglobalappendixALLtop$Pathway)=="Ornamental"]<-"Ornamental - 126 species "

#my_breaks = c(0.5,1,2,3,4,10,16)##to color varimp
estaappendixgraph<-ggplot(as.data.frame(infovariableglobalappendixALLtop), aes(x=factor(Variable))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=0.2) +
  geom_boxplot(aes(lower=Estimate-Se,upper=Estimate+Se,middle=Estimate,ymin=Estimate-2*Se,ymax=Estimate+2*Se, fill=category),stat="identity")+
  facet_wrap(~Pathway,nrow=1)+
  coord_flip()+
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

#scale_fill_gradientn(colours = c("blue", "red"), name = "Variable Importance", trans = "log",breaks = my_breaks, labels = my_breaks)

pdf("estaappendixgraph-Graph.pdf", 14,14) 
# 2. Create a plot
estaappendixgraph
# Close the pdf file
dev.off() 




png("estaappendixgraph-Graph.png", width = 1200, height = 1500, units = "px") 
# 2. Create a plot
estaappendixgraph
# Close the pdf file
dev.off() 








