


rm(list = ls())
setwd("D:/these/Axe_2")

#nb country per species
nonadata<-readRDS("D:/these/Axe_2/outputs/nonadata_established.rds")
nonadata2<-nonadata[,c("Species", "nb.country.establish")]


#nb basin per species
# tedescobassin<-read.csv2("D:/these/database/Leprieur_Tedesco/Drainage_Basins_Table.csv")
# colnames(tedescobassin)
tedescoall<-read.csv2("D:/these/database/Leprieur_Tedesco/Occurrence_Table.csv")
colnames(tedescoall)
introexo<-tedescoall%>% filter(X3.Native.Exotic.Status=="exotic")
introexo<-introexo[,c( c("X6.Fishbase.Valid.Species.Name", "X1.Basin.Name"))]


basins <- introexo[introexo$X6.Fishbase.Valid.Species.Name%in%nonadata$Species,]
length(unique(basins$X6.Fishbase.Valid.Species.Name))
nbbasins<-aggregate(basins, by=list(basins$X6.Fishbase.Valid.Species.Name), FUN=length)
colnames(nbbasins)<-c("Species" ,"nb","basin")
table_c <- merge(x=nbbasins, y=nonadata2, by="Species", all.x=TRUE)


#plot
library("ggpubr")
corrplot<-ggscatter(table_c, x = "basin", y = "nb.country.establish", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of basins where established", ylab = "Number of countries where established")


#plot avec log

library("ggpubr")
table_c$logbasin<-log10(table_c$basin)
table_c$logcountry<-log10(table_c$nb.country.establish)
ggscatter(table_c, x = "logbasin", y = "logcountry", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of basins where established", ylab = "Number of countries where established")




setwd("D:/these/Axe_2/Figures_results")

 #save
pdf("nbcountry_nbbasins.pdf", 7,4) 
# 2. Create a plot
corrplot
# Close the pdf file
dev.off() 




png("nbcountry_nbbasins.png", width = 360, height = 450, units = "px") 
# 2. Create a plot
corrplot
# Close the pdf file
dev.off() 
