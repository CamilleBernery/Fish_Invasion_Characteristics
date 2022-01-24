library (rfishbase)
library(dplyr)
library(ggplot2)
library(naniar)
library(raster)
library(rgdal)
library(shapefiles)

options(FISHBASE_VERSION="19.04")
rm(list = ls())
setwd("D:/these/Axe_2")


nn<-intro[intro$Species %in% no.na.data$Species,]
nncountry<-as.data.frame(unique(nn$TO))


####Keep only freshwater fishes
SP<-species()
Freshfish<-subset(SP, Fresh==-1)
Freshfish<-unique(Freshfish$Species)
# saveRDS(Freshfish,"D:/these/Axe_2/outputs/Freshwater_species")


Freshfish<-readRDS("D:/these/Axe_2/outputs/Freshwater_species")

#####RESPONSE VARIABLES=============================================================####
introd<-introductions()
introall<-introd[, c("Species","From", "TO","Estabwild", "EcolEff", "SocioEff","Reason")]

####keep only the freshwater fishes in the table
Fresh<- introall$Species %in% Freshfish
introall$Freshwater<-Fresh
intro<- introall%>% filter(Freshwater==TRUE)
intro<-as.data.frame(intro)

###gather categories
#Introduction step-------------------#
intro$From<-as.factor(intro$From)
intro$TO<-as.factor(intro$TO)
intro$Species<-as.factor(intro$Species)

levels(intro$From)[levels(intro$From)=="|Unknown"]<-"Unknown"##gather categories with mistakes
levels(intro$From)[levels(intro$From)=="unknown"]<-"Unknown"
levels(intro$From)[levels(intro$From)=="Uknown"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="to be filled"]<-"Unknown"


#create a "yes/no column for the introduction step
introyes<-intro%>% filter(!is.na(TO))##==not introduced species
introyes$Introduced<-"Yes"

introno<-intro%>% filter(is.na(TO))##==introduced species
introno$Introduced<-"No"

intro<-rbind(introyes, introno)

#Establishment step-------------------#
  intro$Estabwild<-as.factor(intro$Estabwild)
levels(intro$Estabwild)<-c("Yes", "No","Unknown","No","Yes","No","No","Yes","Unknown","Unknown","Yes")


#Impact step---------------------------#
intro$EcolEff<-as.factor(intro$EcolEff)
levels(intro$EcolEff)<-c("Unknown","No","No","Yes","Yes","Unknown","Unknown","Yes")
intro$SocioEff<-as.factor(intro$SocioEff)
levels(intro$SocioEff)<-c("Unknown","No","No","Yes","Yes","Unknown","Unknown")

#####Cleaning data####

#Establishment step-------------------#
#cleaning data - if not introduced, thus not established
intro$match.intro_esta<-intro[,"Introduced"]=="No" & intro[,"Estabwild"]=="Yes"  #6
intro$match.intro_esta2<-intro[,"Introduced"]=="No" & intro[,"Estabwild"]=="Unknown"   #0

intro$match.intro_esta[is.na(intro$match.intro_esta)] <- FALSE
intro$match.intro_esta2[is.na(intro$match.intro_esta)] <- FALSE

for (i in 1:length(intro[,1])) {
  if (intro[i,"match.intro_esta"]==TRUE){
    intro[i,"Estabwild"]<-"No"
  }
}



#cleaning data - if unknown establishment status and impacts, thus established
intro$match.impeco_estaun<-intro[,"EcolEff"]=="Yes" & intro[,"Estabwild"]=="Unknown"
intro$match.impsocio_estaun<-intro[,"SocioEff"]=="Yes" & intro[,"Estabwild"]=="Unknown"

#transform NA 
intro$match.impsocio_estaun[is.na(intro$match.impsocio_estaun)] <- FALSE
intro$match.impeco_estaun[is.na(intro$match.impeco_estaun)] <- FALSE

for (i in 1:length(intro[,1])) {
  if (intro[i,"match.impeco_estaun"]==TRUE){
    intro[i,"Estabwild"]<-"Yes"
  }
  if (intro[i,"match.impsocio_estaun"]==TRUE){
    intro[i,"Estabwild"]<-"Yes"
  }
}




#Impact step---------------------------#
#cleaning data - if not established, thus no impacts
intro$match.impeco_esta<-intro[,"EcolEff"]=="Yes" & intro[,"Estabwild"]=="No"

bl2<-subset(intro, match.impeco_esta==TRUE)
spbl<-unique(bl2$Species)

match(spbl, no.na.data$Species)

intro$match.impsocio_esta<-intro[,"SocioEff"]=="Yes" & intro[,"Estabwild"]=="No"

#transform NA in False
intro$match.impsocio_esta[is.na(intro$match.impsocio_esta)] <- FALSE
intro$match.impeco_esta[is.na(intro$match.impeco_esta)] <- FALSE

# for (i in 1:length(intro[,1])) {
#   if (intro[i,"match.impeco_esta"]==TRUE){
#     intro[i,"EcolEff"]<-"No"
#   }
#   if (intro[i,"match.impsocio_esta"]==TRUE){
#     intro[i,"SocioEff"]<-"No"
#   }
# }


for (i in 1:length(intro[,1])) {
  if (intro[i,"match.impeco_esta"]==TRUE){
    intro[i,"Estabwild"]<-"Yes"
  }
    if (intro[i,"match.impsocio_esta"]==TRUE){
      intro[i,"Estabwild"]<-"Yes"
    }
}




#####Location gathering------------------------------------####
length(table(intro$TO))

levels(intro$TO)[levels(intro$TO)=="Solomon Is."]<-"Solomon Islands"
levels(intro$TO)[levels(intro$TO)=="Malaita and Sta. Anna, Solomon Is."]<-"Solomon Islands"
levels(intro$TO)[levels(intro$TO)=="Adriatic Sea, Slovenia"]<-"Slovenia"
levels(intro$TO)[levels(intro$TO)=="Iran (Caspian Sea)"]<-"Iran"
levels(intro$TO)[levels(intro$TO)=="Shiraz"]<-"Iran"
levels(intro$TO)[levels(intro$TO)=="India (Rajasthan)"]<-"India"
levels(intro$TO)[levels(intro$TO)=="River Ganga, Patna, Bihar State"]<-"India"
levels(intro$TO)[levels(intro$TO)=="Central Institute for Fisheries Education (CIFE), Bombay Bureau of Fisheries and Aquatic Resources (BFAR)"]<-"India"
levels(intro$TO)[levels(intro$TO)=="Central Institute for Fisheries Education (CIFE), Bombay"]<-"India"
levels(intro$TO)[levels(intro$TO)=="Hungary (Lake Balaton)"]<-"Hungary"
levels(intro$TO)[levels(intro$TO)=="Head waters of Brahmaputra River, Tibet."]<-"Tibet"
levels(intro$TO)[levels(intro$TO)=="Georgia (Former USSR)"]<-"Georgia"
levels(intro$TO)[levels(intro$TO)=="United States of America"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Oregon"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Florida, USA"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Florida and Texas"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Washington state"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Washington"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Tampa and Miami, Florida, USA"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="United States of America (Lake Superior)"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="San Francisco Bay, California"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="California, Florida, Maryland"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="San Francisco Bay, California"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="California"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Jordan"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Jordan river"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Jordan River"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Pennsylvania, USA"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Lake St. Claire, Ontario"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Lake St. Claire, Michigan"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Baltimore, MD"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Goose Creek"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="North Dakota"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="New York"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Florida"]<-"USA"
levels(intro$TO)[levels(intro$TO)=="Ishigaki Island, Japan"]<-"Japan"
levels(intro$TO)[levels(intro$TO)=="Lake Ashinoko"]<-"Japan"
levels(intro$TO)[levels(intro$TO)=="India, Kashmir Valley."]<-"India"
levels(intro$TO)[levels(intro$TO)=="Western Ghats"]<-"India"
levels(intro$TO)[levels(intro$TO)=="Falkland Islands (Malvinas)"]<-"Falkland Islands"
levels(intro$TO)[levels(intro$TO)=="Entre Rios province, Argentina"]<-"Argentina"
levels(intro$TO)[levels(intro$TO)=="Congo, Republic of"]<-"Congo"
levels(intro$TO)[levels(intro$TO)=="Lualaba River"]<-"Congo"
levels(intro$TO)[levels(intro$TO)=="Zaire"]<-"Congo"
levels(intro$TO)[levels(intro$TO)=="Congo, Dem. Rep. of the"]<-"Congo"
levels(intro$TO)[levels(intro$TO)=="Congo Dem Rep"]<-"Congo"
levels(intro$TO)[levels(intro$TO)=="Kipopo (Shaba)"]<-"Congo"
levels(intro$TO)[levels(intro$TO)=="Cook Islands (Mitiaro)"]<-"Cook Islands"
levels(intro$TO)[levels(intro$TO)=="Rarotonga, Cook Is."]<-"Cook Islands"
levels(intro$TO)[levels(intro$TO)=="China, Hong Kong SAR"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Hong Kong"]<-"China"
levels(intro$TO)[levels(intro$TO)=="China (mainland)"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Taiwan"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Sichuan"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Xinjiang"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Shashi Dam"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Shashi Dam"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Shanghai"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Shanghai"]<-"China"
levels(intro$TO)[levels(intro$TO)=="Qu<e9>bec"]<-"Quebec"
levels(intro$TO)[levels(intro$TO)=="Lake Superior, Quebec"]<-"Quebec"
levels(intro$TO)[levels(intro$TO)=="Pacific side of Panama"]<-"Panama"
levels(intro$TO)[levels(intro$TO)=="United Kingdom (Scotland)"]<-"UK"
levels(intro$TO)[levels(intro$TO)=="United Kingdom (Lake districts)"]<-"UK"
levels(intro$TO)[levels(intro$TO)=="United Kingdom"]<-"UK"
levels(intro$TO)[levels(intro$TO)=="UK (Thames)"]<-"UK"
levels(intro$TO)[levels(intro$TO)=="England"]<-"UK"
levels(intro$TO)[levels(intro$TO)=="Scotland"]<-"UK"
levels(intro$TO)[levels(intro$TO)=="Yugoslavia (Serbia/Montenegro)"]<-"Yugoslavia"
levels(intro$TO)[levels(intro$TO)=="Yugoslavia (SerbiaMontenegro)"]<-"Yugoslavia"
levels(intro$TO)[levels(intro$TO)=="VietNam"]<-"Vietnam"
levels(intro$TO)[levels(intro$TO)=="Research Institute for Aquaculture No. 1, Hanoi"]<-"Vietnam"
levels(intro$TO)[levels(intro$TO)=="Viet Nam"]<-"Vietnam"
levels(intro$TO)[levels(intro$TO)=="Tigris-Euphrates basin, Turkey"]<-"Turkey"
levels(intro$TO)[levels(intro$TO)=="Gazipa?a"]<-"Turkey"
levels(intro$TO)[levels(intro$TO)=="Lake Victoria, Tanzania"]<-"Tanzania"
levels(intro$TO)[levels(intro$TO)=="Thracian Sea, Greece"]<-"Greece"
levels(intro$TO)[levels(intro$TO)=="Rhodes, Greece"]<-"Greece"
levels(intro$TO)[levels(intro$TO)=="Thermaikos Gulf, Chalkidiki, Northern Aegean Sea, Greece"]<-"Greece"
levels(intro$TO)[levels(intro$TO)=="Rwanda (Lake Kivu)"]<-"Rwanda"
levels(intro$TO)[levels(intro$TO)=="Rhine, Basel, Switzerland"]<-"Switzerland"
levels(intro$TO)[levels(intro$TO)=="Tanzania, United Rep. of"]<-"Tanzania"
levels(intro$TO)[levels(intro$TO)=="Mwadingusha, Koni and Nzilo, Tanzania"]<-"Tanzania"
levels(intro$TO)[levels(intro$TO)=="Taiwan Province of China"]<-"Taiwan"
levels(intro$TO)[levels(intro$TO)=="Mekong, Thailand"]<-"Thailand"
levels(intro$TO)[levels(intro$TO)=="National Aquaculture Genetics Research Institute (NAGRI), Bangkok"]<-"Thailand"
levels(intro$TO)[levels(intro$TO)=="NAGRI, Thailand"]<-"Thailand"
levels(intro$TO)[levels(intro$TO)=="Mekong, Laos"]<-"Laos"
levels(intro$TO)[levels(intro$TO)=="Lao People's Dem. Rep."]<-"Laos"
levels(intro$TO)[levels(intro$TO)=="Lao PDR"]<-"Laos"
levels(intro$TO)[levels(intro$TO)=="Maldive Islands"]<-"Maldives"
levels(intro$TO)[levels(intro$TO)=="South Cameroon"]<-"Cameroon"
levels(intro$TO)[levels(intro$TO)=="Slovakia (former Czechoslovakia)"]<-"Slovakia"
levels(intro$TO)[levels(intro$TO)=="Czech Republic (Former Czechoslovakia)"]<-"Czech Republic"
levels(intro$TO)[levels(intro$TO)=="Czech"]<-"Czech Republic"
levels(intro$TO)[levels(intro$TO)=="Eastern Slovakia"]<-"Slovakia"
levels(intro$TO)[levels(intro$TO)=="Shanghai Fisheries University, Shanghai"]<-"Shanghai"
levels(intro$TO)[levels(intro$TO)=="several lakes in central Italy"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="Ombrone basin"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="Rivers of northeastern Italy"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="rivers of northeastern Italy"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="Northern and central Italy"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="central Italy"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="Central Italy"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="Po Basin, Italy"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="Fossa Calda, Tuscany"]<-"Italy"
levels(intro$TO)[levels(intro$TO)=="Mozambique (Zambezi river)"]<-"Mozambique"
levels(intro$TO)[levels(intro$TO)=="Moldova Rep."]<-"Moldova"
levels(intro$TO)[levels(intro$TO)=="Murgab, Turkmenistan"]<-"Turkmenistan"
levels(intro$TO)[levels(intro$TO)=="Uzbekistan (Syr Darya basin)"]<-"Uzbekistan"
levels(intro$TO)[levels(intro$TO)=="Tashkent"]<-"Uzbekistan"
levels(intro$TO)[levels(intro$TO)=="USSR (Russian Fed)"]<-"USSR"
levels(intro$TO)[levels(intro$TO)=="USSR, Former Area of"]<-"USSR"
levels(intro$TO)[levels(intro$TO)=="Former USSR"]<-"USSR"
levels(intro$TO)[levels(intro$TO)=="Uruguay river, Uruguay"]<-"Uruguay"
levels(intro$TO)[levels(intro$TO)=="Zambia (Lake Chila)"]<-"Zambia"
levels(intro$TO)[levels(intro$TO)=="Zambia (Lake Kariba)"]<-"Zambia"
levels(intro$TO)[levels(intro$TO)=="Zambia (Lake Iteshiteshi)"]<-"Zambia"
levels(intro$TO)[levels(intro$TO)=="Lake Kariba, Zambia"]<-"Zambia"
levels(intro$TO)[levels(intro$TO)=="Chilanga"]<-"Zambia"
levels(intro$TO)[levels(intro$TO)=="Zimbabwe (Lake Kariba)"]<-"Zimbabwe"
levels(intro$TO)[levels(intro$TO)=="Lake Darwendale (Zimbabwe)"]<-"Zimbabwe"
levels(intro$TO)[levels(intro$TO)=="Lake McIlwaine"]<-"Zimbabwe"
levels(intro$TO)[levels(intro$TO)=="Zaire (Lake Kivu)"]<-"Zaire"
levels(intro$TO)[levels(intro$TO)=="Korea Rep"]<-"Korea"
levels(intro$TO)[levels(intro$TO)=="Korea D P Rp"]<-"Korea"
levels(intro$TO)[levels(intro$TO)=="Kiribati (Gilbert Is.)"]<-"Kiribati"
levels(intro$TO)[levels(intro$TO)=="Gilbert Islands"]<-"Kiribati"
levels(intro$TO)[levels(intro$TO)=="Caroline Island"]<-"Kiribati"
levels(intro$TO)[levels(intro$TO)=="Kerguelen Island"]<-"Kerguelen"
levels(intro$TO)[levels(intro$TO)=="Kerguelen Is."]<-"Kerguelen"
levels(intro$TO)[levels(intro$TO)=="Kazakhstan (Lake Balkhash)"]<-"Kazakhstan"
levels(intro$TO)[levels(intro$TO)=="Caspian Sea (Kazakhstan)"]<-"Kazakhstan"
levels(intro$TO)[levels(intro$TO)=="Karametniyaz, Turkmenistan"]<-"Turkmenistan"
levels(intro$TO)[levels(intro$TO)=="Caspian Sea (Turkmenistan)"]<-"Turkmenistan"
levels(intro$TO)[levels(intro$TO)=="Caspian Sea (Azerbaijan)"]<-"Azerbaijan"
levels(intro$TO)[levels(intro$TO)=="Canada (Newfoundland)"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Alberta"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Pend Oreille, Kootenay and Okanagan, British Columbia"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Canada (Lake Superior)"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Canada British Columbia"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Canada (Adirondack lakes)"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Canada - British Columbia"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Quebec"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="British Columbia"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Newfoundland"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Nova Scotia"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Ontario"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="Winisk River, Attawapiskat River"]<-"Canada"
levels(intro$TO)[levels(intro$TO)=="C<f4>te d'Ivoire"]<-"Cote d'Ivoire"
levels(intro$TO)[levels(intro$TO)=="Bouak<e9>, C<f4>te d'Ivoire"]<-"Cote d'Ivoire"
levels(intro$TO)[levels(intro$TO)=="Bouak<e9>"]<-"Cote d'Ivoire"
levels(intro$TO)[levels(intro$TO)=="Brazil (Amazon Basin)"]<-"Brazil"
levels(intro$TO)[levels(intro$TO)=="Bosnia"]<-"Bosnia Herzegovina"
levels(intro$TO)[levels(intro$TO)=="Baltic waters, Lithuani"]<-"Lithuania"
levels(intro$TO)[levels(intro$TO)=="Baltic waters, Lithuania"]<-"Lithuania"
levels(intro$TO)[levels(intro$TO)=="Baltic basin, Poland"]<-"Poland"
levels(intro$TO)[levels(intro$TO)=="Azores Is"]<-"Azores Is."
levels(intro$TO)[levels(intro$TO)=="Austrian section of the Danube river"]<-"Austria"
levels(intro$TO)[levels(intro$TO)=="Danube, Austria"]<-"Austria"
levels(intro$TO)[levels(intro$TO)=="Australia (NSW)"]<-"Australia"
levels(intro$TO)[levels(intro$TO)=="Eden, New South Wales"]<-"Australia"
levels(intro$TO)[levels(intro$TO)=="Tasmania and Victoria, Australia"]<-"Australia"
levels(intro$TO)[levels(intro$TO)=="western Australia"]<-"Australia"
levels(intro$TO)[levels(intro$TO)=="Angola (Pangula Lake)"]<-"Angola"
levels(intro$TO)[levels(intro$TO)=="Russian Fed"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Moscow River"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Fed."]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Neva River Baltic Sea"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Federation (Amur River)"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Federation (Caspian Sea , Volga Delta)"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Federation (Former USSR)"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Federation (former USSR)"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Federation"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Federation"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Fed (Former USSR)"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Kamchatka, Poluostrov"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Russian Federation (Black Sea)"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="Lake Imanda, Kola Peninsula"]<-"Russia"
levels(intro$TO)[levels(intro$TO)=="R<e9>union"]<-"Reunion"
levels(intro$TO)[levels(intro$TO)=="Rivers As<f3>n and Nansa"]<-"Spain"
levels(intro$TO)[levels(intro$TO)=="Lake Ba<f1>olas"]<-"Spain"
levels(intro$TO)[levels(intro$TO)=="Yugoslavia"]<-"Serbia and Montenegro"
levels(intro$TO)[levels(intro$TO)=="Serbia Montenegro"]<-"Serbia and Montenegro"
levels(intro$TO)[levels(intro$TO)=="Skadar lake, Serbia and Montenegro"]<-"Serbia and Montenegro"
levels(intro$TO)[levels(intro$TO)=="Serbia, formerly Yugoslavia"]<-"Serbia"
levels(intro$TO)[levels(intro$TO)=="Ukraine (former USSR)"]<-"Ukraine"
levels(intro$TO)[levels(intro$TO)=="Prityat"]<-"Ukraine"
levels(intro$TO)[levels(intro$TO)=="Pripyat"]<-"Ukraine"
levels(intro$TO)[levels(intro$TO)=="Dnieper and Pripyat"]<-"Ukraine"
levels(intro$TO)[levels(intro$TO)=="Lakes Bulera and Luhondo"]<-"Rwanda"
levels(intro$TO)[levels(intro$TO)=="Lake Mugesera"]<-"Rwanda"
levels(intro$TO)[levels(intro$TO)=="Lake Mohasi"]<-"Rwanda"
levels(intro$TO)[levels(intro$TO)=="Suceava River"]<-"Ukraine"
levels(intro$TO)[levels(intro$TO)=="Yap, Caroline Islands"]<-"Caroline Island"
levels(intro$TO)[levels(intro$TO)=="Western Samoa"]<-"Samoa"
levels(intro$TO)[levels(intro$TO)=="Tongatapu Island, Tonga"]<-"Tonga"
levels(intro$TO)[levels(intro$TO)=="Tel Aviv, Israel"]<-"Israel"
levels(intro$TO)[levels(intro$TO)=="Ashdod"]<-"Israel"
levels(intro$TO)[levels(intro$TO)=="Dor, Israel"]<-"Israel"
levels(intro$TO)[levels(intro$TO)=="Lake Kinneret"]<-"Israel"
levels(intro$TO)[levels(intro$TO)=="Kibbutz Gan Shmuel"]<-"Israel"
levels(intro$TO)[levels(intro$TO)=="Syrian Arab Republic"]<-"Syria"
levels(intro$TO)[levels(intro$TO)=="Tahiti"]<-"French Polynesia"
levels(intro$TO)[levels(intro$TO)=="Sulawesi"]<-"Indonesia"
levels(intro$TO)[levels(intro$TO)=="Java"]<-"Indonesia"
levels(intro$TO)[levels(intro$TO)=="Irian Jaya"]<-"Indonesia"
levels(intro$TO)[levels(intro$TO)=="Cirata Reservoir, West Java"]<-"Indonesia"
levels(intro$TO)[levels(intro$TO)=="Bali"]<-"Indonesia"
levels(intro$TO)[levels(intro$TO)=="Research Institute for Freshwater Fisheries, Bogor"]<-"Indonesia"
levels(intro$TO)[levels(intro$TO)=="Indonesia (Irian Jaya)"]<-"Indonesia"
levels(intro$TO)[levels(intro$TO)=="St. Vincent"]<-"St. Vincent and the Grenadines"
levels(intro$TO)[levels(intro$TO)=="St. Lucia"]<-"St. Vincent and the Grenadines"
levels(intro$TO)[levels(intro$TO)=="Saint Lucia"]<-"St. Vincent and the Grenadines"
levels(intro$TO)[levels(intro$TO)=="south Sweden"]<-"Sweden"
levels(intro$TO)[levels(intro$TO)=="Schwentine River"]<-"Germany"
levels(intro$TO)[levels(intro$TO)=="R<fc>gen island"]<-"Germany"
levels(intro$TO)[levels(intro$TO)=="Hamburg"]<-"Germany"
levels(intro$TO)[levels(intro$TO)=="Saibai Island"]<-"Torres Strait Islands"
levels(intro$TO)[levels(intro$TO)=="Rodriguez"]<-"Mauritius"
levels(intro$TO)[levels(intro$TO)=="Ramey Field, Puerto Rico"]<-"Puerto Rico"
levels(intro$TO)[levels(intro$TO)=="Saipan, Mariana Islands"]<-"Mariana Islands"
levels(intro$TO)[levels(intro$TO)=="Pagan, Mariana Islands"]<-"Mariana Islands"
levels(intro$TO)[levels(intro$TO)=="Northern Marianas Islands"]<-"Mariana Islands"
levels(intro$TO)[levels(intro$TO)=="Guam"]<-"Mariana Islands"
levels(intro$TO)[levels(intro$TO)=="Northern Marianas"]<-"Mariana Islands"
levels(intro$TO)[levels(intro$TO)=="Oahu, Hawaii"]<-"Hawaii"
levels(intro$TO)[levels(intro$TO)=="Oub<e9>ira lake and dams"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Mitidja Oueds"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Mazafran Oueds"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Mazafran Oued"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Ain Zada dam"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Cap Djinet dam"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Djorf Torba dam"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Ain Sikhouna"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="B<e9>ni Abb<e8>s"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="El Milia"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Fodda Qued"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="El Golea"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Ghrib dam"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Ain Zada"]<-"Algeria"
levels(intro$TO)[levels(intro$TO)=="Negros, Lake Balinsasayao"]<-"Philippines"
levels(intro$TO)[levels(intro$TO)=="Hagonoy, Bulacan"]<-"Philippines"
levels(intro$TO)[levels(intro$TO)=="Manila Bay"]<-"Philippines"
levels(intro$TO)[levels(intro$TO)=="Lake Zwai, Ethiopia"]<-"Ethiopia"
levels(intro$TO)[levels(intro$TO)=="Lake Langano, Ethiopia"]<-"Ethiopia"
levels(intro$TO)[levels(intro$TO)=="Lake Trichonis"]<-"Greece"
levels(intro$TO)[levels(intro$TO)=="Lake Superior"]<-"Laurentian Great Lakes"
levels(intro$TO)[levels(intro$TO)=="Lake Naivasha"]<-"Kenya"
levels(intro$TO)[levels(intro$TO)=="Lake Kyoga"]<-"Uganda"
levels(intro$TO)[levels(intro$TO)=="Lake Bunyoni"]<-"Uganda"
levels(intro$TO)[levels(intro$TO)=="Wallis and Fortuna"]<-"Wallis and Futuna"
levels(intro$TO)[levels(intro$TO)=="Lake Kikila"]<-"Wallis and Futuna"
levels(intro$TO)[levels(intro$TO)=="Baja California Norte, Mexico"]<-"Mexico"
levels(intro$TO)[levels(intro$TO)=="Baja California"]<-"Mexico"
levels(intro$TO)[levels(intro$TO)=="Flanders"]<-"Belgium"
levels(intro$TO)[levels(intro$TO)=="Fisheries Research Institute (FRI), Mymensingh"]<-"Bangladesh"
levels(intro$TO)[levels(intro$TO)=="FRI, Mymensingh"]<-"Bangladesh"
levels(intro$TO)[levels(intro$TO)=="Boum Long, Cambodia"]<-"Cambodia"
levels(intro$TO)[levels(intro$TO)=="Grenoble"]<-"France"
levels(intro$TO)[levels(intro$TO)=="Cura<e7>ao"]<-"Curacao"
levels(intro$TO)[levels(intro$TO)=="Baltic , Finnish waters"]<-"Finland"
levels(intro$TO)[levels(intro$TO)=="Bangui, Central African Republic"]<-"Central African Republic"
levels(intro$TO)[levels(intro$TO)=="Ci<e9>naga de Santa Marta, Magdalena river"]<-"Colombia"
levels(intro$TO)[levels(intro$TO)=="Viti Levu, Fiji"]<-"Fiji"
levels(intro$TO)[levels(intro$TO)=="Balykchi fish farm"]<-"Kyrgyzstan"
levels(intro$TO)[levels(intro$TO)=="Kirgizia"]<-"Kyrgyzstan"
levels(intro$TO)[levels(intro$TO)=="Efate & Tana Islands, Vanuatu"]<-"Vanuatu"
levels(intro$TO)[levels(intro$TO)=="Brunei"]<-"Brunei Darussalam"
levels(intro$TO)[levels(intro$TO)=="Lake Kyle"]<-"Zimbabwe"
levels(intro$TO)[levels(intro$TO)=="Antigua"]<-"Antigua-and-Barbuda"
levels(intro$TO)[levels(intro$TO)=="Ceylon"]<-"Sri Lanka"
levels(intro$TO)[levels(intro$TO)=="Libyan Arab Jamahiriya"]<-"Libya"

levels(intro$TO)[levels(intro$TO)=="Research Institute for Aquaculture"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Dams, agricultural farms"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Damachi fish farm"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Can Br Colum"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Dams"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Tell"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Europe"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Line Islands"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Islands East of Wallace line"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Macedonia "]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Lago das Cobras"]<-"Unknown"
levels(intro$TO)[levels(intro$TO)=="Bureau of Fisheries and Aquatic Resources (BFAR)"]<-"Unknown"


intro2<-intro[rep(which(intro$TO=="Guadiana river basin, southern Iberia"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Guadiana river basin, southern Iberia")
intro$TO[nbrow[1]]<-"Spain"
intro$TO[nbrow[2]]<-"Portugal"

intro2<-intro[rep(which(intro$TO=="Lake Kariba"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Lake Kariba")
intro$TO[nbrow[1]]<-"Zimbabwe"
intro$TO[nbrow[2]]<-"Zambia"

intro2<-intro[rep(which(intro$TO=="Asi River"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Asi River")
intro$TO[nbrow[1]]<-"Syria"
intro$TO[nbrow[2]]<-"Lebanon"

intro2<-intro[rep(which(intro$TO=="Lake Kivu"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Lake Kivu")
intro$TO[nbrow[1]]<-"Congo"
intro$TO[nbrow[2]]<-"Congo"
intro$TO[nbrow[3]]<-"Rwanda"
intro$TO[nbrow[4]]<-"Rwanda"

intro2<-intro[rep(which(intro$TO=="Lake Victoria"), each = 2),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Lake Victoria")
intro$TO[nbrow[1]]<-"Uganda"
intro$TO[nbrow[2]]<-"Uganda"
intro$TO[nbrow[3]]<-"Uganda"
intro$TO[nbrow[4]]<-"Kenya"
intro$TO[nbrow[5]]<-"Tanzania"
intro$TO[nbrow[6]]<-"Kenya"
intro$TO[nbrow[7]]<-"Tanzania"
intro$TO[nbrow[8]]<-"Kenya"
intro$TO[nbrow[9]]<-"Tanzania"

intro2<-intro[rep(which(intro$TO=="Lakes Victoria and Kanyaboli"), each = 2),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Lakes Victoria and Kanyaboli")
intro$TO[nbrow[1]]<-"Tanzania"
intro$TO[nbrow[2]]<-"Kenya"
intro$TO[nbrow[3]]<-"Tanzania"

intro2<-intro[rep(which(intro$TO=="Lake Maggiore"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Lake Maggiore")
intro$TO[nbrow[1]]<-"Italy"
intro$TO[nbrow[2]]<-"Switzerland"


intro2<-intro[rep(which(intro$TO=="Lake Ohrid"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Lake Ohrid")
intro$TO[nbrow[1]]<-"Macedonia"  ###error / nORTH mACEDONIA
intro$TO[nbrow[2]]<-"Albania"

intro2<-intro[rep(which(intro$TO=="Laurentian Great Lakes"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Laurentian Great Lakes")
intro$TO[nbrow[1]]<-"Canada"
intro$TO[nbrow[2]]<-"Canada"
intro$TO[nbrow[3]]<-"Canada"
intro$TO[nbrow[4]]<-"USA"
intro$TO[nbrow[5]]<-"USA"
intro$TO[nbrow[6]]<-"USA"

intro2<-intro[rep(which(intro$TO=="Aral Sea"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Aral Sea")
intro$TO[nbrow[1]]<-"Kazakhstan"
intro$TO[nbrow[2]]<-"Kazakhstan"
intro$TO[nbrow[3]]<-"Kazakhstan"
intro$TO[nbrow[4]]<-"Kazakhstan"
intro$TO[nbrow[5]]<-"Kazakhstan"
intro$TO[nbrow[6]]<-"Uzbekistan"
intro$TO[nbrow[7]]<-"Uzbekistan"
intro$TO[nbrow[8]]<-"Uzbekistan"
intro$TO[nbrow[9]]<-"Uzbekistan"
intro$TO[nbrow[10]]<-"Uzbekistan"

intro2<-intro[rep(which(intro$TO=="Riga Bay"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Riga Bay")
intro$TO[nbrow[1]]<-"Estonia"
intro$TO[nbrow[2]]<-"Latvia"

intro2<-intro[rep(which(intro$TO=="Gulf of Gdansk"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Gulf of Gdansk")
intro$TO[nbrow[1]]<-"Poland"
intro$TO[nbrow[2]]<-"Russia"

intro2<-intro[rep(which(intro$TO=="Dnieper"), each = 2),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Dnieper")
intro$TO[nbrow[1]]<-"Belarus"
intro$TO[nbrow[2]]<-"Belarus"
intro$TO[nbrow[3]]<-"Belarus"
intro$TO[nbrow[4]]<-"Russia"
intro$TO[nbrow[5]]<-"Ukraine"
intro$TO[nbrow[6]]<-"Russia"
intro$TO[nbrow[7]]<-"Ukraine"
intro$TO[nbrow[8]]<-"Russia"
intro$TO[nbrow[9]]<-"Ukraine"

intro2<-intro[rep(which(intro$TO=="Garonne basin"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Garonne basin")
intro$TO[nbrow[1]]<-"France"
intro$TO[nbrow[2]]<-"Spain"

intro2<-intro[rep(which(intro$TO=="Kebir River"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Kebir River")
intro$TO[nbrow[1]]<-"Syria"
intro$TO[nbrow[2]]<-"Lebanon"

intro2<-intro[rep(which(intro$TO=="Congo River"), each = 1),]
intro<-rbind(intro, intro2)
nbrow<-which(intro$TO=="Congo River")
intro$TO[nbrow[1]]<-"Congo"
intro$TO[nbrow[2]]<-"Angola"




###Number of countries where the species has been introduced----------------------------------------####


intro$TO<-as.factor(intro$TO)
countries<-as.data.frame(table(intro$TO))
countries<-arrange(countries, Freq)

levels(as.factor(as.character(intro$TO)))
length(table(intro$TO))
droplevels(intro)$TO
unique(intro$TO)

spfromto<-intro[,c("Species","TO","Introduced")]
spfromto<-spfromto%>% filter(Introduced=="Yes")

spfromto<-unique(spfromto)

nb.country.intro2.FB<-as.data.frame(table(as.character(as.factor(spfromto$Species))))
colnames(nb.country.intro2.FB)<-c("Species","nb.country.intro2")

intro<-merge(x = intro, y = nb.country.intro2.FB, by = "Species", all.x = TRUE)





#####Number of countries for each step#####

#Introduction
intro$nb.country.intro<-intro$intro$nb.country.intro2
intro$nb.country.intro[is.na(intro$nb.country.intro)] <- 0


#Establishment
spesta2<-intro[,c("Species","TO","Estabwild")]
spesta<-spesta2%>% filter(Estabwild=="Yes")
spesta<-unique(spesta)
nbcountryesta<-as.data.frame(table(as.character(as.factor(spesta$Species))))
colnames(nbcountryesta)<-c("Species","nb.country.establish")

spestano<-spesta2%>% filter(Estabwild=="No")
spestano<-unique(spestano)
spestano$nb.country.establish<-0
spestano2<-spestano[,c("Species","nb.country.establish" )]
nbcounest<-rbind(nbcountryesta, spestano2)

nbcountryesta<-aggregate(nbcounest$nb.country.establish, by=list(Species=nbcounest$Species), FUN=sum)
colnames(nbcountryesta)<-c("Species", "nb.country.establish")
intro<-merge(x = intro, y = nbcountryesta, by = "Species", all.x = TRUE)



#Impact
spimpeco2<-intro[,c("Species","TO","EcolEff")]
spimpeco<-spimpeco2%>% filter(EcolEff=="Yes")
spimpeco<-unique(spimpeco)
nbcountryimpeco<-as.data.frame(table(as.character(as.factor(spimpeco$Species))))
colnames(nbcountryimpeco)<-c("Species","nb.country.impeco")

spimpecono<-spimpeco2%>% filter(EcolEff=="No")
spimpecono<-unique(spimpecono)
spimpecono$nb.country.impeco<-0
spimpecono2<-spimpecono[,c("Species","nb.country.impeco" )]
nbcounimp<-rbind(nbcountryimpeco, spimpecono2)

nbcountryimpeco<-aggregate(nbcounimp$nb.country.impeco, by=list(Species=nbcounimp$Species), FUN=sum)
colnames(nbcountryimpeco)<-c("Species", "nb.country.impeco")
intro<-merge(x = intro, y = nbcountryimpeco, by = "Species", all.x = TRUE)




spimpsocio2<-intro[,c("Species","TO","SocioEff")]
spimpsocio<-spimpsocio2%>% filter(SocioEff=="Yes")
spimpsocio<-unique(spimpsocio)
nbcountryimpsocio<-as.data.frame(table(as.character(as.factor(spimpsocio$Species))))
colnames(nbcountryimpsocio)<-c("Species","nb.country.impsocio")

spimpsociono<-spimpsocio2%>% filter(SocioEff=="No")
spimpsociono<-unique(spimpsociono)
spimpsociono$nb.country.impsocio<-0
spimpsociono2<-spimpsociono[,c("Species","nb.country.impsocio" )]
nbcounimp<-rbind(nbcountryimpsocio, spimpsociono2)

nbcountryimpsocio<-aggregate(nbcounimp$nb.country.impsocio, by=list(Species=nbcounimp$Species), FUN=sum)
colnames(nbcountryimpsocio)<-c("Species", "nb.country.impsocio")

intro<-merge(x = intro, y = nbcountryimpsocio, by = "Species", all.x = TRUE)

####Introduction pathway-----------------------------------------------------------------------####
intro$Reason<-as.factor(intro$Reason)
levels(intro$Reason)
bloup1<-intro[,c("Species", "Reason")]
bloup1<-unique(bloup1)
boup<-table(intro$Reason)
boup1<-table(bloup1$Reason)


levels(intro$Reason)[levels(intro$Reason)=="unkown"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="unknown"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="no data"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="other reasons"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="bait"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="forage"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="removal of natural barrier"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="fill ecological niche"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="off-site preservation"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="research"]<-"Unknown"



levels(intro$Reason)[levels(intro$Reason)=="aquaculture"]<-"Aquaculture"

levels(intro$Reason)[levels(intro$Reason)=="angling/sport"]<-"Sport/Angling"

levels(intro$Reason)[levels(intro$Reason)=="weed control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="mosquito control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="other pest control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="phyto-zooplankton control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="snail control"]<-"Species control"


levels(intro$Reason)[levels(intro$Reason)=="Lessepsian migration"]<-"Diffusion"
levels(intro$Reason)[levels(intro$Reason)=="diffused from other countries"]<-"Diffusion"

levels(intro$Reason)[levels(intro$Reason)=="accidental"]<-"Accidental"
levels(intro$Reason)[levels(intro$Reason)=="accidental with ships"]<-"Accidental"


table(intro$Reason)
bloup1<-intro[,c("Species", "Reason")]
bloup1<-unique(bloup1)
boup<-table(intro$Reason)
boup1<-table(bloup1$Reason)
bap<-subset(intro, Reason == "removal of natural barrier")



#transform the pathways in several binary variables#
intror<-intro[, c("Species","Reason")]

intror[is.na(intror)] <- "Unknown"  ###transform NA 
introname<-levels(as.factor(intro$Reason))
levels(intror$Reason)<-as.factor(levels(intror$Reason))

#one column for each level
library(data.table)
setDT(intror)[, c(levels(intror$Reason), "Reason") := 
                c(lapply(levels(Reason), function(x) as.integer(x == Reason)), .(NULL))]


#one row by species
intror<-intror %>%
  dplyr::group_by(Species ) %>%
  dplyr::summarise_all(funs(sum))

#remove "unknown" column
intror<-as.data.frame(intror)
intror<-intror[ , !(names(intror) %in% "Unknown")]

##number > 0 --> transform in 1
intror[,c(2:length(intror[1,]))][intror[,c(2:length(intror[1,]))]>0]<-1  
sumintror<-rowSums(intror[,c(2:length(intror[1,]))], na.rm=T)
intror$Nb_reasons<-sumintror

intror$Nb_reasons[intror$Nb_reasons=="0"]<-NA  

#Add reasons in intro table
intro<-merge(x = intro, y = intror, by = "Species", all.x = TRUE)









####Number of countries for the 5 most used pathways (number of species) at each step#####
aquaintro<-intro[,c("Species","TO","Reason")]
aquaintro<-aquaintro%>% filter(Reason=="Aquaculture")
aquaintro<-unique(aquaintro)
nbcountryaquaintro<-as.data.frame(table(as.character(as.factor(aquaintro$Species))))
colnames(nbcountryaquaintro)<-c("Species","nb.country.intro.aqua")
intro<-merge(x = intro, y = nbcountryaquaintro, by = "Species", all.x = TRUE)
intro$nb.country.intro.aqua[is.na(intro$nb.country.intro.aqua)] <- 0

fishintro<-intro[,c("Species","TO","Reason")]
fishintro<-fishintro%>% filter(Reason=="fisheries")
fishintro<-unique(fishintro)
nbcountryfishintro<-as.data.frame(table(as.character(as.factor(fishintro$Species))))
colnames(nbcountryfishintro)<-c("Species","nb.country.intro.fish")
intro<-merge(x = intro, y = nbcountryfishintro, by = "Species", all.x = TRUE)
intro$nb.country.intro.fish[is.na(intro$nb.country.intro.fish)] <- 0

ornaintro<-intro[,c("Species","TO","Reason")]
ornaintro<-ornaintro%>% filter(Reason=="ornamental")
ornaintro<-unique(ornaintro)
nbcountryornaintro<-as.data.frame(table(as.character(as.factor(ornaintro$Species))))
colnames(nbcountryornaintro)<-c("Species","nb.country.intro.orna")
intro<-merge(x = intro, y = nbcountryornaintro, by = "Species", all.x = TRUE)
intro$nb.country.intro.orna[is.na(intro$nb.country.intro.orna)] <- 0

acciintro<-intro[,c("Species","TO","Reason")]
acciintro<-acciintro%>% filter(Reason=="Accidental")
acciintro<-unique(acciintro)
nbcountryacciintro<-as.data.frame(table(as.character(as.factor(acciintro$Species))))
colnames(nbcountryacciintro)<-c("Species","nb.country.intro.acci")
intro<-merge(x = intro, y = nbcountryacciintro, by = "Species", all.x = TRUE)
intro$nb.country.intro.acci[is.na(intro$nb.country.intro.acci)] <- 0

anglintro<-intro[,c("Species","TO","Reason")]
anglintro<-anglintro%>% filter(Reason=="Sport/Angling")
anglintro<-unique(anglintro)
nbcountryanglintro<-as.data.frame(table(as.character(as.factor(anglintro$Species))))
colnames(nbcountryanglintro)<-c("Species","nb.country.intro.angl")
intro<-merge(x = intro, y = nbcountryanglintro, by = "Species", all.x = TRUE)
intro$nb.country.intro.angl[is.na(intro$nb.country.intro.angl)] <- 0

aquaesta<-intro[,c("Species","TO","Estabwild", "Reason")]
aquaesta<-aquaesta%>% filter(Reason=="Aquaculture")
aquaesta<-aquaesta%>% filter(Estabwild=="Yes")
aquaesta<-unique(aquaesta)
nbcountryaquaesta<-as.data.frame(table(as.character(as.factor(aquaesta$Species))))
colnames(nbcountryaquaesta)<-c("Species","nb.country.esta.aqua")
intro<-merge(x = intro, y = nbcountryaquaesta, by = "Species", all.x = TRUE)
intro$nb.country.esta.aqua[is.na(intro$nb.country.esta.aqua)] <- 0

fishesta<-intro[,c("Species","TO","Estabwild", "Reason")]
fishesta<-fishesta%>% filter(Reason=="fisheries")
fishesta<-fishesta%>% filter(Estabwild=="Yes")
fishesta<-unique(fishesta)
nbcountryfishesta<-as.data.frame(table(as.character(as.factor(fishesta$Species))))
colnames(nbcountryfishesta)<-c("Species","nb.country.esta.fish")
intro<-merge(x = intro, y = nbcountryfishesta, by = "Species", all.x = TRUE)
intro$nb.country.esta.fish[is.na(intro$nb.country.esta.fish)] <- 0

ornaesta<-intro[,c("Species","TO","Estabwild", "Reason")]
ornaesta<-ornaesta%>% filter(Reason=="ornamental")
ornaesta<-ornaesta%>% filter(Estabwild=="Yes")
ornaesta<-unique(ornaesta)
nbcountryornaesta<-as.data.frame(table(as.character(as.factor(ornaesta$Species))))
colnames(nbcountryornaesta)<-c("Species","nb.country.esta.orna")
intro<-merge(x = intro, y = nbcountryornaesta, by = "Species", all.x = TRUE)
intro$nb.country.esta.orna[is.na(intro$nb.country.esta.orna)] <- 0

acciesta<-intro[,c("Species","TO","Estabwild", "Reason")]
acciesta<-acciesta%>% filter(Reason=="Accidental")
acciesta<-acciesta%>% filter(Estabwild=="Yes")
acciesta<-unique(acciesta)
nbcountryacciesta<-as.data.frame(table(as.character(as.factor(acciesta$Species))))
colnames(nbcountryacciesta)<-c("Species","nb.country.esta.acci")
intro<-merge(x = intro, y = nbcountryacciesta, by = "Species", all.x = TRUE)
intro$nb.country.esta.acci[is.na(intro$nb.country.esta.acci)] <- 0


#####EXPLANATORY VARIABLES=============================================================####
####Order----------------------------------------------------------------------------------####
####Add the family for each species####
fam<-load_taxa()
ORD<-fam[,c("Species", "Order")]

#Add the family in the "intro" table
intro<-merge(x = intro, y = ORD, by = "Species", all.x = TRUE)









###Use by humans--------------------------------------------------------------------------####
# species used for aquaculture, sport, management, ornamental, fisheries
SP<-species()
use<-SP[,c("Species","Importance", "UsedasBait", "Aquarium", "GameFish")]
use$Importance<-as.factor(use$Importance) #relates to fisheries
use$UsedasBait<-as.factor(use$UsedasBait)
use$Aquarium<-as.factor(use$Aquarium)
use$GameFish<-as.factor(use$GameFish)
levels(use$Importance)
levels(use$UsedasBait)
levels(use$Aquarium)
levels(use$GameFish)

####Importance : do not keep "of no interest"  "of potential interest"
levels(use$Importance)<-c(1,1,1,0,0,1)

###UsedasBasit : do not keep "never/rarely"
levels(use$UsedasBait)<- c(0,1,1)

###Aquarium : do not keep "never/rarely"      "potential"
levels(use$Aquarium)<- c(1,1,0,0,1,1)

###GameFish
levels(use$GameFish)<-c(1,0)


#levels(use$Importance)<-as.numeric(levels(use$Importance))[use$Importance]
use$Importance<-as.numeric(as.character(use$Importance))
use$UsedasBait<-as.numeric(as.character(use$UsedasBait))
use$Aquarium<-as.numeric(as.character(use$Aquarium))
use$GameFish<-as.numeric(as.character(use$GameFish))


###is there any species duplicates?
use$Species<-as.factor(use$Species)
blup<-as.data.frame(table(levels(use$Species)))
blup2<-subset(blup, Freq>1) ###no duplicates

###if a species as "1" in one of the 4 uses, it is used by human
###### nb od uses puis remplacer les chiffres superieurs a 0 par 1 pour faire la présence/absence
use$Nb_uses<-rowSums(use[, c(2,3,4,5)], na.rm=T)
data.frame(table(use$Nb_uses))
use$Used_by_humans<-use$Nb_uses
use[,c("Used_by_humans")][use[,c("Used_by_humans")]>0]<-1


##merge with intro
colnames(use)
useinfo<-use[, c("Species", "Nb_uses", "Used_by_humans")]
 intro<-merge(x = intro, y = useinfo, by = "Species", all.x = TRUE)


 ###Diet-----------------------------------------------------------------------------------------------#####
 
# ##sensitivity analysis -- get 100 columns of differents diet
# SENSDIET<-list()##----------------##
# for (sd in 1:100) {#----------------##
#   

food<-fooditems()
diet<-food[, c("Species","FoodI")]

##keep only freshwater fishes of the table
Fresh<- diet$Species %in% Freshfish
diet$Freshwater<-Fresh
diet<- diet%>% filter(Freshwater==TRUE)

##remove the "other" diet
diet$FoodI<-as.factor(diet$FoodI)
levels(diet$FoodI)[levels(diet$FoodI)=="others"]<-NA
diet<-na.omit(diet)

##choose the most represented diet
count1<-rep(1, nrow(diet))
diet$count<-count1
diet<-as.data.frame(diet)
class(diet$Species)
diet$Species<-as.factor(diet$Species)
diet$FoodI<-as.factor(diet$FoodI)
nbeachcat<-diet %>%
  dplyr::group_by(Species, FoodI) %>%
  dplyr::summarise(Total = sum(count, na.rm = TRUE))


maxdiet<-nbeachcat %>% group_by(Species) %>% top_n(1, Total) # pour chaque espèce, sélectionne la (ou les ) diet les plus représentées

###how many species have several diet?
table(maxdiet$FoodI)
blip<-as.data.frame(table(maxdiet$Species))
blipmore1<-subset(blip, Freq>1) ##

####transform the diet in several binary variables
levels(maxdiet$FoodI)<-levels(as.factor(maxdiet$FoodI))
dietname<-levels(as.factor(maxdiet$FoodI))


#Initialise the columns to 0
maxdiet[dietname] <- 0
mapply(function(x, y) maxdiet[x, y] <<- 1, seq(nrow(maxdiet)),  #transformation en variable binaire
       maxdiet$FoodI)

###one row by sp
maxdiet2<-maxdiet[, c("Species","detritus","nekton","plants","zoobenthos","zooplankton")]
maxdiet2<-maxdiet2 %>%
  dplyr::group_by(Species ) %>%
  dplyr::summarise_all(sum)


####count the diet number for each species
count2<-rep(1, nrow(nbeachcat))
numbdiet<-nbeachcat[,c("Species", "FoodI")]
numbdiet<-na.omit(numbdiet)
numbdiet$count<-count2
nbdiet<-numbdiet %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(Total = sum(count, na.rm = TRUE))

colnames(nbdiet)<-c("Species", "Nb.Diet")

###count nb of max diet for each sp
NbDietMax<-rowSums(maxdiet2[,c("detritus","nekton","plants","zoobenthos","zooplankton")])
maxdiet3<-cbind(maxdiet2, NbDietMax)

summary(as.factor(NbDietMax))

##find the main max diet per order-----------------------
ordersp<-unique(intro[,c("Species", "Order")])
nbdietorder1<-merge(ordersp, numbdiet, by="Species")
nbdietorder2<-unique(nbdietorder1) ##to just have the diet occurence per species
nbdietorder<-nbdietorder2 %>%
  dplyr::group_by(Order, FoodI) %>%
  dplyr::summarise(Total = sum(count, na.rm = TRUE))
maxnbdietorder<-as.data.frame(nbdietorder%>% group_by(Order) %>% top_n(1, Total))

#If more than 1 max diet --> choose the diet the most represented in order

severaldiet<-subset(maxdiet3, NbDietMax>1)
severaldietsp<-merge(severaldiet, ordersp, by="Species")
severaldietsp2<-merge(severaldietsp, maxnbdietorder, by="Order")
bloup<-as.data.frame(table(severaldietsp2$Species))
notpossible<-subset(bloup, Freq>1) ###5 species among those with several max diet have an order with several max diet

###are these sp in the pool used for models?
set.seed(123)
pooluseddiet<-readRDS("D:/these/Axe_2/outputs/Specieswithseveraldiet_poolused.rds")
notpossible$Var1
notpossible$Var1 %in% pooluseddiet$Species

maxdiet3[,"maxdiet status"]<-0
for (i in 1:length(maxdiet3[,1])) {####replace the multiple max diet by the max diet in order
  if (maxdiet3[i,"NbDietMax"]>1) {
    spsevdiet<-merge(maxdiet3[i,], ordersp, by="Species")
    or<-spsevdiet$Order
    blap<-maxnbdietorder[maxnbdietorder$Order == or, ]
    bap<-as.character(factor(blap[sample(1:length(blap[,1]), 1), "FoodI"])) ###if more than one max diet in order, choose one randomly
    if (maxdiet3[i, bap]==1) { ###if the order diet correspond to a max diet of the species, put this diet
      maxdiet3[i,c("detritus","nekton","plants","zoobenthos","zooplankton")]<-0
      maxdiet3[i,bap]<-1
    }else{ ##else, choose a max diet randomly in the species diet
      #maxdiet3[i,c("detritus","nekton","plants","zoobenthos","zooplankton")]<-0
      maxdiet3[i,"maxdiet status"]<-"not represented in species"
      blap2<-which(maxdiet3[i,c("Species","detritus","nekton","plants","zoobenthos","zooplankton")]==1)
      bap2<-sample(blap2, 1)
      maxdiet3[i,c("detritus","nekton","plants","zoobenthos","zooplankton")]<-0
      maxdiet3[i,bap2]<-1
    }
    

  }
}


nbdietorder<-as.data.frame(nbdietorder)

nbdietorderwide<-stats::reshape(nbdietorder, idvar = "Order", timevar = "FoodI", direction = "wide")
getwd()
#write.csv(nbdietorderwide, "nbdietorder.csv")



##new column
Maj.Diet<-c()
for (i in 1:length(maxdiet3[,1])) {
    blap<-which(maxdiet3[i,c("Species","detritus","nekton","plants","zoobenthos","zooplankton")]==1)
    name<-colnames(maxdiet3)[blap]
    Maj.Diet<-c(Maj.Diet, name)
  }
  
Maj.Diet



maxdiet4<-cbind(maxdiet3, Maj.Diet)

# ##sensitivity analysis###
# sens<-maxdiet4[,c("Species", "Maj.Diet")]##
# SENSDIET[[sd]]<-as.data.frame(sens)##

###} ###
getwd()
setwd("D:/these/Axe_2/outputs/sensitivity-analysis")
saveRDS(SENSDIET, "SENSDIET.rds")##
setwd("D:/these/Axe_2")

#Add diet in "intro" table
intro<-merge(x = intro, y = nbdiet, by = "Species", all.x = TRUE)
intro<-merge(x = intro, y = maxdiet4[,c("Species", "Maj.Diet", "NbDietMax", "maxdiet status")], by = "Species", all.x = TRUE)




###Parental care--------------------------------------------------------------------####
Pcare<-reproduction()
care<-Pcare[, c("Species","RepGuild1")]

####only keep freshwater fishes
Fresh<- care$Species %in% Freshfish
care$Freshwater<-Fresh
care<- care%>% filter(Freshwater==TRUE)

#add parental care in the "intro" table
careordi<-care[,c(1,2)]
levels(as.factor(careordi$RepGuild1))
careordi$RepGuild1<-factor(careordi$RepGuild1, order = TRUE, 
       levels = c("nonguarders", "guarders", "bearers"))
class(careordi$RepGuild1)
intro<-merge(x = intro, y = careordi, by = "Species", all.x = TRUE)






###Body length------------------------------------------------------------------####
size<-morphometrics()
bsize<-size[, c("Species","TL")]
sizenona<-bsize%>% filter(!is.na(bsize$TL))
length(unique(sizenona$Species))

####keep only freshwater fishes
Fresh<- bsize$Species %in% Freshfish
bsize$Freshwater<-Fresh
bsize<- bsize%>% filter(Freshwater==TRUE)

#how many size have each species?
sizenona<-bsize%>% filter(!is.na(bsize$TL))
TLcount<-unique(bsize)
Tlnb<-data.frame(table(sizenona$Species))
one<-Tlnb%>% filter(Freq==1)
length(one[,1])


##Size median for each species
bodysize<-aggregate(bsize[, "TL"], list(bsize$Species), median)
colnames(bodysize)<-c("Species","TL")

#Add body size in the intro table
intro<-merge(x = intro, y = bodysize, by = "Species", all.x = TRUE)


# #####Physiological tolerance---------------------------------------------------------##### 
#warning : need ~2hours to run

####Tedesco et al., 2017 database
tedescobassin<-read.csv2("D:/these/database/Leprieur_Tedesco/Drainage_Basins_Table.csv")
colnames(tedescobassin)
tedescoall<-read.csv2("D:/these/database/Leprieur_Tedesco/Occurrence_Table.csv")

####keep only freshwater fishes
Freshtedes<- tedescoall$X6.Fishbase.Valid.Species.Name %in% Freshfish
tedescoall$Freshwater<-Freshtedes
tedesco<- tedescoall%>% filter(Freshwater==TRUE)

colnames(tedescoall)
intronative<-tedesco%>% filter(X3.Native.Exotic.Status=="native")
intronative<-intronative[,c( c("X6.Fishbase.Valid.Species.Name", "X1.Basin.Name"))]

bassin<-tedescobassin[,c( c("X1.Basin.Name", "X9.Surface.Area"))]

intronative<-merge(x = intronative, y = bassin, by = "X1.Basin.Name", all.x = TRUE)

##le shapefile
library(rgdal)
library(raster)
library(sp)

shp <- readOGR(dsn = 'D:/these/database/Leprieur_Tedesco/Basin042017_3119.shp')


clim <- getData("worldclim",var="bio",res=2.5)
# Bio1<-clim$bio1
# Bio12<-clim$bio12

Bio5<-clim$bio5  #max temp. warmest month
Bio6<-clim$bio6 #Min temp. coldest month

####une esp?ce peut ?tre associ?e ? plusieurs bassins, donc plutot faire comme ?a :
Bassinspecies<-intronative[,c("X1.Basin.Name","X6.Fishbase.Valid.Species.Name")]
Bassinspecies<-unique(Bassinspecies) #il y a des infos en double!
colnames(Bassinspecies)<-c("BassinName", "Species") #les esp?ces associ?es aux bassins
# i<-"Abramis brama"
# i<-"Cottus kazika"

BassinspeciesLL<-unique(Bassinspecies$Species)

tableBio56<-data.frame(Area.Bassins=numeric(),
                     MaxBio5=numeric(),
                     MinBio6=numeric())

N<-0
for (i in BassinspeciesLL)
{
  N<-N+1
  A<-subset(Bassinspecies, Species==i)
  B<-as.character(as.factor(A$BassinName))

  poly<-subset(shp, BasinName %in% B)
  ext<-extent(poly)

  ##################BIO5
  rshp<-crop(Bio5,ext,snap="out")
  clip5<-raster::mask(rshp,poly)
  clip5<-stack(clip5)
  maxquant5<-cellStats(clip5, stat='quantile', probs=0.95, na.rm=TRUE)
  tableBio56[i,1]<-sum(poly$Surf_area)
  tableBio56[i,2]<-maxquant5


  ##################BIO6
  rshp6<-crop(Bio6,ext,snap="out")
  clip6<-raster::mask(rshp6,poly)
  clip6<-stack(clip6)
  minquant6<-cellStats(clip6, stat='quantile', probs=0.05, na.rm=TRUE)
  tableBio56[i,3]<-minquant6

  print((N/length(BassinspeciesLL))*100)

}
####write.csv2(as.data.frame(tableBio), "BioperSpecies.csv")
write.csv2(as.data.frame(tableBio56), "Bio5_Bio6perSpecies.csv")







###BIO 1 et BIO 12
tableBio<-data.frame(Area.Bassins=numeric(),
                  MedianBio1=numeric(),
                  MeanBio1=numeric(),
                  MaxBio1=numeric(),
                  MinBio1=numeric(),
                  MedianBio12=numeric(),
                  MeanBio12=numeric(),
                  MaxBio12=numeric(),
                  MinBio12=numeric())
N<-0
for (i in BassinspeciesLL)
  {
  N<-N+1
  A<-subset(Bassinspecies, Species==i)
  B<-as.character(as.factor(A$BassinName))

    poly<-subset(shp, BasinName %in% B)
      ext<-extent(poly)

      ##################BIO1
      rshp<-crop(Bio1,ext,snap="out")
      clip1<-raster::mask(rshp,poly)
      clip1<-stack(clip1)
      mean1<-cellStats(clip1, stat='mean', na.rm=TRUE)
      median1<-cellStats(clip1, stat='median', na.rm=TRUE)
      max1<-cellStats(clip1, stat='max', na.rm=TRUE)
      min1<-cellStats(clip1, stat='min', na.rm=TRUE)
      tableBio[i,1]<-sum(poly$Surf_area)
      tableBio[i,2]<-median1
      tableBio[i,3]<-mean1
      tableBio[i,4]<-max1
      tableBio[i,5]<-min1

      ##################BIO12
      rshp12<-crop(Bio12,ext,snap="out")
      clip12<-raster::mask(rshp12,poly)
      clip12<-stack(clip12)
      median12<-cellStats(clip12, stat='median', na.rm=TRUE)
      mean12<-cellStats(clip12, stat='mean', na.rm=TRUE)
      max12<-cellStats(clip12, stat='max', na.rm=TRUE)
      min12<-cellStats(clip12, stat='min', na.rm=TRUE)
      tableBio[i,6]<-median12
      tableBio[i,7]<-mean12
      tableBio[i,8]<-max12
      tableBio[i,9]<-min12

print((N/length(BassinspeciesLL))*100)

}
##write.csv2(as.data.frame(tableBio), "BioperSpecies.csv")
write.csv2(as.data.frame(tableBio56), "Bio5_Bio6perSpecies.csv")


####add to the intro table
getwd()
physiotable<-read.csv2("./outputs/Bio5_Bio6perSpecies.csv")

colnames(physiotable)[1]<-"Species"
colnames(intro)
intro<-merge(x = intro, y = physiotable, by = "Species", all.x = TRUE)

###get the temperature amplitude : 
intro$Amplitudetemp<-intro$MaxBio5-intro$MinBio6   





###########Native ecoregion ----------------------############

#sensitivity analysis -- get 5 colonnes of differents diet
SENSREG<-list()##
for (sr in 1:5) {##


intronative<-tedesco%>% filter(X3.Native.Exotic.Status=="native")
intronative<-intronative[,c( c("X6.Fishbase.Valid.Species.Name", "X1.Basin.Name"))]

region<-tedescobassin[,c( c("X1.Basin.Name", "X3.Ecoregion"))]

regionLEROY<-read.csv("D:/these/database/basins with regions/basins_with_regions.shp.csv", na.strings=c("","NA"))  ###boris database with the new ecoregion
colnames(regionLEROY)
regionLEROY<-regionLEROY[,c("BasinNm.C.80", "Ecoregn.C.80", "Regions.C.80")]
colnames(regionLEROY)<-c( "X1.Basin.Name", "Native_regionTedesco", "Native_regionLeroy")

regionnative<-merge(x = intronative, y = regionLEROY, by = "X1.Basin.Name", all.x = TRUE) ##merge species and native ecoregion thanks to basins
regionnative<-regionnative[,c("X6.Fishbase.Valid.Species.Name","Native_regionTedesco", "Native_regionLeroy","X1.Basin.Name" )]

###save region - species association
#write.csv2(as.data.frame(regionnative), "regionnative.csv")

regionnative<-read.csv2("./regionnative.csv")
regionnative<-unique(regionnative[,c("X6.Fishbase.Valid.Species.Name", "Native_regionLeroy","X1.Basin.Name" )])
length(regionnative[,1])

 colnames(regionnative)[1]<-"Species"
 colnames(regionnative)[3]<-"Basin_Name"
 
 ###Maj region for each species
 ##nb bassin  ==> choose the most represented
 count1<-rep(1, nrow(regionnative))
 regionnative<-regionnative[,c("Species", "Native_regionLeroy")]
 regionnative$count<-count1
 #nbcat<-aggregate(regionnative[, "count"], list(regionnative$Species,regionnative$FoodI ), sum)
 regionnative<-as.data.frame(regionnative)
 class(regionnative$Species)
 regionnative$Species<-as.factor(regionnative$Species)
 
 nbeachcat<-regionnative %>%
   dplyr::group_by(Species, Native_regionLeroy) %>%
   dplyr::summarise(Total = sum(count, na.rm = TRUE))


 ####remove rows with NA
nbeachcat2<-na.omit(nbeachcat)
maxregion<-nbeachcat2 %>%  dplyr::group_by(Species) %>% top_n(1, Total) # pour chaque espèce, sélectionne la (ou les ) regions les plus représentées

###how many species have more than one main region?
 table(maxregion$Native_regionLeroy)
 blip<-as.data.frame(table(maxregion$Species))
 summary(as.factor(blip$Freq))

 ####transform the main region in several binary variables
 levels(maxregion$Native_regionLeroy)<-levels(as.factor(maxregion$Native_regionLeroy))
 regionname<-levels(as.factor(maxregion$Native_regionLeroy))

 #Initialise the columns to 0
 maxregion[regionname] <- 0
 mapply(function(x, y) maxregion[x, y] <<- 1, seq(nrow(maxregion)),  #transformation en variable binaire
        maxregion$Native_regionLeroy)

 ###get one row per sp
 maxregion2<-maxregion[, c("Species","Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")]
 maxregion2<-maxregion2 %>%
   dplyr::group_by(Species ) %>%
   dplyr::summarise_all(sum)

 ###how many filled regions?
 count2<-rep(1, nrow(nbeachcat2))
 numbregion<-nbeachcat2[,c("Species", "Native_regionLeroy")]
 numbregion<-na.omit(numbregion)
 numbregion$count<-count2
 nbregion<-numbregion %>%
   dplyr::group_by(Species) %>%
   dplyr::summarise(Total = sum(count, na.rm = TRUE))

 colnames(nbregion)<-c("Species", "Nb.Native.Region")
 
 
 ##how many Max region?
 NbregionMax<-rowSums(maxregion2[,c("Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")])
 maxregion3<-cbind(maxregion2, NbregionMax)

 summary(as.factor(NbregionMax))
 
 
 
 ##find the main max region per order-----------------------
 ordersp<-unique(intro[,c("Species", "Order")])
 nbregionorder1<-merge(ordersp, numbregion, by="Species")
 nbregionorder2<-unique(nbregionorder1) ##to just have the region occurence per species
 nbregionorder<-nbregionorder2 %>%
   dplyr::group_by(Order, Native_regionLeroy) %>%
   dplyr::summarise(Total = sum(count, na.rm = TRUE))
 maxnbregionorder<-as.data.frame(nbregionorder%>% group_by(Order) %>% top_n(1, Total))
 
 
 
 
 
 #If more than 1 max region --> choose the region the most represented in order
 
 severalregion<-subset(maxregion3, NbregionMax>1)
 severalregionsp<-merge(severalregion, ordersp, by="Species")
 severalregionsp2<-merge(severalregionsp, maxnbregionorder, by="Order")
 bloup<-as.data.frame(table(severalregionsp2$Species))
 notpossible<-subset(bloup, Freq>1) ###5 species among those with several max region have an order with several max region
 
 ###are these sp in the pool used for models?
 set.seed(123)
 poolusedregion<-readRDS("D:/these/Axe_2/outputs/Specieswithseveralregion_poolused.rds")
 notpossible$Var1
 notpossible$Var1 %in% poolusedregion$Species
 
 maxregion3[,"maxregion status"]<-0
 for (i in 1:length(maxregion3[,1])) {####replace the multiple max region by the max region in order
   if (maxregion3[i,"NbregionMax"]>1) {
     spsevregion<-merge(maxregion3[i,], ordersp, by="Species")
     or<-spsevregion$Order
     blap<-maxnbregionorder[maxnbregionorder$Order == or, ]
     bap<-as.character(factor(blap[sample(1:length(blap[,1]), 1), "Native_regionLeroy"])) ###if more than one max region in order, choose one randomly
     if (maxregion3[i, bap]==1) { ###if the order region correspond to a max region of the species, put this region
       maxregion3[i,c("Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")]<-0
       maxregion3[i,bap]<-1
     }else{ ##else, choose a max region randomly in the species region
       #maxregion3[i,c("detritus","nekton","plants","zoobenthos","zooplankton")]<-0
       maxregion3[i,"maxregion status"]<-"not represented in species"
       blap2<-which(maxregion3[i,c("Species","Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")]==1)
       bap2<-sample(blap2, 1)
       maxregion3[i,c("Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")]<-0
       maxregion3[i,bap2]<-1
     }
     
     
   }
 }
 
 

 # ####if more than one region ==> choose randomly
 # for (i in 1:length(maxregion3[,1])) {
 #   if (maxregion3[i,"NbregionMax"]>1) {
 #     blap<-which(maxregion3[i,c("Species","Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")]==1)
 #     bap<-sample(blap, 1)
 #     maxregion3[i,c("Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")]<-0
 #     maxregion3[i,bap]<-1
 #   }
 # 
 # }


 ##new column
 Maj.Region<-c()
 for (i in 1:length(maxregion3[,1])) {
   blap<-which(maxregion3[i,c("Species","Australian","Ethiopian","Madagascan","Nearctic","Neotropical","Palearctic","Sino-Oriental")]==1)
   name<-colnames(maxregion3)[blap]
   Maj.Region<-c(Maj.Region, name)
 }

 Maj.Region

 maxregion4<-cbind(maxregion3, Maj.Region)
 
 ##sensitivity analysis###
 sens2<-maxregion4[,c("Species", "Maj.Region")]##
 SENSREG[[sr]]<-as.data.frame(sens2)##

 } ##
 getwd()
 setwd("D:/these/Axe_2/outputs/sensitivity-analysis")
 saveRDS(SENSREG, "SENSREG.rds")##
 setwd("D:/these/Axe_2")

 #Add nb region in "intro"
 intro<-merge(x = intro, y = nbregion, by = "Species", all.x = TRUE)
 #Add maxregion in "intro"
 intro<-merge(x = intro, y = maxregion4[,c("Species", "Maj.Region", "NbregionMax", "maxregion status")], by = "Species", all.x = TRUE)
 


 ####Morphlogical traits (Su database)----------------------------------------------#######################
 morpho<-read.csv2("./Database/Base Morpho_10traits_10705.csv")
 
 morpho<-transform(morpho, Species=paste(Genus, species, sep=" ")) ##avoir le nom d'espèce complet sans point
 morpho<-morpho[, c("EdHd", "MoBd","JlHd","EhBd","BlBd","HdBd","PFiBd","PFlBl","CFdCPd","Length", "Species")]
 
 intro<-merge(morpho,intro, by="Species", all.y = TRUE)







 #clean the intro table
intro<-subset(intro, select=-c(match.intro_esta, match.intro_esta2,match.impeco_esta,match.impsocio_esta,match.impeco_estaun,match.impsocio_estaun,Freshwater,From))


##save intro table##
#write.csv2(as.data.frame(intro), "./outputs/intro.csv")



######Unique table for each step#########
##INTRODUCTON
colnames(intro)
intro2<-subset(intro, select=-c(TO,Estabwild,EcolEff, SocioEff, Reason))
intro2<-unique(intro2)
Introtable<-intro2 %>% filter(Introduced=="Yes")
Introtableno<-intro2 %>% filter(Introduced=="No")

IntroallTable<-intro2[!(intro2$Species %in% Introtable$Species),]

tableintro<-unique(rbind(Introtable, IntroallTable))
#table(tableintro$Species)
table(tableintro$Introduced)
taloup<-unique(tableintro$Species) #pour être sur que les espèce n'apparaissent qu'une fois


#write.csv2(as.data.frame(tableintro), "./outputs/introunique.csv")


##ETABLISSEMENT
esta<-subset(intro, select=-c(TO,Introduced,EcolEff, SocioEff, Reason))
Estatable<-esta%>% filter(Estabwild=="Yes" | Estabwild=="No")
Estatableno<-esta%>% filter(Estabwild=="No")
Estatableyes<-esta%>% filter(Estabwild=="Yes")
EstaallTable<-Estatable[!(Estatable$Species %in% Estatableyes$Species),]

tableesta<-unique(rbind(Estatableyes, EstaallTable))
taloup<-unique(tableesta$Species)
#table(tableesta$Species)
table(tableesta$Estabwild)

##save
#write.csv2(as.data.frame(tableesta), "./outputs/estaunique.csv")


###IMPACT ECO
impacteco<-subset(intro, select=-c(TO,Introduced,Estabwild, SocioEff, Reason))
impactecotable<-impacteco%>% filter(EcolEff=="Yes" | EcolEff=="No")
impactecotableno<-impacteco%>% filter(EcolEff=="No")
impactecotableyes<-impacteco%>% filter(EcolEff=="Yes")
impactecoallTable<-impactecotable[!(impactecotable$Species %in% impactecotableyes$Species),]

tableimpacteco<-unique(rbind(impactecotableyes, impactecoallTable))
#table(tableesta$Species)
table(tableimpacteco$EcolEff)
taloup<-unique(tableimpacteco$Species)


#write.csv2(as.data.frame(tableimpacteco), "./outputs/impactecounique.csv")


###IMPACT SOCIO
impactSocio<-subset(intro, select=-c(TO,Introduced,Estabwild,EcolEff, Reason))
impactSociotable<-impactSocio%>% filter(SocioEff=="Yes" | SocioEff=="No")
impactSociotableno<-impactSocio%>% filter(SocioEff=="No")
impactSociotableyes<-impactSocio%>% filter(SocioEff=="Yes")
impactSocioallTable<-impactSociotable[!(impactSociotable$Species %in% impactSociotableyes$Species),]

tableimpactSocio<-unique(rbind(impactSociotableyes, impactSocioallTable))
#table(tableesta$Species)
table(tableimpactSocio$SocioEff)
taloup<-unique(tableimpactSocio$Species)


##SAVE
#write.csv2(as.data.frame(tableimpactSocio), "./outputs/impactSociounique.csv")



###gather all unique table
UNIQUEINTRO<-merge(x = tableintro, y = tableesta[,c("Species","Estabwild")], by = "Species", all.x = TRUE)
UNIQUEINTRO<-merge(x = UNIQUEINTRO, y = tableimpacteco[,c("Species","EcolEff")], by = "Species", all.x = TRUE)
UNIQUEINTRO<-merge(x = UNIQUEINTRO, y = tableimpactSocio[,c("Species","SocioEff")], by = "Species", all.x = TRUE)
# ###puis les percentages
# UNIQUEINTRO<-merge(x = UNIQUEINTRO, y = Nbeachstep[,c("Species","Establish.percent","Socioimp.percent","Ecoimp.percent")], by = "Species", all.x = TRUE)


#######change column order
#UNIQUEINTRO<-UNIQUEINTRO%>%relocate(Species, Introduced, Estabwild, Establish.percent,EcolEff, Ecoimp.percent, SocioEff, Socioimp.percent)
UNIQUEINTRO<-UNIQUEINTRO%>%relocate(Species, Introduced, Estabwild,EcolEff, SocioEff)



UNIQUEINTRO<-unique(UNIQUEINTRO)
write.csv2(as.data.frame(UNIQUEINTRO), "./outputs/UNIQUEINTRO_selectedregdietESSAICHANGE.csv")


b<-as.data.frame(table(UNIQUEINTRO$Species))
View(b)
blip<-subset(b, Freq>1) 







####LAST ADJUSTMENTS######
setwd("D:/these/Axe_2")
INTRO<-read.csv2("./outputs/UNIQUEINTRO_selectedregdietESSAICHANGE.csv")

#####data in good format
INTRO<-subset(INTRO, select=-c(X))

colnames(INTRO)
INTRO[c("Introduced","Estabwild","EcolEff", "SocioEff", "Maj.Diet", "Accidental","Sport.Angling","Aquaculture", "Diffusion", "Species.control" ,
        "fisheries","ornamental",
        "RepGuild1", "Maj.Region", "Used_by_humans" )]<-
  lapply(INTRO[c("Introduced","Estabwild","EcolEff", "SocioEff", "Maj.Diet", "Accidental","Sport.Angling","Aquaculture", "Diffusion", "Species.control" ,
                 "fisheries" ,"ornamental",
                 "RepGuild1", "Maj.Region", "Used_by_humans" )], factor)


####Order : gather some orders
nborder<-as.data.frame(table(INTRO$Order))
smallorder<-filter(nborder, Freq<20)
smallordername<-levels(smallorder$Var1)


INTRO$Order<-as.factor(INTRO$Order)
levels(INTRO$Order)
levels(INTRO$Order)[levels(INTRO$Order)=="Amiiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Acipenseriformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Anguilliformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Atheriniformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Beloniformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Carcharhiniformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Ceratodontiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Elopiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Esociformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Gadiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Gasterosteiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Gonorynchiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Gymnotiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Lepidosireniformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Lepisosteiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Mugiliformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Myliobatiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Osmeriformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Osteoglossiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Percopsiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Petromyzontiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Pleuronectiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Polypteriformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Rhinopristiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Scorpaeniformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Synbranchiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Syngnathiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Tetraodontiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Clupeiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Salmoniformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Gobiesociformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Batrachoidiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Lophiiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Ophidiiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Orectolobiformes"]<-"Other"
levels(INTRO$Order)[levels(INTRO$Order)=="Albuliformes"]<-"Other"

# ### 
# INTRO$Nb_uses<-rescale(INTRO$Nb_uses)
# INTRO$Nb_reasons<-rescale(INTRO$Nb_reasons)
# INTRO$TL<-rescale(INTRO$TL)
# INTRO$nb.country.intro2<-rescale(INTRO$nb.country.intro2)
# INTRO$nb.country.intro<-rescale(INTRO$nb.country.intro)
# INTRO$Area.Bassins<-rescale(INTRO$Area.Bassins)
# INTRO$MaxBio5<-rescale(INTRO$MaxBio5)
# INTRO$MinBio6<-rescale(INTRO$MinBio6)
# INTRO$Amplitudetemp<-rescale(INTRO$Amplitudetemp)
# INTRO$BlBd<-rescale(INTRO$BlBd)
# INTRO$PFiBd<-rescale(INTRO$PFiBd)

##############remove species introduced but without filled pathway##########
INTRO<-INTRO %>% filter(!(Introduced=='Yes'& is.na(Nb_reasons)))

####################save the all_good table###########
write.csv2(as.data.frame(INTRO), "./outputs/INTRO_all_good_selectedregdietESSAICHANGE.csv")




