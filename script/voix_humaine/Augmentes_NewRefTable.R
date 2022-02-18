library(data.table)
library(Hmisc)
library(tidyverse)
AugmentationDir="G:/travail_manip_tournesol/travail_sur_voix_humaine/test_50000"
# ToleranceFreq=0.2
# ToleranceTemps=8



WavAugmentes=list.files(AugmentationDir,pattern=".wav$",full.names=T)
AugmentationData=fread(paste0(AugmentationDir,"/AugmentationData.csv"))
names_eti <- c("Cri",	"Espece",	"Type",	"Indice",	"Zone",	"Site",	"Commentaire", "Materiel",	"Confidentiel",	"Date",	"Auteur",	"Etiqueteur", "V1")

if(nrow(AugmentationData)!=length(WavAugmentes)){stop("donnees manquantes")}

NewRef=data.frame()
for (i in 1:nrow(AugmentationData))
{
  #etiquettes initiales
  EtiInitA=fread(paste0(dirname(AugmentationData$FilesA[i]),"/eti/",gsub(".wav",".eti",basename(AugmentationData$FilesA[i])))
  )
  names(EtiInitA)=c(names(EtiInitA)[2:ncol(EtiInitA)],"V1") 

  if(names(EtiInitA)!=names_eti)
    next
  
  ParamA=fread(paste0(dirname(AugmentationData$FilesA[i]),"/txt/",gsub(".wav",".ta",basename(AugmentationData$FilesA[i]))))
  RefA=merge(EtiInitA,ParamA,by.x="Cri",by.y="CallNum") 
  RefA$PeakTime=RefA$StTime+RefA$PosMP*RefA$Dur
  RefA$EndTime=RefA$StTime+RefA$Dur
  #detections nouvelles
  ParamAB=fread(paste0(dirname(WavAugmentes[i]),"/txt/",gsub(".wav",".ta",basename(WavAugmentes[i]))))    
  ParamAB$PeakTime=ParamAB$StTime+ParamAB$PosMP*ParamAB$Dur
  ParamAB$EndTime=ParamAB$StTime+ParamAB$Dur
  
  
  #test avec conditions
    for(k in 1:nrow(RefA)){
    AB_Timek=((ParamAB$PeakTime>=RefA$StTime[k])&(ParamAB$PeakTime<=RefA$EndTime[k]))
    AB_Freqk=((ParamAB$FreqMP>=RefA$Fmin[k])&(ParamAB$FreqMP<=RefA$Fmax[k]))
    
     ParamAB=subset(ParamAB,!(AB_Timek&AB_Freqk))
    }
  ParamAB=cbind(RefA[nrow(ParamAB),c(1:13)], ParamAB)
  ParamAB$Espece <- "Homsap"
  ParamAB$Cri <- NA
  ParamAB$Indice <- NA
  NewRef=rbind(NewRef, ParamAB) 
}
  

table(NewRef$Espece)


fwrite(NewRef,paste0(AugmentationDir,"/NewRefTable.csv"),sep=";")
