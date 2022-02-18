library(data.table)
library(Hmisc)

AugmentationDir="G:/tests_classif6_vs_classif7/RSDBs/data_aug_100000_apres_test6"
ToleranceFreq=0.2
ToleranceTemps=8



WavAugmentes=list.files(AugmentationDir,pattern=".wav$",full.names=T)
AugmentationData=fread(paste0(AugmentationDir,"/AugmentationData.csv"))
names_eti <- c("Cri",	"Espece",	"Type",	"Indice",	"Zone",	"Site",	"Commentaire", "Materiel",	"Confidentiel",	"Date",	"Auteur",	"Etiqueteur", "V1")

if(nrow(AugmentationData)!=length(WavAugmentes)){stop("donnees manquantes")}

NewRef=data.frame()
for (i in 1:nrow(AugmentationData))
{
  #etiquettes initiales
  EtiInitA=fread(paste0(dirname(AugmentationData$FilesA[i]),"/eti/",gsub(".wav",".eti",basename(AugmentationData$FilesA[i]))),sep="\t"
  )
  names(EtiInitA)=c(names(EtiInitA)[2:ncol(EtiInitA)],"V1") 

  EtiInitB=fread(paste0(dirname(AugmentationData$FilesB[i]),"/eti/",gsub(".wav",".eti",basename(AugmentationData$FilesB[i]))),sep="\t"
  )
  names(EtiInitB)=c(names(EtiInitB)[2:ncol(EtiInitB)],"V1")
 if(names(EtiInitA)!=names_eti || names(EtiInitB)!=names_eti)
   next
  
  ParamA=fread(paste0(dirname(AugmentationData$FilesA[i]),"/txt/",gsub(".wav",".ta",basename(AugmentationData$FilesA[i]))))
  RefA=merge(EtiInitA,ParamA,by.x="Cri",by.y="CallNum") 
  
  
  ParamB=fread(paste0(dirname(AugmentationData$FilesB[i]),"/txt/",gsub(".wav",".ta",basename(AugmentationData$FilesB[i]))))
  RefB=merge(EtiInitB,ParamB,by.x="Cri",by.y="CallNum") 
  
  RefAB=rbind(RefA,RefB)
  RefAB$PeakTime=RefAB$StTime+RefAB$PosMP*RefAB$Dur
  #detections nouvelles
  ParamAB=fread(paste0(dirname(WavAugmentes[i]),"/txt/",gsub(".wav",".ta",basename(WavAugmentes[i]))))    
  ParamAB$PeakTime=ParamAB$StTime+ParamAB$PosMP*ParamAB$Dur
  
  #test avec conditions
  NewLabelTable=data.frame()
  for (j in 1:nrow(ParamAB))
  {
    RefCorrj=subset(RefAB,(RefAB$Fmin<=ParamAB$FreqMP[j])&
                      (RefAB$Fmax>=ParamAB$FreqMP[j])&
                      (RefAB$StTime<=ParamAB$PeakTime[j])&
                      ((RefAB$StTime+RefAB$Dur)>=ParamAB$PeakTime[j]))
    print(nrow(RefCorrj))
    #fuzzy matching
    if(nrow(RefCorrj)>0){
      NewLabels=find.matches(cbind(ParamAB$PeakTime[j],ParamAB$FreqMP[j]),cbind(RefCorrj$PeakTime,RefCorrj$FreqMP),tol=c(ToleranceTemps,ToleranceFreq)
                             ,maxmatch=2)
      
      print(NewLabels$matches)
      if(NewLabels$matches[1]==0){
        #stop("test0")
        if(nrow(RefCorrj)==1){
          RefCorrj_labels=subset(RefCorrj,select=names(EtiInitA))
          NewLabelTable=rbind(NewLabelTable,RefCorrj_labels)  
        }else{
          RefCorrj_labels=subset(RefCorrj[1,],select=names(EtiInitA))
          RefCorrj_labels$Espece="ambiguous"
          NewLabelTable=rbind(NewLabelTable,RefCorrj_labels)
        }
        
      }else{
        if(length(NewLabels$matches)==1)
        {
          RefCorrj2=RefCorrj[NewLabels$matches,]
          RefCorrj_labels=subset(RefCorrj2,select=names(EtiInitA))
          NewLabelTable=rbind(NewLabelTable,RefCorrj_labels)
        }else{
          
          #stop("test2")
          RefCorrj_labels=subset(RefCorrj[1,],select=names(EtiInitA))
          RefCorrj_labels$Espece="ambiguous"
          NewLabelTable=rbind(NewLabelTable,RefCorrj_labels)  
          
        }
        
      }
      
    }else{ #cas ou refcorrj est vide
      RefCorrj=RefAB[1,]
      RefCorrj$Espece="unknown"
      RefCorrj_labels=subset(RefCorrj,select=names(EtiInitA))
      NewLabelTable=rbind(NewLabelTable,RefCorrj_labels)
      
    }
    
  }
  Refi=cbind(NewLabelTable,ParamAB)
NewRef=rbind(NewRef,Refi)
}  

table(NewRef$Espece)

fwrite(NewRef,paste0(AugmentationDir,"/NewRefTable.csv"),sep=";")
