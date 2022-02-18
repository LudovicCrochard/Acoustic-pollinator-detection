library(data.table)
library(tuneR)
library(stringr)

RSDB="G:/tests_classif6_vs_classif7/RSDBs/RSDB_apres_test6"
RawTable=fread("G:/tests_classif6_vs_classif7/RSDBs/RSDB_apres_test6_tabase3HF_sansfiltre.csv")
NbFichiersAugmentes=100000
DirAugmentes="G:/tests_classif6_vs_classif7/RSDBs/data_aug_100000_apres_test6"

ListWav=list.files(RSDB,full.names=T,recursive=T,pattern=".wav$")
dir.create(DirAugmentes)

table(RawTable$Espece)
NbDSEpEsp=aggregate(RawTable$Filename,by=list(RawTable$Espece),length)
NbDSEpEsp$Ponderation=1-(NbDSEpEsp$x/max(NbDSEpEsp$x))+0.001


FilesA=vector()
FilesB=vector()
Ratios=vector()
for (i in 1:NbFichiersAugmentes)
{
  SpSel=sample(NbDSEpEsp$Group.1,prob=NbDSEpEsp$Ponderation,size=1,replace=T)
  RawSp=subset(RawTable,RawTable$Espece==SpSel[1])
  FileSp=unique(RawSp$Filename)
  FileSel=sample(FileSp,1)
  RetrieveFile=match(FileSel,basename(ListWav))
  WavA=readWave(ListWav[RetrieveFile])
  FileSelB=sample(ListWav,1)
  WavB=readWave(FileSelB)
  Ratio=sample(c(1:99),1)
  WavAugmente=WavA*Ratio/100+WavB*(1-Ratio)/100
  NumbPad=str_pad(i, nchar(NbFichiersAugmentes), pad = "0")
  WavAugmente = (normalize(WavAugmente, unit="16"))
  writeWave(WavAugmente,filename=paste0(DirAugmentes,"/WavAugmente_",NumbPad,".wav"))
  FilesA=c(FilesA,ListWav[RetrieveFile])
  FilesB=c(FilesB,FileSelB)
  Ratios=c(Ratios,Ratio)
  
}

AugmentationData=data.frame(FilesA,FilesB,Ratios)
fwrite(AugmentationData,paste0(DirAugmentes,"/AugmentationData.csv"),sep=";")

       