library(data.table)
library(tuneR)
library(stringr)

RSDB="G:/travail_manip_tournesol/RSDB"
RawTable=fread("G:/travail_manip_tournesol/data_base/Classif_apres_gros_etiquetage/RSDB_tabase3HF_sansfiltre.csv")
NbFichiersAugmentes=100000
DirAugmentes="G:/travail_manip_tournesol/doc_voix_humaine/base_voix_humaine/fichiers_augmentes/test_with_100000"
VoiceBase= "G:/travail_manip_tournesol/doc_voix_humaine/base_voix_humaine/homogeneisation"

ListWav=list.files(RSDB,full.names=T,recursive=T,pattern=".wav$")
# dir.create(DirAugmentes)
ListVoice= list.files(VoiceBase,full.names=T,recursive=T,pattern="-x10.wav$")
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
  FileSelB=sample(ListVoice,1)
  WavB=readWave(FileSelB)
  Ratio=sample(c(1:99),1)
  # WavB@samp.rate=WavA@samp.rate
  # WavB=mono(WavB, which ="left")
  lmin = min(length(WavA), length(WavB)) #durée enregistrement le plus court
  WavA = extractWave(WavA, from=1, to=lmin)
  WavB = extractWave(WavA, from=1, to=lmin)
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

       