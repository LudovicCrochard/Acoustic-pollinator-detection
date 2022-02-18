library(data.table)

BaseInit=fread("G:/travail_manip_tournesol/data_base/Classif_ap_gros_etiquetage2/RSDB_tabase3HF_sansfiltre.csv")
RefAugmentees=fread("G:/travail_manip_tournesol/travail_sur_voix_humaine/test_50000/NewRefTable.csv")

BaseInit$Cri=BaseInit$CallNum
RefAugmentees$Nesp=RefAugmentees$Espece
RefAugmentees=subset(RefAugmentees,RefAugmentees$Nesp!="")
RefAugmentees=subset(RefAugmentees,RefAugmentees$Nesp!="ambiguous")
RefAugmentees=subset(RefAugmentees,RefAugmentees$Nesp!="unknown")
RefAugmentees$SubNesp= paste(RefAugmentees$Espece, RefAugmentees$Type, sep="_")
BaseAugmentee=rbindlist(list(BaseInit,RefAugmentees),use.names=T,fill=T)
table(BaseAugmentee$Nesp)
table(BaseInit$Nesp)

#Test

fwrite(BaseAugmentee,"BaseAugmentee.csv",sep=";")
