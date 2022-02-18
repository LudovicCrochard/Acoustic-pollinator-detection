library(data.table)

BaseInit=fread("G:/tests_classif6_vs_classif7/RSDBs/RSDB_sans_test6_et_sans_test7_tabase3HF_sansfiltre.csv")
RefAugmentees=fread("G:/tests_classif6_vs_classif7/RSDBs/data_aug_100000_avant_test6/NewRefTable.csv")

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
# BaseAugmentee <- BaseAugmentee[,-c(289,290,291)]

fwrite(BaseAugmentee,"BaseAugmentee.csv",sep=";")
