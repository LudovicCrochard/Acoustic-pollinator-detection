focale       <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/data_focales.csv", col_names = TRUE)
enregistrement <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/bilan_enregistrements.csv", col_names = TRUE)
pheno <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/data_phenologie_new.csv", col_names = TRUE)
suivi <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/fiche_suivi.csv", col_names = TRUE)
library(dplyr)


suivi <- unique(suivi[,c(1,3,4)])
suivi <- na.omit(suivi)

test <- left_join(suivi, pied, by=c("Parcelle", "Ligne", "Pied"))
test1 <- test %>% 
  count(Parcelle, Ligne, Pied, Stade_pied)
test1 <- test1 %>% 
  filter(Stade_pied=="F")
test1$jour_floraison <- test1$n*7
test1$heure <- test1$jour_floraison*16



meta_focale <- unique(focale[,c(1:6,13,14,15,16,18,19,20,21,22)])
obs_focale <- unique(focale[,c(1:6,18,17,23,24,25,26,27)])
pied <- unique(focale[,c(1,2,15,4,5,6,7,8,9,10,11)])
pied <- na.omit(pied)
pied <- filter(pied, Ligne!="l6")

t <- left_join(enregistrement, pied, by=c("Parcelle", "Ligne", "Pied", "semaine_calendaire"))
enregistrement_duree <- aggregate(duree~Parcelle+Ligne+Pied+Stade_pied, data=t, FUN=sum)
enregistrement_duree$heure_enregist <- enregistrement_duree$duree/3600
enregistrement_duree$jour <- enregistrement_duree$heure_enregist/16


enregistrement_duree <- enregistrement_duree %>% 
  filter(Stade_pied=="F")

t <- left_join(test1, enregistrement_duree, by=c("Parcelle", "Ligne", "Pied"))
t[is.na(t)] <- 0
t$pourcentage_floraison <- (t$heure_enregist/t$heure)*100

plot(y=t$Parcelle,x=t$pourcentage_floraison)
abline(v=70, col="blue")


suivi <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/fiche_suivi.csv", col_names = TRUE)
suivi <- unique(suivi[,c(1)])


test2 <- left_join(suivi, pheno, by=c("Parcelle"))
test3 <- filter(test2, Stade_floraison=="F", Pourcentage_floraison>=50)
test4 <- test3 %>% 
  count(Parcelle)
test4$jour <- test4$n*7
test4$heure_floraison <- test4$jour*16


t1 <- left_join(enregistrement, pheno, by=c("Parcelle", "semaine_calendaire"))
t5 <- filter(t1, Stade_floraison=="F")
t5 <- filter(t5, Pourcentage_floraison>=50)

t2 <- filter(t1, Ligne=="l3")
t3 <- filter(t1, is.na(Ligne))
t1 <- rbind(t2, t3)
t1 <- filter(t1, Pourcentage_floraison>=50)
enregistrement_duree_parcelle <- aggregate(duree~Parcelle+Stade_floraison, data=t1, FUN=sum)
enregistrement_duree_parcelle$heure_enregist <- enregistrement_duree_parcelle$duree/3600
enregistrement_duree_parcelle$jour <- enregistrement_duree_parcelle$heure_enregist/16


t4 <- left_join(test4, enregistrement_duree_parcelle, by=c("Parcelle"))
t4[is.na(t4)] <- 0
t4$pourcentage_floraison <- (t4$heure_enregist/t4$heure_floraison)*100

plot(y=t4$Parcelle,x=t4$pourcentage_floraison)
abline(v=70, col="blue")





#Prepa data pour Kaleidoscope
essai       <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/data/enregistrements_focales.csv", col_names = TRUE)
essai <- filter(essai, Focale=="oui")
for( i in 1 : nrow(essai)){
  file.copy(from = essai$Fichier_source[i], to = essai$Chemin_destination[i], copy.mode = TRUE)
}


essai1 <- essai[c(1),]
file.info(essai1$Chemin_s_fichier)
Sys.chmod(essai1$Chemin_s_fichier, mode= "644")

apply(essai,1, file.copy(from = essai$Chemin_s_fichier, to = essai$chemin_d, copy.mode = TRUE)
)

essai       <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/classeur1.csv", col_names = TRUE)
essai1 <- essai[order(essai$Parcelle, essai$ligne),]
write.table(essai1, "enregistrments.csv", row.names=FALSE, sep=",", na=" ")

















library(tuneR)

DirToTreat=dir("E:/Traitement_focale_tournesol_2020/10sec/fichiers_decoupes/parcelle_10486/l3t1",pattern=".wav",full.names=T)
#DirToTreat=subset(DirToTreat,substr(basename(DirToTreat),1,2)=="wn")


for (i in 1:length(DirToTreat))
{
  #DirToTreat=list.files(DirToTreat[i],pattern=".wav$",full.names=T)
  DirToTreat=subset(DirToTreat,!grepl("-x10.wav",DirToTreat))
  for (j in 1:length(DirToTreat))
  {
    Dur=0
    if((file.size(DirToTreat[j]))>40000)
    {
      tempW=readWave(DirToTreat[j])
      Dur=length(tempW@left)/tempW@samp.rate
      if(Dur>0)
      {
        tempW@samp.rate=tempW@samp.rate*10
        writeWave(tempW,gsub(".wav","-x10.wav",DirToTreat[j]))
        
        #savewav(Mix,filename=paste0(DestMix,"/",basename(DirToTreat[j])))
      }
    }
    print(paste(j,DirToTreat[j],Dur))
  }
}

library(dplyr)
library(tidyr)
focales <- readr::read_csv("C:/Users/ludo2/Documents/These/audiomoth/data/enregistrements_focales.csv", col_names = TRUE)
focales1 <- separate(focales, Fichier, c("Date", "Heure"), sep="_")
focales1 <- filter(focales1, Focale=="oui")
focales1 <- unique(focales1[,c(1,2,3,4,7,8,11,13,14,15)])
focales1$Parcelle <- as.numeric(focales1$Parcelle)
focales1 <- focales1[order(focales1$Parcelle),]
focales1 <- filter(focales1, Etiquettage=="non")
focales1 <- filter(focales1, Test_classif=="non")
focales_pol <- filter(focales1, Pol_theorique=="oui")
focales_no_pol <- filter(focales1, Pol_theorique=="non")
focales_no_pol[sample(1:nrow(focales_no_pol), 7, replace=FALSE), ]
focales_pol[sample(1:nrow(focales_pol), 3, replace=FALSE), ]
t <-focales1[sample(1:nrow(focales1), 30, replace=FALSE), ]



library(sampling)
library(dplyr)
test <- readr::read_csv("D:/tests_classif6_vs_classif7/Test7_augmente_2e_essai/Test7_augmentation_10000_classif_IdTot.csv", col_names = TRUE)
test <- IdTot2
test$insecte <- test$Apismel+test$Bomsp+test$insect

for(i in 1:nrow(test)){
  if (test$insecte[i]>0.9){
    
    test$test[i] <- c("high")
    
  } else if (test$insecte[i]>0.8){
    
    test$test[i] <- c("9")
  } else if (test$insecte[i]>0.7){
    
    test$test[i] <- c("8")
  } else if (test$insecte[i]>0.6){
    
    test$test[i] <- c("7")
  } else if (test$insecte[i]>0.5){
    
    test$test[i] <- c("6")
  } else if (test$insecte[i]>0.4){
    
    test$test[i] <- c("5")
  } else if (test$insecte[i]>0.3){
    
    test$test[i] <- c("4")
  } else if (test$insecte[i]>0.2){
    
    test$test[i] <- c("3")
  } else if (test$insecte[i]>0.1){
    
    test$test[i] <- c("2")
  } else if (test$insecte[i]>0){
    
    test$test[i] <- c("1")
  }
}
test <- data.frame(test)
test$Espece <- as.factor(test$Espece)
test$test <- as.factor(test$test)
table(test$Espece, test$test)
table(test$test)

t <- as.numeric(c(rep("1",10)))

t <- c(20,20,20,20,20,7,7,20,2,7)



s=strata(test,c("test"),size=t, method="srswor")
test1 <-getdata(test,s)


write.table(test1, "Test7_classif.csv", col=NA, sep="\t", dec=".")




#Comparaison entre les 2 méthodes de regroupement des espèces
test1 <- readr::read_csv("E:/Manip_tournesol_2020/data/test_classif_IdTot.csv", col_names = TRUE)



for(i in 1:nrow(test1)){
  if (test1$insect[i]>0.9){
    
    test1$score_pol[i] <- c("high")
    
  } else if (test1$insect[i]>0.8){
    
    test1$score_pol[i] <- c("9")
  } else if (test1$insect[i]>0.7){
    
    test1$score_pol[i] <- c("8")
  } else if (test1$insect[i]>0.6){
    
    test1$score_pol[i] <- c("7")
  } else if (test1$insect[i]>0.5){
    
    test1$score_pol[i] <- c("6")
  } else if (test1$insect[i]>0.4){
    
    test1$score_pol[i] <- c("5")
  } else if (test1$insect[i]>0.3){
    
    test1$score_pol[i] <- c("4")
  } else if (test1$insect[i]>0.2){
    
    test1$score_pol[i] <- c("3")
  } else if (test1$insect[i]>0.1){
    
    test1$score_pol[i] <- c("2")
  } else if (test1$insect[i]>0){
    
    test1$score_pol[i] <- c("1")
  }
}


test2 <- readr::read_csv("E:/Traitement_focale_tournesol_2020/test_classif1_sum.csv", col_names = TRUE)
test2 <-left_join(test2, test1, by=c("Group.1"))
test2 <- test2[,c(1:11, 25,26,27,31,32,21,33,12:15)]

write.table(test2, "Test_classiftest.xls", col=NA, sep="\t", dec=".")
