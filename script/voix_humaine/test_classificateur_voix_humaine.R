library(data.table)
library(tidyverse)

focale_classif <- fread("G:/travail_manip_tournesol/travail_sur_voix_humaine/test_50000/focale_for_test_IdTot.csv")
identif_test <- fread("G:/travail_manip_tournesol/base_de_test/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")

identif_voix_humaine <- left_join(focale_classif, identif_test[, c(2,35,37,38)], by="Group.1")
identif_voix_humaine <- filter(identif_voix_humaine, Pol_or_not!="NA")
identif_voix_humaine$score_pol <- identif_voix_humaine$Apismel+identif_voix_humaine$Bomsp+identif_voix_humaine$insect

identif_voix_humaine <- identif_voix_humaine %>% 
  group_by(Group.1) %>%
  filter(score_pol == max(score_pol)) 


for(i in 1:nrow(identif_voix_humaine)){
  if (identif_voix_humaine$score_pol[i]>0.9){
    
    identif_voix_humaine$pol_class_new[i] <- c("6")
    
  } else if (identif_voix_humaine$score_pol[i]>0.8){
    
    identif_voix_humaine$pol_class_new[i] <- c("5")
  } else if (identif_voix_humaine$score_pol[i]>0.7){
    
    identif_voix_humaine$pol_class_new[i] <- c("4")
  } else if (identif_voix_humaine$score_pol[i]>0.6){
    
    identif_voix_humaine$pol_class_new[i] <- c("3")
  } else if (identif_voix_humaine$score_pol[i]>0.5){
    
    identif_voix_humaine$pol_class_new[i] <- c("2")
  } else if (identif_voix_humaine$score_pol[i]>=0){
    
    identif_voix_humaine$pol_class_new[i] <- c("1")
  } 
}

identif_voix_humaine$pol_class_new <- as.factor(as.character(identif_voix_humaine$pol_class_new))
table(identif_voix_humaine$pol_class_new)

# write.csv(identif_voix_humaine, "Resultat_test_classif_voix_humaine.csv")


#Test en equilibrant les classe insect et Homsap

focale_classif <- fread("G:/travail_manip_tournesol/travail_sur_voix_humaine/test_homogeisation_class_pol_homsap/focale_for_test_IdTot.csv")
identif_test <- fread("G:/travail_manip_tournesol/data_base/Classif_avec_ajout_voix_humaine/Resultat_test_classif_voix_humaine.csv")

identif_voix_humaine <- left_join(focale_classif, identif_test[, c(2,33,34,35,37)], by="Group.1")
identif_voix_humaine <- filter(identif_voix_humaine, Pol_or_not!="NA")
identif_voix_humaine$score_pol <- identif_voix_humaine$Apismel+identif_voix_humaine$Bomsp+identif_voix_humaine$insect

identif_voix_humaine <- identif_voix_humaine %>% 
  group_by(Group.1) %>%
  filter(score_pol == max(score_pol)) 


for(i in 1:nrow(identif_voix_humaine)){
  if (identif_voix_humaine$score_pol[i]>0.9){
    
    identif_voix_humaine$pol_class_new_equilibre[i] <- c("6")
    
  } else if (identif_voix_humaine$score_pol[i]>0.8){
    
    identif_voix_humaine$pol_class_new_equilibre[i] <- c("5")
  } else if (identif_voix_humaine$score_pol[i]>0.7){
    
    identif_voix_humaine$pol_class_new_equilibre[i] <- c("4")
  } else if (identif_voix_humaine$score_pol[i]>0.6){
    
    identif_voix_humaine$pol_class_new_equilibre[i] <- c("3")
  } else if (identif_voix_humaine$score_pol[i]>0.5){
    
    identif_voix_humaine$pol_class_new_equilibre[i] <- c("2")
  } else if (identif_voix_humaine$score_pol[i]>=0){
    
    identif_voix_humaine$pol_class_new_equilibre[i] <- c("1")
  } 
}

identif_voix_humaine$pol_class_new_equilibre <- as.factor(as.character(identif_voix_humaine$pol_class_new_equilibre))
table(identif_voix_humaine$pol_class_new_equilibre)

 write.csv(identif_voix_humaine, "Resultat_test_classif_voix_humaine_equilibre.csv")
 