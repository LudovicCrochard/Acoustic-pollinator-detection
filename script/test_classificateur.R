library(data.table)
library(tidyverse)

focale_classif <- fread("D:/travail_manip_tournesol/data_base/Classif_apres_gros_etiquetage/focale_for_test_IdTot.csv")
focale_classif$score_pol <- focale_classif$Apismel+focale_classif$Bomsp+focale_classif$insect

for(i in 1:nrow(focale_classif)){
  if (focale_classif$score_pol[i]>0.9){
    
    focale_classif$pol_class[i] <- c("6")
    
  } else if (focale_classif$score_pol[i]>0.8){
    
    focale_classif$pol_class[i] <- c("5")
  } else if (focale_classif$score_pol[i]>0.7){
    
    focale_classif$pol_class[i] <- c("4")
  } else if (focale_classif$score_pol[i]>0.6){
    
    focale_classif$pol_class[i] <- c("3")
  } else if (focale_classif$score_pol[i]>0.5){
    
    focale_classif$pol_class[i] <- c("2")
  } else if (focale_classif$score_pol[i]>=0){
    
    focale_classif$pol_class[i] <- c("1")
  } 
}

focale_classif$pol_class <- as.factor(as.character(focale_classif$pol_class))
focale_classif %>% count(pol_class)

focale_classif <- focale_classif %>% 
  group_by(Group.1) %>%
  filter(score_pol == max(score_pol)) 
table(focale_classif$pol_class)
t <- aggregate(score_pol~Group.1, data = focale_classif, FUN="max")

#Vérif que non utilisé dans étiquetage d'origine
filelist1 <- fread("D:/travail_manip_tournesol/files_listing_part1.csv")
filelist2 <- fread("D:/travail_manip_tournesol/files_listing_part2.csv")
filelist3 <- fread("D:/travail_manip_tournesol/files_listing_part3.csv")
filelist4 <- fread("D:/travail_manip_tournesol/files_listing_part4.csv")
filelist5 <- fread("D:/travail_manip_tournesol/files_listing_part5.csv")
filelist <- rbind(filelist1, filelist2, filelist3, filelist4, filelist5)
filelist$Group.1 <- paste(filelist$field, filelist$parcelle, filelist$pied, filelist$date, filelist$heure, filelist$ext)
filelist_original <- filelist[,c(9,10)]
filelist_original <- filter(filelist_original, original_data_base=="yes")

focale_classif <- left_join(focale_classif, filelist_original, by="Group.1")
focale_classif1 <- filter(focale_classif, is.na(original_data_base))
###Ancien test
test1 <- fread("D:/travail_manip_tournesol/anciens_tests/test_classif1_sum.csv")
test2 <- fread("D:/travail_manip_tournesol/anciens_tests/Test2_classif.csv")
test3 <- fread("D:/travail_manip_tournesol/anciens_tests/Test3_classif.csv")
test4 <- fread("D:/travail_manip_tournesol/anciens_tests/Test4_classif.csv")
test6 <- fread("D:/travail_manip_tournesol/anciens_tests/Test6_classif.csv")
test7 <- fread("D:/travail_manip_tournesol/anciens_tests/Test7_classif.csv")
test8 <- fread("D:/travail_manip_tournesol/anciens_tests/Test7_classif1.csv")

test1 <- test1[,c(1,12,14)]
test2 <- test2[,c(1,20,22)]
test3 <- test3[,c(1,19,21)]
test4 <- test4[,c(1,19,21)]
test6 <- test6[,c(1,20,22)]
test7 <- test7[,c(1,20,22)]
test8 <- test8[,c(1,20,22)]
ancien_test <- rbind( test2, test3, test4)
names(ancien_test) <- c("Group.1", "Pol_or_not", "Identif_perso")
ancien_test <- rbind(ancien_test, test1, test6, test7, test8)

ancien_test1 <- unique(ancien_test[,c(1,2)])
#####Jeux de données test
test <- as.data.frame(left_join(focale_classif, ancien_test1, by="Group.1"))
test$Group.1 <- as.factor(test$Group.1)
new_test <- filter(test, Pol_or_not!="NA")
table(new_test$pol_class)

t <- as.data.frame(levels(new_test$pol_class))
names(t) <- c("pol_class")
t$nb_a_tirer <- c(100,64,69,56,35,48)
new_test <- as.data.frame(new_test)
set.seed(363)
t2= data.frame()
t3=data.frame()
for(i in 1:nrow(t)){
  t2 <-sample_n(subset(new_test,
                         new_test$pol_class==t$pol_class[i]),t$nb_a_tirer[i])
  t3 <- rbind(t3,t2)
}

table(t3$pol_class, t3$Pol_or_not)
table(new_test$pol_class, new_test$Pol_or_not)

#ajout pour obtenir 100 enregistrements test par classe de score
new_test1 <- subset(test, is.na(Pol_or_not))

t1 <- as.data.frame(levels(new_test$pol_class))
names(t) <- c("pol_class")
t1$nb_a_tirer <- c(0,36,31,44,65,52)
new_test1 <- as.data.frame(new_test1)
set.seed(363)
t2= data.frame()
t4=data.frame()
for(i in 1:nrow(t1)){
  t2 <-sample_n(subset(new_test1,
                       new_test1$pol_class==t$pol_class[i]),t1$nb_a_tirer[i])
  t4 <- rbind(t4,t2)
}

t5 <- rbind(t3, t4)
t5 <- arrange(t5, Group.1)
# write.csv(t5, "data_for_test.csv")


filetest <- fread("D:/travail_manip_tournesol/data_for_test1.csv")
table(filetest$Pol_or_not, filetest$pol_class)
filetest_ancien <- filter(filetest, Identif_perso=="")
filetest_ancien <- left_join(filetest_ancien, ancien_test, by="Group.1")
filetest_ancien <- unique(filetest_ancien)
# write.csv(filetest_ancien, "filetest_ancien.csv")

#obtention des identifications de tout le test
filetest_ancien <- fread("D:/travail_manip_tournesol/test/filetest_ancien.csv")
filetest_new <- filter(filetest, Identif_perso!="")
filetest <- rbind(filetest_ancien, filetest_new[,-38])
filetest <- arrange(filetest, pol_class)

#connaitre nombre d'évènements sonores apres l'étiquetage et avant
test550 <- fread("D:/travail_manip_tournesol/data_base/Classif_apres_gros_etiquetage/RSDB_tabase3HF_sansfiltre.csv")
table(test550$Nesp)
test551 <- fread("D:/travail_manip_tournesol/data_base/Classif_for_etiquetage/RSDB_tabase3HF_sansfiltre.csv")
table(test551$Nesp)
