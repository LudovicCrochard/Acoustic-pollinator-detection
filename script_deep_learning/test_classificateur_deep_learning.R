library(data.table)
library(tidyverse)
library(ROCR)


#################Test classificateur deep learning 
pred_deep <- fread("H:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_2_75_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("H:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
library(dplyr)
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                           "1" = "yes",
                           "0" = "no")
identif <- identif[order(-score_pol),]
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]

<<<<<<< HEAD


#Test classificateur deep learning 2
pred_deep <- fread("D:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_1_25_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("D:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                                 "1" = "yes",
                                 "0" = "no")
identif <- identif[order(-score_pol),]  
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]






#Test classificateur deep learning 3
pred_deep <- fread("D:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_1_50_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("D:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                                 "1" = "yes",
                                 "0" = "no")
identif <- identif[order(-score_pol),] 
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]






#Test classificateur deep learning 4
pred_deep <- fread("D:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_1_75_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("D:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                                 "1" = "yes",
                                 "0" = "no")
identif <- identif[order(-score_pol),] 
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]



#Test classificateur deep learning 4
pred_deep <- fread("D:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_1_100_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("D:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                                 "1" = "yes",
                                 "0" = "no")
identif <- identif[order(-score_pol),] 
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]







#################Test classificateur deep learning 2 batch size=8
pred_deep <- fread("D:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_2_10_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("D:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
library(dplyr)
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                                 "1" = "yes",
                                 "0" = "no")
identif <- identif[order(-score_pol),]
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]



#Test classificateur deep learning 2
pred_deep <- fread("D:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_2_25_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("D:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                                 "1" = "yes",
                                 "0" = "no")
identif <- identif[order(-score_pol),]  
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]






#Test classificateur deep learning 3
pred_deep <- fread("D:/Deep_learning_manip_tournesol_2020/TadariDeep-main/python_sources/txt/predictions_2_50_1_basetest.csv")
pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect

pred_deep <- pred_deep %>% 
  group_by(`fichier wav`)

t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
t <- filter(t, Freq!=0)
t1 <- filter(t, Var1==0)
pred_deep <- pred_deep %>% 
  group_by(`fichier wav`) %>%
  filter(score_pol == max(score_pol))

identif_test <- fread("D:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
identif_test <- rename(identif_test,`fichier wav`=Group.1)
identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")



####Jeu de donnÃ©es Ã©tiquetÃ©
identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
                                 "1" = "yes",
                                 "0" = "no")
identif <- identif[order(-score_pol),] 
identif <- unique(identif[,c(4,30:32)])
pred <- prediction(identif$score_pol, identif$Pol_or_not)                          
=======
#test en supprimant evenement pol non detecte
identif1 <- identif
for(i in 1:nrow(identif1)){
  if(identif1$score_pol[i]<=10){identif1$Pol_or_not[i]=0}
}
pred <- prediction(identif1$score_pol, identif1$Pol_or_not)                          
>>>>>>> 9d9cc9a91fc75c0fc91e40d0ac1396d43c08a08a

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]


#↨pour creuser
identif_t <- filter(identif, Pol_or_not==1)
test <- identif_t[order(identif_t$score_pol),]
t <- separate(data = identif_t, col=`fichier wav`,into = c("Parcelle", "Field", "Pied", "Date", "Heure", "ext"))
table(t$Field)
t <- filter(t, Field=="2187")
t1 <- filter(identif_test, Pol_or_not=="yes")
t2 <- separate(data = t1, col=`fichier wav`,into = c("Parcelle", "Field", "Pied", "Date", "Heure", "ext"))
table(t2$Field)
t2 <- filter(t2, Field=="2187")
