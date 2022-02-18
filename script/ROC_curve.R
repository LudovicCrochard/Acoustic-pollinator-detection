library(ROCR)
####Jeu de données étiqueté
t <- fread("D:/travail_manip_tournesol/etiqueted_files.csv")
t$pol_or_not <- fct_recode(t$pol_or_not,
                           "1" = "yes",
                           "0" = "no")
t <- t[order(-pol_class),]                          
pred <- prediction(t$pol_class, t$pol_or_not)                          
                           
perf <- performance(pred, "tpr", "fpr")
plot(perf)
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]


t1 <- fread("D:/travail_manip_tournesol/etiqueted_files.csv")
t1$pol_or_not <- fct_recode(t1$pol_or_not,
                           "1" = "yes",
                           "0" = "no")
t1 <- t1[order(-score_pol),]                          
pred <- prediction(t1$score_pol, t1$pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]


###Test
library(ROCR)
library(data.table)
library(tidyverse)
library(forcats)
df <- fread("D:/travail_manip_tournesol/base_de_test/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
df$Pol_or_not <- as.factor(df$Pol_or_not)
df$Pol_or_not <- fct_recode(df$Pol_or_not,
                            "1" = "yes",
                            "0" = "no")
df <- df[order(-score_pol),]                          
pred <- prediction(df$score_pol, df$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]

                         
pred1 <- prediction(df$pol_class, df$Pol_or_not)                          

perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)
perf2 <- performance(pred1, "auc")
perf2@y.values[[1]]


#Test avec ajout voix humaine
df <- fread("G:/travail_manip_tournesol/travail_sur_voix_humaine/test_homogeisation_class_pol_homsap/Resultat_test_classif_voix_humaine_equilibre.csv")
df$Pol_or_not <- as.factor(df$Pol_or_not)
df$Pol_or_not <- fct_recode(df$Pol_or_not,
                            "1" = "yes",
                            "0" = "no")
df <- df[order(-score_pol),]                          
pred <- prediction(df$score_pol, df$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)+
  abline(v=0.2, col="red")+
  abline(v=0.1, col="blue")+
  abline(v=0.05, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]

