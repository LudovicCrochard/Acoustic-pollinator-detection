library(data.table)
library(tidyverse)
library(ROCR)


#################Test classificateur deep learning
pred_deep <- fread("H:/traitement_Tadarida_deep_avec_freq20/TadariDeep-main/python_sources/txt/predictions_1_200_1_basetest.csv")
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
identif <- rename(identif,fichier_wav=`fichier wav`)


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

#test en supprimant evenement pol non detecte car trop courts
identif1 <- as.data.frame(identif)
identif2 <- filter(identif1, identif1$fichier_wav=="parcelle_5572_l3t2_20200804_090015_000-x10")
identif3 <- filter(identif1, identif1$fichier_wav=="parcelle_17019_l3t1_20200819_141130_000-x10")
identif4 <- filter(identif1, identif1$fichier_wav=="parcelle_5572_l3t2_20200804_090055_000-x10")
identif2$Pol_or_not <- 0
identif3$Pol_or_not <- 0
identif4$Pol_or_not <- 0
identif2 <- rbind(identif2, identif3, identif4)

identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_5572_l3t2_20200804_090015_000-x10")
identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_17019_l3t1_20200819_141130_000-x10")
identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_5572_l3t2_20200804_090055_000-x10")

identif1 <- rbind(identif1, identif2) 
  # identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_17019_l3t1_20200729_090710_000-x10")
pred <- prediction(identif1$score_pol, identif1$Pol_or_not)                          

perf <- performance(pred, "tpr", "fpr")
plot(perf)
  # abline(v=0.2, col="red")+
  # abline(v=0.1, col="blue")+
  # abline(v=0.05, col="orange")+
  # abline(h = 0.935, col="red")+
  # abline(h=0.855, col="blue")+
  # abline(h=0.74, col="orange")
perf1 <- performance(pred, "auc")
perf1@y.values[[1]]


#↨pour creuser
identif_t <- filter(identif1, Pol_or_not==1)
test <- identif_t[order(-identif_t$score_pol),]

identif_t1 <- filter(identif1, Pol_or_not==0)
test1 <- identif_t1[order(-identif_t1$score_pol),]


t <- separate(data = identif_t, col=`fichier wav`,into = c("Parcelle", "Field", "Pied", "Date", "Heure", "ext"))
table(t$Field)
t <- filter(t, Field=="2187")
t1 <- filter(identif_test, Pol_or_not=="yes")
t2 <- separate(data = t1, col=`fichier wav`,into = c("Parcelle", "Field", "Pied", "Date", "Heure", "ext"))
table(t2$Field)
t2 <- filter(t2, Field=="2187")





#Graph
df <- fread("C:/Users/Ludovic Crochard/OneDrive/Documents/Acoustic-pollinator-detection/point_sur_les_AUC_classif_deep_learning.csv")
library(ggplot2)
names(df) <- c("Classif", "nbepochs", "batchsize", "moteur", "base_ref", "AUC_avant", "AUC_apres")
df$Classif <- as.character(df$Classif)
ggplot(df, aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()

df1 <- split(df,by = "base_ref")
ggplot(df1$`sans homsap`, aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()

ggplot(df1$`avec homsap`, aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()
ggplot(df1$avec_homsap_3000, aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()

df$batchsize <- as.character(df$batchsize)
df2 <- split(df,by = "batchsize")
ggplot(df2$"4", aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()

ggplot(df2$"8", aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()
ggplot(df2$"16", aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()
ggplot(df2$"32", aes(x = nbepochs, y = AUC_apres, color=Classif))+
  geom_point()+
  geom_line()
