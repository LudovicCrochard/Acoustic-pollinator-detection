library(data.table)
library(tidyverse)
library(ROCR)


#################Test classificateur deep learning
df <- as.data.frame(list.files("H:/traitement_Tadarida_deep_avec_freq20/TadariDeep-main/python_sources/txt"))
names(df) <- c("name")
df <- filter(df, grepl(x = df$name, pattern = "prediction"))
df <- filter(df, grepl(x = df$name, pattern = ".csv"))
df1 <- as.data.frame(str_split(df$name, "_", simplify = TRUE))
df1 <- df1[,c(2,3)]
names(df1) <- c("Classif", "nbepochs")
df <- cbind(df, df1)
df$batchsize <- fct_recode(df$Classif,
<<<<<<< HEAD
                           "4"       = "1",
                           "8"       = "2",
                           "16"      = "3",
                           "32"      = "4",
                           "4"       = "5",
                           "8"       = "6",
                           "16"      = "7",
                           "32"      = "8",
                           "4"       = "9",
                           "8"       = "10",
                           "16"      = "11",)
df$database <- fct_recode(df$Classif,
                          "sans_homsap"       = "1",
                          "sans_homsap"       = "2",
                          "sans_homsap"      = "3",
                          "sans_homsap"      = "4",
                          "homsap_1000"       = "5",
                          "homsap_1000"       = "6",
                          "homsap_1000"      = "7",
                          "homsap_1000"      = "8",
                          "homsap_3000"       = "9",
                          "homsap_3000"       = "10",
                          "homsap_3000"      = "11",)
=======
                                   "4"       = "1",
                                   "8"       = "2",
                                   "16"      = "3",
                                   "32"      = "4",
                                   "4"       = "5",
                                   "8"       = "6",
                                   "16"      = "7",
                                   "32"      = "8",
                                   "4"       = "9",
                                   "8"       = "10",
                                   "16"      = "11",)
df$database <- fct_recode(df$Classif,
                           "sans_homsap"       = "1",
                           "sans_homsap"       = "2",
                           "sans_homsap"      = "3",
                           "sans_homsap"      = "4",
                           "homsap_1000"       = "5",
                           "homsap_1000"       = "6",
                           "homsap_1000"      = "7",
                           "homsap_1000"      = "8",
                           "homsap_3000"       = "9",
                           "homsap_3000"       = "10",
                           "homsap_3000"      = "11",)
>>>>>>> df27c45efab496a3476b6f6cb9b5cc3bbc92d37b
df$roc_name <- paste("Classif", df$Classif, df$nbepochs, df$batchsize, ".jpg", sep = "_")
df$auc_avant <- NA
df$auc_apres <- NA
df3 <- data.frame()
for(i in 1:nrow(df)){
  pred_deep <- fread(paste("H:/traitement_Tadarida_deep_avec_freq20/TadariDeep-main/python_sources/txt", df$name[i], sep="/"))
  pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect
  
  # pred_deep <- pred_deep %>% 
  #   group_by(`fichier wav`)
  
  # t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
  # t <- filter(t, Freq!=0)
  # t1 <- filter(t, Var1==0)
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
  # identif <- identif[order(-score_pol),]
  identif <- unique(identif[,c(4,30:32)])
  pred <- prediction(identif$score_pol, identif$Pol_or_not)                          
  
  perf <- performance(pred, "tpr", "fpr")
  plot(perf)+
    abline(v=0.2, col="red")+
    abline(v=0.1, col="blue")+
    abline(v=0.05, col="orange")
  perf1 <- performance(pred, "auc")
  
  
  df$auc_avant[i] <- perf1@y.values[[1]]
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
  perf1 <- performance(pred, "auc")
  perf1@y.values[[1]]
  
  df$auc_apres[i] <- perf1@y.values[[1]]
  
  perf <- performance(pred, "tpr", "fpr")
  # 1. Open jpeg file
  # jpeg(df$roc_name[i], width = 600, height = 600)
  # 2. Create the plot
  plot(perf)+
    abline(v=0.065, col="green")+
    abline(v=0.2, col="red")+
    abline(v=0.1, col="blue")+
    abline(v=0.05, col="orange")+
    text(0.55,0.5,paste("AUC", df$auc_apres[i], sep=" = "))
  # 3. Close the file
  # dev.off()
<<<<<<< HEAD
  #Recuperer toutes les valeurs de tous les points de la courbe
=======
#Recuperer toutes les valeurs de tous les points de la courbe
>>>>>>> df27c45efab496a3476b6f6cb9b5cc3bbc92d37b
  cutoffs <- as.data.frame(pred@cutoffs[[1]])
  tp <- as.data.frame(pred@tp[[1]])
  fp <- as.data.frame(pred@fp[[1]])
  tn <- as.data.frame(pred@tn[[1]])
  fn <- as.data.frame(pred@fn[[1]])
  df2 <- cbind(cutoffs, tp, fp, tn, fn)
  names(df2) <- c("seuil", "tp", "fp", "tn", "fn")
  for(j in 1 : nrow(df2)){
<<<<<<< HEAD
    df2$sensibility[j] <- df2$tp[j]/(df2$tp[j]+df2$fn[j])
    df2$specificity[j] <- df2$tn[j]/(df2$tn[j]+df2$fp[j])
    df2$precision[j] <- df2$tp[j]/(df2$tp[j]+df2$fp[j])
    df2$neg_pred_val[j] <- df2$tn[j]/(df2$tn[j]+df2$fn[j])
    df2$accuracy[j] <- (df2$tp[j]+df2$tn[j])/(df2$tp[j]+df2$fn[j]+df2$tn[j]+df2$fp[j])
=======
  df2$sensibility[j] <- df2$tp[j]/(df2$tp[j]+df2$fn[j])
  df2$specificity[j] <- df2$tn[j]/(df2$tn[j]+df2$fp[j])
  df2$precision[j] <- df2$tp[j]/(df2$tp[j]+df2$fp[j])
  df2$neg_pred_val[j] <- df2$tn[j]/(df2$tn[j]+df2$fn[j])
  df2$accuracy[j] <- (df2$tp[j]+df2$tn[j])/(df2$tp[j]+df2$fn[j]+df2$tn[j]+df2$fp[j])
>>>>>>> df27c45efab496a3476b6f6cb9b5cc3bbc92d37b
  }
  classif <- rep(df$Classif[i], nrow(df2))
  nbepochs <- rep(df$nbepochs[i], nrow(df2))
  df2 <- cbind(classif, nbepochs, df2)
  df3 <- rbind(df3, df2)
}




# pred_deep <- fread("H:/traitement_Tadarida_deep_avec_freq20/TadariDeep-main/python_sources/txt/predictions_8_137_1_basetest.csv")
# pred_deep$score_pol <- pred_deep$Apismel+pred_deep$Bomsp+pred_deep$insect
# 
# pred_deep <- pred_deep %>% 
#   group_by(`fichier wav`)
# 
# t <- as.data.frame(table(pred_deep$score_pol, pred_deep$`fichier wav`))
# t <- filter(t, Freq!=0)
# t1 <- filter(t, Var1==0)
# pred_deep <- pred_deep %>% 
#   group_by(`fichier wav`) %>%
#   filter(score_pol == max(score_pol))
# 
# identif_test <- fread("H:/dossiers_issus_du_disque_7/identif_ech_test_avec_database_avant_ajout_voix_humaine.csv")
# library(dplyr)
# identif_test <- rename(identif_test,`fichier wav`=Group.1)
# identif_test$`fichier wav` <- str_replace(identif_test$`fichier wav`, ".wav", "")
# identif <- left_join(pred_deep, identif_test[, c(2,37,38)], by="fichier wav")
# identif <- rename(identif,fichier_wav=`fichier wav`)
# 
# 
# ####Jeu de donnÃ©es Ã©tiquetÃ©
# identif$Pol_or_not <- fct_recode(identif$Pol_or_not,
#                                  "1" = "yes",
#                                  "0" = "no")
# identif <- identif[order(-score_pol),]
# identif <- unique(identif[,c(4,30:32)])
# pred <- prediction(identif$score_pol, identif$Pol_or_not)                          
# 
# perf <- performance(pred, "tpr", "fpr")
# plot(perf)+
#   abline(v=0.2, col="red")+
#   abline(v=0.1, col="blue")+
#   abline(v=0.05, col="orange")
# perf1 <- performance(pred, "auc")
# perf1@y.values[[1]]
# 
# #test en supprimant evenement pol non detecte car trop courts
# identif1 <- as.data.frame(identif)
# identif2 <- filter(identif1, identif1$fichier_wav=="parcelle_5572_l3t2_20200804_090015_000-x10")
# identif3 <- filter(identif1, identif1$fichier_wav=="parcelle_17019_l3t1_20200819_141130_000-x10")
# identif4 <- filter(identif1, identif1$fichier_wav=="parcelle_5572_l3t2_20200804_090055_000-x10")
# identif2$Pol_or_not <- 0
# identif3$Pol_or_not <- 0
# identif4$Pol_or_not <- 0
# identif2 <- rbind(identif2, identif3, identif4)
# 
# identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_5572_l3t2_20200804_090015_000-x10")
# identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_17019_l3t1_20200819_141130_000-x10")
# identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_5572_l3t2_20200804_090055_000-x10")
# 
# identif1 <- rbind(identif1, identif2) 
# # identif1 <- filter(identif1, identif1$fichier_wav!="parcelle_17019_l3t1_20200729_090710_000-x10")
# pred <- prediction(identif1$score_pol, identif1$Pol_or_not)                          
# 
# perf <- performance(pred, "tpr", "fpr")
# plot(perf)
# abline(v=0.2, col="red")+
# abline(v=0.1, col="blue")+
# abline(v=0.05, col="orange")
# perf1 <- performance(pred, "auc")
# perf1@y.values[[1]]
# 
# 
#↨pour creuser
identif_t <- filter(identif1, Pol_or_not==1)
test <- identif_t[order(-identif_t$score_pol),]

identif_t1 <- filter(identif1, Pol_or_not==0)
test1 <- identif_t1[order(-identif_t1$score_pol),]
pred@predictions[[1]]


perf <- performance(pred, "err")

# 
# 
# t <- separate(data = identif_t, col=`fichier wav`,into = c("Parcelle", "Field", "Pied", "Date", "Heure", "ext"))
# table(t$Field)
# t <- filter(t, Field=="2187")
# t1 <- filter(identif_test, Pol_or_not=="yes")
# t2 <- separate(data = t1, col=`fichier wav`,into = c("Parcelle", "Field", "Pied", "Date", "Heure", "ext"))
# table(t2$Field)
# t2 <- filter(t2, Field=="2187")
# 




#Graph
df$nbepochs <- fct_relevel(df$nbepochs, c("25", "37", "50","62","75","87", "100","112", "125", "137","150","162","175", "187","200" ))

df$Classif <- as.character(df$Classif)
ggplot(df, aes(x = nbepochs, y = auc_apres, group=Classif, color=database))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")

df$database <- as.factor(df$database)
df1 <- filter(df, database=="sans_homsap")
ggplot(df1, aes(x = nbepochs, y = auc_apres, group=Classif, color=Classif))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")

df2 <- filter(df, database=="homsap_1000")
ggplot(df2, aes(x = nbepochs, y = auc_apres,group=Classif, color=Classif))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")
df3 <- filter(df, database=="homsap_3000")
ggplot(df3, aes(x = nbepochs, y = auc_apres,group=Classif, color=Classif))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")

df$batchsize <- as.character(df$batchsize)
df4 <- filter(df, batchsize=="4")
ggplot(df4, aes(x = nbepochs, y = auc_apres, group=Classif, color=Classif))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")
df5 <- filter(df, batchsize=="8")
ggplot(df5, aes(x = nbepochs, y = auc_apres, group=Classif, color=Classif))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")
df6 <- filter(df, batchsize=="16")
ggplot(df6, aes(x = nbepochs, y = auc_apres, group=Classif, color=Classif))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")

df7 <- filter(df, batchsize=="32")
ggplot(df7, aes(x = nbepochs, y = auc_apres, group=Classif, color=Classif))+
  geom_point(size=2)+
  geom_line(size=1)+ ylim(0.85,0.96)+xlab("Nombre d'itération") + ylab("AUC")+
  theme_classic() + scale_color_brewer(palette="Paired")
