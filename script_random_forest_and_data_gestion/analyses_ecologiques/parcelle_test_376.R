library(data.table)
library(tidyverse)
library(stringr)
library(ggplot2)


df376 <- fread("G:/travail_manip_tournesol/parcelle_test_376/parcelle_test_376_IdTot.csv")
df376$score_pol <- df376$Apismel+df376$Bomsp+df376$insect

df376 <- df376 %>% 
  group_by(Group.1) %>%
  filter(score_pol == max(score_pol)) 


for(i in 1:nrow(df376)){
  if (df376$score_pol[i]>0.9){
    
    df376$pol_class[i] <- c("6")
    
  } else if (df376$score_pol[i]>0.8){
    
    df376$pol_class[i] <- c("5")
  } else if (df376$score_pol[i]>0.7){
    
    df376$pol_class[i] <- c("4")
  } else if (df376$score_pol[i]>0.6){
    
    df376$pol_class[i] <- c("3")
  } else if (df376$score_pol[i]>0.5){
    
    df376$pol_class[i] <- c("2")
  } else if (df376$score_pol[i]>=0){
    
    df376$pol_class[i] <- c("1")
  } 
}

df376_pol <- filter(df376, pol_class>=5)

df376_pol <- separate(data = df376_pol, col = Group.1, sep = "_", into = c("Field", "Parcelle", "Pied", "Date", "Heure", "ext"))

df376_pol$Date <- as.Date(df376_pol$Date, format = "%Y%m%d")
df376_pol$Nb_buzz <- 1
df376_pol <- df376_pol[,c(2,3,4,5,38,39)]
df376_pol$jour <- as.numeric(format(df376_pol$Date, "%j"))
df376_pol$semaine <- isoweek(df376_pol$Date)

df376_pol$Heure <- as.numeric(df376_pol$Heure)
df376_pol$num_Heure <- case_when(
  df376_pol$Heure<50000 & df376_pol$Heure>=40000  ~ "6",
  df376_pol$Heure<60000 & df376_pol$Heure>=50000  ~ "7",
  df376_pol$Heure<70000 & df376_pol$Heure>=60000  ~ "8",
  df376_pol$Heure<80000 & df376_pol$Heure>=70000  ~ "9",
  df376_pol$Heure<90000 & df376_pol$Heure>=80000  ~ "10",
  df376_pol$Heure<100000 & df376_pol$Heure>=90000  ~ "11",
  df376_pol$Heure<110000 & df376_pol$Heure>=100000  ~ "12",
  df376_pol$Heure<120000 & df376_pol$Heure>=110000  ~ "13",
  df376_pol$Heure<130000 & df376_pol$Heure>=120000  ~ "14",
  df376_pol$Heure<140000 & df376_pol$Heure>=130000  ~ "15",
  df376_pol$Heure<150000 & df376_pol$Heure>=140000  ~ "16",
  df376_pol$Heure<160000 & df376_pol$Heure>=150000  ~ "17",
  df376_pol$Heure<170000 & df376_pol$Heure>=160000  ~ "18",
  df376_pol$Heure<180000 & df376_pol$Heure>=170000  ~ "19",
  df376_pol$Heure<190000 & df376_pol$Heure>=180000  ~ "20",
  df376_pol$Heure<200000 & df376_pol$Heure>=190000  ~ "21"
)
df376_pol$num_Heure <- as.factor(df376_pol$num_Heure)


#data pied
df_pied <- fread("G:/travail_manip_tournesol/data_tournesol_2020/data_focales.csv")
df_pied$Pied <- paste(df_pied$Ligne, df_pied$Pied, sep="")
df_pied_376 <- filter(df_pied, Parcelle==376 & Pied=="l3t1")
df_pied_376 <- unique(df_pied_376[,c(1:11)])
df_pied_376 <- filter(df_pied_376, Stade_pied!="NA")
df_pied_376$Date <- as.Date(df_pied_376$Date, format="%d/%m/%Y")
df_pied_376$semaine <- isoweek(df_pied_376$Date)
df_pied_376$Parcelle <- as.character(df_pied_376$Parcelle)

df376_pol <- left_join(df376_pol, df_pied_376[,c(1,5:10,12)], by=c("Parcelle", "Pied", "semaine"))



#data parcelle
df_parcelle <- fread("G:/travail_manip_tournesol/data_tournesol_2020/data_penologie_new.csv")
df_parcelle$Date <- as.Date(df_parcelle$Date, format="%Y-%m-%d")
df_parcelle$semaine <- isoweek(df_parcelle$Date)
df_parcelle$Parcelle <- as.character(df_parcelle$Parcelle)
df_parcelle_376 <- filter(df_parcelle, Parcelle=="376")
df376_pol <- left_join(df376_pol, df_parcelle_376[,c(1,4:6,9)], by=c("Parcelle", "semaine"))

#data rendement
df_rendement <- fread("G:/travail_manip_tournesol/data_tournesol_2020/rendement_pied.csv")
df_rendement$Parcelle <- as.character(df_rendement$Parcelle)

df_rendement_sachet <- filter(df_rendement, ID_Zone=="SACHET")
df_rendement_sachet_temoin <- filter(df_rendement_sachet, Traitement=="NF1" | Traitement=="NF2")

for(i in 1:nrow(df_rendement_sachet_temoin)){
  if(df_rendement_sachet_temoin$Traitement[i]=="NF1"){
    df_rendement_sachet_temoin$Pied[i] <- paste("l", df_rendement_sachet_temoin$Individu[i], "t1", sep="")
  }else{
    df_rendement_sachet_temoin$Pied[i] <- paste("l", df_rendement_sachet_temoin$Individu[i], "t2", sep="")
  }
}

df376_pol <- left_join(df376_pol, filter(df_rendement_sachet_temoin, Parcelle=="376" & Pied=="l3t1"), by=c("Parcelle", "Pied"))




#Nombre de buzz par jour en fonction du stade de floraison du pied
df_floraison_pied <- aggregate(Nb_buzz~Stade_pied+jour, data=df376_pol, FUN="sum")
t <- aggregate(Nb_buzz~Stade_pied,data = df_floraison_pied, FUN="mean")
ggplot(data=df_floraison_pied, aes(x = Stade_pied, y=Nb_buzz))+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun.y=mean, geom="point", size=3, col="blue")+
  geom_point(shape=16, position=position_jitter(0.2))+xlab("Stade de floraison du pied")+ylab("Nombre de buzz")+ggtitle("Nombre de buzz par jour en fonction du stade de floraison du pied")


test <- df376_pol
test$Surface_capituale <- pi*(test$Diametre_capitule/2)^2
test$diametre_non_recouvert_par_fleurons <- test$Diametre_capitule-(test$Largeur_fleur_femelle*2+test$`Largeur-fleur_male`*2)
test$surface_non_recouverte_par_fleurons <- pi*(test$diametre_non_recouvert_par_fleurons/2)^2
test$Pourcentage_couverture_fleurons <- ((test$Surface_capituale-test$surface_non_recouverte_par_fleurons)/test$Surface_capituale)*100


#Nb_buzz par jour de la saison
nb_per_day <- aggregate(Nb_buzz~jour, data=df376_pol, FUN="sum")
ggplot(data = nb_per_day, aes(x=jour, y=Nb_buzz))+
  geom_bar(stat="identity")+
  geom_vline(xintercept = 208.5)+
  geom_vline(xintercept=222.5)+xlab("Date (j)")+ylab("Nombre de buzz")+ggtitle("Nombre de buzz par jour au cours de la saison")

#Nombre buzz en fonction du pourcentage de floraison de la parcelle
df376_pol$Pourcentage_floraison <- as.character(df376_pol$Pourcentage_floraison)
df_floraison_parcelle <- aggregate(Nb_buzz~Pourcentage_floraison+jour, data=df376_pol, FUN="sum")
df_floraison_parcelle$Pourcentage_floraison <- as.factor(df_floraison_parcelle$Pourcentage_floraison)
df_floraison_parcelle$Pourcentage_floraison <- fct_relevel(
  df_floraison_parcelle$Pourcentage_floraison,
  "1", "5", "20",
  "100")

ggplot(data=df_floraison_parcelle, aes(x = Pourcentage_floraison, y=Nb_buzz))+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun.y=mean, geom="point", size=3, col="blue")+
  geom_point(shape=16, position=position_jitter(0.2))+
  xlab("Pourcentage de floraison de la parcelle")+ylab("Nombre de buzz")+ggtitle("Nombre de buzz par jour en fonction du pourcentage de floraison de la parcelle")

#Nombre de buzz moyen par heure sur la saison
##Connaitre nombre de fois oùil y a eu des enreigstrements pendant la journée
file3 <- fread("G:/travail_manip_tournesol/files_listing_part3.csv")
file4 <- fread("G:/travail_manip_tournesol/files_listing_part4.csv")

file <- rbind(file3, file4)
file_376 <- filter(file, parcelle=="376")
file_376$date <- as.character(file_376$date)
file_376$date <- as.Date(file_376$date, format = "%Y%m%d")

file_376$jour <- as.numeric(format(file_376$date, "%j"))
file_376$semaine <- isoweek(file_376$date)

file_376$heure <- as.numeric(file_376$heure)
file_376$num_heure <- case_when(
  file_376$heure<50000 & file_376$heure>=40000  ~ "6",
  file_376$heure<60000 & file_376$heure>=50000  ~ "7",
  file_376$heure<70000 & file_376$heure>=60000  ~ "8",
  file_376$heure<80000 & file_376$heure>=70000  ~ "9",
  file_376$heure<90000 & file_376$heure>=80000  ~ "10",
  file_376$heure<100000 & file_376$heure>=90000  ~ "11",
  file_376$heure<110000 & file_376$heure>=100000  ~ "12",
  file_376$heure<120000 & file_376$heure>=110000  ~ "13",
  file_376$heure<130000 & file_376$heure>=120000  ~ "14",
  file_376$heure<140000 & file_376$heure>=130000  ~ "15",
  file_376$heure<150000 & file_376$heure>=140000  ~ "16",
  file_376$heure<160000 & file_376$heure>=150000  ~ "17",
  file_376$heure<170000 & file_376$heure>=160000  ~ "18",
  file_376$heure<180000 & file_376$heure>=170000  ~ "19",
  file_376$heure<190000 & file_376$heure>=180000  ~ "20",
  file_376$heure<200000 & file_376$heure>=190000  ~ "21"
)
file_376$num_heure <- as.factor(file_376$num_heure)

file_376_nb_heure <- unique(file_376[,c(2,3,10,12)])
nb_heure_par_nb_jour <- as.data.frame(table(file_376_nb_heure$num_heure))
names(nb_heure_par_nb_jour) <- c("num_Heure", "Frequence")

test <- aggregate(Nb_buzz~num_Heure, data=df376_pol, FUN="sum")
test <- left_join(test, nb_heure_par_nb_jour, by="num_Heure")
test$moyenne <- test$Nb_buzz/test$Frequence
names(test) <- c("num_Heure", "nbtot_buzz", "Frequence_heure", "Nb_buzz")
test1 <- aggregate(Nb_buzz~num_Heure+jour, data=df376_pol, FUN="sum")
test1$num_Heure <- fct_relevel(
  test1$num_Heure,
  "6", "7","8","9","10","11","12","13","14","15","16","17","18","19", "20","21")


ggplot(data = test1, aes(x=num_Heure, y=Nb_buzz))+
  geom_boxplot(outlier.shape = NA)+
  geom_point()+
  geom_point(data=test,aes(x = num_Heure, y=Nb_buzz), col="red")+
  xlab("Heure")+ylab("Nombre de buzz")+ggtitle("Nombre de buzz moyen par heure au cours de la saison")

  
