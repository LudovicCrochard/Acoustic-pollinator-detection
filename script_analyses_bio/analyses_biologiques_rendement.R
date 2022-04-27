library(data.table)
library(tidyverse)
library(ggplot2)
library(lme4)
df <- fread("~/These/acousticpollinatordetection/data/Ludo_rdt_130122.csv")

#### Test pour voir si la pollinisation a un impact sur le rendement
#On sélectionne les pieds non ensachés et ensachés PM
df_pol <- df %>% 
  filter(ID_Zone=="SACHET") %>% 
  filter(Traitement=="NF1" | Traitement=="NF2" | Traitement=="PM")
df_pol$Traitement[df_pol$Traitement %in% c("NF1","NF2")]<-"NF"
names(df_pol) <- c("Parcelle", "Traitement", "ID_Zone", "Ligne", "Poid_graines_avant_nettoyage", "Poids_graines", "P_graine1",
                     "P_graine2", "P_graine3", "Nb_graines", "PMG", "Hauteur", "Diametre")
df_pol$Parcelle <- as.factor(df_pol$Parcelle)
df_pol$Ligne <- as.factor(df_pol$Ligne)
df_pol$Traitement <- as.factor(df_pol$Traitement)
df_pol$PMG <- as.integer(df_pol$PMG)
df_pol$Poids_graines <- as.integer(df_pol$Poids_graines)


df_pol <- filter(df_pol, Diametre!="NA")
df_pol <- filter(df_pol, Nb_graines!="NA")
cor.test(df_pol$Nb_graines, df_pol$Diametre)

ggplot(df_pol, aes(x = Diametre, y = Nb_graines))+
  geom_point(stat = "identity") + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  xlim(c(0,320))+
  theme_classic()+ scale_color_brewer(palette="Accent")

hist(df_pol$Nb_graines)
hist(df_pol$Poids_graines)
hist(df_pol$PMG)

#Y a-t-il un effet de la pollinisation sur le rendement?
#Rendement~Diametre_pied+Traitement(NF(non ensache vs PM(ensache)))+ (1|Parcelle)
#Nb_graines
library(lmerTest)
m1 <- lmer(Nb_graines ~ Diametre + Traitement + (1|Parcelle), data = df_pol)
summary(m1)
m2 <- glmer(Nb_graines ~ scale(Diametre) + Traitement + (1|Parcelle), data = df_pol, family = "poisson")
summary(m2)
anova(m1, m2) #On garde le modele m1 car l'AIC est plus faible



t <- filter(df_pol, Traitement=="NF")
m1 <- lmer(Nb_graines ~ Diametre + Ligne + (1|Parcelle), data = t)
summary(m1)
m2 <- glmer(Nb_graines ~ Diametre + Ligne + (1|Parcelle), data = t, family = "poisson")
summary(m2)


numcols <- grep("^c\\.",names(df_pol))
dfs <- df_pol
dfs[,numcols] <- scale(dfs[,numcols])
m1_sc <- update(m1,data=dfs)


df_focale <- df %>% 
  filter(ID_Zone=="SACHET" & Traitement=="NF1" | Traitement=="NF2") %>% 
  filter(Individu==3)

info_pied_focal <- fread("~/These/acousticpollinatordetection/data/fiche_suivi.csv")
pied_foc_l3 <- info_pied_focal %>% 
  filter(Ligne=="l3")
pied_foc_l3$Traitement <- NA
for(i in 1:nrow(pied_foc_l3)){
  if(pied_foc_l3$Pied[i]=="t1"){pied_foc_l3$Traitement[i] <-"NF1"
  }else{pied_foc_l3$Traitement[i] <-"NF2"}
}

df_focale <- left_join(df_focale, pied_foc_l3[,c(1,2,11)], by=c("Parcelle", "Traitement"))

pied_foc <- df_focale %>% 
  filter(Audiomoth!="NA") #Pb avec parcelle 8690, Thomas Perrot contacté le 24/03/2022
pied_non_foc <- df %>% 
  filter(ID_Zone=="SACHET" & Traitement=="NF1" | Traitement=="NF2")%>% 
  filter(Individu!=3) %>% 
  rbind(filter(df_focale, is.na(Audiomoth))[,-c(14)])
#Test effet distance à la bordure
df_ligne <- rbind(pied_non_foc, pied_foc[,-c(14)])
names(df_ligne) <- c("Parcelle", "Traitement", "ID_Zone", "Ligne", "Poid_graines_avant_nettoyage", "Poids_graines", "P_graine1",
                     "P_graine2", "P_graine3", "Nb_graines", "PMG", "Hauteur", "Diametre")
df_ligne$Parcelle <- as.factor(df_ligne$Parcelle)
df_ligne$Ligne <- as.factor(df_ligne$Ligne)
df_ligne$PMG <- as.integer(df_ligne$PMG)
df_ligne$Poids_graines <- as.integer(df_ligne$Poids_graines)

hist(as.integer(df_ligne$Poids_graines))
model <- lmer(Nb_graines ~ Diametre + Ligne + (1|Parcelle), data = df_ligne)
summary(model)
model1 <- glmer(Nb_graines ~ Diametre + Ligne  + (1|Parcelle), data = df_ligne, family = "poisson")
summary(model1)


ggplot(data = df_ligne, aes(x = Diametre, y=Poids_graines))+
  geom_point()+
  theme_classic()

ggplot(data = df_ligne, aes(x = as.factor(Ligne), y=Nb_graines))+
  geom_boxplot()+
  theme_classic()
#Donées de comptage donc loi de poisson normalement pour nb_graines



library(nlme)
model <- lme(T.graines~Individu+Diametre+Parcelle, data=df_ligne)

names(pied_foc) <- c("Parcelle", "Traitement_foc", "ID_Zone", "Ligne", "Poid_graines_avant_nettoyage_foc", "Poids_graines_foc", "P_graine1_foc",
                    "P_graine2_foc", "P_graine3_foc", "Nb_graines_foc", "PMG_foc", "Hauteur_foc", "Diametre_foc", "Audiomoth")

names(pied_non_foc) <- c("Parcelle", "Traitement", "ID_Zone", "Ligne", "Poid_graines_avant_nettoyage", "Poids_graines", "P_graine1",
                     "P_graine2", "P_graine3", "Nb_graines", "PMG", "Hauteur", "Diametre")

df_rendement <- left_join(pied_non_foc, pied_foc[,-c(3,4,14)], by="Parcelle")
df_rendement$Ligne <- as.character(df_rendement$Ligne)

ggplot(data = df_rendement, aes(x = Nb_graines_foc, y = Nb_graines, color=Ligne))+
  geom_point()

model <- lm(Nb_graines~Nb_graines_foc+Diametre+Diametre_foc, data=df_rendement)
summary(model)
par(mfrow=c(2,2))
plot(model)



pied_non_foc1 <- aggregate(T.graines~Parcelle, data = pied_non_foc, FUN="mean")
graines_foc_vs_non_foc <- left_join(pied_non_foc1, pied_foc[,c(1,10)], by="Parcelle")
names(graines_foc_vs_non_foc) <- c("Parcelle", "mean_seed_non_focal", "nb_seed_focal")
graines_foc_vs_non_foc$Parcelle <- as.character(graines_foc_vs_non_foc$Parcelle)
ggplot(graines_foc_vs_non_foc, aes(x = nb_seed_focal, y=mean_seed_non_focal))+
  geom_point()+
  geom_smooth(method=lm)
cor.test(x = graines_foc_vs_non_foc$mean_seed_non_focal,y = graines_foc_vs_non_foc$nb_seed_focal)



df_focale <- df %>% 
  filter(ID_Zone=="SACHET" & Traitement=="NF1" | Traitement=="NF2")
ggplot(df_focale, aes(x = Individu, y = T.graines))+
  geom_point()+
  stat_summary(fun.y=mean, geom="point",col="blue", size=4)
