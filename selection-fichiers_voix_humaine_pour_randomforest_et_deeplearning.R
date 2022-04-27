t <- list.files(path = "H:/voix_humaine_pour_ameliorer_base_classif/ima2", pattern="Homsap")
t <- as.data.frame(t)
library(tidyverse)
t1 <- separate(data = t, col = t,into = c("wave", "cri", "homsap", "part1", "part2", "part3","part4"), sep = "--")
t2 <- as.data.frame(unique(t1$wave))

set.seed(200) #Bien relancer set.seed à chaque sample
t3 <- as.data.frame(sample(x = t2$`unique(t1$wave)`, size = 1800, replace = FALSE))
names(t3) <- c("wave")
t4 <- data.frame()
t6 <- data.frame()
for(i in 1:nrow(t3)){
  t4 <- subset(x = t1, subset = t1$wave==t3$wave[i])
  t6 <- rbind(t6, t4)
}

t6$filename <- paste(t6$wave, t6$cri, t6$homsap, t6$part1, t6$part2, t6$part3, t6$part4, sep="--")
#Copie des images Homsap dans Oiseaux2
path_original <-"H:/voix_humaine_pour_ameliorer_base_classif/ima2"
path_dir <- "H:/traitement_Tadarida_deep_avec_freq20/TadariDeep-main/python_sources/oiseaux5/Homsap"

for(j in 1:nrow(t6)){
  file.copy(from = paste(path_original, t6$filename[j], sep="/"), to = path_dir)}



set.seed(200) #Bien relancer set.seed à chaque sample
t10 <- as.data.frame(sample(x = t2$`unique(t1$wave)`, size = 600, replace = FALSE))
names(t10) <- c("wave")
t11 <- data.frame()
t12 <- data.frame()
for(i in 1:nrow(t10)){
  t11 <- subset(x = t1, subset = t1$wave==t10$wave[i])
  t12 <- rbind(t12, t11)
}

t12$filename <- paste(t12$wave, t12$cri, t12$homsap, t12$part1, t12$part2, t12$part3, t12$part4, sep="--")
#Copie des images Homsap dans Oiseaux2
path_original <-"H:/voix_humaine_pour_ameliorer_base_classif/ima2"
path_dir <- "H:/traitement_Tadarida_deep_avec_freq20/TadariDeep-main/python_sources/oiseaux4/Homsap"

for(j in 1:nrow(t12)){file.copy(from = paste(path_original, t12$filename[j], sep="/"), to = path_dir)}
