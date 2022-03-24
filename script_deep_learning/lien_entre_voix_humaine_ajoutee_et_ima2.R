library(data.table)
library(Hmisc)
library(tidyverse)

NewRefTable_rf <- fread("H:/voix_humaine_pour_ameliorer_base_classif/NewRefTable_rf.csv")
NewRefTable_rf$Filename <- str_replace(NewRefTable_rf$Filename, ".wav", ".ta")
NewRefTable_rf$Filename <- as.factor(NewRefTable_rf$Filename)
levels_Filename <- as.data.frame(levels(NewRefTable_rf$Filename))
deep_files_path <- "H:/voix_humaine_pour_ameliorer_base_classif/txt" #RSDB utilisee pour le deep learning


ToleranceFreq=0.2
ToleranceTemps=8


NewLabelTable <- data.frame()
for(i in 1:nrow(levels_Filename)){
  NewSpecies <- data.frame()
  rf_files <- data.frame()
  
rf_files <- filter(NewRefTable_rf, Filename==levels_Filename[i,1])

     deep_ta <- fread(paste(deep_files_path, levels_Filename[i,1], sep="/")) #deep = deep learning
   if(nrow(deep_ta)==0){next}
   deep_ta$PeakTime <- deep_ta$StTime+deep_ta$PosMP*deep_ta$Dur
 
  
  NewLabels=find.matches(cbind(deep_ta$PeakTime,deep_ta$FreqMP),cbind(rf_files$PeakTime,rf_files$FreqMP),tol=c(ToleranceTemps,ToleranceFreq)
                         ,maxmatch=1)
  print(NewLabels$matches)
  
  for(j in 1:length(NewLabels$matches)){ #ne pas utiliser fonction ifelse() car genere des erreurs
    if(NewLabels$matches[j]==0){
      NewSpecies[j,1]=""
    }else{NewSpecies[j,1]=rf_files$Espece[NewLabels$matches[j]]}}
  
  # NewSpecies=ifelse(NewLabels$matches==0,"",rf_file$Espece[NewLabels$matches[1]])
  deep_ta$Espece=NewSpecies
  #RefCorrj=deep_ta[NewLabels$matches,]
  # RefCorrj_labels <- cbind(subset(rf_file,select=names(rf_eti)), RefCorrj)
  NewLabelTable <- rbind(NewLabelTable,deep_ta)
  if(nrow(NewLabelTable)==0){stop}
}

NewLabelTable$CallNum <- as.integer(NewLabelTable$CallNum)
NewLabelTable1 <- filter(NewLabelTable, Espece!="")
NewLabelTable1 <- as.data.frame(NewLabelTable1[,c(1,2,276)])


#Recherche des fichiers images et renommage des fichiers en fonction de l'espece correspondant a l'evenement sonore
images <- as.data.frame(list.files(path = "H:/voix_humaine_pour_ameliorer_base_classif/ima2"))
names(images) <- c("Filename")
library(stringr)
images <- separate(data = images, col = Filename, sep = "--", into = c("Filename", "CallNum", "Intensity", "Duree", "Frequency", "STTime"))
images$CallNum <- as.integer(images$CallNum)
images$Filename <- paste(images$Filename, ".wav",sep="")
images1 <- merge(NewLabelTable1, images, by=c("Filename", "CallNum"))


# images3 <- filter(images3, Espece!="")

image2 <- as.data.frame(paste(images1$Filename, images1$CallNum, images1$Espece, images1$Intensity, images1$Duree, images1$Frequency, images1$STTime, sep="--"))
image2$past_name <- paste(images1$Filename, images1$CallNum, images1$Intensity, images1$Duree, images1$Frequency, images1$STTime, sep="--")
names(image2) <- c("new_name", "past_name")
image2$new_name <- str_replace(image2$new_name, pattern = ".wav", replacement = "")
image2$past_name <- str_replace(image2$past_name, pattern = ".wav", replacement = "")

# images2$past_name <- paste(images3$Filename, images3$CallNum, images3$Intensity, images3$Duree, images3$Frequency, images3$STTime, sep="--")
image2$path <- "H:/voix_humaine_pour_ameliorer_base_classif/ima2/"
image2$ancien_fichier <- paste(image2$path, image2$past_name, sep="")
image2$nouveau_fichier <- paste(image2$path, image2$new_name, sep="")

for(i in 1:nrow(image2)){
  file.rename(to=image2$nouveau_fichier[i], from=image2$ancien_fichier[i])
}


#Copie des images Homsap dans Oiseaux2
images10 <- as.data.frame(list.files(path = "H:/voix_humaine_pour_ameliorer_base_classif/ima2"))
path_original <-"H:/voix_humaine_pour_ameliorer_base_classif/ima2"
path_dir <- "H:/traitement_Tadarida_deep_avec_freq20/TadariDeep-main/python_sources/oiseaux2/Homsap"
t <- filter(images10, str_detect(images10$`list.files(path = "H:/voix_humaine_pour_ameliorer_base_classif/ima2")`,pattern = "Homsap"))
names(t) <- c("Filename")

t1 <- sample_n(tbl = t, size = 1000, replace = FALSE)

for(j in 1:nrow(t1)){
  file.copy(from = paste(path_original, t1$Filename[j], sep="/"), to = path_dir)
}
