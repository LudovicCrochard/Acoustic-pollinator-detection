library(data.table)
library(Hmisc)
library(tidyverse)
rf_ta_files <- as.data.frame(list.files(path = "H:/RSDB", pattern =".ta" , recursive =TRUE))
rf_eti_files <- as.data.frame(list.files(path = "H:/RSDB", pattern =".eti" , recursive =TRUE))
rf_files_path <- "H:/RSDB" #RSDB utilisee pour le random forest
deep_files <- as.data.frame(list.files(path = "H:/traitement_Tadarida_deep_avec_freq20/RSDB_test", pattern =".ta" , recursive =TRUE))
deep_files_path <- "H:/traitement_Tadarida_deep_avec_freq20/RSDB_test" #RSDB utilisee pour le deep learning

names_eti <- c("Cri", "Espece", "Type", "Indice", "Zone", "Site", "Commentaire", "Materiel", "Confidentiel", "Date", "Auteur", "Etiqueteur")
ToleranceFreq=0.2
ToleranceTemps=8

rfta <- data.frame() #rf = random forest et ta = fichiers .ta
rf_eti <- data.frame() # eti = fichiers .eti
NewLabelTable <- data.frame()
for(i in 1:nrow(rf_ta_files)){
  rf_file <- data.frame()
  NewSpecies <- data.frame()
  
  rfta <- fread(paste(rf_files_path,rf_ta_files[i,1], sep="/"))
  rf_eti <- fread(paste(rf_files_path, rf_eti_files[i,1], sep="/"))
  rf_eti <- rf_eti[,-13]
  # if(names(rf_eti)!=names_eti)
  #   next
  names(rf_eti) <- names_eti
  rf_file <- cbind(rf_eti, rfta)
  #rf_file <- filter(rf_file, Espece!="")
  
  deep_ta <- fread(paste(deep_files_path, deep_files[i,1], sep="/")) #deep = deep learning
  if(nrow(deep_ta)==0){next}
  deep_ta$PeakTime <- deep_ta$StTime+deep_ta$PosMP*deep_ta$Dur
  rf_file$PeakTime <- rf_file$StTime+rf_file$PosMP*rf_file$Dur
  
  NewLabels=find.matches(cbind(deep_ta$PeakTime,deep_ta$FreqMP),cbind(rf_file$PeakTime,rf_file$FreqMP),tol=c(ToleranceTemps,ToleranceFreq)
                         ,maxmatch=1)
  print(NewLabels$matches)
  
for(j in 1:length(NewLabels$matches)){ #ne pas utiliser fonction ifelse() car genere des erreurs
  if(NewLabels$matches[j]==0){
    NewSpecies[j,1]=""
  }else{NewSpecies[j,1]=rf_file$Espece[NewLabels$matches[j]]}}
  
  # NewSpecies=ifelse(NewLabels$matches==0,"",rf_file$Espece[NewLabels$matches[1]])
  deep_ta$Espece=NewSpecies
  #RefCorrj=deep_ta[NewLabels$matches,]
  # RefCorrj_labels <- cbind(subset(rf_file,select=names(rf_eti)), RefCorrj)
  NewLabelTable <- rbind(NewLabelTable,deep_ta)
  if(nrow(NewLabelTable)==0){stop}
}
NewLabelTable$CallNum <- as.integer(NewLabelTable$CallNum)
NewLabelTable1 <- as.data.frame(NewLabelTable[,c(1,2,276)])
NewLabelTable1$Filename <- str_replace(NewLabelTable1$Filename, ".wav", "")


#Recherche des fichiers images et renommage des fichiers en fonction de l'espece correspondant a l'evenement sonore
images <- as.data.frame(list.files(path = "H:/traitement_Tadarida_deep_avec_freq20/RSDB_test", full.names = TRUE, recursive = TRUE))
names(images) <- c("Filename")
library(stringr)
images1 <- subset(images, str_detect(images$Filename, pattern = "ima2"))

images2 <- separate(images1, col = Filename, into = c("Disque", "Dossier","RSDB", "date", "type", "fichier"), sep = "/")
images2 <- separate(data = images2, col = fichier, sep = "--", into = c("Filename", "CallNum", "Intensity", "Duree", "Frequency", "STTime"))
images2$CallNum <- as.integer(images2$CallNum)

images3 <- merge(NewLabelTable1, images2, by=c("Filename", "CallNum"))


images3 <- filter(images3, Espece!="")

image4 <- as.data.frame(paste(images3$Filename, images3$CallNum, images3$Espece, images3$Intensity, images3$Duree, images3$Frequency, images3$STTime, sep="--"))
image4$past_name <- paste(images3$Filename, images3$CallNum, images3$Intensity, images3$Duree, images3$Frequency, images3$STTime, sep="--")
names(image4) <- c("new_name", "past_name")

images3$past_name <- paste(images3$Filename, images3$CallNum, images3$Intensity, images3$Duree, images3$Frequency, images3$STTime, sep="--")
images3$path <- paste(images3$Disque, images3$Dossier,images3$RSDB, images3$date, images3$type, sep="/")
image4 <- merge(image4, images3[,c(13,14)], by="past_name")

image4$ancien_fichier <- paste(image4$path, image4$past_name, sep="/")
image4$nouveau_fichier <- paste(image4$path, image4$new_name, sep="/")

for(i in 1:nrow(image4)){
  file.rename(to=image4$nouveau_fichier[i], from=image4$ancien_fichier[i])
}


#Creation nouveaux dossiers pour images des sons
images3$Espece <- as.factor(images3$Espece)
t <- as.data.frame(levels(images3$Espece))
names(t) <- "levels"
t$path <- paste("H:/traitement_Tadarida_deep_avec_freq20/RSDB_images_triees", t$levels, sep="/")
for(i in 1:nrow(t)){
  dir.create(path=t$path[i])
}

#Redistribution des images dans les bons fichiers
for(j in 1:nrow(t)){
    file_a_copier <- subset(image4, str_detect(image4$nouveau_fichier, pattern =t$levels[j]))
    file.copy(from = file_a_copier$nouveau_fichier, to = t$path[j])
}
