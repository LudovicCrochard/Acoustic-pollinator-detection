library(data.table)
library(tidyverse)
library(filesstrings)
part1 <- fread("G:travail_manip_tournesol/files_listing_part1.csv")
part2 <- fread("G:travail_manip_tournesol/files_listing_part2.csv")
part3 <- fread("G:travail_manip_tournesol/files_listing_part3.csv")
part4 <- fread("G:travail_manip_tournesol/files_listing_part4.csv")
part5 <- fread("G:travail_manip_tournesol/files_listing_part5.csv")

file_listing <- rbind(part1, part2, part3, part4, part5)
###Sélection fichiers déjà utilisés pour base de donnees originales (12 focales)
db_originale <- filter(file_listing, original_data_base=="yes")
db_originale$fichier <- paste(db_originale$field, db_originale$parcelle, db_originale$pied,
                              db_originale$date_heure, db_originale$ext, sep = "_")
db_originale$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files/", db_originale$fichier, sep="")
# file.move(db_originale$cheminfichier, "G:/travail_manip_tournesol/12focales_used_for_originale_data_base")

###Sélection des focales restantes
focales_restantes <- filter(file_listing, focale=="yes" & original_data_base!="yes")
focales_l3 <- filter(focales_restantes, pied=="l3t1" | pied=="l3t2")
focales_l1 <- filter(focales_restantes, pied=="l1t1" | pied=="l1t2")

focales_l3$fichier <- paste(focales_l3$field, focales_l3$parcelle, focales_l3$pied,
                            focales_l3$date_heure, focales_l3$ext, sep = "_")
focales_l3$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files/", focales_l3$fichier, sep="")
# file.move(focales_l3$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/focales_non_etiquetees/l3")

focales_l1$fichier <- paste(focales_l1$field, focales_l1$parcelle, focales_l1$pied,
                            focales_l1$date_heure, focales_l1$ext, sep = "_")
focales_l1$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files/", focales_l1$fichier, sep="")
# file.move(focales_l1$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/focales_non_etiquetees/l1")



###Sélection des fichiers hors focales qui sont en l1
l1 <- filter(file_listing, pied=="l1t1" | pied=="l1t2")
l1 <- filter(l1, focale!="yes")
l1$fichier <- paste(l1$field, l1$parcelle, l1$pied,
                    l1$date_heure, l1$ext, sep = "_")
l1$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files/", l1$fichier, sep="")
# file.move(l1$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l1")


###Sélection des fichiers hors focales qui sont en l3
l3 <- filter(file_listing, pied=="l3t1" | pied=="l3t2")
l3 <- filter(l3, focale!="yes")
l3$fichier <- paste(l3$field, l3$parcelle, l3$pied,
                    l3$date_heure, l3$ext, sep = "_")
l3$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files/", l3$fichier, sep="")
# file.move(l3$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3")


###Séparation en plusieur dossiers des l3 pour passer sous Tadarida
part1 <- filter(l3, parcelle=="12" | parcelle=="116" | parcelle=="376" |
                  parcelle=="1823" | parcelle=="2187" | parcelle=="2230")
part1$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/l3/", part1$fichier, sep="")
# file.move(part1$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part1")

part2 <- filter(l3, parcelle=="2939" | parcelle=="3107" | parcelle=="3253" |
                  parcelle=="3271" | parcelle=="4566" | parcelle=="5572")
part2$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/l3/", part2$fichier, sep="")
#file.move(part2$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part2")

part3 <- filter(l3, parcelle=="5595" | parcelle=="5867" | parcelle=="6223" |
                  parcelle=="7029" | parcelle=="7413" | parcelle=="7507")
part3$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/l3/", part3$fichier, sep="")
# file.move(part3$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part3")

part4 <- filter(l3, parcelle=="8690" | parcelle=="9523" | parcelle=="10106" |
                  parcelle=="10486" | parcelle=="10626" | parcelle=="12446")
part4$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/l3/", part4$fichier, sep="")
# file.move(part4$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part4")

part5 <- filter(l3, parcelle=="12473" | parcelle=="13138" | parcelle=="13253" |
                  parcelle=="14499" | parcelle=="15339" | parcelle=="17019")
part5$cheminfichier <- paste("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/l3/", part5$fichier, sep="")
# file.move(part5$cheminfichier, "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part5")
