t <- list.files(path = "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part3", pattern = ".wav", recursive = FALSE, full.names = T)
t <- as.data.frame(t)
t$path <- c(rep("F:/fichiers_hors focales/part1", 50000),rep("F:/fichiers_hors focales/part2", 50000),rep("F:/fichiers_hors focales/part3", 50000),
              rep("F:/fichiers_hors focales/part4", 50000),rep("F:/fichiers_hors focales/part5", 50000),rep("F:/fichiers_hors focales/part6", 50000),
              rep("F:/fichiers_hors focales/part7", 50000),rep("F:/fichiers_hors focales/part8", 50000),rep("F:/fichiers_hors focales/part9", 50000),
              rep("F:/fichiers_hors focales/part10", 50000),rep("F:/fichiers_hors focales/part11", 50000),rep("F:/fichiers_hors focales/part12", 50000),
              rep("F:/fichiers_hors focales/part13", 50000),rep("F:/fichiers_hors focales/part14", 50000),rep("F:/fichiers_hors focales/part15", 50000),
              rep("F:/fichiers_hors focales/part16", 50000),rep("F:/fichiers_hors focales/part17", 50000),rep("F:/fichiers_hors focales/part18", 50000),
              rep("F:/fichiers_hors focales/part19", 57396))

t4 <- c("F:/fichiers_hors focales/part1", "F:/fichiers_hors focales/part2", "F:/fichiers_hors focales/part3",
        "F:/fichiers_hors focales/part4", "F:/fichiers_hors focales/part5", "F:/fichiers_hors focales/part6", 
        "F:/fichiers_hors focales/part7", "F:/fichiers_hors focales/part8", "F:/fichiers_hors focales/part9", 
        "F:/fichiers_hors focales/part10", "F:/fichiers_hors focales/part11", "F:/fichiers_hors focales/part12", 
        "F:/fichiers_hors focales/part13", "F:/fichiers_hors focales/part14", "F:/fichiers_hors focales/part15", 
        "F:/fichiers_hors focales/part16", "F:/fichiers_hors focales/part17", "F:/fichiers_hors focales/part18", 
        "F:/fichiers_hors focales/part19")
for(i in 1: length(t4)){
  dir.create(t4[i])
}

library(filesstrings)
file.move(files = t$t, destinations = t$path)



t <- list.files(path = "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part2", pattern = ".wav", recursive = FALSE, full.names = T)
t <- as.data.frame(t)
t$path <- c(rep("F:/fichiers_hors focales/part20", 50000),rep("F:/fichiers_hors focales/part21", 50000),rep("F:/fichiers_hors focales/part22", 50000),
            rep("F:/fichiers_hors focales/part23", 50000),rep("F:/fichiers_hors focales/part24", 50000),rep("F:/fichiers_hors focales/part25", 50000),
            rep("F:/fichiers_hors focales/part26", 50000),rep("F:/fichiers_hors focales/part27", 50000),rep("F:/fichiers_hors focales/part28", 50000),
            rep("F:/fichiers_hors focales/part29", 50000),rep("F:/fichiers_hors focales/part30", 50000),rep("F:/fichiers_hors focales/part31", 50000),
            rep("F:/fichiers_hors focales/part32", 50000),rep("F:/fichiers_hors focales/part33", 50000),rep("F:/fichiers_hors focales/part34", 50000),
            rep("F:/fichiers_hors focales/part35", 50000),rep("F:/fichiers_hors focales/part36", 50000),rep("F:/fichiers_hors focales/part37", 50000),
            rep("F:/fichiers_hors focales/part38", 57607))

t4 <- c("F:/fichiers_hors focales/part20", "F:/fichiers_hors focales/part21", "F:/fichiers_hors focales/part22",
        "F:/fichiers_hors focales/part23", "F:/fichiers_hors focales/part24", "F:/fichiers_hors focales/part25", 
        "F:/fichiers_hors focales/part26", "F:/fichiers_hors focales/part27", "F:/fichiers_hors focales/part28", 
        "F:/fichiers_hors focales/part29", "F:/fichiers_hors focales/part30", "F:/fichiers_hors focales/part31", 
        "F:/fichiers_hors focales/part32", "F:/fichiers_hors focales/part33", "F:/fichiers_hors focales/part34", 
        "F:/fichiers_hors focales/part35", "F:/fichiers_hors focales/part36", "F:/fichiers_hors focales/part37", 
        "F:/fichiers_hors focales/part38")
for(i in 1: length(t4)){
  dir.create(t4[i])
}

library(filesstrings)
file.move(files = t$t, destinations = t$path)










t <- list.files(path = "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part1", pattern = ".wav", recursive = FALSE, full.names = T)
t <- as.data.frame(t)
t$path <- c(rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part39", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part40", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part41", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part42", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part43", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part44", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part45", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part46", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part47", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part48", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part49", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part50", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part51", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part52", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part53", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part54", 35507))

t4 <- c("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part39", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part40", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part41",
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part42", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part43", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part44", 
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part45", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part46", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part47", 
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part48", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part49", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part50", 
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part51", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part52", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part53", 
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part54")
for(i in 1: length(t4)){
  dir.create(t4[i])
}

library(filesstrings)
file.move(files = t$t, destinations = t$path)









t <- list.files(path = "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part4", pattern = ".wav", recursive = FALSE, full.names = T)
t <- as.data.frame(t)
t$path <- c(rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part55", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part56", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part57", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part58", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part59", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part60", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part61", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part62", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part63", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part64", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part65", 50000),rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part66", 50000),
            rep("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part67", 46359))

t4 <- c("G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part55", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part56", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part57",
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part58", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part59", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part60", 
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part61", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part62", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part63", 
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part64", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part65", "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part66", 
        "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/deep_learning/part67")
for(i in 1: length(t4)){
  dir.create(t4[i])
}

library(filesstrings)
file.move(files = t$t, destinations = t$path)











t <- list.files(path = "G:/travail_manip_tournesol/accelerated_files1/fichiers_hors_focales/l3/part5", pattern = ".wav", recursive = FALSE, full.names = T)
t <- as.data.frame(t)
t$path <- c(rep("F:/fichiers_hors focales/part68", 50000),rep("F:/fichiers_hors focales/part69", 50000),rep("F:/fichiers_hors focales/part70", 50000),
            rep("F:/fichiers_hors focales/par71", 50000),rep("F:/fichiers_hors focales/part72", 50000),rep("F:/fichiers_hors focales/part73", 50000),
            rep("F:/fichiers_hors focales/part74", 50000),rep("F:/fichiers_hors focales/part75", 50000),rep("F:/fichiers_hors focales/part76", 50000),
            rep("F:/fichiers_hors focales/part77", 50000),rep("F:/fichiers_hors focales/part78", 50000),rep("F:/fichiers_hors focales/part79", 50000),
            rep("F:/fichiers_hors focales/part80", 50000),rep("F:/fichiers_hors focales/part81", 18127))

t4 <- c("F:/fichiers_hors focales/part68", "F:/fichiers_hors focales/part69", "F:/fichiers_hors focales/part70",
        "F:/fichiers_hors focales/part71", "F:/fichiers_hors focales/part72", "F:/fichiers_hors focales/part73", 
        "F:/fichiers_hors focales/part74", "F:/fichiers_hors focales/part75", "F:/fichiers_hors focales/part76", 
        "F:/fichiers_hors focales/part77", "F:/fichiers_hors focales/part78", "F:/fichiers_hors focales/part79", 
        "F:/fichiers_hors focales/part80", "F:/fichiers_hors focales/part81")
for(i in 1: length(t4)){
  dir.create(t4[i])
}

library(filesstrings)
file.move(files = t$t, destinations = t$path)
