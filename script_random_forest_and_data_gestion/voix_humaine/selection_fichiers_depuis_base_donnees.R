library(tidyverse)

set.seed(2514)
files_list <- list.files("E:/human_voice/cv-corpus-7.0-2021-07-21/fr/clipsWAV")
sampled_files <- sample(x = files_list, replace = FALSE, size = 10000)
sampled_files <- as.data.frame(sampled_files)
sampled_files$fichier <- paste("E:/human_voice/cv-corpus-7.0-2021-07-21/fr/clipsWAV", sampled_files$sampled_files, sep="/")
file.copy(sampled_files$fichier,"D:/travail_manip_tournesol/doc_voix_humaine/base_voix_humaine")
