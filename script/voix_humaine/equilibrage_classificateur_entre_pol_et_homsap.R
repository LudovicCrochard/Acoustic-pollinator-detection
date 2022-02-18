library(data.table)
library(tidyverse)

df <- fread("G:/travail_manip_tournesol/travail_sur_voix_humaine/test_50000/BaseAugmentee.csv")
df_homsap <- filter(df, Nesp=="Homsap")
df_homsap1 <- sample_n(df_homsap, size = nrow(filter(df, Nesp=="insect" | Nesp=="Apismel" | Nesp=="Bomsp")), replace = F)

df1 <- rbind(filter(df, Nesp!="Homsap"), df_homsap1)

write.csv(df1, "BaseAugmentee_equilibree.csv")
