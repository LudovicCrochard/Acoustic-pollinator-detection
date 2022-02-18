#Res test7 (classif 7 aug) vs test7 (classif7 non aug)
test7_classif6 <- readr::read_csv2("D:/tests_classif6_vs_classif7/test7_data_non_augmentee/test7_classif_IdTot_nonaug.csv", col_names = TRUE)
test7_classif <- readr::read_csv2("D:/tests_classif6_vs_classif7/Test7_augmente_2e_essai/Test7_classif.csv", col_names = TRUE)
test7_classif_aug100000 <- readr::read_csv("D:/tests_classif6_vs_classif7/data_aug_100000/test7_classif_IdTot.csv", col_names = TRUE)


FileBlue=subset(test7_classif$Group.1,test7_classif$Pol_or_not=="yes")
FileBlueAug=subset(test7_classif_aug100000$Group.1,test7_classif_aug100000$Group.1 %in% FileBlue)


library(dplyr)
test7_classif6$Apismel <- as.numeric(test7_classif6$Apismel)
test7_classif6$Bomsp <- as.numeric(test7_classif6$Bomsp)
test7_classif6$insect <- as.numeric(test7_classif6$insect)
test7_classif_aug100000$Apismel <- as.numeric(test7_classif_aug100000$Apismel)
test7_classif_aug100000$Bomsp <- as.numeric(test7_classif_aug100000$Bomsp)
test7_classif_aug100000$insect <- as.numeric(test7_classif_aug100000$insect)
for(i in 1: nrow(test7_classif6)){
  test7_classif6$score_pol_classif6[i] <- sum(test7_classif6$Apismel[i], test7_classif6$Bomsp[i], test7_classif6$insect[i])
}

for(i in 1: nrow(test7_classif_aug100000)){
  test7_classif_aug100000$score_pol_aug_100000[i] <- sum(test7_classif_aug100000$Apismel[i], test7_classif_aug100000$Bomsp[i], test7_classif_aug100000$insect[i])
}

library(dplyr)
test7_classif6 <- aggregate(score_pol_classif6~Group.1, data=test7_classif6, FUN="max")
test1 <- full_join(test7_classif6, test7_classif, by=c("Group.1"))
test1 <- filter(test1, Apismel!="NA")

test7_classif_aug100000 <- aggregate(score_pol_aug_100000~Group.1, data=test7_classif_aug100000, FUN="max")
test2 <- full_join(test1, test7_classif_aug100000, by=c("Group.1"))
test2 <- filter(test2, Apismel!="NA")

test2$insecte <- as.numeric(test2$insecte)
# for(i in 1: nrow(test1)){
#   test1$score_pol_classif6[i] <- sum(test1$Apismel.x[i], test1$Bomsp.x[i], test1$insect.x[i])
#   test1$score_pol_augment[i] <- sum(test1$Apismel.y[i], test1$Bomsp.y[i], test1$insect.y[i])
# }
# test2 <- test1[order(test1$Group.1),]
# test2 <- mutate_all(test2, ~replace(., is.na(.),0))
library(ggplot2)
ggplot(test2,aes(x=score_pol_classif6, y=insecte, color=Pol_or_not)) + geom_point(size=3, alpha=0.3)+
  geom_smooth(method=lm, se=F, fullrange=T)+
  ggtitle("Test 7")+
  xlab("score des pollinisateurs avec classificateur 7 non augmente")+ylab("score des pollinisateurs avec classificateur 7 augmente10000")


ggplot(test2,aes(x=score_pol_classif6, y=score_pol_aug_100000, color=Pol_or_not)) + geom_point(size=3, alpha=0.3)+
  geom_smooth(method=lm, se=F, fullrange=T)+
  ggtitle("Test 7")+
  xlab("score des pollinisateurs avec classificateur 7 non augmente")+ylab("score des pollinisateurs avec classificateur 7 augmente100000")





library(data.table)
#comparaison test1 vs test1 avec classif post test 7
test1_classif <- readr::read_csv("D:/test1_excel/test_classif1_sum.csv", col_names = TRUE)
test1_classif7 <- fread("D:/21102021_test1_classif_ap_test7/test_ac_identif_test1_conserve/sons_IdTot.csv", header = TRUE)
test1_classif7_sans1 <- fread("D:/21102021_test1_classif_ap_test7/test_sans_identif_test1_conserve/sons_IdTot.csv", header = TRUE)

for(i in 1: nrow(test1_classif7)){
  test1_classif7$score_pol_classif7[i] <- sum(test1_classif7$Apismel[i], test1_classif7$Bomsp[i], test1_classif7$insect[i])
}

for(i in 1: nrow(test1_classif7_sans1)){
  test1_classif7_sans1$score_pol_classif7_sans1[i] <- sum(test1_classif7_sans1$Apismel[i], test1_classif7_sans1$Bomsp[i], test1_classif7_sans1$insect[i])
}


test1_classif7 <- aggregate(score_pol_classif7~Group.1, data=test1_classif7, FUN="max")
test1 <- full_join(test1_classif7, test1_classif, by=c("Group.1"))
test1 <- filter(test1, Apismel!="NA")

test1_classif7_sans1 <- aggregate(score_pol_classif7_sans1~Group.1, data=test1_classif7_sans1, FUN="max")
test2 <- full_join(test1, test1_classif7_sans1, by=c("Group.1"))
test2 <- filter(test2, Apismel!="NA")


test2 <- filter(test2, Pol_or_not!="yes_no")
ggplot(test2,aes(x=insecte, y=score_pol_classif7, color=Pol_or_not)) + geom_point(size=3, alpha=0.3)+
  geom_smooth(method=lm, se=F, fullrange=T)+
  ggtitle("Test 1")+
  xlab("score de pollinisateur au moment du test 1")+ylab("score de pollinisateur avec classificateur après le test 7")


ggplot(test2,aes(x=insecte, y=score_pol_classif7_sans1, color=Pol_or_not)) + geom_point(size=3, alpha=0.3)+
  geom_smooth(method=lm, se=F, fullrange=T)+
  ggtitle("Test 1")+
  xlab("score de pollinisateur au moment du test 1")+ylab("score de pollinisateur avec classificateur après le test 7 sans les identification du test1")
