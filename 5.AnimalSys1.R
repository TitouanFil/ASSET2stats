
### 1. Preparation

## a. Libraries loading
library(FactoMineR)
library(factoextra)
library(corrplot)
library(missMDA)
library(tidyr)
library(plyr)
library(dplyr)
library(corrplot)
library(rstatix)
library(stringr)


## b. data loading
CropDesc <- readRDS("AnimalDesc_TF.rds")
All <- readRDS("Newdata_TF.rds")
x <- All[All$S_Area == "Cambodia",]

##CAMBODIA
#Buffalo
Buffalo <- All[!is.na(All$e3_1) & All$S_Area == "Cambodia",c(1,10874,11042:11043,11062:11087)]
Buffalo <- labelled::copy_labels(All, Buffalo)
#Cattle
Cattle <- All[!is.na(All$e3_3) & All$S_Area == "Cambodia",c(1,10774,11044:11045,11062:11087)]
Cattle <- labelled::copy_labels(All, Cattle)
CattleBig <- Cattle[Cattle$e3_3 > 19 & Cattle$e5_4.Cattle > 3,]
mean(CattleBig$e3_3)
CattleSmall <- Cattle[!(Cattle$hhid_re2 %in% CattleBig$hhid_re2),]
mean(CattleSmall$e3_3)
mean(CattleSmall$e5_4.Cattle)
#Pig
Pig <- All[!is.na(All$e3_3) & All$S_Area == "Cambodia",c(1,10774,11046,11126:11178)]
mean(Pig$e3_5[Pig$e30 == "Fattening family raising, but buying feed from market" | Pig$e30 == "Piglet family raising, but buying feed from market"], na.rm = T)
#Chicken
Camb <- All[All$S_Area == "Son La province",]
Camb$Poultry <- rowSums(subset(Camb, select = c("e3_13", "e3_14", "e3_16")), na.rm = TRUE)
mean(Camb$Poultry[Camb$e44 == "Family raising but larger scale for selling meat"], na.rm = T)
#Duck/muscovy

#Nb of households with cattle buffalo
Camb <- All[All$S_Area =="Dien Bien province",]
sumCB <- ifelse(is.na(Camb$e3_1),0,Camb$e3_1) + ifelse(is.na(Camb$e3_3),0,Camb$e3_3)
sum(sumCB > 0)
sumP <- ifelse(is.na(Camb$e3_13),0,Camb$e3_13) + ifelse(is.na(Camb$e3_14),0,Camb$e3_14) +
  ifelse(is.na(Camb$e3_16),0,Camb$e3_16)
sum(sumP > 0)


##LAO
#Buffalo
Buffalo <- All[!is.na(All$e3_1) & All$S_Area == "Lao",c(1,10874,11042:11043,11062:11087)]
Buffalo <- labelled::copy_labels(All, Buffalo)
mean(Buffalo$e3_1[Buffalo$e5_4.Buffalo > 3 & Buffalo$e3_1 > 9])
#Cattle
Cattle <- All[!is.na(All$e3_3) & All$S_Area == "Lao",c(1,10774,11044:11045,11062:11087)]
Cattle <- labelled::copy_labels(All, Cattle)
CattleBig <- Cattle[Cattle$e3_3 > 19 & Cattle$e5_4.Cattle > 3,]
mean(CattleBig$e3_3)
CattleSmall <- Cattle[!(Cattle$hhid_re2 %in% CattleBig$hhid_re2),]
mean(CattleSmall$e3_3)
mean(CattleSmall$e5_4.Cattle)
#Pig
Pig <- All[!is.na(All$e3_5) & All$S_Area == "Lao",c(1,10774,11046,11126:11178)]
PigSellers <- Pig[Pig$e5_4.Cattle > 9,]
PigNorm <- Pig[Pig$e5_4.Cattle < 9,]
mean(Pig$e3_5[Pig$e30 == "Piglet family raising using only local feed stuffs and kitchen waste"])
#Chicken
Lao <- All[All$S_Area == "Lao",]
Lao$Poultry <- rowSums(subset(Lao, select = c("e3_13", "e3_14", "e3_16")), na.rm = TRUE)
mean(Lao$Poultry[Lao$e44 == "Family raising for consumption" | Lao$e44 == ""], na.rm = T)
#Duck/muscovy

#Son La province
#Buffalo
Buffalo <- All[!is.na(All$e3_1) & All$S_Area == "Son La province",c(1,10874,11042:11043,11062:11087)]
Buffalo <- labelled::copy_labels(All, Buffalo)
mean(Buffalo$e3_1[Buffalo$e5_4.Buffalo > 3 & Buffalo$e3_1 > 9])
#Cattle
Cattle <- All[!is.na(All$e3_3) & All$S_Area == "Son La province",c(1,10774,11044:11045,11062:11087)]
Cattle <- labelled::copy_labels(All, Cattle)
CattleBig <- Cattle[Cattle$e3_3 > 9,]
mean(CattleBig$e3_3)
CattleSmall <- Cattle[!(Cattle$hhid_re2 %in% CattleBig$hhid_re2),]
mean(CattleSmall$e3_3)
mean(CattleSmall$e5_4.Cattle)
#Pig
Pig <- All[!is.na(All$e3_5) & All$S_Area == "Son La province",c(1,10774,11046,11126:11178)]
PigSellers <- Pig[Pig$e5_4.Cattle > 9,]
PigNorm <- Pig[Pig$e5_4.Cattle < 9,]
mean(Pig$e3_5[Pig$e30 == "Fattening family raising using only local feed stuffs and kitchen waste" | Pig$e30 == ""], na.rm = T)
#Chicken
Chicken <- All[All$S_Area == "Son La province",]
Chicken$Poultry <- rowSums(subset(Chicken, select = c("e3_13", "e3_14", "e3_16")), na.rm = TRUE)
mean(Chicken$Poultry[Chicken$e44 == "Family raising but larger scale for selling meat"], na.rm = T)
#Duck/muscovy


#Dien Bien province
#Buffalo
Buffalo <- All[!is.na(All$e3_1) & All$S_Area == "Dien Bien province",c(1,10874,11042:11043,11062:11087)]
Buffalo <- labelled::copy_labels(All, Buffalo)
mean(Buffalo$e3_1[Buffalo$e5_4.Buffalo > 3 & Buffalo$e3_1 > 9])
#Cattle
Cattle <- All[!is.na(All$e3_3) & All$S_Area == "Dien Bien province",c(1,10774,11044:11045,11062:11087)]
Cattle <- labelled::copy_labels(All, Cattle)
Rumi <- All[All$S_Area == "Dien Bien province",]
Rumi$Rumi <-  rowSums(subset(Rumi, select = c("e3_1", "e3_3")), na.rm = TRUE)
mean(Rumi$Rumi[Rumi$e16 != "Confined in a barn,"], na.rm = T)
CattleIn <- Cattle[Cattle$e16 != "Confined in a barn,",]
CattleOut <- Cattle[Cattle$e16 == "Confined in a barn,",]
mean(CattleOut$e5_4.Cattle, na.rm = T)
mean(CattleIn$e5_4.Cattle, na.rm = T)
#Pig
Pig <- All[!is.na(All$e3_5) & All$S_Area == "Dien Bien province",c(1,10774,11046,11126:11178)]
PigSellers <- Pig[Pig$e5_4.Cattle > 9,]
PigNorm <- Pig[Pig$e5_4.Cattle < 9,]
mean(Pig$e3_5[Pig$e30 == "Fattening family raising using only local feed stuffs and kitchen waste"], na.rm = T)
#Chicken
Chicken <- All[All$S_Area == "Dien Bien province",]
Chicken$Poultry <- rowSums(subset(Chicken, select = c("e3_13", "e3_14", "e3_16")), na.rm = TRUE)
x <- Chicken[Chicken$Poultry > 0 & !is.na(Chicken$Poultry),]
mean(Chicken$Poultry[Chicken$e44 == "Family raising for consumption" | Chicken$e44 == ""], na.rm = T)
#Duck/muscovy
