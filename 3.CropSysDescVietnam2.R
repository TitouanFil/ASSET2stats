
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
CropDesc <- readRDS("CropDesc_TF.rds")
All <- readRDS("Newdata_TF.rds")


###2. "Cropping systems" exploration

##a. Lao
#Data preparation
All$hhid_re2 <- as.character(All$hhid_re2)
CropAreaC <- All %>% 
  filter(S_Area == "Dien Bien province") %>%
  select(hhid_re2,commune_eng_preload,starts_with("d2_132.")) %>%
  select_if(~ !all(is.na(.))) %>%
  mutate_all( ~ ifelse(is.na(.), 0, .))
colnames(CropAreaC) <- str_replace(colnames(CropAreaC), "d2_132.","")

CropAreaVeggie <- CropAreaC[,c(6:7,9:11,15:21,25,27,32:41,43:48,50:53)]
CropAreaVeggie <- CropAreaVeggie %>%
  mutate(NbPositives = rowSums(select(., 3:(ncol(.) - 1)) > 0))
CropAreaVeggie$Veg <- rowSums(CropAreaVeggie[,c(1:4,6:12,14:35)])
mean(CropAreaVeggie$NbPositives[(CropAreaVeggie$`Summer-autumn season rice` > 0 | CropAreaVeggie$`Winter-Spring season rice` > 0) & CropAreaVeggie$Veg > 0])

CropAreaVeggup <- CropAreaC[,c(23,26,49,54)]
CropAreaVeggup <- CropAreaVeggup %>%
  mutate(NbPositives = rowSums(select(., 3:(ncol(.) - 1)) > 0))
mean(CropAreaVeggup$NbPositives[CropAreaVeggup$NbPositives > 0])

#Matrix of correlation - TOT
resT <- cor(CropAreaC[,c(3:53)], use="pairwise.complete.obs")
resT <- round(resT, 2)
corrplot(resT, type = "upper", 
         tl.col = "black", tl.srt = 45)


#Sum of lowland vegetables are
CropAreaC$Lowveggie <-  CropAreaC$`Long bean, Chinese`+ CropAreaC$`Cucumber - lowl` +
  CropAreaC$`Mustard greens - lowl` + CropAreaC$`Pak choy` + CropAreaC$`Mung bean` +
  CropAreaC$`Garlic - lowl` + CropAreaC$`Spring Onions` + CropAreaC$`Chinese flowering Cabbages/ choysum` +
  CropAreaC$`Cabbages - lowl` + CropAreaC$Carrot + CropAreaC$Kohlrabi +
  CropAreaC$`Sweet potatoes, tuber - lowl`+ CropAreaC$`Winter melon - lowl` +
  CropAreaC$`Potatoes - lowl` + CropAreaC$`Pumpkin - lowl` + CropAreaC$`Hmong mustard` +
  CropAreaC$Tomato + CropAreaC$`Lettuce, romaine, leaves` + CropAreaC$Mustard2 +
  CropAreaC$`Chinese kale/ Gailan` + CropAreaC$`Local bean` + CropAreaC$`Chrysanthemum, leaves` +
  CropAreaC$`Cauliflowers, Broccoli` + CropAreaC$`Turmeric, root` + CropAreaC$`Local mustard` +
  CropAreaC$`Black bean` + CropAreaC$`Basil, sweet, leaves` + CropAreaC$`Sawtooth herb / Culantro` +
  CropAreaC$`Kidney bean` + CropAreaC$`French bean - lowl` + CropAreaC$`Eggplant - lowl`
   
Dummy <- CropAreaC[,c(1,11,25,56)]

Dudu <- Dummy[Dummy$`Summer-autumn season rice` > 0 & Dummy$`Winter-Spring season rice` > 0 & Dummy$Lowveggie > 0,]
x <- Dummy[Dummy$`Summer-autumn season rice` == 0 & Dummy$`Winter-Spring season rice` == 0 & Dummy$Lowveggie >0,]
mean(x$`Summer-autumn season rice`)


CropAreaC$Highveggie <- CropAreaC$Kudzu + CropAreaC$`Eggplant - upln`
x <- CropAreaC$Highveggie[CropAreaC$Highveggie >0]

CropAreaC$Trees <- CropAreaC$Longan + CropAreaC$Bananas +
  CropAreaC$Peach + CropAreaC$Oranges + CropAreaC$`Pomelos and grapefruits` +
  CropAreaC$Littchi

x <- CropAreaC$Trees2[CropAreaC$Trees2 > 0]

CropDesc[,(37:95)] <- as.numeric(CropDesc[,(37:95)])
CropDesc$sumAEP <- sum(CropDesc[,(37:95)])

mean(Dummy$Lowveggie[Dummy$Lowveggie >0])
mean(Dudu$Lowveggie)

mean(Dudu$`Summer-autumn season rice`) + mean(Dudu$`Winter-Spring season rice`)
mean(Dede$`Winter-Spring season rice`)


#Analysis crop per crop
maizegrai <- All %>%
  filter(S_Area == "Dien Bien province") %>%
  select("hhid_re2", matches("Maize corn"))
colnames(maizegrai) <- str_replace(colnames(maizegrai),".Plum","")

maizegraiSell <- mean(maizegrai$`d2_132.Maize corn`[maizegrai$`d2_136.Maize corn` == 0 & !is.na(maizegrai$`d2_136.Maize corn`)])


mean(maizegrai$d2_132[maizegrai$d2_135 == 0], na.rm = T)
mean(maizegrai$d2_132[maizegrai$d2_135 > 0 & maizegrai$], na.rm = T)

for (i in 20:78){
  maizegrai[,i] <- as.numeric(maizegrai[,i])
}
maizegrai$SumPrac <- rowSums(maizegrai[,20:78], na.rm = T)
mean(maizegrai$d2_132[maizegrai$d2_136 > 0], na.rm = T)
mean(maizegrai$d2_ha135, na.rm = T)

maizegrai <- maizegrai[!is.na(maizegrai$d2_132),]
maizegraiSelling <- maizegrai[maizegrai$d2_136 > 0,]
maizegraiNoSelling <- maizegrai[maizegrai$d2_136 == 0,]
summary(maizegraiSelling[,c(4,6:7,9:18)])

