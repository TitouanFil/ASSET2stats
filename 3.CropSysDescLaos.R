
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
  filter(S_Area == "Lao") %>%
  select(hhid_re2,commune_eng_preload,starts_with("d2_132.")) %>%
  select_if(~ !all(is.na(.))) %>%
  mutate_all( ~ ifelse(is.na(.), 0, .))
colnames(CropAreaC) <- str_replace(colnames(CropAreaC), "d2_132.","")

CropAreaVeggie <- CropAreaC[,c(4:5,7:8,12,18,20:40,42:43,46:47,49:53,55:56)]
CropAreaVeggie <- CropAreaVeggie %>%
  mutate(NbPositives = rowSums(select(., 3:(ncol(.) - 1)) > 0))

mean(CropAreaVeggie$NbPositives[CropAreaVeggie$NbPositives > 1 & CropAreaVeggie$`Summer-autumn season rice` == 0])

#Matrix of correlation - TOT
resT <- cor(CropAreaC[,c(3:56)], use="pairwise.complete.obs")
resT <- round(resT, 2)
corrplot(resT, type = "upper", 
         tl.col = "black", tl.srt = 45)


CropAreaC$Pere <- CropAreaC$`Tea leaves` + CropAreaC$`Strawberry - upln` +
  CropAreaC$`Strawberry - lowl` + CropAreaC$`Ginger - lowl`+ CropAreaC$`Ginger - upln`
sum(CropAreaC$Corn > 0)
CropAreaC$Ann <- CropAreaC$`Upland rice2` + CropAreaC$`Peanut - upln` +
  CropAreaC$`jobs_tears - lowl` + CropAreaC$`jobs_tears - upln`
CropAreaC$Corn <- CropAreaC$maize_hybrid + CropAreaC$maize_traditional

#Sum of lowland vegetables are
CropAreaC$Lowveggie <-
  CropAreaC$`Basil, sweet, leaves` +
  CropAreaC$`Bell pepper, red` +
  CropAreaC$`Black bean` +
  CropAreaC$`Cabbages - lowl` +
  CropAreaC$Carrot +
  CropAreaC$`Cauliflowers, Broccoli` +
  CropAreaC$`Chili - lowl` +
  CropAreaC$`Chinese flowering Cabbages/ choysum` +
  CropAreaC$`Chinese kale/ Gailan` +
  CropAreaC$`Chinese lettuce - lowl` +
  CropAreaC$`Chinese plantain, leaves` +
  CropAreaC$`Chrysanthemum, leaves` +
  CropAreaC$`Coriander - lowl` +
  CropAreaC$`Cucumber - lowl` +
  CropAreaC$Dill +
  CropAreaC$`Eggplant - lowl` +
  CropAreaC$`French bean - lowl` +
  CropAreaC$`Garlic - lowl` +
  CropAreaC$gourd +
  CropAreaC$`Ginger - lowl` +
  CropAreaC$`Hmong mustard` +
  CropAreaC$`kale_flower - lowl` +
  CropAreaC$`Kidney bean` +
  CropAreaC$Kohlrabi +
  CropAreaC$`Lettuce, romaine, leaves` +
  CropAreaC$`Local bean` +
  CropAreaC$`Local mustard` + 
  CropAreaC$`Long bean - lowl` +
  CropAreaC$`Long bean, Chinese` +
  CropAreaC$`Mung bean` +
  CropAreaC$Mustard2 +
  CropAreaC$`Mustard greens - lowl` +
  CropAreaC$`Napa Cabbages` +
  CropAreaC$Onions2 +
  CropAreaC$`Pak choy` + 
  CropAreaC$`Peanut - lowl` +
  CropAreaC$`Potatoes - lowl` +
  CropAreaC$`Pumpkin - lowl`+
  CropAreaC$`Salad/lettuce` +
  CropAreaC$`Sawtooth herb / Culantro` +
  CropAreaC$`Spring Onions` +
  CropAreaC$`Strawberry - lowl` +
  CropAreaC$`Sweet potatoes, tuber - lowl`+
  CropAreaC$Tomato +
  CropAreaC$`Turmeric, root` +
  CropAreaC$`Winter melon - lowl` +
  CropAreaC$`Zucchini, common green, fruit`
  
     
Dummy <- CropAreaC[,c(1,15,65,109)]
#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(Dummy, "RnV_TF.rds")

x <- Dummy$hhid_re2[Dummy$`Summer-autumn season rice` > 0 & Dummy$Lowveggie > 0]
y <- Dummy$hhid_re2[Dummy$`Summer-autumn season rice` > 0 & Dummy$Lowveggie == 0]
z <- Dummy$hhid_re2[Dummy$`Summer-autumn season rice` == 0 & Dummy$Lowveggie > 0]
Dududu <- Dummy[Dummy$`Summer-autumn season rice` == 0 & Dummy$Lowveggie > 0,]

#Analysis crop per crop
maizetrad <- All %>%
  filter(S_Area == "Lao") %>%
  select("hhid_re2", matches("maize_traditional"))
colnames(maizetrad) <- str_replace(colnames(maizetrad),".maize_traditional","")
maizetrad <- maizetrad[!is.na(maizetrad$d2_132),]
maizetradSelling <- maizetrad[maizetrad$d2_136 > 0,]
maizetradNoSelling <- maizetrad[maizetrad$d2_136 == 0,]
summary(maizetradSelling[,c(4,6:7,9:18)])

15/249
