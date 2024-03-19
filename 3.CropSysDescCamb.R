
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

##a. Cambodia
#Data preparation
All$hhid_re2 <- as.character(All$hhid_re2)
CropAreaC <- All %>% 
  filter(S_Area == "Cambodia") %>%
  select(hhid_re2,commune_eng_preload,starts_with("d2_132.")) %>%
  select_if(~ !all(is.na(.))) %>%
  mutate_all( ~ ifelse(is.na(.), 0, .))
colnames(CropAreaC) <- str_replace(colnames(CropAreaC), "d2_132.","")

CropAreaVeggie <- CropAreaC[,c(3,20:22,25,27:29)]
CropAreaVeggie <- CropAreaVeggie %>%
  mutate(NbPositives = rowSums(select(., 1:(ncol(.) - 1)) > 0))
mean(CropAreaVeggie$NbPositives[CropAreaVeggie$NbPositives > 1])


#Matrix of correlation - TOT
resT <- cor(CropAreaC[,c(3:30)], use="pairwise.complete.obs")
resT <- round(resT, 2)
corrplot(resT, type = "upper", 
         tl.col = "black", tl.srt = 45)
x <- CropAreaC[,c(5:9,23:24)]
x$Dum <- rowSums(x)
sum(x$Dum == 0)
#Diversity of cropping systems - RICE (When necesary, replace broadcast & transplant)
#We isolate area of rice systems and other lowland crops
RiceAreaAssC <- CropAreaC %>%
  select(hhid_re2,commune_eng_preload,`Wet season rice broadcast`, `Wet season rice transplant`,
         `Peanut - lowl`,`Melon local, young`,`Savoy cabbage curly`,
         `Long bean, Chinese`, `Cucumber - lowl`,`Water spinach`,
         `Mustard greens - lowl`,`Pak choy`,Amaranth)
#we create a new column to identify what kind of systems depending on broadcasted, transplanted or both
RiceAreaAssC$RiceSa <- ifelse(RiceAreaAssC$`Wet season rice broadcast` != 0 &
                                RiceAreaAssC$`Wet season rice transplant` != 0,
                              "B&T",ifelse(RiceAreaAssC$`Wet season rice broadcast` == 0 &
                                 RiceAreaAssC$`Wet season rice transplant` != 0,
                                "T",ifelse(RiceAreaAssC$`Wet season rice broadcast` != 0 &
                                             RiceAreaAssC$`Wet season rice transplant` == 0,
                                           "B","/")))
RiceAreaAssC$RiceTot <- RiceAreaAssC$`Wet season rice broadcast` + RiceAreaAssC$`Wet season rice transplant`
#% of each systems
RiceAreaAssC %>%
  count(RiceSa) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))
#Plot of area dispersion for each kind of systems
ggplot(RiceAreaAssC, aes(x = RiceSa, y = RiceTot)) +
  geom_point() +
  labs(x = "RiceType", y = "Total rice area")
#We will run a PCA, first we select the data related to rice
RicePCA <- All %>% 
  filter(S_Area == "Cambodia") %>%
  select(hhid_re2,commune_eng_preload,matches("Wet season rice transplant"))
summary(RicePCA[,11])
292/475


summary(as.factor(RicePCA[,10]))
#We add a column including the sum of total practices
for (i in 20:78){
  RicePCA[,i] <- as.numeric(RicePCA[,i])
}
RicePCA$SumPrac <- rowSums(RicePCA[,20:78], na.rm = T)
#And a column with % of production sold
RicePCA$ProdSoldPc <- ifelse(RicePCA$`d2_136.Wet season rice broadcast` / RicePCA$`d2_135.Wet season rice broadcast` <= 1,
                             round(RicePCA$`d2_136.Wet season rice broadcast` / RicePCA$`d2_135.Wet season rice broadcast`, digits = 2),
                             1)
#We select useful columns
RicePCA <- RicePCA[,c(1:2,5,11,13:14,18,19,79:81,84,85)]
#Remove households who do not grow rice
RicePCA <- RicePCA %>%
  filter(!is.na(`d2_132.Wet season rice broadcast`))
#Replace NA by "NA" for PCA
for (i in 7:11){
  RicePCA[,i] <- as.character(RicePCA[,i])
  RicePCA[,i] <- ifelse(is.na(RicePCA[,i]), "NA", RicePCA[,i])
  RicePCA[,i] <- as.factor(RicePCA[,i])
}
#Replace NA by 0 for quantitative variables
for (i in c(4,13)){
 RicePCA[,i] <- ifelse(is.na(RicePCA[,i]), 0, RicePCA[,i])
}
#Remove useless information from columns names
colnames(RicePCA) <- str_replace(colnames(RicePCA), "Wet season rice broadcast", "")
#Remove useless information
RicePCA <- RicePCA[,-(8:11)]
#FAMD
res <- FAMD(RicePCA[,c(3:9)])
summary(res)

#Additional
summary(RicePCA[RicePCA$d2_137. == 0,])
summary(RicePCA[RicePCA$d2_137. != 0,])
RiceAreaAssC$NonRiceTot <- rowSums(RiceAreaAssC[,c(5:13)])
RicePCA$hhid_re2 <- as.character(RicePCA$hhid_re2)
Vegerice <- subset(RicePCA, hhid_re2 %in% c("2149","2519","2330","2500","2493"))

#Diversity of cropping systems - FRUIT
FruitAreaC <- CropAreaC %>%
  select(hhid_re2,commune_eng_preload,Longan, Guava, Coconuts, Sweetsop,
         Lime, Mangoes, Bananas)
resF <- cor(FruitAreaC[,c(3:9)], use="pairwise.complete.obs")
resF <- round(resF, 2)
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)

#Diversity of cropping systems - UPLAND CROPS
#Cashew nut
CashewSum <- All %>% 
  filter(S_Area == "Cambodia") %>%
  select(hhid_re2,commune_eng_preload,matches("Cashew nut"))
mean(CashewSum$`d2_ha136.Cashew nut`, na.rm = T)
ggplot(CashewSum, aes(y = `d2_ha136.Cashew nut`, x = `d32_11.Cashew nut`)) +
  geom_point() +
  labs(x = "Commune", y = "Cashew density")
for (i in 20:78){
  CashewSum[,i] <- as.numeric(CashewSum[,i])
}
CashewSum$SumPrac <- rowSums(CashewSum[,20:78], na.rm = T)
CashewSum$ShareSold <- ifelse(CashewSum$`d2_136.Cashew nut` / CashewSum$`d2_135.Cashew nut` <= 1,
                              round(CashewSum$`d2_136.Cashew nut` / CashewSum$`d2_135.Cashew nut`, digits = 2),
                              1)
#PCA
CashewPCA <- CashewSum[,c(1:2,5,11,13:15,84:85)]
CashewPCA$ShareSold <- ifelse(is.na(CashewPCA$ShareSold), "0",CashewPCA$ShareSold)

colnames(CashewPCA) <- str_replace(colnames(CashewPCA), "Cashew nut","")
CashewPCA <- CashewPCA[!is.na(CashewPCA$`d2_132.`),]
res.pca <- PCA(CashewPCA[,3:8], scale.unit = T, ncp = 15, graph = TRUE)
fviz_pca_biplot(res.pca, ggtheme = theme_minimal())

#Additionnal analyses
CashewSumYoung <- CashewPCA[CashewPCA$`d2_ha135.Cashew nut` == 0,]
summary(CashewSumYoung)
CashewSumComm <- CashewPCA[CashewPCA$`d2_ha135.Cashew nut` != 0,]
summary(CashewSumComm)

#For other upland crops (we replace the crop name each time) 
UplandCropSum <- All %>% 
  filter(S_Area == "Cambodia") %>%
  select(hhid_re2,commune_eng_preload,matches("Upland rice2"))
colnames(UplandCropSum) <- str_replace(colnames(UplandCropSum), "Upland rice2","")
UplandCropSum <- UplandCropSum %>%
  filter(!is.na(d2_132.))
for (i in 20:78){
  UplandCropSum[,i] <- as.numeric(UplandCropSum[,i])
}
UplandCropSum$SumPrac <- rowSums(UplandCropSum[,20:78], na.rm = T)
UplandCropSum$ShareSold <- ifelse(UplandCropSum$`d2_136.` / UplandCropSum$`d2_135.` <= 1,
                              round(UplandCropSum$`d2_136.` / UplandCropSum$`d2_135.`, digits = 2),
                              1)
UplandCropSum2 <- UplandCropSum[,c(1:2,5,13:19,79:82,84:85)]
DummSell <- UplandCropSum2[!is.na(UplandCropSum2$GrossProductRaw.),]
DummEat <- UplandCropSum2[is.na(UplandCropSum2$GrossProductRaw.),]
summary(DummEat)
#PCA
res.pca <- PCA(UplandCropSum2[,c(3:5,15)], scale.unit = T, ncp = 15, graph = TRUE)
summary(res.pca)
fviz_pca_biplot(res.pca, ggtheme = theme_minimal())
summary(UplandCropSum2)
#Additionnal
Dumm <- All %>%
  select(`d2_132.Cassava2`,`d2_132.Cassava - mixed cropping`)
