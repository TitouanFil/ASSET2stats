### 1 - Prerequisites


## a - Package loading
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
library(tibble)
library(RColorBrewer)
library(labelled)
library(sjlabelled)
library(stringr)
library(tidyr)
library(survey)
#Options
options(warn=1)

## b - Work directory and data loading
#We use the output datasets which were displayed during previous mission
#to check if the datasets are clean
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
CropSystems3C_TF <- readRDS("Newdata_TF.rds")


#Cropping and raising systems list per country
{
##Cambodia
#"RtcC" =  Transplanted rice Household consumption
#"RtsC" = Transplanted rice Household consumption and selling
#"RbcC" = Broadcasted rice Household consumption
#"RbsC" = Broadcasted rice Household consumption and selling
#"RVLC" = Br or Tr rice with off-season vegetables
#"CWpC" = Cashew productive plantations
#"CWiC" = Cashew young plantation with annual crops intercropping
#"CWyC" = Cashew young plantation no intercropping
#"CA$C" = Cassava monocropping commercial
#"SO$C" = Soybean monocropping commercial
#"URcC" = Upland rice monocropping self-c and selling
#"FTpC" = Fruit trees productive
#"FTyC" = Young fruit trees
#"FToC" = Fruit homegarden
#"RUlC" = Cattle/Buffalo medium-large scale
#"RUsC" = Cattle/Buffalo small-medium scale
#"PmaC" = Pigs fattening/piglets buying feed from the market
#"PpkC" = piglets raising using kitchen waste, crop residues
#"PfkC" = pig fattening using kitchen waste, crop residues
#"POlC" = Poultry large scale
#"POsC" = Poultry small scale
  
##Laos
#"RlaL" = Summer-Autumn season rice monocropping
#"RVoL" = Summer-Autumn season rice with off-season vegetables cropping
#"VulL" = Vegetables on upland/lowland
#"FTRL" = Fruit trees
#"TeaL" = Tea plantation
#"GinL" = Ginger plantation
#"StrL" = Strawberry plantation"
#"URcL" = Upland rice monocropping for consumption
#"URsL" = Upland rice monocropping for consumption and selling
#"PEAL" = Peanut monocropping
#"JOTL" = Job tears monocropping
#"MtcL" = Maize traditionnal for consumption
#"MtsL" = Maize traditionnal for consumption and selling
#"MhsL" = Maize hybrid for commercial purposes
#"RUlL" = Cattle/Buffalo medium-large scale
#"RUsL" = Cattle/Buffalo small-medium scale
#"PmaL" = Pigs fattening/piglets buying feed from the market
#"PpkL" = piglets raising using kitchen waset, crop residues
#"PfkL" = pig fattening using kitchen waste, crop residues
#"POeL" = Poultry large scale eggs
#"POlL" = Poultry large scale meat
#"POsL" = Poultry small scale meat
#"GOTL" = Goats
  
##Vietnam - Son La province
#"RdbS" = Summer-Autumn season rice and winter-spring season rice in a same year
#"RVoS" = Summer-Autumn season rice with off-season vegetables cropping  
#"RsaS" = Summer-Autumn season rice monocropping
#"RwsS" = Winter-spring season rice monocropping
#"VloS" = Lowland vegetables
#"FRAS" = Fruit trees Apricot
#"FRMS" = Fruit trees Mango
#"FRPS" = Fruit trees Plum
#"FRLS" = Fruit trees Longan
#"FROS" = Fruit trees Other species
#"ArrS" = Arrowroot plantation
#"GinS" = Ginger plantation
#"TeaS" = Tea plantation
#"VupS" = Upland vegetables
#"CAcS" = Cassava monocropping consumption
#"CAsS" = Cassava monocropping consumption selling
#"URcS" = Upland rice monocropping self-c and selling
#"MgcS" = Maize traditionnal for consumption
#"MgsS" = Maize traditionnal for consumption and selling
#"MbiS" = Maize hybrid for commercial purposes
#"RUlS" = Cattle/Buffalo medium-large scale
#"RUsS" = Cattle/Buffalo small-medium scale
#"PmaS" = Pigs fattening/piglets buying feed from the market
#"PpkS" = piglets raising using kitchen waset, crop residues
#"PfkS" = pig fattening using kitchen waste, crop residues
#"POlS" = Poultry large scale
#"POsS" = Poultry small scale
#"GOTS" = Goats

  
##Vietnam - Dien Bien province
#"RdbD" = Summer-Autumn season rice and winter-spring season rice in a same year
#"RVoD" = Summer-Autumn season rice with off-season vegetables cropping  
#"RsaD" = Summer-Autumn season rice monocropping
#"RwsD" = Winter-spring season rice monocropping
#"FTMD" = Fruit trees miscellaneous
#"VupD" = Upland vegetables
#"CAsD" = Cassava monocropping consumption selling
#"URcD" = Upland rice monocropping self-consumption
#"MgcD" = Maize traditionnal for consumption
#"MgsD" = Maize traditionnal for consumption and selling
#"MbiD" = Maize hybrid for commercial purposes
#"RUcD" = Cattle/Buffalo confined
#"RUgD" = Cattle/Buffalo daily grazing
#"PmaD" = Pigs fattening/piglets buying feed from the market
#"PkiD" = piglets/fattening raising using kitchen waset, crop residues
#"POlD" = Poultry large scale for selling meat or eggs
#"POsD" = Poultry small scale
}


Guide <- data.frame(
Country = c(rep("Cambodia", times = 19),rep("Lao", times = 23),
            rep("Son La province", times = 28),rep("Dien Bien province", times = 17)),
Sys = c("RtcC","RtsC","RbcC","RbsC","RVLC","CWpC","CWiC","CWyC","CA$C","SO$C","URcC",
        "FTmC","RUlC","RUsC","PmaC","PpkC","PfkC","POlC","POsC","RlaL","RVoL","VulL",
        "FTRL","TeaL","GinL","StrL","URcL","URsL","PEAL","JOTL","MtcL","MtsL","MhsL","RUlL",
        "RUsL","PmaL","PpkL","PfkL","POeL","POlL","POsL","GOTL","RdbS","RVoS",
        "RsaS","RwsS","VloS","FRAS","FRMS","FRPS","FRLS","FROS","ArrS","GinS",
        "TeaS","VupS","CAcS","CAsS","URcS","MgcS","MgsS","MbiS","RUlS","RUsS",
        "PmaS","PpkS","PfkS","POlS","POsS","GOTS","RdbD","RVoD","RsaD","RwsD",
        "FTMD","VupD","CAsD","URcD","MgcD","MgsD","MbiD","RUcD","RUgD","PmaD",
        "PkiD","POlD","POsD"),
text = c('ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Wet season rice transplant` > 0 &
         CropSystems3C_TF$`d2_136.Wet season rice transplant` == 0,CropSystems3C_TF$`d2_132.Wet season rice transplant`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Wet season rice transplant` > 0 &
         CropSystems3C_TF$`d2_136.Wet season rice transplant` > 0,CropSystems3C_TF$`d2_132.Wet season rice transplant`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Wet season rice broadcast` > 0 &
         CropSystems3C_TF$`d2_136.Wet season rice broadcast` == 0,CropSystems3C_TF$`d2_132.Wet season rice broadcast`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Wet season rice broadcast` > 0 &
         CropSystems3C_TF$`d2_136.Wet season rice broadcast` > 0,CropSystems3C_TF$`d2_132.Wet season rice broadcast`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$hhid_re2 %in% c("2500","2493","2330","2519"),1,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Cashew nut` > 0 &
         CropSystems3C_TF$`d2_135.Cashew nut` > 0,CropSystems3C_TF$`d2_132.Cashew nut`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Cashew - mixed cropping` > 0,CropSystems3C_TF$`d2_132.Cashew - mixed cropping`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Cashew nut` > 0 &
         CropSystems3C_TF$`d2_135.Cashew nut` == 0,CropSystems3C_TF$`d2_132.Cashew nut`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Cassava2` > 0,CropSystems3C_TF$`d2_132.Cassava2`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Soybean2` > 0,CropSystems3C_TF$`d2_132.Soybean2`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$`d2_132.Upland rice2` > 0,CropSystems3C_TF$`d2_132.Upland rice2`,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & (CropSystems3C_TF$hhid_re2 %in% c(2953,2212,2671) |
       CropSystems3C_TF$d2_132.Sweetsop > 0 | 
       CropSystems3C_TF$d2_132.Lime > 0 |
       CropSystems3C_TF$d2_132.Mangoes > 0 |
       CropSystems3C_TF$d2_132.Coconuts > 0 |
       CropSystems3C_TF$d2_132.Bananas > 0),1,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 24 &
       rowSums(CropSystems3C_TF[, c("e5_4.Buffalo", "e5_4.Cattle")], na.rm = TRUE) > 3 ,rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & rowSums(CropSystems3C_TF[, c("e3_1","e3_2","e3_3","e3_4")], na.rm = TRUE) > 0 &
       !(rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 25 &
       rowSums(CropSystems3C_TF[, c("e5_4.Buffalo", "e5_4.Cattle")], na.rm = TRUE) > 3), rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & (CropSystems3C_TF$e30 == "Piglet family raising, but buying feed from market" |
       CropSystems3C_TF$e30 == "Fattening family raising, but buying feed from market"),CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$e30 == "Piglet family raising using only local feed stuffs and kitchen waste",CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & CropSystems3C_TF$e30 != "Piglet family raising using only local feed stuffs and kitchen waste" &
       CropSystems3C_TF$e30 != "Piglet family raising, but buying feed from market" & CropSystems3C_TF$e30 != "Fattening family raising, but buying feed from market" &
       CropSystems3C_TF$e3_5 > 0,CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE) > 99,rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Cambodia" & rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE) > 0 &
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE) < 100,rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = T),0)',
       
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$`d2_132.Summer-autumn season rice` > 0 &
         is.na(CropSystems3C_TF$'d2_132.Peanut - lowl' > 0 | 
         CropSystems3C_TF$'d2_132.Long bean - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Chinese lettuce - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Chili - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Onions2' > 0 |
         CropSystems3C_TF$'d2_132.Coriander - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Chinese flowering Cabbages/ choysum' > 0 |
         CropSystems3C_TF$'d2_132.Napa Cabbages' > 0 |
         CropSystems3C_TF$'d2_132.Dill' > 0 |
         CropSystems3C_TF$'d2_132.Bell pepper, red' > 0 |
         CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Mustard greens - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Mung bean' > 0 |
         CropSystems3C_TF$'d2_132.Garlic - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Spring Onions' > 0 |
         CropSystems3C_TF$'d2_132.Salad/lettuce' > 0 |
         CropSystems3C_TF$'d2_132.Cabbages - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Carrot' > 0 |
         CropSystems3C_TF$'d2_132.kale_flower - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Hmong Cucumber' > 0 |
         CropSystems3C_TF$'d2_132.gourd' > 0),CropSystems3C_TF$`d2_132.Summer-autumn season rice`,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$`d2_132.Summer-autumn season rice` > 0 &
       (CropSystems3C_TF$'d2_132.Peanut - lowl' > 0 | 
       CropSystems3C_TF$'d2_132.Long bean - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chinese lettuce - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chili - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Onions2' > 0 |
       CropSystems3C_TF$'d2_132.Coriander - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chinese flowering Cabbages/ choysum' > 0 |
       CropSystems3C_TF$'d2_132.Napa Cabbages' > 0 |
       CropSystems3C_TF$'d2_132.Dill' > 0 |
       CropSystems3C_TF$'d2_132.Bell pepper, red' > 0 |
       CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Mustard greens - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Mung bean' > 0 |
       CropSystems3C_TF$'d2_132.Garlic - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Spring Onions' > 0 |
       CropSystems3C_TF$'d2_132.Salad/lettuce' > 0 |
       CropSystems3C_TF$'d2_132.Cabbages - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Carrot' > 0 |
       CropSystems3C_TF$'d2_132.kale_flower - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Hmong Cucumber' > 0 |
       CropSystems3C_TF$'d2_132.gourd' > 0),CropSystems3C_TF$`d2_132.Summer-autumn season rice`,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & (CropSystems3C_TF$`d2_132.Summer-autumn season rice` == 0 &
       (CropSystems3C_TF$'d2_132.Chinese lettuce - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chili - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Onions2' > 0 |
       CropSystems3C_TF$'d2_132.Coriander - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chinese flowering Cabbages/ choysum' > 0 |
       CropSystems3C_TF$'d2_132.Napa Cabbages' > 0 |
       CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Garlic - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Spring Onions' > 0 |
       CropSystems3C_TF$'d2_132.Salad/lettuce' > 0 |
       CropSystems3C_TF$'d2_132.Hmong Cucumber' > 0)) |
       (CropSystems3C_TF$'d2_132.Chili - upln' > 0 |
       CropSystems3C_TF$'d2_132.Cucumber - upln' > 0 |
       CropSystems3C_TF$'d2_132.Pumpkin - upln' > 0 |
       CropSystems3C_TF$'d2_132.Eggplant - upln' > 0 |
       CropSystems3C_TF$'d2_132.Mustard greens - upln' > 0 |
       CropSystems3C_TF$'d2_132.Chinese lettuce - upln' > 0 |
       CropSystems3C_TF$'d2_132.Garlic - upln' > 0 |
       CropSystems3C_TF$'d2_132.Coriander - upln' > 0 |
       CropSystems3C_TF$'d2_132.kale_flower - upln' > 0 |
       CropSystems3C_TF$'d2_132.Long bean - upln' > 0 |
       CropSystems3C_TF$'d2_132.taro' > 0 |
       CropSystems3C_TF$'d2_132.Chinese plantain, leaves' > 0),
       rowSums(CropSystems3C_TF[, c('d2_132.Chinese lettuce - lowl',
       'd2_132.Chili - lowl','d2_132.Onions2','d2_132.Coriander - lowl',
       'd2_132.Chinese flowering Cabbages/ choysum','d2_132.Napa Cabbages',
       'd2_132.Cucumber - lowl','d2_132.Garlic - lowl','d2_132.Spring Onions',
       'd2_132.Salad/lettuce','d2_132.Hmong Cucumber','d2_132.Chili - upln',
       'd2_132.Cucumber - upln','d2_132.Pumpkin - upln','d2_132.Eggplant - upln',
       'd2_132.Mustard greens - upln','d2_132.Chinese lettuce - upln',
       'd2_132.Garlic - upln','d2_132.Coriander - upln',
       'd2_132.kale_flower - upln','d2_132.Long bean - upln',
       'd2_132.taro','d2_132.Chinese plantain, leaves')], na.rm = TRUE),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & (CropSystems3C_TF$d2_132.Plum > 0 |
         CropSystems3C_TF$d2_132.Peach > 0), rowSums(CropSystems3C_TF[, c('d2_132.Plum', 'd2_132.Peach')], na.rm = TRUE),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$'d2_132.Tea leaves' > 0,
       CropSystems3C_TF$'d2_132.Tea leaves',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & (CropSystems3C_TF$'d2_132.Ginger - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Ginger - upln' > 0), rowSums(CropSystems3C_TF[, c('d2_132.Ginger - lowl', 'd2_132.Ginger - upln')], na.rm = TRUE),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & (CropSystems3C_TF$'d2_132.Strawberry - lowl' > 0 |
         CropSystems3C_TF$'d2_132.Strawberry - upln' > 0), rowSums(CropSystems3C_TF[, c('d2_132.Strawberry - lowl', 'd2_132.Strawberry - upln')], na.rm = TRUE),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$'d2_132.Upland rice2' > 0 &
       CropSystems3C_TF$'d2_136.Upland rice2' == 0 ,CropSystems3C_TF$'d2_132.Upland rice2',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$'d2_132.Upland rice2' > 0 &
       CropSystems3C_TF$'d2_136.Upland rice2' > 0 ,CropSystems3C_TF$'d2_132.Upland rice2',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$'d2_132.Peanut - upln' > 0,
       CropSystems3C_TF$'d2_132.Peanut - upln',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & (CropSystems3C_TF$`d2_132.jobs_tears - lowl` > 0 |
         CropSystems3C_TF$`d2_132.jobs_tears - upln` > 0), rowSums(CropSystems3C_TF[, c('d2_132.jobs_tears - lowl', 'd2_132.jobs_tears - upln')], na.rm = TRUE),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$'d2_132.maize_traditional' > 0 &
       CropSystems3C_TF$'d2_136.maize_traditional' == 0 ,CropSystems3C_TF$'d2_132.maize_traditional',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$'d2_132.maize_traditional' > 0 &
       CropSystems3C_TF$'d2_136.maize_traditional' > 0 ,CropSystems3C_TF$'d2_132.maize_traditional',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Lao' & CropSystems3C_TF$'d2_132.maize_hybrid' > 0,
       CropSystems3C_TF$'d2_132.maize_hybrid',0)",
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 19 &
       rowSums(CropSystems3C_TF[, c("e12_2", "e12_4", "e12_6")], na.rm = TRUE) > 4999 ,rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & rowSums(CropSystems3C_TF[, c("e3_1","e3_2","e3_3","e3_4")], na.rm = TRUE) > 0 &
       !(rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 19 &
       rowSums(CropSystems3C_TF[, c("e12_2", "e12_4", "e12_6")], na.rm = TRUE) > 4999), rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & (CropSystems3C_TF$e30 == "Piglet family raising, but buying feed from market" |
       CropSystems3C_TF$e30 == "Fattening family raising, but buying feed from market"),CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & CropSystems3C_TF$e30 == "Piglet family raising using only local feed stuffs and kitchen waste",CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & !(CropSystems3C_TF$e30 == "Piglet family raising using only local feed stuffs and kitchen waste" |
       CropSystems3C_TF$e30 == "Piglet family raising, but buying feed from market" | CropSystems3C_TF$e30 == "Fattening family raising, but buying feed from market") &
       CropSystems3C_TF$e3_5 > 0,CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & (CropSystems3C_TF$e44 == "Family raising but larger scale for selling eggs"),
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & (CropSystems3C_TF$e44 == "Family raising but larger scale for selling meat"),
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & !(CropSystems3C_TF$e44 == "Family raising but larger scale for selling meat" |
       CropSystems3C_TF$e44 == "Family raising but larger scale for selling eggs") &
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE) > 0,
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Lao" & CropSystems3C_TF$e3_7 > 0,
       CropSystems3C_TF$e3_7,0)',
       
       
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$`d2_132.Summer-autumn season rice` > 0 &
       CropSystems3C_TF$`d2_132.Winter-Spring season rice` > 0, rowSums(CropSystems3C_TF[, c('d2_132.Summer-autumn season rice','d2_132.Winter-Spring season rice')], na.rm = T)/2,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & rowSums(CropSystems3C_TF[, c('d2_132.Summer-autumn season rice','d2_132.Winter-Spring season rice')], na.rm = T) > 0 &
       (CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 | 
       CropSystems3C_TF$'d2_132.Pumpkin - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Winter melon - lowl' > 0),
       rowSums(CropSystems3C_TF[, c('d2_132.Summer-autumn season rice','d2_132.Winter-Spring season rice')], na.rm = T),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$`d2_132.Summer-autumn season rice` > 0 &
       is.na(CropSystems3C_TF$`d2_132.Winter-Spring season rice`) &
       is.na(CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 | 
       CropSystems3C_TF$'d2_132.Pumpkin - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Winter melon - lowl' > 0),
       CropSystems3C_TF$'d2_132.Summer-autumn season rice',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$`d2_132.Winter-Spring season rice` > 0 &
       is.na(CropSystems3C_TF$`d2_132.Summer-autumn season rice`) &
       is.na(CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 | 
       CropSystems3C_TF$'d2_132.Pumpkin - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Winter melon - lowl' > 0),
       CropSystems3C_TF$'d2_132.Winter-Spring season rice',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & is.na(CropSystems3C_TF$`d2_132.Winter-Spring season rice` == 0) &
               is.na(CropSystems3C_TF$`d2_132.Summer-autumn season rice`) &
               (CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 |
                CropSystems3C_TF$'d2_132.Mustard greens - lowl' > 0 |
                CropSystems3C_TF$'d2_132.Coriander - lowl' > 0 |
                CropSystems3C_TF$'d2_132.Cabbages - lowl' > 0 |
                CropSystems3C_TF$'d2_132.Tomato' > 0 |
                CropSystems3C_TF$'d2_132.French bean - lowl' > 0 |
                CropSystems3C_TF$'d2_132.Spring Onions' > 0 |
                CropSystems3C_TF$'d2_132.Chinese flowering Cabbages/ choysum' > 0 |
                CropSystems3C_TF$'d2_132.Winter melon - lowl' > 0 |
                CropSystems3C_TF$'d2_132.Hmong mustard' > 0 |
                CropSystems3C_TF$'d2_132.Local bean' > 0 |
                CropSystems3C_TF$'d2_132.Local mustard' > 0 |
                CropSystems3C_TF$'d2_132.Rice bean' > 0 |
                CropSystems3C_TF$'d2_132.Zucchini, common green, fruit' > 0),
                rowSums(CropSystems3C_TF[, c('d2_132.Cucumber - lowl','d2_132.Mustard greens - lowl',
                'd2_132.Coriander - lowl','d2_132.Cabbages - lowl','d2_132.Tomato','d2_132.French bean - lowl',
                'd2_132.Spring Onions','d2_132.Chinese flowering Cabbages/ choysum',
                'd2_132.Winter melon - lowl','d2_132.Hmong mustard','d2_132.Local bean',
                'd2_132.Local mustard','d2_132.Rice bean','d2_132.Zucchini, common green, fruit')], na.rm = TRUE),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$d2_132.Apricot > 0,
       CropSystems3C_TF$d2_132.Apricot,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$d2_132.Mangoes > 0,
       CropSystems3C_TF$d2_132.Mangoes,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$d2_132.Plum > 0,
       CropSystems3C_TF$d2_132.Plum,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$d2_132.Longan > 0,
       CropSystems3C_TF$d2_132.Longan,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' &
       (CropSystems3C_TF$d2_132.Avocado > 0 |
       CropSystems3C_TF$d2_132.Chayote > 0 |
       CropSystems3C_TF$d2_132.Coconuts > 0 |
       CropSystems3C_TF$'d2_132.Jack fruit' > 0 |
       CropSystems3C_TF$d2_132.Oranges > 0 |
       CropSystems3C_TF$'d2_132.Passion fruit' > 0 |
       CropSystems3C_TF$'d2_132.Peach' > 0 |
       CropSystems3C_TF$'d2_132.Pomelos and grapefruits' > 0),
       rowSums(CropSystems3C_TF[, c('d2_132.Avocado','d2_132.Chayote',
       'd2_132.Coconuts','d2_132.Jack fruit','d2_132.Oranges',
       'd2_132.Passion fruit','d2_132.Peach','d2_132.Pomelos and grapefruits')],na.rm = T),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$'d2_132.Arrowroot, root' > 0,
       CropSystems3C_TF$'d2_132.Arrowroot, root',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$'d2_132.Ginger - upln' > 0,
       CropSystems3C_TF$'d2_132.Ginger - upln',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$'d2_132.Tea leaves' > 0,
       CropSystems3C_TF$'d2_132.Tea leaves',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' &
       (CropSystems3C_TF$'d2_132.Cucumber - upln' > 0 |
       CropSystems3C_TF$'d2_132.Pumpkin - upln' > 0 |
       CropSystems3C_TF$'d2_132.French bean - upln' > 0 |
       CropSystems3C_TF$'d2_132.Cabbages - upln' > 0 |
       CropSystems3C_TF$'d2_132.Winter melon - upln' > 0 |
       CropSystems3C_TF$'d2_132.Potatoes - upln' > 0),
       rowSums(CropSystems3C_TF[, c('d2_132.Cucumber - upln','d2_132.Pumpkin - upln',
       'd2_132.French bean - upln','d2_132.Cabbages - upln','d2_132.Winter melon - upln',
       'd2_132.Potatoes - upln')], na.rm = T),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$d2_132.Cassava2 > 0 &
       CropSystems3C_TF$d2_136.Cassava2 == 0,CropSystems3C_TF$d2_132.Cassava2,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$d2_132.Cassava2 > 0 &
       CropSystems3C_TF$d2_136.Cassava2 > 0,CropSystems3C_TF$d2_132.Cassava2,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$'d2_132.Upland rice2' > 0,
       CropSystems3C_TF$'d2_132.Upland rice2',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$'d2_132.Maize corn' > 0 &
       CropSystems3C_TF$'d2_136.Maize corn' == 0,CropSystems3C_TF$'d2_132.Maize corn',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$'d2_132.Maize corn' > 0 &
       CropSystems3C_TF$'d2_136.Maize corn' > 0,CropSystems3C_TF$'d2_132.Maize corn',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Son La province' & CropSystems3C_TF$'d2_132.Maize biomass' > 0,
       CropSystems3C_TF$'d2_132.Maize biomass',0)",
       'ifelse(CropSystems3C_TF$S_Area == "Son La province" & rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 9 &
       rowSums(CropSystems3C_TF[, c("e12_2", "e12_4", "e12_6")], na.rm = TRUE) > 4999 ,rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Son La province" & rowSums(CropSystems3C_TF[, c("e3_1","e3_2","e3_3","e3_4")], na.rm = TRUE) > 0 &
       !(rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 19 &
       rowSums(CropSystems3C_TF[, c("e12_2", "e12_4", "e12_6")], na.rm = TRUE) > 4999), rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
              'ifelse(CropSystems3C_TF$S_Area == "Son La province" & (CropSystems3C_TF$e30 == "Piglet family raising, but buying feed from market" |
       CropSystems3C_TF$e30 == "Fattening family raising, but buying feed from market"),CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Son La province" & CropSystems3C_TF$e30 == "Piglet family raising using only local feed stuffs and kitchen waste",CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Son La province" & !(CropSystems3C_TF$e30 == "Piglet family raising using only local feed stuffs and kitchen waste" |
       CropSystems3C_TF$e30 == "Piglet family raising, but buying feed from market" | CropSystems3C_TF$e30 == "Fattening family raising, but buying feed from market") &
       CropSystems3C_TF$e3_5 > 0,CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Son La province" & CropSystems3C_TF$e44 == "Family raising but larger scale for selling meat",
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Son La province" & rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE) > 0 &
       CropSystems3C_TF$e44 != "Family raising but larger scale for selling meat",
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Son La province" & CropSystems3C_TF$e3_7 > 0,
       CropSystems3C_TF$e3_7,0)',

       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$`d2_132.Summer-autumn season rice` > 0 &
       CropSystems3C_TF$`d2_132.Winter-Spring season rice` > 0, rowSums(CropSystems3C_TF[, c('d2_132.Summer-autumn season rice','d2_132.Winter-Spring season rice')], na.rm = T)/2,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & rowSums(CropSystems3C_TF[, c('d2_132.Summer-autumn season rice','d2_132.Winter-Spring season rice')], na.rm = T) > 0 &
       (CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 | 
       CropSystems3C_TF$'d2_132.Mustard greens - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Mung bean' > 0 |
       CropSystems3C_TF$'d2_132.Spring Onions' > 0 |
       CropSystems3C_TF$'d2_132.Cabbages - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Carrot' > 0 |
       CropSystems3C_TF$'d2_132.Potatoes - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Honeydew' > 0 |
       CropSystems3C_TF$'d2_132.Tomato' > 0 |
       CropSystems3C_TF$'d2_132.Mustard2' > 0 |
       CropSystems3C_TF$'d2_132.Local bean' > 0 |
       CropSystems3C_TF$'d2_132.Chrysanthemum, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Turmeric, root' > 0 |
       CropSystems3C_TF$'d2_132.Black bean' > 0 |
       CropSystems3C_TF$'d2_132.French bean - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Kudzu' > 0 |
       CropSystems3C_TF$'d2_132.Long bean, Chinese' > 0 |
       CropSystems3C_TF$'d2_132.Pak choy' > 0 |
       CropSystems3C_TF$'d2_132.Garlic - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chinese flowering Cabbages/ choysum' > 0 |
       CropSystems3C_TF$'d2_132.Pumpkin - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Hmong mustard' > 0 |
       CropSystems3C_TF$'d2_132.Lettuce, romaine, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Chinese kale/ Gailan' > 0 |
       CropSystems3C_TF$'d2_132.Cauliflowers, Broccoli' > 0 |
       CropSystems3C_TF$'d2_132.Local mustard' > 0 |
       CropSystems3C_TF$'d2_132.Basil, sweet, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Kidney bean' > 0 |
       CropSystems3C_TF$'d2_132.Eggplant - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Sweet potatoes, tuber - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Winter melon - lowl' > 0),
       rowSums(CropSystems3C_TF[, c('d2_132.Summer-autumn season rice','d2_132.Winter-Spring season rice')], na.rm = T)/2,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$`d2_132.Summer-autumn season rice` > 0 &
       is.na(CropSystems3C_TF$`d2_132.Winter-Spring season rice`) &
       is.na(CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 | 
       CropSystems3C_TF$'d2_132.Mustard greens - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Mung bean' > 0 |
       CropSystems3C_TF$'d2_132.Spring Onions' > 0 |
       CropSystems3C_TF$'d2_132.Cabbages - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Carrot' > 0 |
       CropSystems3C_TF$'d2_132.Potatoes - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Honeydew' > 0 |
       CropSystems3C_TF$'d2_132.Tomato' > 0 |
       CropSystems3C_TF$'d2_132.Mustard2' > 0 |
       CropSystems3C_TF$'d2_132.Local bean' > 0 |
       CropSystems3C_TF$'d2_132.Chrysanthemum, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Turmeric, root' > 0 |
       CropSystems3C_TF$'d2_132.Black bean' > 0 |
       CropSystems3C_TF$'d2_132.French bean - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Kudzu' > 0 |
       CropSystems3C_TF$'d2_132.Long bean, Chinese' > 0 |
       CropSystems3C_TF$'d2_132.Pak choy' > 0 |
       CropSystems3C_TF$'d2_132.Garlic - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chinese flowering Cabbages/ choysum' > 0 |
       CropSystems3C_TF$'d2_132.Pumpkin - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Hmong mustard' > 0 |
       CropSystems3C_TF$'d2_132.Lettuce, romaine, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Chinese kale/ Gailan' > 0 |
       CropSystems3C_TF$'d2_132.Cauliflowers, Broccoli' > 0 |
       CropSystems3C_TF$'d2_132.Local mustard' > 0 |
       CropSystems3C_TF$'d2_132.Basil, sweet, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Kidney bean' > 0 |
       CropSystems3C_TF$'d2_132.Eggplant - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Sweet potatoes, tuber - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Winter melon - lowl' > 0),
       CropSystems3C_TF$'d2_132.Summer-autumn season rice',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$`d2_132.Winter-Spring season rice` > 0 &
       is.na(CropSystems3C_TF$`d2_132.Summer-autumn season rice`) &
       is.na(CropSystems3C_TF$'d2_132.Cucumber - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Mustard greens - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Mung bean' > 0 |
       CropSystems3C_TF$'d2_132.Spring Onions' > 0 |
       CropSystems3C_TF$'d2_132.Cabbages - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Carrot' > 0 |
       CropSystems3C_TF$'d2_132.Potatoes - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Honeydew' > 0 |
       CropSystems3C_TF$'d2_132.Tomato' > 0 |
       CropSystems3C_TF$'d2_132.Mustard2' > 0 |
       CropSystems3C_TF$'d2_132.Local bean' > 0 |
       CropSystems3C_TF$'d2_132.Chrysanthemum, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Turmeric, root' > 0 |
       CropSystems3C_TF$'d2_132.Black bean' > 0 |
       CropSystems3C_TF$'d2_132.French bean - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Kudzu' > 0 |
       CropSystems3C_TF$'d2_132.Long bean, Chinese' > 0 |
       CropSystems3C_TF$'d2_132.Pak choy' > 0 |
       CropSystems3C_TF$'d2_132.Garlic - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Chinese flowering Cabbages/ choysum' > 0 |
       CropSystems3C_TF$'d2_132.Pumpkin - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Hmong mustard' > 0 |
       CropSystems3C_TF$'d2_132.Lettuce, romaine, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Chinese kale/ Gailan' > 0 |
       CropSystems3C_TF$'d2_132.Cauliflowers, Broccoli' > 0 |
       CropSystems3C_TF$'d2_132.Local mustard' > 0 |
       CropSystems3C_TF$'d2_132.Basil, sweet, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Kidney bean' > 0 |
       CropSystems3C_TF$'d2_132.Eggplant - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Sweet potatoes, tuber - lowl' > 0 |
       CropSystems3C_TF$'d2_132.Winter melon - lowl' > 0),
       CropSystems3C_TF$'d2_132.Winter-Spring season rice',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' &
       (CropSystems3C_TF$d2_132.Bananas > 0 |
       CropSystems3C_TF$d2_132.Littchi > 0 |
       CropSystems3C_TF$d2_132.Longan > 0 |
       CropSystems3C_TF$'d2_132.Oranges' > 0 |
       CropSystems3C_TF$d2_132.Peach > 0 |
       CropSystems3C_TF$'d2_132.Pomelos and grapefruits' > 0),
       rowSums(CropSystems3C_TF[, c('d2_132.Bananas','d2_132.Littchi',
       'd2_132.Longan','d2_132.Oranges','d2_132.Peach','d2_132.Pomelos and grapefruits')], na.rm = T),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' &
       (CropSystems3C_TF$'d2_132.Eggplant - upln' > 0 |
       CropSystems3C_TF$'d2_132.Kohlrabi' > 0 |
       CropSystems3C_TF$'d2_132.Sweet potatoes, leaves' > 0 |
       CropSystems3C_TF$'d2_132.Sawtooth herb / Culantro'),
       rowSums(CropSystems3C_TF[, c('d2_132.Eggplant - upln','d2_132.Kohlrabi',
       'd2_132.Sweet potatoes, leaves','d2_132.Sawtooth herb / Culantro')], na.rm = T),0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$d2_132.Cassava2 > 0,
       CropSystems3C_TF$d2_132.Cassava2,0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$'d2_132.Upland rice2' > 0,
       CropSystems3C_TF$'d2_132.Upland rice2',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$'d2_132.Maize corn' > 0 &
       CropSystems3C_TF$'d2_136.Maize corn' == 0,CropSystems3C_TF$'d2_132.Maize corn',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$'d2_132.Maize corn' > 0 &
       CropSystems3C_TF$'d2_136.Maize corn' > 0,CropSystems3C_TF$'d2_132.Maize corn',0)",
       "ifelse(CropSystems3C_TF$S_Area == 'Dien Bien province' & CropSystems3C_TF$'d2_132.Maize biomass' > 0,
       CropSystems3C_TF$'d2_132.Maize biomass',0)",
       'ifelse(CropSystems3C_TF$S_Area == "Dien Bien province" & rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 0 &
       CropSystems3C_TF$e16 != "Confined in a barn," ,rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Dien Bien province" & rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = TRUE) > 0 &
       CropSystems3C_TF$e16 == "Confined in a barn," ,rowSums(CropSystems3C_TF[, c("e3_1", "e3_3")], na.rm = T),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Dien Bien province" & (CropSystems3C_TF$e30 == "Piglet family raising, but buying feed from market" |
       CropSystems3C_TF$e30 == "Fattening family raising, but buying feed from market"),CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Dien Bien province" & CropSystems3C_TF$e3_5 > 0 &
       !(CropSystems3C_TF$e30 == "Piglet family raising, but buying feed from market" |
       CropSystems3C_TF$e30 == "Fattening family raising, but buying feed from market"),CropSystems3C_TF$e3_5,0)',
       'ifelse(CropSystems3C_TF$S_Area == "Dien Bien province" & (CropSystems3C_TF$e44 == "Family raising but larger scale for selling meat" |
       CropSystems3C_TF$e44 == "Family raising but larger scale for selling chick"),
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE),0)',
       'ifelse(CropSystems3C_TF$S_Area == "Dien Bien province" & rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE) > 0 &
       !(CropSystems3C_TF$e44 == "Family raising but larger scale for selling meat" |
       CropSystems3C_TF$e44 == "Family raising but larger scale for selling chick"),
       rowSums(CropSystems3C_TF[, c("e3_13", "e3_14", "e3_16")], na.rm = TRUE),0)'))



#Loop
i = 29

for (i in 1:nrow(Guide)){
  CropSystems3C_TF[[Guide$Sys[i]]] <- eval(parse(text = Guide$text[i]))
  CropSystems3C_TF[[Guide$Sys[i]]] <- ifelse(is.na(CropSystems3C_TF[[Guide$Sys[i]]]), "0",CropSystems3C_TF[[Guide$Sys[i]]])
  CropSystems3C_TF[[Guide$Sys[i]]] <- as.character(CropSystems3C_TF[[Guide$Sys[i]]])
  CropSystems3C_TF[[Guide$Sys[i]]] <- as.numeric(CropSystems3C_TF[[Guide$Sys[i]]])
  }

#Additionnal variables:
#Access to private forage/pastureland
CropSystems3C_TF$ForagePastureTot <- rowSums(CropSystems3C_TF[, c("e12_2", "e12_4", "e12_6")], na.rm = TRUE)
#Fish farming
CropSystems3C_TF$FishfarmingPondSize <- CropSystems3C_TF$e9
#Sources of income
CropSystems3C_TF$FirstIncomeSource <- CropSystems3C_TF$b4_1
CropSystems3C_TF$SecondIncomeSource <- CropSystems3C_TF$b4_2
CropSystems3C_TF$ThirdIncomeSource <- CropSystems3C_TF$b4_3
#%of total income
CropSystems3C_TF$FirstIncomeSourceShare <- CropSystems3C_TF$b5_1
CropSystems3C_TF$SecondIncomeSourceShare <- CropSystems3C_TF$b5_2
CropSystems3C_TF$ThirdIncomeSourceShare <- CropSystems3C_TF$b5_3
#Household labor force
CropSystems3C_TF$NbhMembers <- rowSums(!is.na(CropSystems3C_TF[,grep("a2.", names(CropSystems3C_TF))]))
CropSystems3C_TF$NbhActiveMembers <- rowSums(!is.na(CropSystems3C_TF[,grep("a6.", names(CropSystems3C_TF))]) &
                                   CropSystems3C_TF[,grep("a6.", names(CropSystems3C_TF))] != "Study/training" &
                                   CropSystems3C_TF[,grep("a6.", names(CropSystems3C_TF))] != "Unable to work")
CropSystems3C_TF$NbhFarmMembersPrim <- rowSums(!is.na(CropSystems3C_TF[,grep("a6.", names(CropSystems3C_TF))]) &
                                     CropSystems3C_TF[,grep("a6.", names(CropSystems3C_TF))] == "Agricultural work on their own farm (including livestock management)")
CropSystems3C_TF$NbhFarmMembersSec <- rowSums(!is.na(CropSystems3C_TF[,grep("a7.", names(CropSystems3C_TF))]) &
                                    CropSystems3C_TF[,grep("a7.", names(CropSystems3C_TF))] == "Agricultural work on their own farm (including livestock management)")
#NbMigratingMember
CropSystems3C_TF$NbMigratingMembers <- rowSums(!is.na(CropSystems3C_TF[,grep("a10.", names(CropSystems3C_TF))]) &
                                                 CropSystems3C_TF[,grep("a10.", names(CropSystems3C_TF))] == "Yes")
#Nb of months during which they are migratin
CropSystems3C_TF$YearlyRatemigrationperMigrant <- ifelse(CropSystems3C_TF$NbMigratingMembers > 0, (rowSums(!is.na(CropSystems3C_TF[,grep("a13", names(CropSystems3C_TF))]) &
                                                                                                             CropSystems3C_TF[,grep("a13", names(CropSystems3C_TF))] == "1") / 12) / 
                                                           CropSystems3C_TF$NbMigratingMembers, NA)
#External labor force
CropSystems3C_TF$ValueHiredWorkersPerYear <- CropSystems3C_TF$d35
#EXTERNAL-MECHANIZED SERVICE USED by hh
#Land preparation
CropSystems3C_TF$`ExtMechaServices-Land preparation` <- CropSystems3C_TF$d361
#Sowing
CropSystems3C_TF$`ExtMechaServices-Sowing` <- CropSystems3C_TF$d362
#Weed management
CropSystems3C_TF$`ExtMechaServices-Weed management` <- CropSystems3C_TF$d364
#Pest and disease management
CropSystems3C_TF$`ExtMechaServices-Pest and disease management` <- CropSystems3C_TF$d365
#Water/irrigation management
CropSystems3C_TF$`ExtMechaServices-Water/irrigation management` <- CropSystems3C_TF$d367
#Harvest
CropSystems3C_TF$`ExtMechaServices-Harvest` <- CropSystems3C_TF$d368
#Transportation
CropSystems3C_TF$`ExtMechaServices-Transportation` <- CropSystems3C_TF$d369
#Post-harvest processing
CropSystems3C_TF$`ExtMechaServices-Post-harvest processing` <- CropSystems3C_TF$d3610
#None
CropSystems3C_TF$`ExtMechaServices-None of above` <- CropSystems3C_TF$d360

#ASSETS
#Wheel hand tractor
CropSystems3C_TF$NbWheelHandTractor <- CropSystems3C_TF$k2_2
#Wheel tractor
CropSystems3C_TF$NbWheelTractor <- CropSystems3C_TF$k2_3
#Truck
CropSystems3C_TF$NbTruck <- CropSystems3C_TF$k2_5
#Land leveler
CropSystems3C_TF$NbLandLeveler <- CropSystems3C_TF$k2_7
#Rice planter
CropSystems3C_TF$NbRicePlanter <- CropSystems3C_TF$k2_8
#Maize planter
CropSystems3C_TF$NbMaizePlanter <- CropSystems3C_TF$k2_9
#Cassava disc ridging tool
CropSystems3C_TF$NbCassavaDiscRidgingTool <- CropSystems3C_TF$k2_10
#Cassava harvesting tool
CropSystems3C_TF$NbCassavaHarvestingTool <- CropSystems3C_TF$k2_11
#Rice thresher
CropSystems3C_TF$NbRiceThresher <- CropSystems3C_TF$k2_12
#Rice mill
CropSystems3C_TF$NbRiceMill <- CropSystems3C_TF$k2_13
#Backpack sprayer
CropSystems3C_TF$NbBackpackSprayer <- CropSystems3C_TF$k2_14
#Motorpump sprayer
CropSystems3C_TF$NbMotorPumpSprayer <- CropSystems3C_TF$k2_15
#Grass cutter
CropSystems3C_TF$NbGrassCutter <- CropSystems3C_TF$k2_16
#Grass chopping machine
CropSystems3C_TF$NbGrassChoppingMachine <- CropSystems3C_TF$k2_17
#Water pump
CropSystems3C_TF$NbWaterPump <- CropSystems3C_TF$k2_18
#Irrigation system
CropSystems3C_TF$NbIrrigationSystem <- CropSystems3C_TF$k2_19

#Average nb of days spent for hunting
CropSystems3C_TF$'NbDaysSpent-Hunting' <- CropSystems3C_TF$d7_11 
#Average income from Hunting
CropSystems3C_TF$'AvIncome-Hunting' <- CropSystems3C_TF$d7_13
#Average nb of days spent for fishing
CropSystems3C_TF$'NbDaysSpent-Fishing' <- CropSystems3C_TF$d7_21
#Average income from Fishing
CropSystems3C_TF$'AvIncome-Fishing' <- CropSystems3C_TF$d7_23
#Average nb of days spent for fuelwood
CropSystems3C_TF$'NbDaysSpent-Fuelwood' <- CropSystems3C_TF$d7_31
#Average income from Fuelwood
CropSystems3C_TF$'AvIncome-Fuelwood' <- CropSystems3C_TF$d7_33
#Average nb of days spent for mushrooms
CropSystems3C_TF$'NbDaysSpent-Mushrooms' <- CropSystems3C_TF$d7_41
#Average income from Mushrooms
CropSystems3C_TF$'AvIncome-Mushrooms' <- CropSystems3C_TF$d7_43
#Average nb of days spent for Bamboo shoots
CropSystems3C_TF$'NbDaysSpent-Bamboo shoots' <- CropSystems3C_TF$d7_51
#Average income from Bamboo shoots
CropSystems3C_TF$'AvIncome-Bamboo shoots' <- CropSystems3C_TF$d7_53
#Average nb of days spent for Bamboo poles
CropSystems3C_TF$'NbDaysSpent-Bamboo poles' <- CropSystems3C_TF$d7_61
#Average income from Bamboo poles
CropSystems3C_TF$'AvIncome-Bamboo poles' <- CropSystems3C_TF$d7_63
#Average nb of days spent for Broom Grass
CropSystems3C_TF$'NbDaysSpent-Broom Grass' <- CropSystems3C_TF$d7_71
#Average income from Broom Grass
CropSystems3C_TF$'AvIncome-Broom Grass' <- CropSystems3C_TF$d7_73
#Average nb of days spent for Honey
CropSystems3C_TF$'NbDaysSpent-Honey' <- CropSystems3C_TF$d7_81
#Average income from Honey
CropSystems3C_TF$'AvIncome-Honey' <- CropSystems3C_TF$d7_83
#Average nb of days spent for Rattan
CropSystems3C_TF$'NbDaysSpent-Rattan' <- CropSystems3C_TF$d7_91
#Average income from Rattan
CropSystems3C_TF$'AvIncome-Rattan' <- CropSystems3C_TF$d7_93
#Average nb of days spent for Wild pepper
CropSystems3C_TF$'NbDaysSpent-Wild pepper' <- CropSystems3C_TF$d7_131
#Average income from Wild pepper
CropSystems3C_TF$'AvIncome-Wild pepper' <- CropSystems3C_TF$d7_133
#Average nb of days spent for Medicinal plants
CropSystems3C_TF$'NbDaysSpent-Medicinal plants' <- CropSystems3C_TF$d7_141
#Average income from Medicinal plants
CropSystems3C_TF$'AvIncome-Medicinal plants' <- CropSystems3C_TF$d7_143
#Average nb of days spent for Wooden poles
CropSystems3C_TF$'NbDaysSpent-Wooden poles' <- CropSystems3C_TF$d7_161
#Average income from Wooden poles
CropSystems3C_TF$'AvIncome-Wooden poles' <- CropSystems3C_TF$d7_163
#Average nb of days spent for Leave for thatch roof
CropSystems3C_TF$'NbDaysSpent-LeaveThatchRoof' <- CropSystems3C_TF$d7_181
#Average income from Leave for thatch roof
CropSystems3C_TF$'AvIncome-LeaveThatchRoof' <- CropSystems3C_TF$d7_183

#Seed provenance
#%households who conserve and use own seeds
CropSystems3C_TF$`PerCentH - SeedProvenance` <- CropSystems3C_TF[,9959]
#Seed source 1: From the village seller
CropSystems3C_TF$`SeedProvenance` <- CropSystems3C_TF$d32_2


#We export the file 
write.csv(CropSystems3C_TF[,c(1:2,5:7,11541:11694)], file = "ProdSys.csv")

#We export another complete database including Cropping/Raising systems
#and Production systems
#We import the production system
ProdSysID <- read.csv2("ProdSysID.csv")
ProdSysID$hhid_re2 <- as.character(ProdSysID$hhid_re2)
CropSystems3C_TF$hhid_re2 <- as.character(CropSystems3C_TF$hhid_re2)
CropSystems3C_TF <- full_join(CropSystems3C_TF, ProdSysID[,c(1,3)], by = "hhid_re2")

#And coordinates
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_cleaning&Analysis-Titouan2023/ASSETOutputs/ASSET_db")
#Cambodia
HouseholdCambodia_TF <- readRDS("HouseholdCambodia_TF.rds")
#Laos
HouseholdLaos_TF <- readRDS("HouseholdLaos_TF.rds")
#Vietnam
HouseholdVietnam_TF <- readRDS("HouseholdVietnam_TF.rds")
#We merge 3 country
HouseholdVietnam_TF[,2271] <- as.character(HouseholdVietnam_TF[,2271])
HouseholdVietnam_TF[,2272] <- as.character(HouseholdVietnam_TF[,2272])
HouseholdVietnam_TF[,2273] <- as.character(HouseholdVietnam_TF[,2273])
Coordinates <- rbind(HouseholdCambodia_TF[,c(1,2496:2498)],
                     HouseholdLaos_TF[,c(1,2288:2290)],
                     HouseholdVietnam_TF[,c(1,2271:2273)])
#We merge all
colnames(Coordinates)[1] <- "hhid_re2"
CropSystems3C_TF <- full_join(CropSystems3C_TF, Coordinates, by = "hhid_re2")

#We export the new complete database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(CropSystems3C_TF, "NewdataProd_TF.rds")

#We export a small table that will be used for mapping analyzes
write.csv(CropSystems3C_TF[,c(1:7,11696:11699)], file = "ProdSysSIG.csv")

#We create another table including 
