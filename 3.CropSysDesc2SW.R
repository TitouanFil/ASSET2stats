### 1. Preparation

## a. Libraries loadings
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
library(survey)

## b. data loading
CropDesc <- readRDS("CropDesc_TF.rds")
All <- readRDS("Newdata_TF.rds")
Cropsys <- read.csv2("Cropsys.csv")
RnV <- readRDS("RnV_TF.rds")
Cropsyssum <- read.csv2("Cropsyssum.csv")

## c. - Lists
{
  Sarealist <- unique(Cropsys$Sarea)
  Croplist <- unique(Cropsys$Crop)
  AEP <- c("Rainwater collection/conservation","Greywater recycling",
           "Ponds (for water conservation)","Terraces building","Swales digging",
           "Land levelling","Mulching","Other","Sowing in contour lines",
           "Natural or planted grass strips","Trees conservation in agricultural plots",
           "Agroforestry (trees + crops)","Crop residues maintained to cover the soil",
           "Use of cover crops","Reduced to no-tillage","Other","Animal manure",
           "Compost (heap)","Bokashi (fermented organic matter)",
           "Legume-based green manure","Pulses in association and/or rotation with main crop",
           "Cover crops in association and/or rotation with main crop","Biochar",
           "Crop residue maintenance","Recycling crop waste",
           "Ramial Wood Chip (RWC) or other wood chips","Organic agro-industrial waste",
           "Other methods","Crop rotation / intercropping","Cover crops","Mulching / shading",
           "Sowing date / rate / depth","Crop spatial arrangement","Seed cleaning before sowing",
           "Cultivar choice","Crop mixtures","Nutrient placement","Patch/ban spraying",
           "Bioherbicide","Mowing / slashing","Grazing","Post harvest weed seed destruction in field",
           "Any other methods","Crop rotation / intercropping","Flower strips","Hedgerows",
           "Soil health maintenance/improvement","Sanitation practices (removal of damaged/infected plants and fruits)",
           "Planting date","Water and nutrient management","Cultivar choice (tolerant/resistant) / cultivar mixture",
           "Biopesticide / organic pesticide","Commercial biological control agents (BCAs)",
           "Home-made efficient microorganism (EM)","Commercial efficient microorganism (EM)",
           "Pheromone traps","Protein baits","Weaver ant","Any other methods",
           "Crops requiring more water than others","Crops requiring heavy fertilization to thrive",
           "Crops requiring heavy use of pesticides to thrive","Conservation of traditionnal, local seeds")
  AEPcat <- rep(c("AEP Water", "AEP SoilCons", "AEP SoilFert", "AEP Weed",
                  "AEP PestnDisease", "Obs", ""), times = c(8, 8, 12, 15, 16, 3,1))
  AEPvar <- c("d131_11.","d132_11.","d133_11.","d134_11.","d135_11.","d136_11.",
              "d137_11.","d138_11.","d151_11.","d152_11.","d153_11.","d154_11.",
              "d155_11.","d156_11.","d157_11.","d158_11.","d18_111.","d18_211.",
              "d18_311.","d18_411.","d18_511.","d18_611.","d18_711.","d18_811.",
              "d18_911.","d18_1011.","d18_111_1.","d18_9911.","d21_121.","d21_221.",
              "d21_321.","d21_421.","d21_521.","d21_621.","d21_721.","d21_821.",
              "d21_921.","d21_1021.","d21_1121.","d21_1221.","d21_1321.","d21_1421.",
              "d21_9921.","d27_111.","d27_211.","d27_311.","d27_411.","d27_511.",
              "d27_611.","d27_711.","d27_811.","d27_911.","d27_1011.","d27111_1.",
              "d27_1211.","d27_1311.","d27_1411.","d27_1511.","d27_9911.",
              "d30_21","d30_41","d30_61","d32_11")
  AEP <- rbind(AEPcat,AEP,AEPvar)
  RnV$RiceTot <- RnV$`Summer-autumn season rice` + RnV$`Winter-Spring season rice`
  RnV$DR <- ifelse(RnV$`Summer-autumn season rice` > 0 & RnV$`Winter-Spring season rice` > 0, 1, 0)
  Rid <- RnV$hhid_re2[RnV$RiceTot > 0 & RnV$Lowveggie == 0 & RnV$DR == 0]
  RnVid <- RnV$hhid_re2[RnV$RiceTot > 0 & RnV$Lowveggie > 0]
  Vid <- RnV$hhid_re2[RnV$RiceTot == 0 & RnV$Lowveggie > 0]
  DRid <- RnV$hhid_re2[RnV$DR == 1 & RnV$Lowveggie == 0]
}

### 2 - Crop average values Table

#We add all the calculated columns to Cropsys dataframe
for (col_name in names(CropDesc[3:99])) {
  Cropsys[[col_name]] <- NA
}
All$SW_Weight <- as.character(All$SW_Weight)
All$SW_Weight <- as.numeric(All$SW_Weight)
new_row <- c(rep(NA, 84), 0)


Cropsys$id <- 1:nrow(Cropsys)
for (i in Sarealist){
  Dumm <- All[All$S_Area == i,]
  Cropsys2 <- Cropsys[Cropsys$Sarea == i,]
  for (k in 1:nrow(Cropsys2)){
    j <- Cropsys2$Crop[k]
    Dumm1 <- Dumm %>%
      select("hhid_re2",matches(grep(j, names(Dumm), value = TRUE)),starts_with("SW_"))
     if(Cropsys2$Process[k] == "Norm"){
      Cropsys2[k,5:101] <- CropDesc[CropDesc$Sarea == Cropsys2$Sarea[k] & CropDesc$Crop == Cropsys2$Crop[k],3:99]
      }else if(Cropsys2$Process[k] == "No"){
       Cropsys2[k,5:101] <- NA
       } else {if(Cropsys2$Process[k] == "Cons"){
        Dumm1 <- Dumm1[is.na(Dumm1[,9]) | Dumm1[,9] == 0,] 
        }else if(Cropsys2$Process[k] == "Sell"){
         Dumm1 <- Dumm1[Dumm1[,9] > 0 & !is.na(Dumm1[,9]),]
         }else if(Cropsys2$Process[k] == "Prod"){
          Dumm1 <- Dumm1[Dumm1[,13] > 0 & !is.na(Dumm1[,13]),]
          }else if(Cropsys2$Process[k] == "NoProd"){
           Dumm1 <- Dumm1[is.na(Dumm1[,13]) | Dumm1[,13] == 0,]
           }else if(Cropsys2$Process[k] == "R"){
            Dumm1 <- Dumm1[Dumm1$hhid_re2 %in% Rid,]
            Dumm1 <- Dumm1[!is.na(Dumm1$SW_Weight),]
            }else if(Cropsys2$Process[k] == "RnV"){
             Dumm1 <- Dumm1[Dumm1$hhid_re2 %in% RnVid,]
             Dumm1 <- Dumm1[!is.na(Dumm1$SW_Weight),]
             }else if(Cropsys2$Process[k] == "V"){
              Dumm1 <- Dumm1[Dumm1$hhid_re2 %in% Vid,]
              Dumm1 <- Dumm1[!is.na(Dumm1$SW_Weight),]
              Dumm1 <- rbind(Dumm1, new_row)
              }else if(Cropsys2$Process[k] == "VnR"){
               Dumm1 <- Dumm1[Dumm1$hhid_re2 %in% RnVid & Dumm1[,4] > 0,]
               Dumm1 <- Dumm1[!is.na(Dumm1$SW_Weight),]
               Dumm1 <- rbind(Dumm1, new_row)
              }else if(Cropsys2$Process[k] == "DR"){
                Dumm1 <- Dumm1[Dumm1$hhid_re2 %in% DRid & Dumm1[,4] > 0,]
                Dumm1 <- Dumm1[!is.na(Dumm1$SW_Weight),]
                Dumm1 <- rbind(Dumm1, new_row)
               }else{
                x <- strsplit(gsub("\"", "", Cropsys2$Process[k]), ",")[[1]]
                Dumm1 <- Dumm1[Dumm1$hhid_re2 %in% x,]
                Dumm1 <- rbind(Dumm1, new_row)
               }
         survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm1)
         #N = Households growing this crop
         Cropsys2$N[k] <- sum(Dumm1$SW_Weight[!(is.na(Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)]) |
                                                                                         Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)] == "")])
         #PerCentofHouseholds = % of households growing this crop (per study area)
         Cropsys2$PerCentofHouseholds[k] <-
           round(Cropsys2$N[k]/sum(Dumm$SW_Weight), digits = 3)
         #Area - m2
         Cropsys2$`Area - m2`[k] <-
           round(svymean(~ Dumm1[, grep("d2_132", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 3)
         #MSeedUnit
         Ccount <- table(Dumm1[grep("d2_133", names(Dumm1), value = TRUE)])
         Ccount <- Ccount[!is.na(names(Ccount)) & names(Ccount) != ""]
         Cropsys2$MSeedUnit[k] <- 
           names(Ccount)[which.max(Ccount)]
         #PerCentMSeedUnit
         Cropsys2$PerCentMSeedUnit[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == Cropsys2$MSeedUnit[k]], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Seed amount /ha - Kg of seed = Seed amount used for sowing
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed")
         #
         Cropsys2$'Seed amount /ha - Kg of seed'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed", na.rm = T) != 0,
                  round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                        grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
         #Seed amount /ha - Gr of seed
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed")
         #
         Cropsys2$'Seed amount /ha - Gr of seed'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed", na.rm = T) != 0,
                  round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                        grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
         #Seed amount /ha - Number of seedlings
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings")
         #
         Cropsys2$'Seed amount /ha - Number of seedlings'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings", na.rm = T) != 0,
                  round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                        grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
         #Seed amount /ha - Number of stems
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems")
         #
         Cropsys2$'Seed amount /ha - Number of stems'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems", na.rm = T) != 0,
                  round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                        grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
         #Seed amount /ha - Tuber
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber")
         #
         Cropsys2$'Seed amount /ha - Tuber'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber", na.rm = T) != 0,
                  round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                        grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)    
         #Yield - Kg/ha
         Cropsys2$'Yield - Kg/ha'[k] <- 
           round(svymean(~ Dumm1[, grep("d2_ha135", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 0)
         #PerCentSellers = % of households selling this crop (% of households of study area)
         Cropsys2$PerCentSellers[k] <- 
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)] > 1], na.rm = T)  /
                   Cropsys2$N[k], digits = 3)
         #MFormWhenSold
         Ccount <- table(Dumm1[grep("d2_139", names(Dumm1), value = TRUE)])
         Cropsys2$MFormWhenSold[k] <- 
           names(Ccount)[which.max(Ccount)]
         #PerCentMFormWhenSold
         Cropsys2$PerCentMFormWhenSold[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == Cropsys2$MFormWhenSold[k]], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Amount sold - Kg/ha - Total
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)] != 0)
         #
         Cropsys2$'Amount sold - Kg/ha - Total'[k] <-
           round(svymean(~ Dumm1[Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)]),
                                 grep("d2_ha136", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0)
         #Amount sold - Kg/ha - Dried
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_139", names(Dumm1))] == "Dried")
         #
         Cropsys2$'Amount sold - Kg/ha - Dry'[k] <-
           ifelse(sum(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Dried", na.rm = T) != 0,
                  round(svymean(~ Dumm1[Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Dried" & !is.na(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)]),
                                        grep("d2_ha136", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA) 
         #Amount sold - Kg/ha - Fresh
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_139", names(Dumm1))] == "Fresh")
         #
         Cropsys2$'Amount sold - Kg/ha - Fresh'[k] <-
           ifelse(sum(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Fresh", na.rm = T) != 0,
                  round(svymean(~ Dumm1[Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Fresh" & !is.na(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)]),
                                        grep("d2_ha136", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA) 
         #PerCentHarvestSold = Percentage of harvest sold by households who sell
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_ha136", names(Dumm1))] != 0)
         #
         Cropsys2$'PerCentHarvestSold'[k] <- 
           round(svymean(~ Dumm1[Dumm1[, grep("d2_ha136", names(Dumm1))] != 0 & !is.na(Dumm1[, grep("d2_ha136", names(Dumm1))]),grep("d2_ha136", names(Dumm1), value = TRUE)],
                         design = sub_svy_design, na.rm = TRUE) /
                   svymean(~ Dumm1[Dumm1[, grep("d2_ha136", names(Dumm1))] != 0 & !is.na(Dumm1[, grep("d2_ha136", names(Dumm1))]),grep("d2_ha135", names(Dumm1), value = TRUE)],
                           design = sub_svy_design, na.rm = TRUE), digits = 3)
         #Selling price - $/kg ("0" values removed)
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_137", names(Dumm1), value = TRUE)] != 0)
         #
         Cropsys2$`Selling price - $/kg`[k] <-
           round(svymean(~ Dumm1[Dumm1[,grep("d2_137", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("d2_137", names(Dumm1), value = TRUE)]),
                                 grep("d2_137", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 2)
         #NumberSpecies (remove values > 10)
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_138", names(Dumm1), value = TRUE)] < 11)
         #
         Cropsys2$`NumberSpecies`[k] <-
           round(svymean(~ Dumm1[Dumm1[,grep("d2_138", names(Dumm1), value = TRUE)] < 11 & !is.na(Dumm1[,grep("d2_138", names(Dumm1), value = TRUE)]),
                                 grep("d2_138", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 1)
         #GrossProductRAW - $/household ("0" values removed)
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("GrossProductRaw", names(Dumm1), value = TRUE)] != 0)
         #
         Cropsys2$`GrossProductRAW - $/household`[k] <-
           round(svymean(~ Dumm1[Dumm1[,grep("GrossProductRaw", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("GrossProductRaw", names(Dumm1), value = TRUE)]),
                                 grep("GrossProductRaw", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0)
         #GrossProductHA - $/ha ("0" values removed)
         sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("GrossProductha", names(Dumm1), value = TRUE)] != 0)
         #
         Cropsys2$`GrossProduct - $/ha`[k] <-
           round(svymean(~ Dumm1[Dumm1[,grep("GrossProductha", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("GrossProductha", names(Dumm1), value = TRUE)]),
                                 grep("GrossProductha", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0)
         
         #Reason% - Household consumption preferences = Why household did grow this crop, % of households growing this crop, 1st option
         Cropsys2$`Reason% - Household consumption preferences`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Household consumption preferences"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Reason% - Market price and demand = Why household did grow this crop, % of households growing this crop, 2nd option
         Cropsys2$`Reason% - Market price and demand`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Market price and demand"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Reason% - Well adapted to local conditions (soil, climate, …) = Why household did grow this crop, % of households growing this crop, 3rd option
         Cropsys2$`Reason% - Well adapted to local conditions`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Well adapted to local conditions (soil, climate, …)"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Diseases = Main constraint faced during cropping, % of households growing this crop, 1st option
         Cropsys2$`Constraint% - Diseases`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Diseases"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Insects = Main constraint faced during cropping, % of households growing this crop, 2nd option
         Cropsys2$`Constraint% - Insects`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Insects"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Market price = Main constraint faced during cropping, % of households growing this crop, 3rd option
         Cropsys2$`Constraint% - Market price`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Market price"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Other agronomic constraints = Main constraint faced during cropping, % of households growing this crop, 4th option
         Cropsys2$`Constraint% - Other agronomic constraints`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Other agronomic constraints"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Product quality = Main constraint faced during cropping, % of households growing this crop, 5th option
         Cropsys2$`Constraint% - Product quality`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Product quality"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Soil fertility = Main constraint faced during cropping, % of households growing this crop, 6th option
         Cropsys2$`Constraint% - Soil fertility`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Soil fertility"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Water management = Main constraint faced during cropping, % of households growing this crop, 7th option
         Cropsys2$`Constraint% - Water management`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Water management"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - Weed = Main constraint faced during cropping, % of households growing this crop, 8th option
         Cropsys2$`Constraint% - Weed`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Weed"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         #Constraint% - No constraint = Main constraint faced during cropping, % of households growing this crop, 9th option
         Cropsys2$`Constraint% - No constraint`[k] <-
           round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "No constraint"], na.rm = T) /
                   Cropsys2$N[k], digits = 3)
         for (l in 1:ncol(AEP)){
           #Loop for different AEP practices with associated categories
           Dum <- paste0(AEP[1,l],"-",AEP[2,l])
           Cropsys2[[Dum]] <- ifelse(Cropsys2$Crop == Cropsys2$Crop[k] & Cropsys2$Cropsys == Cropsys2$Cropsys[k],
                                     round(sum(Dumm1$SW_Weight[!is.na(Dumm1[, grep(AEP[3, l], names(Dumm1), value = TRUE)]) & 
                                                 Dumm1[, grep(AEP[3, l], names(Dumm1), value = TRUE)] != 0], na.rm = TRUE) /
                                             Cropsys2$N[Cropsys2$Crop == Cropsys2$Crop[k] & Cropsys2$Cropsys == Cropsys2$Cropsys[k]], digits = 3), 
                                     Cropsys2[[Dum]])
         }
       }
    }

for (m in Cropsys2$id){
 Cropsys[Cropsys$id == m,5:101] <- Cropsys2[Cropsys2$id == m,5:101]
}
}

    



  

#We remove/replace unlikely values
Cropsys$`Amount sold - Kg/ha - Total` <- ifelse(Cropsys$`Amount sold - Kg/ha - Total` > Cropsys$`Yield - Kg/ha`,
                                                 Cropsys$`Yield - Kg/ha`,Cropsys$`Amount sold - Kg/ha - Total`)
Cropsys$`Reason% - Market price and demand` <- ifelse(Cropsys$`Reason% - Market price and demand` > 1,
                                                       1,Cropsys$`Reason% - Market price and demand`)
Cropsys$`AEP SoilFert-Animal manure` <- ifelse(Cropsys$`AEP SoilFert-Animal manure` > 1,
                                                1,Cropsys$`AEP SoilFert-Animal manure`)
Cropsys$`AEP SoilFert-Organic agro-industrial waste` <- ifelse(Cropsys$`AEP SoilFert-Organic agro-industrial waste` > 1,
                                                                1,Cropsys$`AEP SoilFert-Organic agro-industrial waste`)
Cropsys$`AEP Weed-Crop rotation / intercropping` <- ifelse(Cropsys$`AEP Weed-Crop rotation / intercropping` > 1,
                                                            1,Cropsys$`AEP Weed-Crop rotation / intercropping`)
Cropsys$`Obs-Crops requiring more water than others` <- ifelse(Cropsys$`Obs-Crops requiring more water than others` > 1,
                                                                1,Cropsys$`Obs-Crops requiring more water than others`)
Cropsys$`Obs-Crops requiring heavy fertilization to thrive` <- ifelse(Cropsys$`Obs-Crops requiring heavy fertilization to thrive` > 1,
                                                                       1,Cropsys$`Obs-Crops requiring heavy fertilization to thrive`)
Cropsys$`-Conservation of traditionnal, local seeds` <- ifelse(Cropsys$`-Conservation of traditionnal, local seeds` > 1,
                                                                1,Cropsys$`-Conservation of traditionnal, local seeds`)

#We modify one last column content
Cropsys$MFormWhenSold <- ifelse(Cropsys$MFormWhenSold == "",
                                  "Unknown",Cropsys$MFormWhenSold)

#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(Cropsys, "Cropsys_TF.rds")


Rice <- c("Wet season rice broadcast","Upland rice - mixed cropping","Upland rice2",
  "Wet season rice transplant","Summer-autumn season rice","Winter-Spring season rice")
  
Cereo <- c("Cassava2","Cassava - mixed cropping","Soybean2","Soybean - mixed cropping",
  "maize_traditional","maize_hybrid","Maize biomass","Maize corn")
  
VegeFr <- c("Guava","Cashew - mixed cropping","Peanut - lowl","Sweetsop","Other fibre crops cotton etc.",
  "Savoy cabbage curly","Cucumber - lowl","Water spinach","Pak choy","Cashew nut",
  "Longan","Mangoes","Coconuts","Lime","Melon local, young","Long bean, Chinese",    
  "Bananas","Mustard greens - lowl","Amaranth","Strawberry - upln","Other vegetable crop - upln",
  "Plum","Long bean - lowl","Chinese lettuce - lowl","Chili - lowl","Onions2","Coriander - lowl",
  "Chinese flowering Cabbages/ choysum","Other vegetable crop - lowl",                             
  "Napa Cabbages","Dill","Chili - upln","Bell pepper, red","Peanut - upln","Cucumber - upln",                               
  "sugarcane","Pumpkin - upln","Ginger - lowl","Eggplant - upln","Coffee, green",
  "Mustard greens - upln","Tea leaves","Peach","Strawberry - lowl","Other fruit crop",                         
  "Mung bean","Garlic - lowl","Spring Onions","Salad/lettuce","Cabbages - lowl","Chinese lettuce - upln","Carrot",
  "Garlic - upln","kale_flower - lowl","Coriander - upln","jobs_tears - lowl",
  "kale_flower - upln","Ginger - upln","Hmong Cucumber","Long bean - upln",
  "taro","gourd","Chinese plantain, leaves","Other annual crop","jobs_tears - upln","Sunhemp - mixed cropping",
  "Oranges","Littchi","Pumpkin - lowl","Tomato","French bean - lowl","Jack fruit",
  "Avocado","French bean - upln","Sauropus","Cabbages - upln","Chayote",                            
  "Passion fruit","Macadamia","Pomelos and grapefruits","Winter melon - lowl",
  "Hmong mustard","Local bean","Local mustard","Other perenial crop",
  "Apricot","Winter melon - upln","Rice bean","Arrowroot, root","Potatoes - upln",                    
  "Zucchini, common green, fruit","Potatoes - lowl","Honeydew",
  "Mustard2","Chrysanthemum, leaves","Turmeric, root","Black bean","Kudzu",                              
  "Kohlrabi","Lettuce, romaine, leaves","Chinese kale/ Gailan","Cauliflowers, Broccoli",             
  "Basil, sweet, leaves","Kidney bean","Eggplant - lowl","Sweet potatoes, leaves",             
  "Sawtooth herb / Culantro","Sweet potatoes, tuber - lowl")

          

  
i = "Cambodia"
j = "Broadcasted rice - household consumption"
### 3 - Cropping systems summary
Cropsysb <- Cropsys
Cropsysb$YieldABS <- Cropsysb$`Yield - Kg/ha`*(Cropsysb$`Area - m2`/10000)
Cropsysb$SoldABS <- Cropsysb$`Amount sold - Kg/ha - Total`*(Cropsysb$`Area - m2`/10000)
Cropsysb$AEPW <- rowSums(Cropsysb[,39:46])
Cropsysb$AEPSC <- rowSums(Cropsysb[47:54])
Cropsysb$AEPSF <- rowSums(Cropsysb[55:66])
Cropsysb$AEPW <- rowSums(Cropsysb[67:81])
Cropsysb$AEPPnD <- rowSums(Cropsysb[82:97])
for (i in Sarealist){
 for (j in unique(Cropsyssum$Cropsys[Cropsyssum$Sarea == i])){
   #Number of households concerned
  Cropsyssum$N[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
     round(max(Cropsys$N[Cropsys$Sarea == i & Cropsys$Cropsys == j]), digits = 0)
  #% of households concerned
 Cropsyssum$PerCent.of.households[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   round(Cropsyssum$N[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] / sum(All$SW_Weight[All$S_Area == i]), digits = 2)
 #Nb of varieties
 Cropsyssum$Nb.of.varieties.included.per.specie[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
  sum((Cropsys$NumberSpecies[Cropsys$Sarea == i & Cropsys$Cropsys == j]*Cropsys$N[Cropsys$Sarea == i & Cropsys$Cropsys == j])/
        sum(Cropsys$N[Cropsys$Sarea == i & Cropsys$Cropsys == j]))
 #Self consumed production Rice
 Cropsyssum$Production.for.consumption.kg.ABS.Rice[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$YieldABS[Cropsys$Sarea == i & Cropsys$Cropsys == j & Cropsys$Crop %in% Rice]-
       Cropsysb$SoldABS[Cropsys$Sarea == i & Cropsys$Cropsys == j & Cropsys$Crop %in% Rice])
 #Self consumed production Other gluc crops
 Cropsyssum$Production.for.consumption.kg.ABS.Cassava.corn.soybean[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$YieldABS[Cropsys$Sarea == i & Cropsys$Cropsys == j & Cropsys$Crop %in% Cereo]-
          Cropsysb$SoldABS[Cropsys$Sarea == i & Cropsys$Cropsys == j & Cropsys$Crop %in% Cereo])
 #Self consumed production Other crops
 Cropsyssum$Production.for.consumption.kg.ABS.Fruits.vegetables.etc[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$YieldABS[Cropsys$Sarea == i & Cropsys$Cropsys == j & Cropsys$Crop %in% VegeFr]-
          Cropsysb$SoldABS[Cropsys$Sarea == i & Cropsys$Cropsys == j & Cropsys$Crop %in% VegeFr])
 #Gross product
 #AEPW
 Cropsyssum$Nb.of.AEP.Water.conservation[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$AEPW[Cropsys$Sarea == i & Cropsys$Cropsys == j])
 #AESC
 Cropsyssum$Nb.of.AEP.Soil.conservation[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$AEPSC[Cropsys$Sarea == i & Cropsys$Cropsys == j])
 #AESF
 Cropsyssum$Nb.of.AEP.Soil.fertility.management[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$AEPSF[Cropsys$Sarea == i & Cropsys$Cropsys == j])
 #AEW
 Cropsyssum$Nb.of.AEP.Weed.management[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$AEPW[Cropsys$Sarea == i & Cropsys$Cropsys == j])
 #PnD
 Cropsyssum$Nb.of.AEP.Pest.and.disease.management[Cropsyssum$Sarea == i & Cropsyssum$Cropsys == j] <-
   mean(Cropsysb$AEPPnD[Cropsys$Sarea == i & Cropsys$Cropsys == j])
 }
}

#Required changes
Cropsyssum$Nb.of.varieties.included[Cropsyssum$Cropsys == "Summer-autumn and winter-spring season rice"] <- 
  2*Cropsyssum$Nb.of.varieties.included[Cropsyssum$Cropsys == "Summer-autumn and winter-spring season rice"]
#
Cropsyssum$Production.for.consumption.kg.ABS.Rice[Cropsyssum$Cropsys == "Summer-autumn and winter-spring season rice"] <- 
  2*Cropsyssum$Production.for.consumption.kg.ABS.Rice[Cropsyssum$Cropsys == "Summer-autumn and winter-spring season rice"]
#
Cropsyssum$Production.for.consumption.kg.ABS.Fruits.vegetables.etc <-
  ifelse(!is.nan(Cropsyssum$Production.for.consumption.kg.ABS.Fruits.vegetables.etc),
       Cropsyssum$Production.for.consumption.kg.ABS.Fruits.vegetables.etc,0)
Cropsyssum$Production.for.consumption.kg.ABS.Fruits.vegetables.etc <-
  Cropsyssum$Production.for.consumption.kg.ABS.Fruits.vegetables.etc*
  Cropsyssum$Average.nb.of.plant.species.included
#

#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(Cropsyssum, "Cropsyssum.rds")
