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
Cropsys <- read.csv2("CropsysCamb.csv")
Cropsys <- Cropsys[,1:4]


### 2. Cambodia - We display important statistics for each cropping system
for (col_name in names(CropDesc[,3:99])){
  Cropsys[[col_name]] <- NA
}


  
## d - Lists
{
  Croplist <- unique(Cropsys$Crop)
  x <- c("N", "PerCentofHouseholds","Area - ha")
  y <- c("","","d2_132")
  Varlist <- data.frame(x,y)
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
}


### 2 - Crop average values Table

#We add all the calculated columns through a pipe
Dumm <- All[All$S_Area == "Cambodia" ,]
i = "Cambodia"
k = 1
  for (k in 1:nrow(Cropsys)){
    j <- Cropsys$Crop[k]
    Dumm1 <- Dumm %>%
      select("hhid_re2", matches(Cropsys$Crop[k]))
     if(Cropsys$Process[k] == "Norm"){
     Cropsys[k,5:101] <- CropDesc[CropDesc$Crop == Cropsys$Crop[k],3:99]
      }else{if(Cropsys$Process[k] == "No"){
       Cropsys[k,5:101] <- NA
       }else{if(Cropsys$Process[k] == "Cons"){
        Dumm1 <- Dumm1[is.na(Dumm1[,9]) | Dumm1[,9] == 0,] 
        }else{if(Cropsys$Process[k] == "Sell"){
         Dumm1 <- Dumm1[Dumm1[,9] > 0 & !is.na(Dumm1[,9]),]
         }else{if(Cropsys$Process[k] == "Prod"){
          Dumm1 <- Dumm1[Dumm1[,13] > 0 & !is.na(Dumm1[,13]),]
          }else{if(Cropsys$Process[k] == "NoProd"){
           Dumm1 <- Dumm1[is.na(Dumm1[,13]) | Dumm1[,13] == 0,]
          }else{
            x <- strsplit(gsub("\"", "", Cropsys$Process[k]), ",")[[1]]
            Dumm1 <- Dumm1[Dumm1$hhid_re2 %in% x,]  
          }}}}
         #N = Households growing this crop
         Cropsys$N[k] <- sum(!(is.na(Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)]) |
                                                                      Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)] == ""))
         #PerCentofHouseholds = % of households growing this crop (per study area)
         Cropsys$PerCentofHouseholds[k] <-
           round(Cropsys$N[k]/nrow(Dumm), digits = 3)
         #Area - m2
         Cropsys$`Area - m2`[k] <-
           round(mean(Dumm1[,grep("d2_132", names(Dumm1))], na.rm = T), digits = 0)
         #MSeedUnit
         Ccount <- table(Dumm1[grep("d2_133", names(Dumm1), value = TRUE)])
         Ccount <- Ccount[!is.na(names(Ccount)) & names(Ccount) != ""]
         Cropsys$MSeedUnit[k] <- 
           names(Ccount)[which.max(Ccount)]
         #PerCentMSeedUnit
         Cropsys$PerCentMSeedUnit[k] <-
           round(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == Cropsys$MSeedUnit[k], na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Seed amount /ha - Kg of seed = Seed amount used for sowing
         Cropsys$'Seed amount /ha - Kg of seed'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed", na.rm = T) != 0,
                  round(mean(Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed",grep("d2_ha134", names(Dumm1))], na.rm = T), digits = 0), NA)
         #Seed amount /ha - Gr of seed
         Cropsys$'Seed amount /ha - Gr of seed'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed", na.rm = T) != 0,
                  round(mean(Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed",grep("d2_ha134", names(Dumm1))], na.rm = T), digits = 0), NA)
         #Seed amount /ha - Number of seedlings
         Cropsys$'Seed amount /ha - Number of seedlings'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings", na.rm = T) != 0,
                  round(mean(Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings",grep("d2_ha134", names(Dumm1))], na.rm = T), digits = 0), NA)
         #Seed amount /ha - Number of stems
         Cropsys$'Seed amount /ha - Number of stems'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems", na.rm = T) != 0,
                  round(mean(Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems",grep("d2_ha134", names(Dumm1))], na.rm = T), digits = 0), NA)
         #Seed amount /ha - Tuber
         Cropsys$'Seed amount /ha - Tuber'[k] <-
           ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber", na.rm = T) != 0,
                  round(mean(Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber",grep("d2_ha134", names(Dumm1))], na.rm = T), digits = 0), NA)
         #Yield - Kg/ha
         Cropsys$'Yield - Kg/ha'[k] <- 
           round(mean(Dumm1[,grep("d2_ha135", names(Dumm1))], na.rm = T), digits = 0)
         #PerCentSellers = % of households selling this crop (% of households of study area)
         Cropsys$PerCentSellers[k] <- 
           round(sum(Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)] > 1, na.rm = T)  /
                   Cropsys$N[k], digits = 3)
         #MFormWhenSold
         Ccount <- table(Dumm1[grep("d2_139", names(Dumm1), value = TRUE)])
         Cropsys$MFormWhenSold[k] <- 
           names(Ccount)[which.max(Ccount)]
         #PerCentMFormWhenSold
         Cropsys$PerCentMFormWhenSold[k] <-
           round(sum(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == Cropsys$MFormWhenSold[k], na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Amount sold - Kg/ha - Total
         Cropsys$'Amount sold - Kg/ha - Total'[k] <-
           round(mean(Dumm1[Dumm1[, grep("d2_ha136", names(Dumm1))] != 0,grep("d2_ha136", names(Dumm1))], na.rm = T), digits = 0)
         #Amount sold - Kg/ha - Dried
         Cropsys$'Amount sold - Kg/ha - Dry'[k] <-
           ifelse(sum(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Dried", na.rm = T) != 0,
                  round(mean(Dumm1[Dumm1[, grep("d2_139", names(Dumm1))] == "Dried", grep("d2_ha136", names(Dumm1))], na.rm = T), digits = 0), NA)
         #Amount sold - Kg/ha - Fresh
         Cropsys$'Amount sold - Kg/ha - Fresh'[k] <-
           ifelse(sum(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Fresh", na.rm = T) != 0,
                  round(mean(Dumm1[Dumm1[, grep("d2_139", names(Dumm1))] == "Fresh", grep("d2_ha136", names(Dumm1))], na.rm = T), digits = 0), NA)
         #PerCentHarvestSold = Percentage of harvest sold by households who sell
         Cropsys$'PerCentHarvestSold'[k] <- 
           round(mean(Dumm1[Dumm1[, grep("d2_ha136", names(Dumm1))] != 0,grep("d2_ha136", names(Dumm1), value = TRUE)] /
                        Dumm1[Dumm1[, grep("d2_ha136", names(Dumm1))] != 0,grep("d2_ha135", names(Dumm1), value = TRUE)], na.rm = T), digits = 3)
         #Selling price - $/kg ("0" values removed)
         Cropsys$`Selling price - $/kg`[k] <-
           round(mean(Dumm1[Dumm1[,grep("d2_137", names(Dumm1))] != 0,grep("d2_137", names(Dumm1))], na.rm = T), digits = 2)
         #NumberSpecies (remove values > 10)
         Cropsys$`NumberSpecies`[k] <-
           round(mean(Dumm1[Dumm1[,grep("d2_138", names(Dumm1))] < 11,grep("d2_138", names(Dumm1))], na.rm = T), digits = 1)
         #GrossProductRAW - $/household ("0" values removed)
         Cropsys$`GrossProductRAW - $/household`[k] <-
           round(mean(Dumm1[Dumm1[,grep("GrossProductRaw", names(Dumm1))] != 0,grep("GrossProductRaw", names(Dumm1))], na.rm = T), digits = 0)
         #GrossProductHA - $/ha ("0" values removed)
         Cropsys$`GrossProduct - $/ha`[k] <-
           round(mean(Dumm1[Dumm1[,grep("GrossProductha", names(Dumm1))] != 0,grep("GrossProductha", names(Dumm1))], na.rm = T), digits = 0)
         #Reason% - Household consumption preferences = Why household did grow this crop, % of households growing this crop, 1st option
         Cropsys$`Reason% - Household consumption preferences`[k] <-
           round(sum(Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Household consumption preferences", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Reason% - Market price and demand = Why household did grow this crop, % of households growing this crop, 2nd option
         Cropsys$`Reason% - Market price and demand`[k] <-
           round(sum(Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Market price and demand", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Reason% - Well adapted to local conditions (soil, climate, …) = Why household did grow this crop, % of households growing this crop, 3rd option
         Cropsys$`Reason% - Well adapted to local conditions`[k] <-
           round(sum(Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Well adapted to local conditions (soil, climate, …)", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Diseases = Main constraint faced during cropping, % of households growing this crop, 1st option
         Cropsys$`Constraint% - Diseases`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Diseases", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Insects = Main constraint faced during cropping, % of households growing this crop, 2nd option
         Cropsys$`Constraint% - Insects`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Insects", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Market price = Main constraint faced during cropping, % of households growing this crop, 3rd option
         Cropsys$`Constraint% - Market price`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Market price", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Other agronomic constraints = Main constraint faced during cropping, % of households growing this crop, 4th option
         Cropsys$`Constraint% - Other agronomic constraints`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Other agronomic constraints", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Product quality = Main constraint faced during cropping, % of households growing this crop, 5th option
         Cropsys$`Constraint% - Product quality`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Product quality", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Soil fertility = Main constraint faced during cropping, % of households growing this crop, 6th option
         Cropsys$`Constraint% - Soil fertility`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Soil fertility", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Water management = Main constraint faced during cropping, % of households growing this crop, 7th option
         Cropsys$`Constraint% - Water management`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Water management", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - Weed = Main constraint faced during cropping, % of households growing this crop, 8th option
         Cropsys$`Constraint% - Weed`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Weed", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         #Constraint% - No constraint = Main constraint faced during cropping, % of households growing this crop, 9th option
         Cropsys$`Constraint% - No constraint`[k] <-
           round(sum(Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "No constraint", na.rm = T) /
                   Cropsys$N[k], digits = 3)
         for (l in 1:ncol(AEP)){
           #Loop for different AEP practices with associated categories
           Dum <- paste0(AEP[1,l],"-",AEP[2,l])
           Cropsys[[Dum]] <- ifelse(Cropsys$Crop == Cropsys$Crop[k] & Cropsys$Cropsys == Cropsys$Cropsys[k],
                                    round(sum(!is.na(Dumm1[, grep(AEP[3, l], names(Dumm1), value = TRUE)]) & 
                                                Dumm1[, grep(AEP[3, l], names(Dumm1), value = TRUE)] != 0, na.rm = TRUE) /
                                            Cropsys$N[Cropsys$Crop == Cropsys$Crop[k] & Cropsys$Cropsys == Cropsys$Cropsys[k]], digits = 3), 
                                    Cropsys[[Dum]])
         }
       }
}}

    



  

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








