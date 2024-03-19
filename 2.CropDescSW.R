### 1 - Prerequisites

## a - Package loading
library(ggplot2)
library(dplyr)
library(tibble)
library(RColorBrewer)
library(labelled)
library(sjlabelled)
library(stringr)
library(survey)
#Options
options(warn=1)

## b - Work directory and data loading
#We use the output datasets which were displayed during previous mission
#to check if the datasets are clean
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
CropSystems3C_TF <- readRDS("Newdata_TF.rds")
croplist <- readRDS("croplist.rds")



## d - Lists
{
  Sarealist <- c("Cambodia", "Lao", "Dien Bien province", "Son La province")
  HouseholdN <- c(609, 549, 302, 292)
  Sarealist <- cbind(Sarealist,HouseholdN)
  Croplist <- croplist
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
#(Aim: Create a table with hhid as row and country, district/commune information,
#And all crops information as columns, with average value per crop for numeric variable,
#And % of individuals concerned based on households growing this crop for factor variables

#First we create the basis of the table with Area and crop information
CropDesc <- expand.grid(Column1 = Sarealist[,1], Column2 = Croplist)
colnames(CropDesc) <- c("Sarea","Crop")
#No we add the required columns for different variables


#We add all the calculated columns through a pipe
for (i in Sarealist[,1]){
  #We filter data per study area first
  Dumm <- CropSystems3C_TF[CropSystems3C_TF$S_Area == i ,]
  for (j in Croplist){
    #We then filter data per crop type
    Dumm1 <- Dumm %>%
      select(matches(grep(j, names(Dumm), value = TRUE)),starts_with("SW_"))
    ## c - Sampling design
    survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm1)
    #"If" loop to remove non-existent crop-country combination
    if(!all(is.na(Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)]) |
           Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)] == "")){
           #N = Households growing this crop
           CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j] <- sum(Dumm1$SW_Weight[!(is.na(Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)]) |
                                                                           Dumm1[,grep("d2_132", names(Dumm1), value = TRUE)] == "")])
           #PerCentofHouseholds = % of households growing this crop (per study area)
           CropDesc$PerCentofHouseholds[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j]/sum(Dumm1$SW_Weight), digits = 3)
           #Area - m2
           CropDesc$`Area - m2`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(svymean(~ Dumm1[, grep("d2_132", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 3)
           #MSeedUnit
           Ccount <- table(Dumm1[grep("d2_133", names(Dumm1), value = TRUE)])
           Ccount <- Ccount[!is.na(names(Ccount)) & names(Ccount) != ""]
           CropDesc$MSeedUnit[CropDesc$Sarea == i & CropDesc$Crop == j] <- 
             names(Ccount)[which.max(Ccount)]
           #PerCentMSeedUnit
           CropDesc$PerCentMSeedUnit[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == CropDesc$MSeedUnit[CropDesc$Sarea == i & CropDesc$Crop == j]], na.rm = T) /
             CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Seed amount /ha - Kg of seed = Seed amount used for sowing
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed")
           #
           CropDesc$'Seed amount /ha - Kg of seed'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed", na.rm = T) != 0,
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Kg of seed" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
           #Seed amount /ha - Gr of seed
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed")
           #
           CropDesc$'Seed amount /ha - Gr of seed'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed", na.rm = T) != 0,
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Gr of seed" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
           #Seed amount /ha - Number of seedlings
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings")
           #
           CropDesc$'Seed amount /ha - Number of seedlings'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings", na.rm = T) != 0,
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of seedlings" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
           #Seed amount /ha - Number of stems
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems")
           #
           CropDesc$'Seed amount /ha - Number of stems'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems", na.rm = T) != 0,
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Number of stems" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)
           #Seed amount /ha - Tuber
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber")
           #
           CropDesc$'Seed amount /ha - Tuber'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             ifelse(sum(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber", na.rm = T) != 0,
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)] == "Tuber" & !is.na(Dumm1[,grep("d2_133", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha134", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA)    
           #Yield - Kg/ha
           CropDesc$'Yield - Kg/ha'[CropDesc$Sarea == i & CropDesc$Crop == j] <- 
             round(svymean(~ Dumm1[, grep("d2_ha135", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 0)
           #PerCentSellers = % of households selling this crop (% of households of study area)
           CropDesc$PerCentSellers[CropDesc$Sarea == i & CropDesc$Crop == j] <- 
            round(sum(Dumm1$SW_Weight[Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)] > 1], na.rm = T)  /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #MFormWhenSold
           Ccount <- table(Dumm1[grep("d2_139", names(Dumm1), value = TRUE)])
           CropDesc$MFormWhenSold[CropDesc$Sarea == i & CropDesc$Crop == j] <- 
             names(Ccount)[which.max(Ccount)]
           #PerCentMFormWhenSold
           CropDesc$PerCentMFormWhenSold[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == CropDesc$MFormWhenSold[CropDesc$Sarea == i & CropDesc$Crop == j]], na.rm = T) /
             CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Amount sold - Kg/ha - Total
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)] != 0)
           #
           CropDesc$'Amount sold - Kg/ha - Total'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("d2_ha136", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha136", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0)
           #Amount sold - Kg/ha - Dried
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_139", names(Dumm1))] == "Dried")
           #
           CropDesc$'Amount sold - Kg/ha - Dry'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             ifelse(sum(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Dried", na.rm = T) != 0,
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Dried" & !is.na(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha136", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA) 
           #Amount sold - Kg/ha - Fresh
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_139", names(Dumm1))] == "Fresh")
           #
           CropDesc$'Amount sold - Kg/ha - Fresh'[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             ifelse(sum(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Fresh", na.rm = T) != 0,
                    round(svymean(~ Dumm1[Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)] == "Fresh" & !is.na(Dumm1[,grep("d2_139", names(Dumm1), value = TRUE)]),
                                          grep("d2_ha136", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0), NA) 
           #PerCentHarvestSold = Percentage of harvest sold by households who sell
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_ha136", names(Dumm1))] != 0)
           #
           CropDesc$'PerCentHarvestSold'[CropDesc$Sarea == i & CropDesc$Crop == j] <- 
             round(svymean(~ Dumm1[Dumm1[, grep("d2_ha136", names(Dumm1))] != 0 & !is.na(Dumm1[, grep("d2_ha136", names(Dumm1))]),grep("d2_ha136", names(Dumm1), value = TRUE)],
                           design = sub_svy_design, na.rm = TRUE) /
                     svymean(~ Dumm1[Dumm1[, grep("d2_ha136", names(Dumm1))] != 0 & !is.na(Dumm1[, grep("d2_ha136", names(Dumm1))]),grep("d2_ha135", names(Dumm1), value = TRUE)],
                            design = sub_svy_design, na.rm = TRUE), digits = 3)
           #Selling price - $/kg ("0" values removed)
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_137", names(Dumm1), value = TRUE)] != 0)
           #
           CropDesc$`Selling price - $/kg`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(svymean(~ Dumm1[Dumm1[,grep("d2_137", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("d2_137", names(Dumm1), value = TRUE)]),
                                   grep("d2_137", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 2)
           #NumberSpecies (remove values > 10)
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("d2_138", names(Dumm1), value = TRUE)] < 11)
           #
           CropDesc$`NumberSpecies`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(svymean(~ Dumm1[Dumm1[,grep("d2_138", names(Dumm1), value = TRUE)] < 11 & !is.na(Dumm1[,grep("d2_138", names(Dumm1), value = TRUE)]),
                                   grep("d2_138", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 1)
           #GrossProductRAW - $/household ("0" values removed)
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("GrossProductRaw", names(Dumm1), value = TRUE)] != 0)
           #
           CropDesc$`GrossProductRAW - $/household`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(svymean(~ Dumm1[Dumm1[,grep("GrossProductRaw", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("GrossProductRaw", names(Dumm1), value = TRUE)]),
                                   grep("GrossProductRaw", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0)
           #GrossProductHA - $/ha ("0" values removed)
           sub_svy_design <- subset(survey_design, subset = Dumm1[,grep("GrossProductha", names(Dumm1), value = TRUE)] != 0)
           #
           CropDesc$`GrossProduct - $/ha`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(svymean(~ Dumm1[Dumm1[,grep("GrossProductha", names(Dumm1), value = TRUE)] != 0 & !is.na(Dumm1[,grep("GrossProductha", names(Dumm1), value = TRUE)]),
                                   grep("GrossProductha", names(Dumm1))], design = sub_svy_design, na.rm = TRUE), digits = 0)
           
          #Reason% - Household consumption preferences = Why household did grow this crop, % of households growing this crop, 1st option
           CropDesc$`Reason% - Household consumption preferences`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Household consumption preferences"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Reason% - Market price and demand = Why household did grow this crop, % of households growing this crop, 2nd option
           CropDesc$`Reason% - Market price and demand`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Market price and demand"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Reason% - Well adapted to local conditions (soil, climate, …) = Why household did grow this crop, % of households growing this crop, 3rd option
           CropDesc$`Reason% - Well adapted to local conditions`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_a", names(Dumm1), value = TRUE)] == "Well adapted to local conditions (soil, climate, …)"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Diseases = Main constraint faced during cropping, % of households growing this crop, 1st option
           CropDesc$`Constraint% - Diseases`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Diseases"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Insects = Main constraint faced during cropping, % of households growing this crop, 2nd option
           CropDesc$`Constraint% - Insects`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Insects"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Market price = Main constraint faced during cropping, % of households growing this crop, 3rd option
           CropDesc$`Constraint% - Market price`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Market price"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Other agronomic constraints = Main constraint faced during cropping, % of households growing this crop, 4th option
           CropDesc$`Constraint% - Other agronomic constraints`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Other agronomic constraints"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Product quality = Main constraint faced during cropping, % of households growing this crop, 5th option
           CropDesc$`Constraint% - Product quality`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Product quality"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Soil fertility = Main constraint faced during cropping, % of households growing this crop, 6th option
           CropDesc$`Constraint% - Soil fertility`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Soil fertility"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Water management = Main constraint faced during cropping, % of households growing this crop, 7th option
           CropDesc$`Constraint% - Water management`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Water management"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - Weed = Main constraint faced during cropping, % of households growing this crop, 8th option
           CropDesc$`Constraint% - Weed`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "Weed"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           #Constraint% - No constraint = Main constraint faced during cropping, % of households growing this crop, 9th option
           CropDesc$`Constraint% - No constraint`[CropDesc$Sarea == i & CropDesc$Crop == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("d81_b", names(Dumm1), value = TRUE)] == "No constraint"], na.rm = T) /
                     CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3)
           for (k in 1:ncol(AEP)){
             #Loop for different AEP practices with associated categories
             Dum <- paste0(AEP[1,k],"-",AEP[2,k])
             if (!(Dum %in% names(CropDesc))) {
               # Create the column at the first round
               CropDesc <- add_column(CropDesc, !!Dum := NA_real_)
             }
             CropDesc[[Dum]] <- ifelse(CropDesc$Sarea == i & CropDesc$Crop == j,
                                       round(sum(Dumm1$SW_Weight[!is.na(Dumm1[, grep(AEP[3, k], names(Dumm1), value = TRUE)]) & 
                                                   Dumm1[, grep(AEP[3, k], names(Dumm1), value = TRUE)] != 0], na.rm = TRUE) /
                                               CropDesc$N[CropDesc$Sarea == i & CropDesc$Crop == j], digits = 3), 
                                       CropDesc[[Dum]])
      }
    }
  }
}

#We remove/replace unlikely values
CropDesc$`Amount sold - Kg/ha - Total` <- ifelse(CropDesc$`Amount sold - Kg/ha - Total` > CropDesc$`Yield - Kg/ha`,
                                                 CropDesc$`Yield - Kg/ha`,CropDesc$`Amount sold - Kg/ha - Total`)
CropDesc$`Reason% - Market price and demand` <- ifelse(CropDesc$`Reason% - Market price and demand` > 1,
                                                 1,CropDesc$`Reason% - Market price and demand`)
CropDesc$`AEP SoilFert-Animal manure` <- ifelse(CropDesc$`AEP SoilFert-Animal manure` > 1,
                                                       1,CropDesc$`AEP SoilFert-Animal manure`)
CropDesc$`AEP SoilFert-Organic agro-industrial waste` <- ifelse(CropDesc$`AEP SoilFert-Organic agro-industrial waste` > 1,
                                                1,CropDesc$`AEP SoilFert-Organic agro-industrial waste`)
CropDesc$`AEP Weed-Crop rotation / intercropping` <- ifelse(CropDesc$`AEP Weed-Crop rotation / intercropping` > 1,
                                                                1,CropDesc$`AEP Weed-Crop rotation / intercropping`)
CropDesc$`Obs-Crops requiring more water than others` <- ifelse(CropDesc$`Obs-Crops requiring more water than others` > 1,
                                                            1,CropDesc$`Obs-Crops requiring more water than others`)
CropDesc$`Obs-Crops requiring heavy fertilization to thrive` <- ifelse(CropDesc$`Obs-Crops requiring heavy fertilization to thrive` > 1,
                                                                1,CropDesc$`Obs-Crops requiring heavy fertilization to thrive`)
CropDesc$`-Conservation of traditionnal, local seeds` <- ifelse(CropDesc$`-Conservation of traditionnal, local seeds` > 1,
                                                                       1,CropDesc$`-Conservation of traditionnal, local seeds`)
#We remove useless rows
CropDesc2 <- CropDesc[!is.na(CropDesc$N),]

#We modify one last column content
CropDesc2$MFormWhenSold <- ifelse(CropDesc2$MFormWhenSold == "",
                                  "Unknown",CropDesc2$MFormWhenSold)

#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(CropDesc2, "CropDesc_TF.rds")


#Pense-bête
#Crops: 7 -> 8290 
#Reasons AEP: 8291 -> 8349
#Additionnal info Practices: 8350 -> 8361
#HHmembers: 8362 -> 9081
#Labor general: 9082 -> 9091, 9136
#Labor Tasks: 9092 -> 9135
#Services: 9137 -> 9148
#Equipments: 9149 -> 9167


