### 1 - Prerequisites

## a - Package loading
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
CropSystems3C_TF <- readRDS("NewdataProd_TF.rds")
ProdsysID <- read.csv2("ProdSysID.csv")
ProdsysID$hhid_re2 <- as.character(ProdsysID$hhid_re2)
HouseSys <- read.csv2("Prodsyssum.csv")

CropSystems3C_TF <- full_join(CropSystems3C_TF, ProdsysID[,c(1,6)], by = "hhid_re2")

#Part for getting proper % for households
survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = CropSystems3C_TF)
sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF[,11627] > 0 & CropSystems3C_TF$S_Area == "Dien Bien province"], na.rm = T) / 
  sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Dien Bien province"])
dumm <- CropSystems3C_TF[CropSystems3C_TF[,11627] > 0 & CropSystems3C_TF$S_Area == "Dien Bien province",]
survey_designb <- svydesign(ids = ~1, weights = ~SW_Weight, data = dumm)
round(svymean(~ dumm[,11627], design = survey_designb, na.rm = TRUE), digits = 2)
#Forage = 11628
sum(CropSystems3C_TF$SW_Weight[(CropSystems3C_TF[,11619] > 0 | CropSystems3C_TF[,11621] > 0 | CropSystems3C_TF[,11620] > 0 | CropSystems3C_TF[,11620] > 0
                                | CropSystems3C_TF[,11620] > 0 | CropSystems3C_TF[,11620] > 0) & CropSystems3C_TF$S_Area == "Dien Bien province"], na.rm = T) / 
sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Dien Bien province"])
sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF[,11700] == "PFCa" & CropSystems3C_TF$S_Area == "Dien Bien province"], na.rm = T) / 
  sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Dien Bien province"])

#Part for creating crop summary table
Cropsyssum <- read.csv2("Cropsyssum.csv")
#Some columns must be renamed
colnames(CropSystems3C_TF)[11549:11550] <- gsub("\\$", "c", colnames(CropSystems3C_TF)[11549:11550])
#Loop for table fulfilling
for (i in 1:nrow(Cropsyssum)){
  #Subsampling
  Dumm <- CropSystems3C_TF[CropSystems3C_TF[grep(Cropsyssum$Code[i], names(CropSystems3C_TF))] > 0,]
  #Survey design creation
  survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm)
  #N - Number of households
  Cropsyssum$N[i] <- round(sum(Dumm$SW_Weight, na.rm = T), digits = 0)
  #PerCent of households concerned (N study area)
  Cropsyssum$PerCentHouseholds[i] <- round(sum(Dumm$SW_Weight, na.rm = T) /
    sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == Cropsyssum$Sarea[i]]), digits = 2)
  #We create a list with all the crops included in this system
  chaine <- Cropsyssum$Included.crops[i]
  crops <- strsplit(chaine, "\\|")[[1]]
  #We subset the dataframe again based on the crops required
  Dumm1 <- Dumm[,grep(Cropsyssum$Included.crops[i], names(Dumm), value = TRUE)]
    if(length(crops) > 1){
      #Number of crop species included
      Cropsyssum$NbSpecies[i] <- round(mean(rowSums(Dumm1[,grep("d2_132.", names(Dumm1),value = T)] > 0,na.rm = T)), digits = 1)
      #Number of varieties included
      Cropsyssum$NbVarieties[i] <- round(mean(rowSums(Dumm1[,grep("d2_138.", names(Dumm1),value = T)],na.rm = T)), digits = 1)
      #Average area
      x <- apply(Dumm1[,grep("d2_132.", names(Dumm1),value = T)], 1, function(row) max(row, na.rm = TRUE))
      Cropsyssum$Areaha[i] <- round(mean(x[!x == "-Inf"], na.rm = T) / 10000, digits = 2)
      #Production for consumption (/ha) - Rice
      Dumm2 <- Dumm1[,grepl("rice|Rice", names(Dumm1))]
      if(Cropsyssum$Code[i] %in% c("RdbS","RdbD","RVoD","RVoS")){
      Cropsyssum$ProdConsRiceha[i] <- round(mean(rowSums(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)],na.rm = T) - rowSums(Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)],na.rm = T)), digits = 0)
      } else {
      Cropsyssum$ProdConsRiceha[i] <- round(mean(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)] - Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)], na.rm = T), digits = 0)}
      #Production for consumption (/ha) - Cassava,Corn,Soybean
      Dumm2 <- Dumm1[,grepl("cassava|Cassava|soybean|Soybean|maize|Maize|corn|Corn", names(Dumm1))]
      if(ncol(Dumm2) >81 ){
      Cropsyssum$ProdConsCropha[i] <- round(mean(rowSums(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)],na.rm = T) - rowSums(Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)],na.rm = T)), digits = 0)
      } else if (ncol(Dumm2) > 0){
      Cropsyssum$ProdConsCropha[i] <- round(mean(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)] - Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)]), digits = 0)
      }
      #Production for consumption (/ha) - Fruit,vegetables,other
      Dumm2 <- Dumm1[,!grepl("rice|Rice|cassava|Cassava|soybean|Soybean|maize|Maize|corn|Corn", names(Dumm1))]
      if (!"ProdConsVegeFruitOha" %in% names(Cropsyssum)) {
      # Créer une nouvelle colonne remplie de NA
      Cropsyssum$ProdConsVegeFruitOha <- NA
      }
        if(ncol(Dumm2) >81 ){
      Cropsyssum$ProdConsVegeFruitOha[i] <- round(mean(rowSums(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)],na.rm = T) - rowSums(Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)],na.rm = T)), digits = 0)
      } else if (ncol(Dumm2) > 0){
      Cropsyssum$ProdConsVegeFruitOha[i] <- round(mean(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)] - Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)]), digits = 0)
      }
      Cropsyssum$PerCentHLocalSeeds[i] <- round(mean(rowSums(Dumm1[,grep("d32.", names(Dumm1),value = T)] == "1", na.rm = T), na.rm = T), digits = 2)
    } else {
      #Number of crop species included
      Cropsyssum$NbSpecies[i] <- 1
      #Number of varieties included
      Cropsyssum$NbVarieties[i] <- round(mean(Dumm1[,grep("d2_138.", names(Dumm1),value = T)]), digits = 0)
      #Average area
      Cropsyssum$Areaha[i] <- round(mean(Dumm1[,grep("d2_132.", names(Dumm1),value = T)])/10000, digits = 2)
      #Production for consumption (/ha) - Rice
      Dumm2 <- Dumm1[,grepl("rice|Rice", names(Dumm1))]
      Cropsyssum$ProdConsRiceha[i] <-round(mean(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)] - Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)],na.rm = T), digits = 0)
      #Production for consumption (/ha) - Cassava,Corn,Soybean
      Dumm2 <- Dumm1[,grepl("cassava|Cassava|soybean|Soybean|maize|Maize|corn|Corn", names(Dumm1))]
      if(ncol(Dumm2) >0 ){
        Cropsyssum$ProdConsCropha[i] <- round(mean(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)] - Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)],na.rm = T), digits = 0)
      }
      #Production for consumption (/ha) - Fruit,vegetables,other
      Dumm2 <- Dumm1[,!grepl("rice|Rice|cassava|Cassava|soybean|Soybean|maize|Maize|corn|Corn", names(Dumm1))]
        if (ncol(Dumm2) > 0){
        Cropsyssum$ProdConsVegeFruitOha[i] <- round(mean(Dumm2[,grep("d2_ha135.", names(Dumm2),value = T)] - Dumm2[,grep("d2_ha136.", names(Dumm2),value = T)]), digits = 0)
        }
      Cropsyssum$PerCentHLocalSeeds[i] <- round(mean(Dumm1[,grep("d32.", names(Dumm1),value = T)] == "1", na.rm = T), digits = 2)
    }
  #Nb of practices for water conservation
  Cropsyssum$NbAEPracticesWaterCons[i] <- round(mean(rowSums(Dumm1[,grep("d13", names(Dumm1),value = T)] == "1", na.rm = T)), digits = 2)
  #Nb of practices for soil conservation
  Cropsyssum$NbAEPracticesSoilCons[i] <- round(mean(rowSums(Dumm1[,grep("d15", names(Dumm1),value = T)] == "1", na.rm = T)), digits = 2)
  #Nb of practices for soil fertility management
  Cropsyssum$NbAEPracticesSoilFert[i] <- round(mean(rowSums(Dumm1[,grep("d18", names(Dumm1),value = T)] == "1", na.rm = T)), digits = 2)
  #Nb of practices for weeds management
  Cropsyssum$NbAEPracticesWeed[i] <- round(mean(rowSums(Dumm1[,grep("d21", names(Dumm1),value = T)] == "1", na.rm = T)), digits = 2)
  #Nb of practices for pest and disease management
  Cropsyssum$NbAEPracticesPestDisease[i] <- round(mean(rowSums(Dumm1[,grep("d27", names(Dumm1),value = T)] == "1", na.rm = T)), digits = 2)
  }

#Post-processing
Cropsyssum <- Cropsyssum %>%   relocate(ProdConsCropha,ProdConsVegeFruitOha, .after = ProdConsRiceha)
colnames(Cropsyssum)[2:12] <- c("Cropping system","code","includedCrops","Type","NbHouseholds",
                          "%Households","NbCropSpecies","NbCropVarieties",
                          "Area(ha)","RiceProdSelfCons(KGperHa)","CassavaMaizeSoybeanProdSelfCons(KGperHa)",
                          "FruitVegeOtherCropsProdSelfCons(KGperHa)")

#Table export
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(Cropsyssum[,-c(3:4,15:19)], "Cropsyssum1.rds")
saveRDS(Cropsyssum[,-c(3:4,8:14)], "Cropsyssum2.rds")


#Part for creating animal summary table
Animalsyssum <- read.csv2("Animalsyssum.csv")
#Loop for table fulfilling
for (i in 1:nrow(Animalsyssum)){
  #Subsampling
  Dumm <- CropSystems3C_TF[CropSystems3C_TF[grep(Animalsyssum$Code[i], names(CropSystems3C_TF))] > 0,]
  #Survey design creation
  survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm)
  #N - Number of households
  Animalsyssum$N[i] <- round(sum(Dumm$SW_Weight, na.rm = T), digits = 0)
  #PerCent of households concerned (N study area)
  Animalsyssum$PerCentHouseholds[i] <- round(sum(Dumm$SW_Weight, na.rm = T) /
                                             sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == Animalsyssum$Sarea[i]]), digits = 2)
  #Nb head adult
  Animalsyssum$NbHeadAdult[i] <- round(mean(Dumm[,grep(Animalsyssum$Code[i],names(Dumm))], na.rm = T), digits = 1)
  #Loop for each animal category
  if(grepl("Cattle", Animalsyssum$Included.animals[i])){
    #Nb head young
    Animalsyssum$NbHeadYoung[i] <- round(mean(rowSums(Dumm[c("e3_2", "e3_4")], na.rm = TRUE)), digits = 1)
    #Nb animals consumed (%held)
    Dumm1 <- Dumm[,grep(Animalsyssum$Included.animals[i], names(Dumm), value = TRUE)]
    Animalsyssum$'AnimalSelfConsumed-PerCentadult'[i] <- round(mean(rowSums(Dumm1[,grep("e5_2.", names(Dumm1))], na.rm = T) / 
      Dumm[,grep(Animalsyssum$Code[i],names(Dumm))]), digits = 2)
    #%mortality
    Animalsyssum$'NbAnimalDead-PerCentadult'[i] <- round(mean(rowSums(Dumm1[,grep("e5_1.", names(Dumm1))], na.rm = T) / 
                                                   Dumm[,grep(Animalsyssum$Code[i],names(Dumm))]), digits = 2)
    #%using concentrate from the farm
    Animalsyssum$PerCentHUsingConcentrateFarm[i] <- round(sum(Dumm$SW_Weight[Dumm$e25_11 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using concentrate from the market
    Animalsyssum$PerCentHUsingConcentrateMarket[i] <- round(sum(Dumm$SW_Weight[Dumm$e25_12 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using traditionnal treatment
    Animalsyssum$PerCentHUsingTreatmentTraditionnal[i] <- round(sum(Dumm$SW_Weight[Dumm$e271 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using chemical treatment
    Animalsyssum$PerCentHUsingTreatmentChemical[i] <- round(sum(Dumm$SW_Weight[Dumm$e272 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics as a treatment for diseases
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e291 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics to prevent diseases
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e292 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics to promote animal growth
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e293 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%h having a contract with the buyer
    Animalsyssum$ContractSelling[i] <- round(sum(Dumm$SW_Weight[!(Dumm1[,grep("b21_1.Cattle", names(Dumm1))] == "" |
                                                  Dumm1[,grep("b21_1.Cattle", names(Dumm1))] ==  "No contract/ no prior arrangements" |
                                                  Dumm1[,grep("b21_1.Cattle", names(Dumm1))] == "Do not know")], na.rm = T) / 
                                                              sum(Dumm$SW_Weight), digits = 2)
  } else if (grepl("Pig", Animalsyssum$Included.animals[i])){
    #Nb head young
    Animalsyssum$NbHeadYoung[i] <- round(mean(Dumm$e3_5, na.rm = T), digits = 1)
    #Nb animals consumed (%held)
    Dumm1 <- Dumm[,grep(Animalsyssum$Included.animals[i], names(Dumm), value = TRUE)]
    Animalsyssum$'AnimalSelfConsumed-PerCentadult'[i] <- round(mean(Dumm1[,grep("e5_2.", names(Dumm1))] / 
                                                              Dumm[,grep(Animalsyssum$Code[i],names(Dumm))]), digits = 2)
    #%mortality
    Animalsyssum$'NbAnimalDead-PerCentadult'[i] <- round(mean(Dumm1[,grep("e5_1.", names(Dumm1))] / 
                                                        Dumm[,grep(Animalsyssum$Code[i],names(Dumm))]), digits = 2)
    #%using concentrate from the farm
    Animalsyssum$PerCentHUsingConcentrateFarm[i] <- round(sum(Dumm$SW_Weight[Dumm$e39_11 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using concentrate from the market
    Animalsyssum$PerCentHUsingConcentrateMarket[i] <- round(sum(Dumm$SW_Weight[Dumm$e39_12 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using traditionnal treatment
    Animalsyssum$PerCentHUsingTreatmentTraditionnal[i] <- round(sum(Dumm$SW_Weight[Dumm$e411 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using chemical treatment
    Animalsyssum$PerCentHUsingTreatmentChemical[i] <- round(sum(Dumm$SW_Weight[Dumm$e412 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics as a treatment for diseases
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e431 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics to prevent diseases
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e432 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics to promote animal growth
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e433 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%h having a contract with the buyer
    Animalsyssum$ContractSelling[i] <- round(sum(Dumm$SW_Weight[!(Dumm1[,grep("b21_1.Pig", names(Dumm1))] == "" |
                                                              Dumm1[,grep("b21_1.Pig", names(Dumm1))] ==  "No contract/ no prior arrangements" |
                                                              Dumm1[,grep("b21_1.Pig", names(Dumm1))] == "Do not know")], na.rm = T) / 
      sum(Dumm$SW_Weight), digits = 2)
  } else if (grepl("Chicken", Animalsyssum$Included.animals[i])){
    #Nb head young
    Animalsyssum$NbHeadYoung[i] <- NA
    #Nb animals consumed (%held)
    Dumm1 <- Dumm[,grep(Animalsyssum$Included.animals[i], names(Dumm), value = TRUE)]
    Animalsyssum$'AnimalSelfConsumed-PerCentadult'[i] <- round(mean(rowSums(Dumm1[,grep("e5_2.", names(Dumm1))], na.rm = T) / 
                                                                      Dumm[,grep(Animalsyssum$Code[i],names(Dumm))]), digits = 2)
    #%mortality
    Animalsyssum$'NbAnimalDead-PerCentadult'[i] <- round(mean(rowSums(Dumm1[,grep("e5_1.", names(Dumm1))], na.rm = T) / 
                                                                Dumm[,grep(Animalsyssum$Code[i],names(Dumm))]), digits = 2)
    #%using concentrate from the farm
    Animalsyssum$PerCentHUsingConcentrateFarm[i] <- round(sum(Dumm$SW_Weight[Dumm$e53_11 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using concentrate from the market
    Animalsyssum$PerCentHUsingConcentrateMarket[i] <- round(sum(Dumm$SW_Weight[Dumm$e53_12 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using traditionnal treatment
    Animalsyssum$PerCentHUsingTreatmentTraditionnal[i] <- round(sum(Dumm$SW_Weight[Dumm$e551 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%using chemical treatment
    Animalsyssum$PerCentHUsingTreatmentChemical[i] <- round(sum(Dumm$SW_Weight[Dumm$e552 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics as a treatment for diseases
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e571 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics to prevent diseases
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e572 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #% used antibiotics to promote animal growth
    Animalsyssum$PerCentHUsingAntibioticTreatlment[i] <- round(sum(Dumm$SW_Weight[Dumm$e573 == '1'], na.rm = T) /
      sum(Dumm$SW_Weight, na.rm = T), digits = 2)
    #%h having a contract with the buyer
    Animalsyssum$ContractSelling[i] <- round(sum(Dumm$SW_Weight[!(Dumm1[,grep("b21_1.Chicken", names(Dumm1))] == "" |
                                                              Dumm1[,grep("b21_1.Chicken", names(Dumm1))] ==  "No contract/ no prior arrangements" |
                                                              Dumm1[,grep("b21_1.Chicken", names(Dumm1))] == "Do not know")], na.rm = T) / 
      sum(Dumm$SW_Weight), digits = 2)

  }
}


#Table export
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(Animalsyssum[,-c(3:4,13:17)], "Animalsyssum1.rds")
saveRDS(Animalsyssum[,-c(3:4,8:12)], "Animalsyssum2.rds")


# Part for agroecological scores
#Data import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_cleaning&Analysis-Titouan2023/ASSETOutputs/ASSET_AEScores")
AEScores <- read.csv2("AEScores-SubComp.csv")
#We merge it with table including data
colnames(AEScores)[2] <- "hhid_re2"
AEScores$hhid_re2 <- as.character(AEScores$hhid_re2)
CropSystems3C_TF <- full_join(CropSystems3C_TF,AEScores, by = "hhid_re2")
#File saving under R format
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(CropSystems3C_TF, "NewdataProdAE_TF.rds")
#We import other required files
HouseSys2 <- readRDS("HouseSys.rds")
#We create new columns
CropSystems3C_TF$LRiceTot <- rowSums(CropSystems3C_TF[c("RbcC","RbsC","RVLC",
"RtcC","RtsC","RVoL","RlaL","RVoS","RwsS","RdbS","RsaS","RVoD","RsaD","RdbD",
"RwsD")])
CropSystems3C_TF$MaizeTot <- rowSums(CropSystems3C_TF[c("MtsL","MtcL","MhsL",
"MbiS","MgsS","MgcS","MgsD","MgcD","MbiD")])
CropSystems3C_TF$CassavaTot <-  rowSums(CropSystems3C_TF[c("CAcC","CAcS","CAsS",
"CAsD")])
CropSystems3C_TF$FruitTot <- rowSums(CropSystems3C_TF[c("FTmC","FTRL","FRAS",
"FRPS","FRMS","FRLS","FROS","FTMD")])
CropSystems3C_TF$CashewTot <- rowSums(CropSystems3C_TF[c("CWiC","CWpC","CWyC")])
CropSystems3C_TF$OtherCropsTot <- rowSums(CropSystems3C_TF[c("SOcC","URcC","StrL",
"VulL","PEAL","JOTL","GinL","URcL","TeaL","RVoS","TeaS","VupS","URcS","GinS",
"ArrS","VupD","URcD")])
CropSystems3C_TF$CattleBuffTot <- rowSums(CropSystems3C_TF[c("RUsC","RUlC","RUsL",
"RUlL","RUsS","RUgD","RUcD")])
CropSystems3C_TF$PigTot <- rowSums(CropSystems3C_TF[c("PmaC","PpkC","PfkC","PmaL",
"PpkL","PfkL","PmaS","PpkS","PfkS","PmaD","PkiD")])
CropSystems3C_TF$PoultryTot <- rowSums(CropSystems3C_TF[c("POlC","POsC","POeL",
"POlL","POsL","POsS","POlS","POsD","POlD")])
CropSystems3C_TF <- CropSystems3C_TF %>%
  relocate(LRiceTot,MaizeTot,CassavaTot, FruitTot,
           OtherCropsTot,CattleBuffTot,PigTot,PoultryTot, .after = ForagePastureTot)
CropSystems3C_TF$PoultryTot <- rowSums(CropSystems3C_TF[c("POlC","POsC","POeL",
"POlL","POsL","POsS","POlS","POsD","POlD")])
CropSystems3C_TF$RiceConsumedha <- rowSums(CropSystems3C_TF[c("d2_ha135.Wet season rice broadcast",
                                              "d2_ha135.Wet season rice transplant",
                                              "d2_ha135.Upland rice - mixed cropping",
                                              "d2_ha135.Upland rice2","d2_ha135.Summer-autumn season rice",
                                              "d2_ha135.Winter-Spring season rice")], na.rm = T) -
                                   rowSums(CropSystems3C_TF[c("d2_ha136.Wet season rice broadcast",
                                              "d2_ha136.Wet season rice transplant",
                                              "d2_ha136.Upland rice - mixed cropping",
                                              "d2_ha136.Upland rice2","d2_ha136.Summer-autumn season rice",
                                              "d2_ha136.Winter-Spring season rice")], na.rm = T)
CropSystems3C_TF$CassavaCornSoybeanConsumedha <- rowSums(CropSystems3C_TF[c("d2_ha135.Cassava2",
                                                  "d2_ha135.Cassava - mixed cropping",
                                                  "d2_ha135.Soybean2","d2_ha135.Soybean - mixed cropping",
                                                  "d2_ha135.maize_traditional","d2_ha135.maize_hybrid",
                                                  "d2_ha135.Maize biomass","d2_ha135.Maize corn")], na.rm = T) -
                                                 rowSums(CropSystems3C_TF[c("d2_ha136.Cassava2",
                                                  "d2_ha136.Cassava - mixed cropping",
                                                  "d2_ha136.Soybean2","d2_ha136.Soybean - mixed cropping",
                                                  "d2_ha136.maize_traditional","d2_ha136.maize_hybrid",
                                                  "d2_ha136.Maize biomass","d2_ha136.Maize corn")], na.rm = T)
CropSystems3C_TF$VegeFruitsOtherConsumedha <- rowSums(CropSystems3C_TF[c("d2_ha135.Cashew - mixed cropping",
                                               "d2_ha135.Peanut - lowl","d2_ha135.Guava",
                                               "d2_ha135.Sweetsop",
                                               "d2_ha135.Other fibre crops cotton etc.",
                                               "d2_ha135.Savoy cabbage curly",
                                               "d2_ha135.Cucumber - lowl",
                                               "d2_ha135.Water spinach","d2_ha135.Pak choy",
                                               "d2_ha135.Cashew nut","d2_ha135.Longan",
                                               "d2_ha135.Mangoes","d2_ha135.Coconuts",
                                               "d2_ha135.Lime","d2_ha135.Melon local, young",
                                               "d2_ha135.Long bean, Chinese","d2_ha135.Sunhemp - mixed cropping",
                                               "d2_ha135.Bananas","d2_ha135.Mustard greens - lowl",
                                               "d2_ha135.Amaranth","d2_ha135.Strawberry - upln",
                                               "d2_ha135.Other vegetable crop - upln","d2_ha135.Plum",
                                               "d2_ha135.Other annual crop","d2_ha135.Long bean - lowl",
                                               "d2_ha135.Chinese lettuce - lowl","d2_ha135.Chili - lowl",
                                               "d2_ha135.Onions2","d2_ha135.Coriander - lowl",
                                               "d2_ha135.Chinese flowering Cabbages/ choysum","d2_ha135.Other vegetable crop - lowl",
                                               "d2_ha135.Napa Cabbages","d2_ha135.Dill",
                                               "d2_ha135.Chili - upln","d2_ha135.Bell pepper, red",
                                               "d2_ha135.Peanut - upln","d2_ha135.Cucumber - upln",
                                               "d2_ha135.sugarcane","d2_ha135.Pumpkin - upln",
                                               "d2_ha135.jobs_tears - upln","d2_ha135.Ginger - lowl",
                                               "d2_ha135.Eggplant - upln","d2_ha135.Coffee, green",
                                               "d2_ha135.Mustard greens - upln","d2_ha135.Tea leaves",
                                               "d2_ha135.Peach","d2_ha135.Strawberry - lowl",
                                               "d2_ha135.Other fruit crop","d2_ha135.Mung bean",
                                               "d2_ha135.Garlic - lowl","d2_ha135.Spring Onions",
                                               "d2_ha135.Salad/lettuce","d2_ha135.Cabbages - lowl",
                                               "d2_ha135.Chinese lettuce - upln","d2_ha135.Carrot",
                                               "d2_ha135.Garlic - upln","d2_ha135.kale_flower - lowl",
                                               "d2_ha135.Coriander - upln","d2_ha135.jobs_tears - lowl",
                                               "d2_ha135.kale_flower - upln","d2_ha135.Ginger - upln",
                                               "d2_ha135.Hmong Cucumber","d2_ha135.Long bean - upln",
                                               "d2_ha135.taro","d2_ha135.gourd","d2_ha135.Chinese plantain, leaves",
                                               "d2_ha135.Oranges","d2_ha135.Littchi",
                                               "d2_ha135.Pumpkin - lowl","d2_ha135.Tomato",
                                               "d2_ha135.French bean - lowl","d2_ha135.Jack fruit",
                                               "d2_ha135.Avocado","d2_ha135.French bean - upln",
                                               "d2_ha135.Sauropus","d2_ha135.Cabbages - upln",
                                               "d2_ha135.Chayote","d2_ha135.Passion fruit",
                                               "d2_ha135.Macadamia","d2_ha135.Pomelos and grapefruits",
                                               "d2_ha135.Winter melon - lowl","d2_ha135.Hmong mustard",
                                               "d2_ha135.Local bean","d2_ha135.Local mustard",
                                               "d2_ha135.Other perenial crop","d2_ha135.Apricot",
                                               "d2_ha135.Winter melon - upln","d2_ha135.Rice bean",
                                               "d2_ha135.Arrowroot, root","d2_ha135.Potatoes - upln",
                                               "d2_ha135.Zucchini, common green, fruit","d2_ha135.Potatoes - lowl",
                                               "d2_ha135.Honeydew","d2_ha135.Mustard2",
                                               "d2_ha135.Chrysanthemum, leaves","d2_ha135.Turmeric, root",
                                               "d2_ha135.Black bean","d2_ha135.Kudzu",
                                               "d2_ha135.Kohlrabi","d2_ha135.Lettuce, romaine, leaves",
                                               "d2_ha135.Chinese kale/ Gailan","d2_ha135.Cauliflowers, Broccoli",
                                               "d2_ha135.Basil, sweet, leaves","d2_ha135.Kidney bean",
                                               "d2_ha135.Eggplant - lowl","d2_ha135.Sweet potatoes, leaves",
                                               "d2_ha135.Sawtooth herb / Culantro","d2_ha135.Sweet potatoes, tuber - lowl")], na.rm = T) -
                              rowSums(CropSystems3C_TF[c("d2_ha136.Cashew - mixed cropping",
                             "d2_ha136.Peanut - lowl","d2_ha136.Guava",
                             "d2_ha136.Sweetsop",
                             "d2_ha136.Other fibre crops cotton etc.",
                             "d2_ha136.Savoy cabbage curly",
                             "d2_ha136.Cucumber - lowl",
                             "d2_ha136.Water spinach","d2_ha136.Pak choy",
                             "d2_ha136.Cashew nut","d2_ha136.Longan",
                             "d2_ha136.Mangoes","d2_ha136.Coconuts",
                             "d2_ha136.Lime","d2_ha136.Melon local, young",
                             "d2_ha136.Long bean, Chinese","d2_ha136.Sunhemp - mixed cropping",
                             "d2_ha136.Bananas","d2_ha136.Mustard greens - lowl",
                             "d2_ha136.Amaranth","d2_ha136.Strawberry - upln",
                             "d2_ha136.Other vegetable crop - upln","d2_ha136.Plum",
                             "d2_ha136.Other annual crop","d2_ha136.Long bean - lowl",
                             "d2_ha136.Chinese lettuce - lowl","d2_ha136.Chili - lowl",
                             "d2_ha136.Onions2","d2_ha136.Coriander - lowl",
                             "d2_ha136.Chinese flowering Cabbages/ choysum","d2_ha136.Other vegetable crop - lowl",
                             "d2_ha136.Napa Cabbages","d2_ha136.Dill",
                             "d2_ha136.Chili - upln","d2_ha136.Bell pepper, red",
                             "d2_ha136.Peanut - upln","d2_ha136.Cucumber - upln",
                             "d2_ha136.sugarcane","d2_ha136.Pumpkin - upln",
                             "d2_ha136.jobs_tears - upln","d2_ha136.Ginger - lowl",
                             "d2_ha136.Eggplant - upln","d2_ha136.Coffee, green",
                             "d2_ha136.Mustard greens - upln","d2_ha136.Tea leaves",
                             "d2_ha136.Peach","d2_ha136.Strawberry - lowl",
                             "d2_ha136.Other fruit crop","d2_ha136.Mung bean",
                             "d2_ha136.Garlic - lowl","d2_ha136.Spring Onions",
                             "d2_ha136.Salad/lettuce","d2_ha136.Cabbages - lowl",
                             "d2_ha136.Chinese lettuce - upln","d2_ha136.Carrot",
                             "d2_ha136.Garlic - upln","d2_ha136.kale_flower - lowl",
                             "d2_ha136.Coriander - upln","d2_ha136.jobs_tears - lowl",
                             "d2_ha136.kale_flower - upln","d2_ha136.Ginger - upln",
                             "d2_ha136.Hmong Cucumber","d2_ha136.Long bean - upln",
                             "d2_ha136.taro","d2_ha136.gourd","d2_ha136.Chinese plantain, leaves",
                             "d2_ha136.Oranges","d2_ha136.Littchi",
                             "d2_ha136.Pumpkin - lowl","d2_ha136.Tomato",
                             "d2_ha136.French bean - lowl","d2_ha136.Jack fruit",
                             "d2_ha136.Avocado","d2_ha136.French bean - upln",
                             "d2_ha136.Sauropus","d2_ha136.Cabbages - upln",
                             "d2_ha136.Chayote","d2_ha136.Passion fruit",
                             "d2_ha136.Macadamia","d2_ha136.Pomelos and grapefruits",
                             "d2_ha136.Winter melon - lowl","d2_ha136.Hmong mustard",
                             "d2_ha136.Local bean","d2_ha136.Local mustard",
                             "d2_ha136.Other perenial crop","d2_ha136.Apricot",
                             "d2_ha136.Winter melon - upln","d2_ha136.Rice bean",
                             "d2_ha136.Arrowroot, root","d2_ha136.Potatoes - upln",
                             "d2_ha136.Zucchini, common green, fruit","d2_ha136.Potatoes - lowl",
                             "d2_ha136.Honeydew","d2_ha136.Mustard2",
                             "d2_ha136.Chrysanthemum, leaves","d2_ha136.Turmeric, root",
                             "d2_ha136.Black bean","d2_ha136.Kudzu",
                             "d2_ha136.Kohlrabi","d2_ha136.Lettuce, romaine, leaves",
                             "d2_ha136.Chinese kale/ Gailan","d2_ha136.Cauliflowers, Broccoli",
                             "d2_ha136.Basil, sweet, leaves","d2_ha136.Kidney bean",
                             "d2_ha136.Eggplant - lowl","d2_ha136.Sweet potatoes, leaves",
                             "d2_ha136.Sawtooth herb / Culantro","d2_ha136.Sweet potatoes, tuber - lowl")], na.rm = T)
CropSystems3C_TF$RiceSoldha <- rowSums(CropSystems3C_TF[c("d2_ha136.Wet season rice broadcast",
                             "d2_ha136.Wet season rice transplant",
                             "d2_ha136.Upland rice - mixed cropping",
                             "d2_ha136.Upland rice2","d2_ha136.Summer-autumn season rice",
                             "d2_ha136.Winter-Spring season rice")], na.rm = T)
CropSystems3C_TF$CassavaCornSoybeanSoldha <- rowSums(CropSystems3C_TF[c("d2_ha136.Cassava2",
                             "d2_ha136.Cassava - mixed cropping",
                             "d2_ha136.Soybean2","d2_ha136.Soybean - mixed cropping",
                             "d2_ha136.maize_traditional","d2_ha136.maize_hybrid",
                             "d2_ha136.Maize biomass","d2_ha136.Maize corn")], na.rm = T)
CropSystems3C_TF$VegeFruitsOtherSoldha <- rowSums(CropSystems3C_TF[c("d2_ha136.Cashew - mixed cropping",
                             "d2_ha136.Peanut - lowl","d2_ha136.Guava",
                             "d2_ha136.Sweetsop",
                             "d2_ha136.Other fibre crops cotton etc.",
                             "d2_ha136.Savoy cabbage curly",
                             "d2_ha136.Cucumber - lowl",
                             "d2_ha136.Water spinach","d2_ha136.Pak choy",
                             "d2_ha136.Cashew nut","d2_ha136.Longan",
                             "d2_ha136.Mangoes","d2_ha136.Coconuts",
                             "d2_ha136.Lime","d2_ha136.Melon local, young",
                             "d2_ha136.Long bean, Chinese","d2_ha136.Sunhemp - mixed cropping",
                             "d2_ha136.Bananas","d2_ha136.Mustard greens - lowl",
                             "d2_ha136.Amaranth","d2_ha136.Strawberry - upln",
                             "d2_ha136.Other vegetable crop - upln","d2_ha136.Plum",
                             "d2_ha136.Other annual crop","d2_ha136.Long bean - lowl",
                             "d2_ha136.Chinese lettuce - lowl","d2_ha136.Chili - lowl",
                             "d2_ha136.Onions2","d2_ha136.Coriander - lowl",
                             "d2_ha136.Chinese flowering Cabbages/ choysum","d2_ha136.Other vegetable crop - lowl",
                             "d2_ha136.Napa Cabbages","d2_ha136.Dill",
                             "d2_ha136.Chili - upln","d2_ha136.Bell pepper, red",
                             "d2_ha136.Peanut - upln","d2_ha136.Cucumber - upln",
                             "d2_ha136.sugarcane","d2_ha136.Pumpkin - upln",
                             "d2_ha136.jobs_tears - upln","d2_ha136.Ginger - lowl",
                             "d2_ha136.Eggplant - upln","d2_ha136.Coffee, green",
                             "d2_ha136.Mustard greens - upln","d2_ha136.Tea leaves",
                             "d2_ha136.Peach","d2_ha136.Strawberry - lowl",
                             "d2_ha136.Other fruit crop","d2_ha136.Mung bean",
                             "d2_ha136.Garlic - lowl","d2_ha136.Spring Onions",
                             "d2_ha136.Salad/lettuce","d2_ha136.Cabbages - lowl",
                             "d2_ha136.Chinese lettuce - upln","d2_ha136.Carrot",
                             "d2_ha136.Garlic - upln","d2_ha136.kale_flower - lowl",
                             "d2_ha136.Coriander - upln","d2_ha136.jobs_tears - lowl",
                             "d2_ha136.kale_flower - upln","d2_ha136.Ginger - upln",
                             "d2_ha136.Hmong Cucumber","d2_ha136.Long bean - upln",
                             "d2_ha136.taro","d2_ha136.gourd","d2_ha136.Chinese plantain, leaves",
                             "d2_ha136.Oranges","d2_ha136.Littchi",
                             "d2_ha136.Pumpkin - lowl","d2_ha136.Tomato",
                             "d2_ha136.French bean - lowl","d2_ha136.Jack fruit",
                             "d2_ha136.Avocado","d2_ha136.French bean - upln",
                             "d2_ha136.Sauropus","d2_ha136.Cabbages - upln",
                             "d2_ha136.Chayote","d2_ha136.Passion fruit",
                             "d2_ha136.Macadamia","d2_ha136.Pomelos and grapefruits",
                             "d2_ha136.Winter melon - lowl","d2_ha136.Hmong mustard",
                             "d2_ha136.Local bean","d2_ha136.Local mustard",
                             "d2_ha136.Other perenial crop","d2_ha136.Apricot",
                             "d2_ha136.Winter melon - upln","d2_ha136.Rice bean",
                             "d2_ha136.Arrowroot, root","d2_ha136.Potatoes - upln",
                             "d2_ha136.Zucchini, common green, fruit","d2_ha136.Potatoes - lowl",
                             "d2_ha136.Honeydew","d2_ha136.Mustard2",
                             "d2_ha136.Chrysanthemum, leaves","d2_ha136.Turmeric, root",
                             "d2_ha136.Black bean","d2_ha136.Kudzu",
                             "d2_ha136.Kohlrabi","d2_ha136.Lettuce, romaine, leaves",
                             "d2_ha136.Chinese kale/ Gailan","d2_ha136.Cauliflowers, Broccoli",
                             "d2_ha136.Basil, sweet, leaves","d2_ha136.Kidney bean",
                             "d2_ha136.Eggplant - lowl","d2_ha136.Sweet potatoes, leaves",
                             "d2_ha136.Sawtooth herb / Culantro","d2_ha136.Sweet potatoes, tuber - lowl")], na.rm = T)
CropSystems3C_TF$CattleBuffConsumedperAdult <- rowSums(CropSystems3C_TF[c("e5_2.Cattle","e5_2.Buffalo")], na.rm = T) / 
                                               rowSums(CropSystems3C_TF[c("e3_1","e3_3")], na.rm = T) 
CropSystems3C_TF$CattleBuffConsumedperAdult <- ifelse(is.nan(CropSystems3C_TF$CattleBuffConsumedperAdult) |
  is.infinite(CropSystems3C_TF$CattleBuffConsumedperAdult),
0,CropSystems3C_TF$CattleBuffConsumedperAdult)
CropSystems3C_TF$CattleBuffSoldperAdult <- rowSums(CropSystems3C_TF[c("e5_4.Cattle","e5_4.Buffalo")], na.rm = T) / 
  rowSums(CropSystems3C_TF[c("e3_1","e3_3")], na.rm = T)
CropSystems3C_TF$CattleBuffSoldperAdult <- ifelse(is.nan(CropSystems3C_TF$CattleBuffSoldperAdult) |
                                                        is.infinite(CropSystems3C_TF$CattleBuffSoldperAdult),
                                                      0,CropSystems3C_TF$CattleBuffSoldperAdult)
CropSystems3C_TF$PigsConsumedperAdult <- CropSystems3C_TF$`e5_2.Pig` / CropSystems3C_TF$e3_5
CropSystems3C_TF$PigsConsumedperAdult <- ifelse(is.nan(CropSystems3C_TF$PigsConsumedperAdult) |
                                                    is.infinite(CropSystems3C_TF$PigsConsumedperAdult),
                                                  0,CropSystems3C_TF$PigsConsumedperAdult)
CropSystems3C_TF$PigsSoldperAdult <- CropSystems3C_TF$`e5_4.Pig` / CropSystems3C_TF$e3_5     
CropSystems3C_TF$PigsSoldperAdult <- ifelse(is.nan(CropSystems3C_TF$PigsSoldperAdult) |
                                                  is.infinite(CropSystems3C_TF$PigsSoldperAdult),
                                                0,CropSystems3C_TF$PigsSoldperAdult)
CropSystems3C_TF$PoultryConsumedperAdult <- rowSums(CropSystems3C_TF[c("e5_2.Chicken","e5_2.Goose","e5_2.Duck or/and Muscovy")], na.rm = T) / 
  rowSums(CropSystems3C_TF[c("e3_13","e3_14","e3_16")], na.rm = T)
CropSystems3C_TF$PoultryConsumedperAdult <- ifelse(is.nan(CropSystems3C_TF$PoultryConsumedperAdult) |
                                              is.infinite(CropSystems3C_TF$PoultryConsumedperAdult),
                                            0,CropSystems3C_TF$PoultryConsumedperAdult)
CropSystems3C_TF$PoultrySoldperAdult <- rowSums(CropSystems3C_TF[c("e5_2.Chicken","e5_2.Goose","e5_2.Duck or/and Muscovy")], na.rm = T) / 
  rowSums(CropSystems3C_TF[c("e3_13","e3_14","e3_16")], na.rm = T)
CropSystems3C_TF$PoultrySoldperAdult <- ifelse(is.nan(CropSystems3C_TF$PoultrySoldperAdult) |
                                                     is.infinite(CropSystems3C_TF$PoultrySoldperAdult),
                                                   0,CropSystems3C_TF$PoultrySoldperAdult)
IncomeSources <- c("Financial support / gift","Non-farm income (own business: shop, trader/collector etc.)", 
"Non-farm wages (salaried work in private or public company)","Other  ${b3_2oth}",
"Pension","Remittances","Rented land","Sell derived/processed products","Selling cashew from the farm",
"Selling cassava from the farm","Selling cattle and buffalo","Selling labor",
"Selling maize from the farm","Selling other crops from the farm","Selling other farm products: ${b3_1oth}",
"Selling own fruits","Selling own vegetables","Selling pigs","Selling poultry", 
"Selling rice from the farm","Selling soybean from the farm")
i = "Financial support / gift"
for (i in IncomeSources){
  if (i %in% colnames(CropSystems3C_TF)) {
    CropSystems3C_TF[[i]] <- NA
  }
  CropSystems3C_TF[[i]] <- ifelse(CropSystems3C_TF$b4_1 == i,CropSystems3C_TF$b5_1,0) +
    ifelse(CropSystems3C_TF$b4_2 == i,CropSystems3C_TF$b5_2,0) +
    ifelse(CropSystems3C_TF$b4_3 == i,CropSystems3C_TF$b5_3,0)
}
CropSystems3C_TF$NbDaysNTFP <- rowSums(CropSystems3C_TF[,c("d7_11","d7_21",
        "d7_31","d7_41","d7_51","d7_61","d7_71","d7_81","d7_91","d7_101",
        "d7_111","d7_121","d7_131","d7_141","d7_151","d7_161","d7_171",
        "d7_181","d7_991")], na.rm = T)
CropSystems3C_TF$IncomeNTFP <- rowSums(CropSystems3C_TF[,c("d7_13","d7_23",
                                                           "d7_33","d7_43","d7_53","d7_63","d7_73","d7_83","d7_93","d7_103",
                                                           "d7_113","d7_123","d7_133","d7_143","d7_153","d7_163","d7_173",
                                                           "d7_183","d7_993")], na.rm = T)
#We use currencies values for April 2023:
#Cambodia: 1$ = 4074 riel
#Laos: 1$ = 17110 kips
#Vietnam: 1$ = 23452 dongs
CropSystems3C_TF$IncomeNTFP <- ifelse(CropSystems3C_TF$S_Area == "Cambodia",
                                      CropSystems3C_TF$IncomeNTFP/4074, 
                                      ifelse(CropSystems3C_TF$S_Area == "Lao",
                                      CropSystems3C_TF$IncomeNTFP/17110,
                                      CropSystems3C_TF$IncomeNTFP/23452))


#Loop for fulfilling HouseSys 2
for (i in 1:nrow(HouseSys2)){
  Dumm <- CropSystems3C_TF[CropSystems3C_TF$CAT == HouseSys2$Code[i],]
  #Survey design creation
  survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm)
  #%Households for each cropping/raising systems + Average area/Nb of heads
  for (j in colnames(CropSystems3C_TF)[11541:11627]){
    if (!paste0(j, "PerCentH") %in% colnames(HouseSys2)) {
      HouseSys2[[paste0(j, "PerCentH")]] <- NA
    }
    HouseSys2[i,paste0(j, "PerCentH")] <- round(sum(Dumm$SW_Weight[Dumm[[j]] > 0], na.rm = T) / sum(Dumm$SW_Weight, na.rm = T), digits = 2)
      if (!paste0(j,"Amount(HaorHead)") %in% colnames(HouseSys2)) {
        HouseSys2[[paste0(j,"Amount(HaorHead)")]] <- NA
      }
    HouseSys2[i,paste0(j,"Amount(HaorHead)")] <- round(svymean(Dumm[[j]], design = survey_design, na.rm = TRUE), digits = 0)
  }
  for (j in colnames(CropSystems3C_TF)[11628:11636]){
    if (!paste0(j,"Amount(HaorHead)") %in% colnames(HouseSys2)) {
      HouseSys2[[paste0(j,"Amount(HaorHead)")]] <- NA
    }
    HouseSys2[i,paste0(j,"Amount(HaorHead)")] <- round(svymean(Dumm[[j]], design = survey_design, na.rm = TRUE), digits = 0)
  } 
  for (j in colnames(CropSystems3C_TF)[c(11715:11757)]){
    if (!j %in% colnames(HouseSys2)) {
      HouseSys2[[j]] <- NA
    }
    HouseSys2[i,j] <- round(svymean(Dumm[[j]], design = survey_design, na.rm = TRUE), digits = 1)
  } 
  for (j in colnames(CropSystems3C_TF)[c(11759:11764,11771:11791)]){
    if (!j %in% colnames(HouseSys2)) {
      HouseSys2[[j]] <- NA
    }
    HouseSys2[i,j] <- round(svymean(Dumm[[j]], design = survey_design, na.rm = TRUE), digits = 0)
  } 
  for (j in colnames(CropSystems3C_TF)[c(11765:11770,11792:11793)]){
    if (!j %in% colnames(HouseSys2)) {
      HouseSys2[[j]] <- NA
    }
    HouseSys2[i,j] <- round(svymean(Dumm[[j]], design = survey_design, na.rm = TRUE), digits = 2)
  }
}

HouseSys2$MigrationRate <- (HouseSys2$NbMigratingMembers*HouseSys2$YearlyRatemigrationperMigrant)/HouseSys2$NbActiveMembers

#Table export
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(HouseSys2,"Housesys2.rds")

