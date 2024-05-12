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
CropSystems3C_TF <- readRDS("Newdata_TF.rds")
animallist <- c("Cattle","Pig","Duck or/and Muscovy","Chicken","Buffalo","Horse",
                "Other poultry","Goat","Other cattle","Goose","Rabbit")

## d - Lists
{
  Sarealist <- c("Cambodia", "Lao", "Dien Bien province", "Son La province")
  HouseholdN <- c(sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Cambodia"]),
                  sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Lao"]),
                  sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Dien Bien province"]),
                  sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Son La province"]))
  Sarealist <- cbind(Sarealist,HouseholdN)
  Sarealist <- as.data.frame(Sarealist)
  colnames(Sarealist)[1] <- "S_Area"
}


### 2 - Crop average values Table
#(Aim: Create a table with hhid as row and country, district/commune information,
#And all animal raised information as columns, with average value per animal specie(s) for numeric variable,
#And % of individuals concerned based on households raising this animal for factor variables

#First we create the basis of the table with Area and crop information
AnimalDesc <- expand.grid(Column1 = Sarealist[,1], Column2 = animallist)
colnames(AnimalDesc) <- c("S_Area","specie")
#First we add the information about the number of household raising each specie
Raisers <- CropSystems3C_TF %>%
  group_by(S_Area) %>%
  summarize(`Buffalo - Raising` = sum(.data$SW_Weight[!is.na(e3_1) & e3_1 > 0 | !is.na(e3_2) & e3_2 > 0], na.rm = TRUE),
            `Cattle - Raising`= sum(.data$SW_Weight[!is.na(e3_3) & e3_3 >0 | !is.na(e3_4) & e3_4 >0], na.rm = TRUE),
            `Pig - Raising` = sum(.data$SW_Weight[!is.na(e3_5) & e3_5 >0 | !is.na(e3_6) & e3_6 >0], na.rm = TRUE),
            `Goat - Raising` = sum(.data$SW_Weight[!is.na(e3_7) & e3_7 >0 | !is.na(e3_8) & e3_8 >0], na.rm = TRUE),
            `Sheep - Raising` = sum(.data$SW_Weight[!is.na(e3_9) & e3_9 >0 | !is.na(e3_10) & e3_10 >0], na.rm = TRUE),
            `Horse - Raising` = sum(.data$SW_Weight[!is.na(e3_11) & e3_11 >0], na.rm = TRUE),
            `Rabbit - Raising` = sum(.data$SW_Weight[!is.na(e3_12) & e3_12 >0], na.rm = TRUE),
            `Chicken - Raising` = sum(.data$SW_Weight[!is.na(e3_13) & e3_13 >0], na.rm = TRUE),
            `Duck or/and Muscovy - Raising` = sum(.data$SW_Weight[!is.na(e3_14) & e3_14 >0], na.rm = TRUE),
            `Goose - Raising` = sum(.data$SW_Weight[!is.na(e3_16) & e3_16 >0], na.rm = TRUE),
            `Other cattle - Raising` = sum(.data$SW_Weight[!is.na(e3_98) & e3_98 >0], na.rm = TRUE),
            `Other poultry - Raising` = sum(.data$SW_Weight[!is.na(e3_99) & e3_99 >0], na.rm = TRUE))
Raisersb <- gather(Raisers, specie, HouseholdsRaising, `Buffalo - Raising`:`Other poultry - Raising`, factor_key=TRUE)
Raisersb$specie <- str_replace(Raisersb$specie, " - Raising", "")
colnames(Raisersb)[3] <- "N"
AnimalDesc <- left_join(AnimalDesc,Raisersb, by = c("S_Area","specie"))
AnimalDesc$N <- ifelse(AnimalDesc$N == 0, NA,AnimalDesc$N)
AnimalDesc <- left_join(AnimalDesc,Sarealist, by = "S_Area")
AnimalDesc$HouseholdN <- as.numeric(AnimalDesc$HouseholdN)
AnimalDesc$PerCentofHouseholds <- round(AnimalDesc$N / AnimalDesc$HouseholdN, digits = 3)
AnimalDesc$N <- round(AnimalDesc$N, digits = 0)
AnimalDesc$HouseholdN <- round(AnimalDesc$HouseholdN, digits = 0)
#We'll add the average number of adults and small animals in the corresponding columns
HeadCount <- CropSystems3C_TF %>%
  group_by(S_Area) %>%
  group_modify(~{
    # Create survey design object for each group
    survey_design_group <- svydesign(~1, data = ., weights = ~SW_Weight)
    # Calculate svymean for each group
    data.frame(`Buffalo - Adult` = svymean(~e3_1, design = survey_design_group, na.rm = TRUE),
               `Buffalo - Young` = svymean(~e3_2, design = survey_design_group, na.rm = TRUE),
               `Cattle - Adult` = svymean(~e3_3, design = survey_design_group, na.rm = TRUE),
               `Cattle - Young` = svymean(~e3_4, design = survey_design_group, na.rm = TRUE),
               `Pig - Adult` = svymean(~e3_5, design = survey_design_group, na.rm = TRUE),
               `Pig - Young` = svymean(~e3_6, design = survey_design_group, na.rm = TRUE),
               `Goat - Adult` = svymean(~e3_7, design = survey_design_group, na.rm = TRUE),
               `Goat - Young` = svymean(~e3_8, design = survey_design_group, na.rm = TRUE),
               `Sheep - Adult` = svymean(~e3_9, design = survey_design_group, na.rm = TRUE),
               `Sheep - Young` = svymean(~e3_10, design = survey_design_group, na.rm = TRUE),
               `Horse - Adult` = svymean(~e3_11, design = survey_design_group, na.rm = TRUE),
               `Rabbit - Adult` = svymean(~e3_12, design = survey_design_group, na.rm = TRUE),
               `Chicken - Adult` = svymean(~e3_13, design = survey_design_group, na.rm = TRUE),
               `Duck or/and Muscovy - Adult` = svymean(~e3_14, design = survey_design_group, na.rm = TRUE),
               `Goose - Adult` = svymean(~e3_16, design = survey_design_group, na.rm = TRUE),
               `Other cattle - Adult` = svymean(~e3_98, design = survey_design_group, na.rm = TRUE),
               `Other poultry - Adult` = svymean(~e3_99, design = survey_design_group, na.rm = TRUE))
               })
HeadCountAdult <- HeadCount[,c(1,2,6,10,14,18,22,24,26,28,30,32,34)]
colnames(HeadCountAdult) <- str_replace(colnames(HeadCountAdult), "...Adult.mean","")
HeadCountAdultb <- gather(HeadCountAdult, specie, head, `Buffalo`:`Other.poultry`, factor_key=TRUE)
HeadCountYoung <- HeadCount[,c(1,4,8,12,16,20)]
colnames(HeadCountYoung) <- str_replace(colnames(HeadCountYoung), "...Young.mean","")
HeadCountYoungb <- gather(HeadCountYoung, specie, headY, `Buffalo`:`Sheep`, factor_key=TRUE)
HeadCountN <- left_join(HeadCountAdultb,HeadCountYoungb, by = c("S_Area","specie") )
AnimalDesc <- left_join(AnimalDesc, HeadCountN, by = c("S_Area","specie"))
colnames(AnimalDesc)[6:7] <- c("NbHeadAdult","NbHeadYoung")
AnimalDesc$NbHeadAdult <- round(AnimalDesc$NbHeadAdult, digits = 1)
AnimalDesc$NbHeadYoung <- round(AnimalDesc$NbHeadYoung, digits = 1)
AnimalDesc <- AnimalDesc[,-4]

#No we add the required columns for different variables

#First some columns needs to be renamed in the original database
colnames(CropSystems3C_TF)[c(11075,11083,11091,11099,11130,11138,11147,11155,11191,11208)] <- 
  c("e191T","e199T","e241T","e249T","e341T","e349T","e381T","e389T","e489T","e529T")

#We add all the calculated columns through a for loop
for (i in Sarealist[,1]){
  #We filter data per study area first
  Dumm <- CropSystems3C_TF[CropSystems3C_TF$S_Area == i ,]
  for (j in animallist){
    #We then filter data per animal type
    Dumm1 <- Dumm %>%
      select(matches(grep(j, names(Dumm), value = TRUE)),starts_with("SW_"))
    ## c - Sampling design
    survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm1)
    #"If" loop to remove non-existent crop-country combination
    if(!all(is.na(AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j]))){
           #NbBreeds = Number of breeds of this animal specie raised
           AnimalDesc$NbBreeds[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <- 
             round(svymean(~ Dumm1[, grep("e5_a.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #LocalBreedPerCent = % of households raising local breeds
           AnimalDesc$LocalBreedPerCent [AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_b.", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)           
           #NbLocalBreed - Average number of local breed per household
           AnimalDesc$NbLocalBreed[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep("e5_c.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #CrossingBreedPerCent = % of households crossing local breeds with other breeds
           AnimalDesc$CrossingBreedPerCent[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
            round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_d.", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
                   AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)           
           #NbDeadAnimalLastYear - Average nb of dead animals during last year
           AnimalDesc$NbDeadAnimalLastYear[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep("e5_1.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #NbConsumedAnimalLastYear - Average number of animals slaughtered and self consumed by the household
           AnimalDesc$NbConsumedAnimalLastYear[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep("e5_2.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #NbGivenAnimalLastYear - Average number of animals given by the household
           AnimalDesc$NbGivenAnimalLastYear[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep("e5_3.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #NbSoldAnimalLastYear - Average number of animals sold by the household
           AnimalDesc$NbSoldAnimalLastYear[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep(paste0("e5_4.",j), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #SellingPrice($perHead) - Average selling price $/head
           AnimalDesc$`SellingPrice - DolperHead`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep("e5_41.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #Gross product - $/head or kg
           AnimalDesc$`Gross product - $/head or kg`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep(paste0("e5_43.",j), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #WeightHeadSold(kgperHead) - Average weight sold per head
           AnimalDesc$`WeightHeadSold - kgperHead`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep(paste0("e5_42.",j), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #NbBoughtAnimalLastYear - Average number of bought animals last year
           AnimalDesc$NbBoughtAnimalLastYear[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep(paste0("e5_5.",j), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #BuyingPrice($perHead) - Average buying price $/head
           AnimalDesc$`BuyingPrice - DolperHead`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
           round(svymean(~ Dumm1[, grep(paste0("e5_51.",j), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #WeightHeadBought(kgperHead) - Average weight bought per head
           AnimalDesc$`WeightHeadBought - kgperHead`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(svymean(~ Dumm1[, grep(paste0("e5_52.",j), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
           #Main outlet -	Village collector
           AnimalDesc$`Main outlet -	Village collector`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_d.", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet - Collector outside the village`
           AnimalDesc$`Main outlet -	Collector outside the village`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Collector outside the village"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet -	Trader in the district`
           AnimalDesc$`Main outlet -	Trader in the district`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Trader in the district"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet -	Trader from the province`
           AnimalDesc$`Main outlet -	Trader from the province`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Trader from the province"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet -	Trader from another province`
           AnimalDesc$`Main outlet -	Trader from another province`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Trader from another province"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet -	Cooperative of which you are a member`
           AnimalDesc$`Main outlet -	Cooperative of which you are a member`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Cooperative of which you are a member"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet -	Cooperative of which you are not a member`
           AnimalDesc$`Main outlet -	Cooperative of which you are not a member`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Cooperative of which you are not a member"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet - Consumers on the farm`
           AnimalDesc$`Main outlet -	Consumers on the farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Consumers on the farm"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet - Local markets where you sell your products directly to final consumers`
           AnimalDesc$`Main outlet -	Local markets where you sell your products directly to final consumers`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Local markets where you sell your products directly to final consumers"], na.rm = T)/
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet - Consumers through online sales`
           AnimalDesc$`Main outlet -	Consumers through online sales`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Consumers through online sales"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet - Foreign trader (e.g. from China)`
           AnimalDesc$`Main outlet -	Foreign trader (e.g. from China)`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Foreign trader (e.g. from China)"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`Main outlet - Processors`
           AnimalDesc$`Main outlet -	Processors`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)]== "Processors"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`ShareSoldMainOutlet - Less than 25%`
           AnimalDesc$`ShareSoldMainOutlet - Less than 25%`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)] == "Less than 25%"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`ShareSoldMainOutlet - 25-50%`
           AnimalDesc$`ShareSoldMainOutlet - 25-50%`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)] == "25-50%"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`ShareSoldMainOutlet - 50-75%`
           AnimalDesc$`ShareSoldMainOutlet - 50-75%`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)]== "50-75%"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #`ShareSoldMainOutlet - Over 75%`
           AnimalDesc$`ShareSoldMainOutlet - Over 75%`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)] == "Over 75%"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #InputsSoldProvidedBuyer
           AnimalDesc$InputsSoldProvidedBuyer[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_11.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #InputsCreditProvidedBuyer
           AnimalDesc$InputsCreditProvidedBuyer[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_12.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #CashCreditProvidedBuyer
           AnimalDesc$CashCreditProvidedBuyer[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_13.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #TechnicalAdvProvidedBuyer
           AnimalDesc$TechnicalAdvProvidedBuyer[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_14.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #MarketInfoProvidedBuyer
           AnimalDesc$MarketInfoProvidedBuyer[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_15.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #RegularSalesProvidedBuyer
           AnimalDesc$RegularSalesProvidedBuyer[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_16.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #OtherProvidedBuyer
           AnimalDesc$OtherProvidedBuyer[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_199.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3) 
           #`ContractKind - Formal contract`
           AnimalDesc$`ContractKind - Formal contract`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "Formal contract"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3) 
           #`ContractKind - Informal contract`
           AnimalDesc$`ContractKind - Informal contract`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "Informal contract"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3) 
           #`ContractKind - No contract/ no prior arrangements`
           AnimalDesc$`ContractKind - No contract/ no prior arrangements`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "No contract/ no prior arrangements"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3) 
           #`ContractKind - Spot relations`
           AnimalDesc$`ContractKind - Spot relations`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "Spot relations"], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
           #HouseholdsSellingPerCent
           AnimalDesc$HouseholdsSellingPerCent[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
             round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_41.", names(Dumm1), value = TRUE)] > 0], na.rm = T) /
                     AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    }
  }
  #Now we include additional information related to cattle/buffalo, pigs, poultry
  #Cattle
  for (j in c("Buffalo","Cattle","Other cattle")){
  #We then filter data per animal type
  x <- ifelse(j == "Buffalo","e3_1",ifelse(j == "Cattle","e3_3","e3_98"))
  Dumm1 <- Dumm[Dumm[[x]] > 0 & !is.na(Dumm[[x]]),]
  if(nrow(Dumm1) > 1){
  ## c - Sampling design
  survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm1)
  #`Dry season day - Confined in a barn,`
  AnimalDesc$`Dry season day - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e16", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`
  AnimalDesc$`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e16", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season day - Attached in the grazing area, home yard without fence`
  AnimalDesc$`Dry season day - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e16", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season day - Grazing with shepherd,`
  AnimalDesc$`Dry season day - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e16", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season day - Free grazing without Shepherd`
  AnimalDesc$`Dry season day - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e16", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season night - Confined in a barn,`
  AnimalDesc$`Dry season night - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e17", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`
  AnimalDesc$`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e17", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season night - Attached in the grazing area, home yard without fence`
  AnimalDesc$`Dry season night - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e17", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season night - Grazing with shepherd,`
  AnimalDesc$`Dry season night - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e17", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season night - Free grazing without Shepherd`
  AnimalDesc$`Dry season night - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e17", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #DrySeasonGrazing
  AnimalDesc$`DrySeasonGrazing`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e18", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Grazing in public area`
  AnimalDesc$`Dry season feeding - Grazing in public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e191T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Grazing in own pasture area`
  AnimalDesc$`Dry season feeding - Grazing in own pasture area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e192", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Cutting and carry natural grass / vegetables from public area`
  AnimalDesc$`Dry season feeding - Cutting and carry natural grass / vegetables from public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e193", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Cutting and carry natural grass / vegetables from own area`
  AnimalDesc$`Dry season feeding - Cutting and carry natural grass / vegetables from own area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e194", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Cutting and carry of forage`
  AnimalDesc$`Dry season feeding - Cutting and carry of forage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e195", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
  AnimalDesc$`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e196", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
  AnimalDesc$`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e197", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Silage`
  AnimalDesc$`Dry season feeding - Silage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e198", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Kitchen waste`
  AnimalDesc$`Dry season feeding - Kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e199T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Rice, rice brand, broken rice… bought from market`
  AnimalDesc$`Dry season feeding - Rice, rice brand, broken rice… bought from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e1910", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`
  AnimalDesc$`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e1911", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Dry season feeding - Waste from processing rice wine, soybean milke`
  AnimalDesc$`Dry season feeding - Waste from processing rice wine, soybean milke`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e1912", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)

  #`Wet season day - Confined in a barn,`
  AnimalDesc$`Wet season day - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e21", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`
  AnimalDesc$`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e21", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season day - Attached in the grazing area, home yard without fence`
  AnimalDesc$`Wet season day - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e21", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season day - Grazing with shepherd,`
  AnimalDesc$`Wet season day - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e21", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season day - Free grazing without Shepherd`
  AnimalDesc$`Wet season day - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e21", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season night - Confined in a barn,`
  AnimalDesc$`Wet season night - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e22", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`
  AnimalDesc$`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e22", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season night - Attached in the grazing area, home yard without fence`
  AnimalDesc$`Wet season night - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e22", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season night - Grazing with shepherd,`
  AnimalDesc$`Wet season night - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e22", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season night - Free grazing without Shepherd`
  AnimalDesc$`Wet season night - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e22", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #WetSeasonGrazing
  AnimalDesc$`WetSeasonGrazing`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e23", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Grazing in public area`
  AnimalDesc$`Wet season feeding - Grazing in public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e241T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Grazing in own pasture area`
  AnimalDesc$`Wet season feeding - Grazing in own pasture area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e242", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Cutting and carry natural grass / vegetables from public area`
  AnimalDesc$`Wet season feeding - Cutting and carry natural grass / vegetables from public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e243", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Cutting and carry natural grass / vegetables from own area`
  AnimalDesc$`Wet season feeding - Cutting and carry natural grass / vegetables from own area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e244", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Cutting and carry of forage`
  AnimalDesc$`Wet season feeding - Cutting and carry of forage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e245", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
  AnimalDesc$`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e246", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
  AnimalDesc$`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e247", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Silage`
  AnimalDesc$`Wet season feeding - Silage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e248", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Kitchen waste`
  AnimalDesc$`Wet season feeding - Kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e249T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Rice, rice brand, broken rice… bought from market`
  AnimalDesc$`Wet season feeding - Rice, rice brand, broken rice… bought from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e2410", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`
  AnimalDesc$`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e2411", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Restaurant / Market waste`
  AnimalDesc$`Wet season feeding - Restaurant / Market waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e2412", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Wet season feeding - Waste from processing rice wine, soybean milke`
  AnimalDesc$`Wet season feeding - Waste from processing rice wine, soybean milke`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e2413", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`ConcentrateUse - From farm` 
  AnimalDesc$`ConcentrateUse - From farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e25_11", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`ConcentrateUse - From market` 
  AnimalDesc$`ConcentrateUse - From market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e25_12", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Parasites - Internal parasites` 
  AnimalDesc$`Parasites - Internal parasites`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e26_11", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Parasites - Tick` 
  AnimalDesc$`Parasites - Tick`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e26_12", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Parasites - Worms` 
  AnimalDesc$`Parasites - Worms`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e26_13", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Care - Traditional treatment` 
  AnimalDesc$`Care - Traditional treatment`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e271", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Care - Chemicals` 
  AnimalDesc$`Care - Chemicals`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e272", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`CareReasonYes - Better for environment/ AE` 
  AnimalDesc$`CareReasonYes - Better for environment/ AE`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e27a_1", names(Dumm1), value = TRUE)] =="Better for environment/ AE"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`CareReasonYes - Human health` 
  AnimalDesc$`CareReasonYes - Human health`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e27a_1", names(Dumm1), value = TRUE)] == "Human health"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`CareReasonYes - Treatment / prevention` 
  AnimalDesc$`CareReasonYes - Treatment / prevention` [AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e27a_1", names(Dumm1), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`CareReasonNo - Not available on the market` 
  AnimalDesc$`CareReasonNo - Not available on the market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e27a_2", names(Dumm1), value = TRUE)] == "Not available on the market"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`CareReasonNo - Too expensive` 
  AnimalDesc$`CareReasonNo - Too expensive`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e27a_2", names(Dumm1), value = TRUE)] == "Too expensive"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`CareReasonNo - No need, not severe disease` 
  AnimalDesc$`CareReasonNo - No need, not severe disease`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e27a_2", names(Dumm1), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`CareReasonNo - Too difficult to handle animal or no VT service` 
  AnimalDesc$`CareReasonNo - Too difficult to handle animal or no VT service`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e27a_2", names(Dumm1), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)  
  #`Diseases` 
  AnimalDesc$`Diseases`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e28", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Antibiotics - For treatment diseases only` 
  AnimalDesc$`Antibiotics - For treatment diseases only`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e291", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Antibiotics - For prevention of diseases only` 
  AnimalDesc$`Antibiotics - For prevention of diseases only`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e292", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`Antibiotics - For growth promotion` 
  AnimalDesc$`Antibiotics - For growth promotion`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e293", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonYes - Better for environment/ AE` 
  AnimalDesc$`AntibioticsReasonYes - Better for environment/ AE`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_1", names(Dumm1), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonYes - Human health` 
  AnimalDesc$`AntibioticsReasonYes - Human health`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_1", names(Dumm1), value = TRUE)] == "Human health"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonYes - Treatment / prevention` 
  AnimalDesc$`AntibioticsReasonYes - Treatment / prevention`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_1", names(Dumm1), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonNo - Not available on the market` 
  AnimalDesc$`AntibioticsReasonNo - Not available on the market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_2", names(Dumm1), value = TRUE)] == "Not available on the market,"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonNo - Too expensive` 
  AnimalDesc$`AntibioticsReasonNo - Too expensive`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_2", names(Dumm1), value = TRUE)] == "Too expensive"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonNo - Already include in the feed` 
  AnimalDesc$`AntibioticsReasonNo - Already include in the feed`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_2", names(Dumm1), value = TRUE)] == "Already include in the feed"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonNo - No need, not severe disease` 
  AnimalDesc$`AntibioticsReasonNo - No need, not severe disease`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_2", names(Dumm1), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  #`AntibioticsReasonNo - Too difficult to handle animal or no VT service` 
  AnimalDesc$`AntibioticsReasonNo - Too difficult to handle animal or no VT service`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
    round(sum(Dumm1$SW_Weight[Dumm1[,grep("e29_2", names(Dumm1), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
            AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  }}
  for (j in c("Pig")){
    x <- "e3_5"
    Dumm1 <- Dumm[Dumm[[x]] > 0 & !is.na(Dumm[[x]]),]
    ## c - Sampling design
    survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm1)
    #`PigRaisingSystem - Fattening family raising using only local feed stuffs and kitchen waste`
    AnimalDesc$`PigRaisingSystem - Fattening family raising using only local feed stuffs and kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e30", names(Dumm1), value = TRUE)] == "Fattening family raising using only local feed stuffs and kitchen waste"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`PigRaisingSystem - Fattening family raising, but buying feed from market`
    AnimalDesc$`PigRaisingSystem - Fattening family raising, but buying feed from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e30", names(Dumm1), value = TRUE)] == "Fattening family raising, but buying feed from market"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`PigRaisingSystem - Piglet family raising using only local feed stuffs and kitchen waste`
    AnimalDesc$`PigRaisingSystem - Piglet family raising using only local feed stuffs and kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e30", names(Dumm1), value = TRUE)] == "Piglet family raising using only local feed stuffs and kitchen waste"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`PigRaisingSystem - Piglet family raising, but buying feed from market`
    AnimalDesc$`PigRaisingSystem - Piglet family raising, but buying feed from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e30", names(Dumm1), value = TRUE)] == "Piglet family raising, but buying feed from market"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Confined in a barn,`
    AnimalDesc$`Dry season day - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e31", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e31", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Dry season day - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e31", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Grazing with shepherd,`
    AnimalDesc$`Dry season day - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e31", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Free grazing without Shepherd`
    AnimalDesc$`Dry season day - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e31", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Confined in a barn,`
    AnimalDesc$`Dry season night - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e32", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e32", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Dry season night - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e32", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Grazing with shepherd,`
    AnimalDesc$`Dry season night - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e32", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Free grazing without Shepherd`
    AnimalDesc$`Dry season night - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e32", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #DrySeasonGrazing
    AnimalDesc$`DrySeasonGrazing`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e33", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Grazing in public area`
    AnimalDesc$`Dry season feeding - Grazing in public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e341T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Grazing in own pasture area`
    AnimalDesc$`Dry season feeding - Grazing in own pasture area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e342", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Cutting and carry natural grass / vegetables from public area`
    AnimalDesc$`Dry season feeding - Cutting and carry natural grass / vegetables from public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e343", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Cutting and carry natural grass / vegetables from own area`
    AnimalDesc$`Dry season feeding - Cutting and carry natural grass / vegetables from own area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e344", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Cutting and carry of forage`
    AnimalDesc$`Dry season feeding - Cutting and carry of forage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e345", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
    AnimalDesc$`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e346", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
    AnimalDesc$`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e347", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Silage`
    AnimalDesc$`Dry season feeding - Silage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e348", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Kitchen waste`
    AnimalDesc$`Dry season feeding - Kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e349T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Rice, rice brand, broken rice… bought from market`
    AnimalDesc$`Dry season feeding - Rice, rice brand, broken rice… bought from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3410", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`
    AnimalDesc$`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3411", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Restaurant / Market waste`
    AnimalDesc$`Dry season feeding - Restaurant / Market waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3412", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Waste from processing rice wine, soybean milke`
    AnimalDesc$`Dry season feeding - Waste from processing rice wine, soybean milke`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3413", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    
    #`Wet season day - Confined in a barn,`
    AnimalDesc$`Wet season day - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e35", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e35", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Wet season day - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e35", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Grazing with shepherd,`
    AnimalDesc$`Wet season day - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e35", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Free grazing without Shepherd`
    AnimalDesc$`Wet season day - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e35", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Confined in a barn,`
    AnimalDesc$`Wet season night - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e36", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e36", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Wet season night - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e36", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Grazing with shepherd,`
    AnimalDesc$`Wet season night - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e36", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Free grazing without Shepherd`
    AnimalDesc$`Wet season night - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e36", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #WetSeasonGrazing
    AnimalDesc$`WetSeasonGrazing`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e37", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Grazing in public area`
    AnimalDesc$`Wet season feeding - Grazing in public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e381T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Grazing in own pasture area`
    AnimalDesc$`Wet season feeding - Grazing in own pasture area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e382", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Cutting and carry natural grass / vegetables from public area`
    AnimalDesc$`Wet season feeding - Cutting and carry natural grass / vegetables from public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e383", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Cutting and carry natural grass / vegetables from own area`
    AnimalDesc$`Wet season feeding - Cutting and carry natural grass / vegetables from own area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e384", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Cutting and carry of forage`
    AnimalDesc$`Wet season feeding - Cutting and carry of forage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e385", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
    AnimalDesc$`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e386", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
    AnimalDesc$`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e387", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Silage`
    AnimalDesc$`Wet season feeding - Silage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e388", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Kitchen waste`
    AnimalDesc$`Wet season feeding - Kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e389T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Rice, rice brand, broken rice… bought from market`
    AnimalDesc$`Wet season feeding - Rice, rice brand, broken rice… bought from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3810", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`
    AnimalDesc$`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3811", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Restaurant / Market waste`
    AnimalDesc$`Wet season feeding - Restaurant / Market waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3812", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Waste from processing rice wine, soybean milke`
    AnimalDesc$`Wet season feeding - Waste from processing rice wine, soybean milke`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e3813", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`ConcentrateUse - From farm` 
    AnimalDesc$`ConcentrateUse - From farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e39_11", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`ConcentrateUse - From market` 
    AnimalDesc$`ConcentrateUse - From market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e39_12", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Parasites - Internal parasites` 
    AnimalDesc$`Parasites - Internal parasites`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e40_1", names(Dumm1), value = TRUE)] == "Internal parasites,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Parasites - Tick` 
    AnimalDesc$`Parasites - Tick`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e40_1", names(Dumm1), value = TRUE)] == "Tick,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Parasites - Worms` 
    AnimalDesc$`Parasites - Worms`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e40_1", names(Dumm1), value = TRUE)] == "Worms"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Care - Traditional treatment` 
    AnimalDesc$`Care - Traditional treatment`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e411", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Care - Chemicals` 
    AnimalDesc$`Care - Chemicals`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e412", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonYes - Better for environment/ AE` 
    AnimalDesc$`CareReasonYes - Better for environment/ AE`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e41_1", names(Dumm1), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonYes - Human health` 
    AnimalDesc$`CareReasonYes - Human health`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e41_1", names(Dumm1), value = TRUE)] == "Human health"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonYes - Treatment / prevention` 
    AnimalDesc$`CareReasonYes - Treatment / prevention` [AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e41_1", names(Dumm1), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - Not available on the market` 
    AnimalDesc$`CareReasonNo - Not available on the market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e41_2", names(Dumm1), value = TRUE)] == "Not available on the market"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - Too expensive` 
    AnimalDesc$`CareReasonNo - Too expensive`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e41_2", names(Dumm1), value = TRUE)] == "Too expensive"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - No need, not severe disease` 
    AnimalDesc$`CareReasonNo - No need, not severe disease`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e41_2", names(Dumm1), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - Too difficult to handle animal or no VT service` 
    AnimalDesc$`CareReasonNo - Too difficult to handle animal or no VT service`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e41_2", names(Dumm1), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)  
    #`Diseases` 
    AnimalDesc$`Diseases`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e42", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Antibiotics - For treatment diseases only` 
    AnimalDesc$`Antibiotics - For treatment diseases only`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e431", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Antibiotics - For prevention of diseases only` 
    AnimalDesc$`Antibiotics - For prevention of diseases only`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e432", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Antibiotics - For growth promotion` 
    AnimalDesc$`Antibiotics - For growth promotion`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e433", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonYes - Better for environment/ AE` 
    AnimalDesc$`AntibioticsReasonYes - Better for environment/ AE`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_1", names(Dumm1), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonYes - Human health` 
    AnimalDesc$`AntibioticsReasonYes - Human health`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_1", names(Dumm1), value = TRUE)] == "Human health"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonYes - Treatment / prevention` 
    AnimalDesc$`AntibioticsReasonYes - Treatment / prevention`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_1", names(Dumm1), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Not available on the market` 
    AnimalDesc$`AntibioticsReasonNo - Not available on the market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_2", names(Dumm1), value = TRUE)] == "Not available on the market,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Too expensive` 
    AnimalDesc$`AntibioticsReasonNo - Too expensive`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_2", names(Dumm1), value = TRUE)] == "Too expensive"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Already include in the feed` 
    AnimalDesc$`AntibioticsReasonNo - Already include in the feed`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_2", names(Dumm1), value = TRUE)] == "Already include in the feed"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - No need, not severe disease` 
    AnimalDesc$`AntibioticsReasonNo - No need, not severe disease`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_2", names(Dumm1), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Too difficult to handle animal or no VT service` 
    AnimalDesc$`AntibioticsReasonNo - Too difficult to handle animal or no VT service`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e43_2", names(Dumm1), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  }
  for (j in c("Duck or/and Muscovy","Chicken","Other poultry","Goose")){
    x <- ifelse(j == "Duck or/and Muscovy","e3_14",
                ifelse(j == "Chicken","e3_13",
                       ifelse(j == "Other poultry","e3_99","e3_16")))
    Dumm1 <- Dumm[Dumm[[x]] > 0 & !is.na(Dumm[[x]]),]
    if(nrow(Dumm1) > 1){
    ## c - Sampling design
    survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm1)
    #`PoultryRaisingSystem - Family raising but larger scale for selling chick`
    AnimalDesc$`PoultryRaisingSystem - Family raising but larger scale for selling chick`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e44", names(Dumm1), value = TRUE)] == "Family raising but larger scale for selling chick"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`PoultryRaisingSystem - Family raising but larger scale for selling eggs`
    AnimalDesc$`PoultryRaisingSystem - Family raising but larger scale for selling eggs`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e44", names(Dumm1), value = TRUE)] == "Family raising but larger scale for selling eggs"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`PoultryRaisingSystem - Family raising but larger scale for selling meat`
    AnimalDesc$`PoultryRaisingSystem - Family raising but larger scale for selling meat`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e44", names(Dumm1), value = TRUE)] == "Family raising but larger scale for selling meat"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`PoultryRaisingSystem - Family raising for consumption`
    AnimalDesc$`PoultryRaisingSystem - Family raising for consumption`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e44", names(Dumm1), value = TRUE)] == "Family raising for consumption"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Confined in a barn,`
    AnimalDesc$`Dry season day - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e45", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e45", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Dry season day - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e45", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Grazing with shepherd,`
    AnimalDesc$`Dry season day - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e45", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season day - Free grazing without Shepherd`
    AnimalDesc$`Dry season day - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e45", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Confined in a barn,`
    AnimalDesc$`Dry season night - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e46", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e46", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Dry season night - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e46", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Grazing with shepherd,`
    AnimalDesc$`Dry season night - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e46", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season night - Free grazing without Shepherd`
    AnimalDesc$`Dry season night - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e46", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #DrySeasonGrazing
    AnimalDesc$`DrySeasonGrazing`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e47", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Grazing in public area`
    AnimalDesc$`Dry season feeding - Grazing in public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e481", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Grazing in own pasture area`
    AnimalDesc$`Dry season feeding - Grazing in own pasture area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e482", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Cutting and carry natural grass / vegetables from public area`
    AnimalDesc$`Dry season feeding - Cutting and carry natural grass / vegetables from public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e483", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Cutting and carry natural grass / vegetables from own area`
    AnimalDesc$`Dry season feeding - Cutting and carry natural grass / vegetables from own area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e484", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Cutting and carry of forage`
    AnimalDesc$`Dry season feeding - Cutting and carry of forage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e485", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
    AnimalDesc$`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e486", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
    AnimalDesc$`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e487", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Silage`
    AnimalDesc$`Dry season feeding - Silage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e488", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Kitchen waste`
    AnimalDesc$`Dry season feeding - Kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e489T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Rice, rice brand, broken rice… bought from market`
    AnimalDesc$`Dry season feeding - Rice, rice brand, broken rice… bought from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e4810", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`
    AnimalDesc$`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e4811", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Restaurant / Market waste`
    AnimalDesc$`Dry season feeding - Restaurant / Market waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e4812", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Dry season feeding - Waste from processing rice wine, soybean milke`
    AnimalDesc$`Dry season feeding - Waste from processing rice wine, soybean milke`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e4813", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    
    #`Wet season day - Confined in a barn,`
    AnimalDesc$`Wet season day - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e49", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e49", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Wet season day - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e49", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Grazing with shepherd,`
    AnimalDesc$`Wet season day - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e49", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season day - Free grazing without Shepherd`
    AnimalDesc$`Wet season day - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e49", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Confined in a barn,`
    AnimalDesc$`Wet season night - Confined in a barn,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e50", names(Dumm1), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalDesc$`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e50", names(Dumm1), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Attached in the grazing area, home yard without fence`
    AnimalDesc$`Wet season night - Attached in the grazing area, home yard without fence`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e50", names(Dumm1), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Grazing with shepherd,`
    AnimalDesc$`Wet season night - Grazing with shepherd,`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e50", names(Dumm1), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season night - Free grazing without Shepherd`
    AnimalDesc$`Wet season night - Free grazing without Shepherd`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e50", names(Dumm1), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #WetSeasonGrazing
    AnimalDesc$`WetSeasonGrazing`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e51", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Grazing in public area`
    AnimalDesc$`Wet season feeding - Grazing in public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e521", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Grazing in own pasture area`
    AnimalDesc$`Wet season feeding - Grazing in own pasture area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e522", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Cutting and carry natural grass / vegetables from public area`
    AnimalDesc$`Wet season feeding - Cutting and carry natural grass / vegetables from public area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e523", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Cutting and carry natural grass / vegetables from own area`
    AnimalDesc$`Wet season feeding - Cutting and carry natural grass / vegetables from own area`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e524", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Cutting and carry of forage`
    AnimalDesc$`Wet season feeding - Cutting and carry of forage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e525", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
    AnimalDesc$`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e526", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
    AnimalDesc$`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e527", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Silage`
    AnimalDesc$`Wet season feeding - Silage`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e528", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Kitchen waste`
    AnimalDesc$`Wet season feeding - Kitchen waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e529T", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Rice, rice brand, broken rice… bought from market`
    AnimalDesc$`Wet season feeding - Rice, rice brand, broken rice… bought from market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5210", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`
    AnimalDesc$`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5211", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Restaurant / Market waste`
    AnimalDesc$`Wet season feeding - Restaurant / Market waste`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5212", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Wet season feeding - Waste from processing rice wine, soybean milke`
    AnimalDesc$`Wet season feeding - Waste from processing rice wine, soybean milke`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5213", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`ConcentrateUse - From farm` 
    AnimalDesc$`ConcentrateUse - From farm`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e53_11", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`ConcentrateUse - From market` 
    AnimalDesc$`ConcentrateUse - From market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e53_12", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Parasites - Internal parasites` 
    AnimalDesc$`Parasites - Internal parasites`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e54_1", names(Dumm1), value = TRUE)] == "Internal parasites,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Parasites - Tick` 
    AnimalDesc$`Parasites - Tick`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e54_1", names(Dumm1), value = TRUE)] == "Tick,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Parasites - Worms` 
    AnimalDesc$`Parasites - Worms`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e54_1", names(Dumm1), value = TRUE)] == "Worms"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Care - Traditional treatment` 
    AnimalDesc$`Parasites - Worms`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e551", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Care - Chemicals` 
    AnimalDesc$`Care - Chemicals`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e552", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonYes - Better for environment/ AE` 
    AnimalDesc$`CareReasonYes - Better for environment/ AE`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e55a_1", names(Dumm1), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonYes - Human health` 
    AnimalDesc$`CareReasonYes - Human health`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e55a_1", names(Dumm1), value = TRUE)] == "Human health"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonYes - Treatment / prevention` 
    AnimalDesc$`CareReasonYes - Treatment / prevention` [AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e55a_1", names(Dumm1), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - Not available on the market` 
    AnimalDesc$`CareReasonNo - Not available on the market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e55a_2", names(Dumm1), value = TRUE)] == "Not available on the market"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - Too expensive` 
    AnimalDesc$`CareReasonNo - Too expensive`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e55a_2", names(Dumm1), value = TRUE)] == "Too expensive"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - No need, not severe disease` 
    AnimalDesc$`CareReasonNo - No need, not severe disease`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e55a_2", names(Dumm1), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`CareReasonNo - Too difficult to handle animal or no VT service` 
    AnimalDesc$`CareReasonNo - Too difficult to handle animal or no VT service`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e55a_2", names(Dumm1), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)  
    #`Diseases` 
    AnimalDesc$`Diseases`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e56", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Antibiotics - For treatment diseases only` 
    AnimalDesc$`Antibiotics - For treatment diseases only`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e571", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Antibiotics - For prevention of diseases only` 
    AnimalDesc$`Antibiotics - For prevention of diseases only`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e572", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`Antibiotics - For growth promotion` 
    AnimalDesc$`Antibiotics - For growth promotion`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e573", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Not available on the market` 
    AnimalDesc$`AntibioticsReasonNo - Not available on the market`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e57_2", names(Dumm1), value = TRUE)] == "Not available on the market,"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Too expensive` 
    AnimalDesc$`AntibioticsReasonNo - Too expensive`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e57_2", names(Dumm1), value = TRUE)] == "Too expensive"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Already include in the feed` 
    AnimalDesc$`AntibioticsReasonNo - Already include in the feed`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e57_2", names(Dumm1), value = TRUE)] == "Already include in the feed"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - No need, not severe disease` 
    AnimalDesc$`AntibioticsReasonNo - No need, not severe disease`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e57_2", names(Dumm1), value = TRUE)] =="No need, not severe disease"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
    #`AntibioticsReasonNo - Too difficult to handle animal or no VT service` 
    AnimalDesc$`CareReasonNo - Too difficult to handle animal or no VT service`[AnimalDesc$S_Area == i & AnimalDesc$specie == j] <-
      round(sum(Dumm1$SW_Weight[Dumm1[,grep("e57_2", names(Dumm1), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
              AnimalDesc$N[AnimalDesc$S_Area == i & AnimalDesc$specie == j], digits = 3)
  }
}}

#Round max values to 1
AnimalDesc$LocalBreedPerCent <- ifelse(AnimalDesc$LocalBreedPerCent > 1, 1, AnimalDesc$LocalBreedPerCent)
#Round max values to 1
AnimalDesc$`ShareSoldMainOutlet - Over 75%` <- ifelse(AnimalDesc$`ShareSoldMainOutlet - Over 75%` > 1, 1, AnimalDesc$`ShareSoldMainOutlet - Over 75%`)
#Round max values to 1
AnimalDesc$`ContractKind - No contract/ no prior arrangements` <- ifelse(AnimalDesc$`ContractKind - No contract/ no prior arrangements` > 1, 1, AnimalDesc$`ContractKind - No contract/ no prior arrangements`)
#Round max values to 1
AnimalDesc$HouseholdsSellingPerCent <- ifelse(AnimalDesc$HouseholdsSellingPerCent > 1, 1, AnimalDesc$HouseholdsSellingPerCent)
#We remove columns including only NA or 0 values
AnimalDesc <- AnimalDesc[, colSums(!is.na(AnimalDesc)) > 0]
AnimalDesc <- AnimalDesc[, colSums(AnimalDesc != 0, na.rm = TRUE) > 0]
#We remove rows corresponding to inexistant animal-country combination
AnimalDesc <- AnimalDesc[!is.na(AnimalDesc$N),]
#We move a column
AnimalDesc <- AnimalDesc %>%
  relocate(HouseholdsSellingPerCent, .after = `PoultryRaisingSystem - Family raising for consumption`)


#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(AnimalDesc, "AnimalDesc_TF.rds")


#Pense-bête
#Crops: 7 -> 8290 
#Reasons AEP: 8291 -> 8349
#Additionnal info Practices: 8350 -> 8361
#HHmembers: 8362 -> 9081
#Labor general: 9082 -> 9091, 9136
#Labor Tasks: 9092 -> 9135
#Services: 9137 -> 9148
#Equipments: 9149 -> 9167


