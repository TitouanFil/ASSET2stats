
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
library(survey)


## b. data loading
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
AnimalDesc <- readRDS("AnimalDesc_TF.rds")
All <- readRDS("NewDataProd_TF.rds")
AnimalSys <- read.csv2("Animalsys.csv")

## c. Add columns
All$BuffaloT <- ifelse((!is.na(All$e3_1) & All$e3_1 > 0 | !is.na(All$e3_2) & All$e3_2 > 0), All$e3_1, 0)
All$BuffaloY <- ifelse((!is.na(All$e3_1) & All$e3_1 > 0 | !is.na(All$e3_2) & All$e3_2 > 0), All$e3_2, 0)
All$CattleT <- ifelse((!is.na(All$e3_3) & All$e3_3 > 0 | !is.na(All$e3_4) & All$e3_4 > 0), All$e3_3, 0)
All$CattleY <- ifelse((!is.na(All$e3_3) & All$e3_3 > 0 | !is.na(All$e3_4) & All$e3_4 > 0), All$e3_4, 0)
All$PigT <- ifelse((!is.na(All$e3_5) & All$e3_5 > 0 | !is.na(All$e3_6) & All$e3_6 > 0), All$e3_5, 0)
All$PigY <- ifelse((!is.na(All$e3_5) & All$e3_5 > 0 | !is.na(All$e3_6) & All$e3_6 > 0), All$e3_6, 0)
All$GoatT <- ifelse((!is.na(All$e3_7) & All$e3_7 > 0 | !is.na(All$e3_8) & All$e3_8 > 0), All$e3_7, 0)
All$GoatY <- ifelse((!is.na(All$e3_7) & All$e3_7 > 0 | !is.na(All$e3_8) & All$e3_8 > 0), All$e3_8, 0)
All$ChickenT <- ifelse((!is.na(All$e3_13) & All$e3_13 > 0), 1, 0)
All$`Duck or/and MuscovyT` <- ifelse((!is.na(All$e3_14) & All$e3_14 > 0), 1, 0)
All$GooseT <- ifelse((!is.na(All$e3_16) & All$e3_16 > 0), 1, 0)

### 2 - Crop average values Table
#(Aim: Create a table with hhid as row and country, district/commune information,
#And all animal raised information as columns, with average value per animal specie(s) for numeric variable,
#And % of individuals concerned based on households raising this animal for factor variables

#First we add the information about the number of household raising each specie
#And the nb of head adult and young
for (i in 1:nrow(AnimalSys)){
  AnimalSys$'N Animal'[i] <- round(sum(All$SW_Weight[All$S_Area == AnimalSys$Sarea[i] & All[,grep(paste0(AnimalSys$Animalkind[i],"T"), names(All), value = TRUE)] > 0]), digits = 0)
  AnimalSys$'N Animals sys'[i] <- round(sum(All$SW_Weight[All$S_Area == AnimalSys$Sarea[i] & All[,grep(AnimalSys$Code[i], names(All), value = TRUE)] > 0 & All[,grep(paste0(AnimalSys$Animalkind[i],"T"), names(All), value = TRUE)] > 0]), digits = 0)
  AnimalSys$PerCentofHouseholds[i] <- round(AnimalSys$'N Animals sys'[i]/sum(All$SW_Weight[All$S_Area == AnimalSys$Sarea[i]]), digits = 3)
}

#First some columns needs to be renamed in the original database
colnames(All)[c(11075,11083,11091,11099,11130,11138,11147,11155,11191,11208)] <- 
  c("e191T","e199T","e241T","e249T","e341T","e349T","e381T","e389T","e489T","e529T")

i = 1
#We add all the calculated columns through a for loop
for (i in 1:nrow(AnimalSys)){
  #We filter data per study area first
  Dumm <- All[All$S_Area == AnimalSys$Sarea[i] & All[,grep(AnimalSys$Code[i], names(All), value = TRUE)] > 0 &
              All[,grep(paste0(AnimalSys$Animalkind[i],"T"), names(All), value = TRUE)] > 0,]
    #We then filter data per animal type
    Dumm1 <- Dumm %>%
      select(matches(grep(AnimalSys$Animalkind[i], names(Dumm), value = TRUE)),starts_with("SW_"))
    ## c - Sampling design
    survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm1)
      #NbHeadAdult = Number of breeds of this animal specie raised
      AnimalSys$NbHeadAdult[i] <- 
      round(svymean(~ Dumm1[, grep(paste0(AnimalSys$Animalkind[i],"T"), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #NbHeadYoung = Number of breeds of this animal specie raised
      AnimalSys$NbHeadYoung[i] <- ifelse(AnimalSys$Animalkind[i] %in% c("Cattle","Buffalo","Pig","Goat"),
      round(svymean(~ Dumm1[, grep(paste0(AnimalSys$Animalkind[i],"Y"), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2), NA)
      #NbBreeds = Number of breeds of this animal specie raised
      AnimalSys$NbBreeds[i] <- 
        round(svymean(~ Dumm1[, grep("e5_a.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #LocalBreedPerCent = % of households raising local breeds
      AnimalSys$LocalBreedPerCent [i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_b.", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$N[i], digits = 3)           
      #NbLocalBreed - Average number of local breed per household
      AnimalSys$NbLocalBreed[i] <-
        round(svymean(~ Dumm1[, grep("e5_c.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #CrossingBreedPerCent = % of households crossing local breeds with other breeds
      AnimalSys$CrossingBreedPerCent[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_d.", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$N[i], digits = 3)           
      #NbDeadAnimalLastYear - Average nb of dead animals during last year
      AnimalSys$NbDeadAnimalLastYear[i] <-
        round(svymean(~ Dumm1[, grep("e5_1.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #NbConsumedAnimalLastYear - Average number of animals slaughtered and self consumed by the household
      AnimalSys$NbConsumedAnimalLastYear[i] <-
        round(svymean(~ Dumm1[, grep("e5_2.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #NbGivenAnimalLastYear - Average number of animals given by the household
      AnimalSys$NbGivenAnimalLastYear[i] <-
        round(svymean(~ Dumm1[, grep("e5_3.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #NbSoldAnimalLastYear - Average number of animals sold by the household
      AnimalSys$NbSoldAnimalLastYear[i] <-
        round(svymean(~ Dumm1[, grep(paste0("e5_4.",AnimalSys$Animalkind[i]), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #SellingPrice($perHead) - Average selling price $/head
      AnimalSys$`SellingPrice - DolperHead`[i] <-
        round(svymean(~ Dumm1[, grep("e5_41.", names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #Gross product - $/head or kg
      AnimalSys$`Gross product - $/head or kg`[i] <-
        round(svymean(~ Dumm1[, grep(paste0("e5_43.",AnimalSys$Animalkind[i]), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #WeightHeadSold(kgperHead) - Average weight sold per head
      AnimalSys$`WeightHeadSold - kgperHead`[i] <-
        round(svymean(~ Dumm1[, grep(paste0("e5_42.",AnimalSys$Animalkind[i]), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #NbBoughtAnimalLastYear - Average number of bought animals last year
      AnimalSys$NbBoughtAnimalLastYear[i] <-
        round(svymean(~ Dumm1[, grep(paste0("e5_5.",AnimalSys$Animalkind[i]), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #BuyingPrice($perHead) - Average buying price $/head
      AnimalSys$`BuyingPrice - DolperHead`[i] <-
        round(svymean(~ Dumm1[, grep(paste0("e5_51.",AnimalSys$Animalkind[i]), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #WeightHeadBought(kgperHead) - Average weight bought per head
      AnimalSys$`WeightHeadBought - kgperHead`[i] <-
        round(svymean(~ Dumm1[, grep(paste0("e5_52.",AnimalSys$Animalkind[i]), names(Dumm1))], design = survey_design, na.rm = TRUE), digits = 2)
      #Main outlet -	Village collector
      AnimalSys$`Main outlet -	Village collector`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_d.", names(Dumm1), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet - Collector outside the village`
      AnimalSys$`Main outlet -	Collector outside the village`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Collector outside the village"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet -	Trader in the district`
      AnimalSys$`Main outlet -	Trader in the district`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Trader in the district"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet -	Trader from the province`
      AnimalSys$`Main outlet -	Trader from the province`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Trader from the province"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet -	Trader from another province`
      AnimalSys$`Main outlet -	Trader from another province`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Trader from another province"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet -	Cooperative of which you are a member`
      AnimalSys$`Main outlet -	Cooperative of which you are a member`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Cooperative of which you are a member"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet -	Cooperative of which you are not a member`
      AnimalSys$`Main outlet -	Cooperative of which you are not a member`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Cooperative of which you are not a member"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet - Consumers on the farm`
      AnimalSys$`Main outlet -	Consumers on the farm`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Consumers on the farm"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet - Local markets where you sell your products directly to final consumers`
      AnimalSys$`Main outlet -	Local markets where you sell your products directly to final consumers`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Local markets where you sell your products directly to final consumers"], na.rm = T)/
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet - Consumers through online sales`
      AnimalSys$`Main outlet -	Consumers through online sales`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Consumers through online sales"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet - Foreign trader (e.g. from China)`
      AnimalSys$`Main outlet -	Foreign trader (e.g. from China)`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)] == "Foreign trader (e.g. from China)"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Main outlet - Processors`
      AnimalSys$`Main outlet -	Processors`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b18_1.", names(Dumm1), value = TRUE)]== "Processors"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ShareSoldMainOutlet - Less than 25%`
      AnimalSys$`ShareSoldMainOutlet - Less than 25%`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)] == "Less than 25%"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ShareSoldMainOutlet - 25-50%`
      AnimalSys$`ShareSoldMainOutlet - 25-50%`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)] == "25-50%"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ShareSoldMainOutlet - 50-75%`
      AnimalSys$`ShareSoldMainOutlet - 50-75%`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)]== "50-75%"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ShareSoldMainOutlet - Over 75%`
      AnimalSys$`ShareSoldMainOutlet - Over 75%`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b19_1.", names(Dumm1), value = TRUE)] == "Over 75%"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #InputsSoldProvidedBuyer
      AnimalSys$InputsSoldProvidedBuyer[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_11.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #InputsCreditProvidedBuyer
      AnimalSys$InputsCreditProvidedBuyer[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_12.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #CashCreditProvidedBuyer
      AnimalSys$CashCreditProvidedBuyer[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_13.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #TechnicalAdvProvidedBuyer
      AnimalSys$TechnicalAdvProvidedBuyer[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_14.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #MarketInfoProvidedBuyer
      AnimalSys$MarketInfoProvidedBuyer[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_15.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #RegularSalesProvidedBuyer
      AnimalSys$RegularSalesProvidedBuyer[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_16.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #OtherProvidedBuyer
      AnimalSys$OtherProvidedBuyer[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b20_199.", names(Dumm1), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3) 
      #`ContractKind - Formal contract`
      AnimalSys$`ContractKind - Formal contract`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "Formal contract"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3) 
      #`ContractKind - Informal contract`
      AnimalSys$`ContractKind - Informal contract`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "Informal contract"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3) 
      #`ContractKind - No contract/ no prior arrangements`
      AnimalSys$`ContractKind - No contract/ no prior arrangements`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "No contract/ no prior arrangements"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3) 
      #`ContractKind - Spot relations`
      AnimalSys$`ContractKind - Spot relations`[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("b21_1.", names(Dumm1), value = TRUE)] == "Spot relations"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #HouseholdsSellingPerCent
      AnimalSys$HouseholdsSellingPerCent[i] <-
        round(sum(Dumm1$SW_Weight[Dumm1[,grep("e5_41.", names(Dumm1), value = TRUE)] > 0], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
  #Now we include additional information related to cattle/buffalo, pigs, poultry
  #Cattle 
      if (AnimalSys$Animalkind[i] %in% c("Cattle","Buffalo")){
      ## c - Sampling design
      survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm)
      #`Dry season day - Confined in a barn,`
      AnimalSys$`Dry season day - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e16", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e16", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Attached in the grazing area, home yard without fence`
      AnimalSys$`Dry season day - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e16", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Grazing with shepherd,`
      AnimalSys$`Dry season day - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e16", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Free grazing without Shepherd`
      AnimalSys$`Dry season day - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e16", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Confined in a barn,`
      AnimalSys$`Dry season night - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e17", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e17", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Attached in the grazing area, home yard without fence`
      AnimalSys$`Dry season night - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e17", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Grazing with shepherd,`
      AnimalSys$`Dry season night - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e17", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Free grazing without Shepherd`
      AnimalSys$`Dry season night - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e17", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #DrySeasonGrazing
      AnimalSys$`DrySeasonGrazing`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e18", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Grazing in public area`
      AnimalSys$`Dry season feeding - Grazing in public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e191T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Grazing in own pasture area`
      AnimalSys$`Dry season feeding - Grazing in own pasture area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e192", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Cutting and carry natural grass / vegetables from public area`
      AnimalSys$`Dry season feeding - Cutting and carry natural grass / vegetables from public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e193", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Cutting and carry natural grass / vegetables from own area`
      AnimalSys$`Dry season feeding - Cutting and carry natural grass / vegetables from own area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e194", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Cutting and carry of forage`
      AnimalSys$`Dry season feeding - Cutting and carry of forage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e195", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
      AnimalSys$`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e196", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
      AnimalSys$`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e197", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Silage`
      AnimalSys$`Dry season feeding - Silage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e198", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Kitchen waste`
      AnimalSys$`Dry season feeding - Kitchen waste`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e199T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Rice, rice brand, broken rice… bought from market`
      AnimalSys$`Dry season feeding - Rice, rice brand, broken rice… bought from market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e1910", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`
      AnimalSys$`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e1911", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Restaurant / Market waste`
      AnimalSys$`Dry season feeding - Restaurant / Market waste`[i] <- NA
      #`Dry season feeding - Waste from processing rice wine, soybean milke`
      AnimalSys$`Dry season feeding - Waste from processing rice wine, soybean milke`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e1912", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      
      #`Wet season day - Confined in a barn,`
      AnimalSys$`Wet season day - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e21", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e21", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Attached in the grazing area, home yard without fence`
      AnimalSys$`Wet season day - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e21", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Grazing with shepherd,`
      AnimalSys$`Wet season day - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e21", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Free grazing without Shepherd`
      AnimalSys$`Wet season day - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e21", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Confined in a barn,`
      AnimalSys$`Wet season night - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e22", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e22", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Attached in the grazing area, home yard without fence`
      AnimalSys$`Wet season night - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e22", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Grazing with shepherd,`
      AnimalSys$`Wet season night - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e22", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Free grazing without Shepherd`
      AnimalSys$`Wet season night - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e22", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #WetSeasonGrazing
      AnimalSys$`WetSeasonGrazing`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e23", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Grazing in public area`
      AnimalSys$`Wet season feeding - Grazing in public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e241T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Grazing in own pasture area`
      AnimalSys$`Wet season feeding - Grazing in own pasture area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e242", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Cutting and carry natural grass / vegetables from public area`
      AnimalSys$`Wet season feeding - Cutting and carry natural grass / vegetables from public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e243", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Cutting and carry natural grass / vegetables from own area`
      AnimalSys$`Wet season feeding - Cutting and carry natural grass / vegetables from own area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e244", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Cutting and carry of forage`
      AnimalSys$`Wet season feeding - Cutting and carry of forage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e245", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
      AnimalSys$`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e246", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
      AnimalSys$`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e247", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Silage`
      AnimalSys$`Wet season feeding - Silage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e248", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Kitchen waste`
      AnimalSys$`Wet season feeding - Kitchen waste`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e249T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Rice, rice brand, broken rice… bought from market`
      AnimalSys$`Wet season feeding - Rice, rice brand, broken rice… bought from market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e2410", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`
      AnimalSys$`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e2411", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Restaurant / Market waste`
      AnimalSys$`Wet season feeding - Restaurant / Market waste`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e2412", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Waste from processing rice wine, soybean milke`
      AnimalSys$`Wet season feeding - Waste from processing rice wine, soybean milke`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e2413", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ConcentrateUse - From farm` 
      AnimalSys$`ConcentrateUse - From farm`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e25_11", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ConcentrateUse - From market` 
      AnimalSys$`ConcentrateUse - From market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e25_12", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Parasites - Internal parasites` 
      AnimalSys$`Parasites - Internal parasites`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e26_11", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Parasites - Tick` 
      AnimalSys$`Parasites - Tick`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e26_12", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Parasites - Worms` 
      AnimalSys$`Parasites - Worms`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e26_13", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Care - Traditional treatment` 
      AnimalSys$`Care - Traditional treatment`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e271", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Care - Chemicals` 
      AnimalSys$`Care - Chemicals`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e272", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonYes - Better for environment/ AE` 
      AnimalSys$`CareReasonYes - Better for environment/ AE`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e27a_1", names(Dumm), value = TRUE)] =="Better for environment/ AE"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonYes - Human health` 
      AnimalSys$`CareReasonYes - Human health`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e27a_1", names(Dumm), value = TRUE)] == "Human health"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonYes - Treatment / prevention` 
      AnimalSys$`CareReasonYes - Treatment / prevention` [i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e27a_1", names(Dumm), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - Not available on the market` 
      AnimalSys$`CareReasonNo - Not available on the market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e27a_2", names(Dumm), value = TRUE)] == "Not available on the market"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - Too expensive` 
      AnimalSys$`CareReasonNo - Too expensive`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e27a_2", names(Dumm), value = TRUE)] == "Too expensive"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - No need, not severe disease` 
      AnimalSys$`CareReasonNo - No need, not severe disease`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e27a_2", names(Dumm), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - Too difficult to handle animal or no VT service` 
      AnimalSys$`CareReasonNo - Too difficult to handle animal or no VT service`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e27a_2", names(Dumm), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)  
      #`Diseases` 
      AnimalSys$`Diseases`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e28", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Antibiotics - For treatment diseases only` 
      AnimalSys$`Antibiotics - For treatment diseases only`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e291", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Antibiotics - For prevention of diseases only` 
      AnimalSys$`Antibiotics - For prevention of diseases only`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e292", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Antibiotics - For growth promotion` 
      AnimalSys$`Antibiotics - For growth promotion`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e293", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonYes - Better for environment/ AE` 
      AnimalSys$`AntibioticsReasonYes - Better for environment/ AE`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_1", names(Dumm), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonYes - Human health` 
      AnimalSys$`AntibioticsReasonYes - Human health`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_1", names(Dumm), value = TRUE)] == "Human health"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonYes - Treatment / prevention` 
      AnimalSys$`AntibioticsReasonYes - Treatment / prevention`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_1", names(Dumm), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Not available on the market` 
      AnimalSys$`AntibioticsReasonNo - Not available on the market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_2", names(Dumm), value = TRUE)] == "Not available on the market,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Too expensive` 
      AnimalSys$`AntibioticsReasonNo - Too expensive`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_2", names(Dumm), value = TRUE)] == "Too expensive"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Already include in the feed` 
      AnimalSys$`AntibioticsReasonNo - Already include in the feed`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_2", names(Dumm), value = TRUE)] == "Already include in the feed"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - No need, not severe disease` 
      AnimalSys$`AntibioticsReasonNo - No need, not severe disease`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_2", names(Dumm), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Too difficult to handle animal or no VT service` 
      AnimalSys$`AntibioticsReasonNo - Too difficult to handle animal or no VT service`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e29_2", names(Dumm), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      }
    #Pigs
    if (AnimalSys$Animalkind[i] == "Pig"){
     #`Dry season day - Confined in a barn,`
    AnimalSys$`Dry season day - Confined in a barn,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e31", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalSys$`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e31", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season day - Attached in the grazing area, home yard without fence`
    AnimalSys$`Dry season day - Attached in the grazing area, home yard without fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e31", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season day - Grazing with shepherd,`
    AnimalSys$`Dry season day - Grazing with shepherd,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e31", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season day - Free grazing without Shepherd`
    AnimalSys$`Dry season day - Free grazing without Shepherd`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e31", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season night - Confined in a barn,`
    AnimalSys$`Dry season night - Confined in a barn,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e32", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalSys$`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e32", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season night - Attached in the grazing area, home yard without fence`
    AnimalSys$`Dry season night - Attached in the grazing area, home yard without fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e32", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season night - Grazing with shepherd,`
    AnimalSys$`Dry season night - Grazing with shepherd,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e32", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season night - Free grazing without Shepherd`
    AnimalSys$`Dry season night - Free grazing without Shepherd`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e32", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #DrySeasonGrazing
    AnimalSys$`DrySeasonGrazing`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e33", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Grazing in public area`
    AnimalSys$`Dry season feeding - Grazing in public area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e341T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Grazing in own pasture area`
    AnimalSys$`Dry season feeding - Grazing in own pasture area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e342", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Cutting and carry natural grass / vegetables from public area`
    AnimalSys$`Dry season feeding - Cutting and carry natural grass / vegetables from public area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e343", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Cutting and carry natural grass / vegetables from own area`
    AnimalSys$`Dry season feeding - Cutting and carry natural grass / vegetables from own area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e344", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Cutting and carry of forage`
    AnimalSys$`Dry season feeding - Cutting and carry of forage`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e345", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
    AnimalSys$`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e346", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
    AnimalSys$`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e347", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Silage`
    AnimalSys$`Dry season feeding - Silage`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e348", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Kitchen waste`
    AnimalSys$`Dry season feeding - Kitchen waste`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e349T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Rice, rice brand, broken rice… bought from market`
    AnimalSys$`Dry season feeding - Rice, rice brand, broken rice… bought from market`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3410", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`
    AnimalSys$`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3411", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Restaurant / Market waste`
    AnimalSys$`Dry season feeding - Restaurant / Market waste`[i] <- 
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3412", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Dry season feeding - Waste from processing rice wine, soybean milke`
    AnimalSys$`Dry season feeding - Waste from processing rice wine, soybean milke`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3413", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    
    #`Wet season day - Confined in a barn,`
    AnimalSys$`Wet season day - Confined in a barn,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e35", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalSys$`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e35", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season day - Attached in the grazing area, home yard without fence`
    AnimalSys$`Wet season day - Attached in the grazing area, home yard without fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e35", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season day - Grazing with shepherd,`
    AnimalSys$`Wet season day - Grazing with shepherd,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e35", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season day - Free grazing without Shepherd`
    AnimalSys$`Wet season day - Free grazing without Shepherd`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e35", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season night - Confined in a barn,`
    AnimalSys$`Wet season night - Confined in a barn,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e36", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`
    AnimalSys$`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e36", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season night - Attached in the grazing area, home yard without fence`
    AnimalSys$`Wet season night - Attached in the grazing area, home yard without fence`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e36", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season night - Grazing with shepherd,`
    AnimalSys$`Wet season night - Grazing with shepherd,`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e36", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season night - Free grazing without Shepherd`
    AnimalSys$`Wet season night - Free grazing without Shepherd`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e36", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #WetSeasonGrazing
    AnimalSys$`WetSeasonGrazing`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e37", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Grazing in public area`
    AnimalSys$`Wet season feeding - Grazing in public area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e381T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Grazing in own pasture area`
    AnimalSys$`Wet season feeding - Grazing in own pasture area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e382", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Cutting and carry natural grass / vegetables from public area`
    AnimalSys$`Wet season feeding - Cutting and carry natural grass / vegetables from public area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e383", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Cutting and carry natural grass / vegetables from own area`
    AnimalSys$`Wet season feeding - Cutting and carry natural grass / vegetables from own area`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e384", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Cutting and carry of forage`
    AnimalSys$`Wet season feeding - Cutting and carry of forage`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e385", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
    AnimalSys$`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e386", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
    AnimalSys$`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e387", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Silage`
    AnimalSys$`Wet season feeding - Silage`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e388", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Kitchen waste`
    AnimalSys$`Wet season feeding - Kitchen waste`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e389T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Rice, rice brand, broken rice… bought from market`
    AnimalSys$`Wet season feeding - Rice, rice brand, broken rice… bought from market`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3810", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`
    AnimalSys$`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3811", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Restaurant / Market waste`
    AnimalSys$`Wet season feeding - Restaurant / Market waste`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3812", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Wet season feeding - Waste from processing rice wine, soybean milke`
    AnimalSys$`Wet season feeding - Waste from processing rice wine, soybean milke`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e3813", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`ConcentrateUse - From farm` 
    AnimalSys$`ConcentrateUse - From farm`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e39_11", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`ConcentrateUse - From market` 
    AnimalSys$`ConcentrateUse - From market`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e39_12", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Parasites - Internal parasites` 
    AnimalSys$`Parasites - Internal parasites`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e40_1", names(Dumm), value = TRUE)] == "Internal parasites,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Parasites - Tick` 
    AnimalSys$`Parasites - Tick`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e40_1", names(Dumm), value = TRUE)] == "Tick,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Parasites - Worms` 
    AnimalSys$`Parasites - Worms`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e40_1", names(Dumm), value = TRUE)] == "Worms"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Care - Traditional treatment` 
    AnimalSys$`Care - Traditional treatment`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e411", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Care - Chemicals` 
    AnimalSys$`Care - Chemicals`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e412", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`CareReasonYes - Better for environment/ AE` 
    AnimalSys$`CareReasonYes - Better for environment/ AE`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e41_1", names(Dumm), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`CareReasonYes - Human health` 
    AnimalSys$`CareReasonYes - Human health`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e41_1", names(Dumm), value = TRUE)] == "Human health"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`CareReasonYes - Treatment / prevention` 
    AnimalSys$`CareReasonYes - Treatment / prevention` [i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e41_1", names(Dumm), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`CareReasonNo - Not available on the market` 
    AnimalSys$`CareReasonNo - Not available on the market`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e41_2", names(Dumm), value = TRUE)] == "Not available on the market"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`CareReasonNo - Too expensive` 
    AnimalSys$`CareReasonNo - Too expensive`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e41_2", names(Dumm), value = TRUE)] == "Too expensive"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`CareReasonNo - No need, not severe disease` 
    AnimalSys$`CareReasonNo - No need, not severe disease`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e41_2", names(Dumm), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`CareReasonNo - Too difficult to handle animal or no VT service` 
    AnimalSys$`CareReasonNo - Too difficult to handle animal or no VT service`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e41_2", names(Dumm), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)  
    #`Diseases` 
    AnimalSys$`Diseases`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e42", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Antibiotics - For treatment diseases only` 
    AnimalSys$`Antibiotics - For treatment diseases only`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e431", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Antibiotics - For prevention of diseases only` 
    AnimalSys$`Antibiotics - For prevention of diseases only`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e432", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`Antibiotics - For growth promotion` 
    AnimalSys$`Antibiotics - For growth promotion`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e433", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonYes - Better for environment/ AE` 
    AnimalSys$`AntibioticsReasonYes - Better for environment/ AE`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_1", names(Dumm), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonYes - Human health` 
    AnimalSys$`AntibioticsReasonYes - Human health`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_1", names(Dumm), value = TRUE)] == "Human health"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonYes - Treatment / prevention` 
    AnimalSys$`AntibioticsReasonYes - Treatment / prevention`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_1", names(Dumm), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonNo - Not available on the market` 
    AnimalSys$`AntibioticsReasonNo - Not available on the market`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_2", names(Dumm), value = TRUE)] == "Not available on the market,"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonNo - Too expensive` 
    AnimalSys$`AntibioticsReasonNo - Too expensive`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_2", names(Dumm), value = TRUE)] == "Too expensive"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonNo - Already include in the feed` 
    AnimalSys$`AntibioticsReasonNo - Already include in the feed`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_2", names(Dumm), value = TRUE)] == "Already include in the feed"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonNo - No need, not severe disease` 
    AnimalSys$`AntibioticsReasonNo - No need, not severe disease`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_2", names(Dumm), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
    #`AntibioticsReasonNo - Too difficult to handle animal or no VT service` 
    AnimalSys$`AntibioticsReasonNo - Too difficult to handle animal or no VT service`[i] <-
      round(sum(Dumm$SW_Weight[Dumm[,grep("e43_2", names(Dumm), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
              AnimalSys$`N Animals sys`[i], digits = 3)
  }
  #Poultry
  if (AnimalSys$Animalkind[i] %in% c("Duck or/and Muscovy","Chicken","Goose")){
      #`Dry season day - Confined in a barn,`
      AnimalSys$`Dry season day - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e45", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Dry season day - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e45", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Attached in the grazing area, home yard without fence`
      AnimalSys$`Dry season day - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e45", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Grazing with shepherd,`
      AnimalSys$`Dry season day - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e45", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season day - Free grazing without Shepherd`
      AnimalSys$`Dry season day - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e45", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Confined in a barn,`
      AnimalSys$`Dry season night - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e46", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Dry season night - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e46", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Attached in the grazing area, home yard without fence`
      AnimalSys$`Dry season night - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e46", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Grazing with shepherd,`
      AnimalSys$`Dry season night - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e46", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season night - Free grazing without Shepherd`
      AnimalSys$`Dry season night - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e46", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #DrySeasonGrazing
      AnimalSys$`DrySeasonGrazing`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e47", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Grazing in public area`
      AnimalSys$`Dry season feeding - Grazing in public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e481", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Grazing in own pasture area`
      AnimalSys$`Dry season feeding - Grazing in own pasture area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e482", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Cutting and carry natural grass / vegetables from public area`
      AnimalSys$`Dry season feeding - Cutting and carry natural grass / vegetables from public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e483", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Cutting and carry natural grass / vegetables from own area`
      AnimalSys$`Dry season feeding - Cutting and carry natural grass / vegetables from own area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e484", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Cutting and carry of forage`
      AnimalSys$`Dry season feeding - Cutting and carry of forage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e485", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
      AnimalSys$`Dry season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e486", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
      AnimalSys$`Dry season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e487", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Silage`
      AnimalSys$`Dry season feeding - Silage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e488", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Kitchen waste`
      AnimalSys$`Dry season feeding - Kitchen waste`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e489T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Rice, rice brand, broken rice… bought from market`
      AnimalSys$`Dry season feeding - Rice, rice brand, broken rice… bought from market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e4810", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`
      AnimalSys$`Dry season feeding - Rice, rice brand, broken rice… bought from own farm`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e4811", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Restaurant / Market waste`
      AnimalSys$`Dry season feeding - Restaurant / Market waste`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e4812", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Dry season feeding - Waste from processing rice wine, soybean milke`
      AnimalSys$`Dry season feeding - Waste from processing rice wine, soybean milke`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e4813", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      
      #`Wet season day - Confined in a barn,`
      AnimalSys$`Wet season day - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e49", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Wet season day - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e49", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Attached in the grazing area, home yard without fence`
      AnimalSys$`Wet season day - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e49", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Grazing with shepherd,`
      AnimalSys$`Wet season day - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e49", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season day - Free grazing without Shepherd`
      AnimalSys$`Wet season day - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e49", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Confined in a barn,`
      AnimalSys$`Wet season night - Confined in a barn,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e50", names(Dumm), value = TRUE)] == "Confined in a barn,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`
      AnimalSys$`Wet season night - Confined in a shelter in the grazing area, or home yard with fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e50", names(Dumm), value = TRUE)] == "Confined in a shelter in the grazing area, or home yard with fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Attached in the grazing area, home yard without fence`
      AnimalSys$`Wet season night - Attached in the grazing area, home yard without fence`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e50", names(Dumm), value = TRUE)] == "Attached in the grazing area, home yard without fence"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Grazing with shepherd,`
      AnimalSys$`Wet season night - Grazing with shepherd,`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e50", names(Dumm), value = TRUE)] == "Grazing with shepherd,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season night - Free grazing without Shepherd`
      AnimalSys$`Wet season night - Free grazing without Shepherd`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e50", names(Dumm), value = TRUE)] == "Free grazing without Shepherd"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #WetSeasonGrazing
      AnimalSys$`WetSeasonGrazing`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e51", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Grazing in public area`
      AnimalSys$`Wet season feeding - Grazing in public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e521", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Grazing in own pasture area`
      AnimalSys$`Wet season feeding - Grazing in own pasture area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e522", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Cutting and carry natural grass / vegetables from public area`
      AnimalSys$`Wet season feeding - Cutting and carry natural grass / vegetables from public area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e523", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Cutting and carry natural grass / vegetables from own area`
      AnimalSys$`Wet season feeding - Cutting and carry natural grass / vegetables from own area`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e524", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Cutting and carry of forage`
      AnimalSys$`Wet season feeding - Cutting and carry of forage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e525", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`
      AnimalSys$`Wet season feeding - Own crop residues (rice straw, maize stem...) grazing after harvest`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e526", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`
      AnimalSys$`Wet season feeding - Own crop residues (rice straw, maize stem…) collected and stored`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e527", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Silage`
      AnimalSys$`Wet season feeding - Silage`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e528", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Kitchen waste`
      AnimalSys$`Wet season feeding - Kitchen waste`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e529T", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Rice, rice brand, broken rice… bought from market`
      AnimalSys$`Wet season feeding - Rice, rice brand, broken rice… bought from market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e5210", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`
      AnimalSys$`Wet season feeding - Rice, rice brand, broken rice… bought from own farm`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e5211", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Restaurant / Market waste`
      AnimalSys$`Wet season feeding - Restaurant / Market waste`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e5212", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Wet season feeding - Waste from processing rice wine, soybean milke`
      AnimalSys$`Wet season feeding - Waste from processing rice wine, soybean milke`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e5213", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ConcentrateUse - From farm` 
      AnimalSys$`ConcentrateUse - From farm`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e53_11", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`ConcentrateUse - From market` 
      AnimalSys$`ConcentrateUse - From market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e53_12", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Parasites - Internal parasites` 
      AnimalSys$`Parasites - Internal parasites`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e54_1", names(Dumm), value = TRUE)] == "Internal parasites,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Parasites - Tick` 
      AnimalSys$`Parasites - Tick`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e54_1", names(Dumm), value = TRUE)] == "Tick,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Parasites - Worms` 
      AnimalSys$`Parasites - Worms`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e54_1", names(Dumm), value = TRUE)] == "Worms"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Care - Traditional treatment` 
      AnimalSys$`Parasites - Worms`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e551", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Care - Chemicals` 
      AnimalSys$`Care - Chemicals`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e552", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonYes - Better for environment/ AE` 
      AnimalSys$`CareReasonYes - Better for environment/ AE`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e55a_1", names(Dumm), value = TRUE)] == "Better for environment/ AE"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonYes - Human health` 
      AnimalSys$`CareReasonYes - Human health`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e55a_1", names(Dumm), value = TRUE)] == "Human health"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonYes - Treatment / prevention` 
      AnimalSys$`CareReasonYes - Treatment / prevention` [i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e55a_1", names(Dumm), value = TRUE)] == "Treatment / prevention"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - Not available on the market` 
      AnimalSys$`CareReasonNo - Not available on the market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e55a_2", names(Dumm), value = TRUE)] == "Not available on the market"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - Too expensive` 
      AnimalSys$`CareReasonNo - Too expensive`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e55a_2", names(Dumm), value = TRUE)] == "Too expensive"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - No need, not severe disease` 
      AnimalSys$`CareReasonNo - No need, not severe disease`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e55a_2", names(Dumm), value = TRUE)] == "No need, not severe disease"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`CareReasonNo - Too difficult to handle animal or no VT service` 
      AnimalSys$`CareReasonNo - Too difficult to handle animal or no VT service`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e55a_2", names(Dumm), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)  
      #`Diseases` 
      AnimalSys$`Diseases`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e56", names(Dumm), value = TRUE)] == "Yes"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Antibiotics - For treatment diseases only` 
      AnimalSys$`Antibiotics - For treatment diseases only`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e571", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Antibiotics - For prevention of diseases only` 
      AnimalSys$`Antibiotics - For prevention of diseases only`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e572", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`Antibiotics - For growth promotion` 
      AnimalSys$`Antibiotics - For growth promotion`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e573", names(Dumm), value = TRUE)] == "1"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Not available on the market` 
      AnimalSys$`AntibioticsReasonNo - Not available on the market`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e57_2", names(Dumm), value = TRUE)] == "Not available on the market,"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Too expensive` 
      AnimalSys$`AntibioticsReasonNo - Too expensive`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e57_2", names(Dumm), value = TRUE)] == "Too expensive"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Already include in the feed` 
      AnimalSys$`AntibioticsReasonNo - Already include in the feed`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e57_2", names(Dumm), value = TRUE)] == "Already include in the feed"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - No need, not severe disease` 
      AnimalSys$`AntibioticsReasonNo - No need, not severe disease`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e57_2", names(Dumm), value = TRUE)] =="No need, not severe disease"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
      #`AntibioticsReasonNo - Too difficult to handle animal or no VT service` 
      AnimalSys$`CareReasonNo - Too difficult to handle animal or no VT service`[i] <-
        round(sum(Dumm$SW_Weight[Dumm[,grep("e57_2", names(Dumm), value = TRUE)] == "Too difficult to handle animal or no VT service"], na.rm = T) /
                AnimalSys$`N Animals sys`[i], digits = 3)
    }
  }

#Round max values to 1
AnimalSys$`ShareSoldMainOutlet - Over 75%` <- ifelse(AnimalSys$`ShareSoldMainOutlet - Over 75%` > 1, 1, AnimalSys$`ShareSoldMainOutlet - Over 75%`)
#Round max values to 1
AnimalSys$`ContractKind - No contract/ no prior arrangements` <- ifelse(AnimalSys$`ContractKind - No contract/ no prior arrangements` > 1, 1, AnimalSys$`ContractKind - No contract/ no prior arrangements`)
#Round max values to 1
AnimalSys$HouseholdsSellingPerCent <- ifelse(AnimalSys$HouseholdsSellingPerCent > 1, 1, AnimalSys$HouseholdsSellingPerCent)
#We remove columns including only NA or 0 values
AnimalSys <- AnimalSys[, colSums(!is.na(AnimalSys)) > 0]
AnimalSys <- AnimalSys[, colSums(AnimalSys != 0, na.rm = TRUE) > 0]


#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(AnimalSys, "AnimalSys_TF.rds")
