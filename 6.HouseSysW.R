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

## c - list
Sarealist <- c("Cambodia", "Lao", "Dien Bien province", "Son La province")
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
         "Pheromone traps","Protein baits","Weaver ant","Any other methods")
AEPcat <- rep(c("AEP Water", "AEP SoilCons", "AEP SoilFert", "AEP Weed",
                "AEP PestnDisease"), times = c(8, 8, 12, 15, 16))
AEPvar <- c("d131_2","d132_2","d133_2","d134_2","d135_2","d136_2",
            "d137_2","d138_2","d151_2","d152_2","d153_2","d154_2",
            "d155_2","d156_2","d157_2","d158_2","d18_12","d18_22",
            "d18_32","d18_42","d18_52","d18_62","d18_72","d18_82",
            "d18_92","d18_102","d18_112_a","d18_992","d21_13","d21_23",
            "d21_33","d21_43","d21_53","d21_63","d21_73","d21_83",
            "d21_93","d21_103","d21_113","d2112_3","d21_133","d21_143",
            "d21_993","d27_12","d27_22","d27_32","d27_42","d27_52",
            "d27_62","d27_72","d27_82","d27_92","d27_102","d27112",
            "d27_122","d27_132","d27_142","d27_152","d27_992")
AEP <- rbind(AEPcat,AEP,AEPvar)

### 2 - Table fulfilling
i = 2
k = 1
#Loop to fulfil the table
for (i in 1:nrow(HouseSys)){
  Dumm <- CropSystems3C_TF[CropSystems3C_TF$S_Area == HouseSys$Country[i] &
                             CropSystems3C_TF$CAT == HouseSys$Code[i],]
  #Loop for reasons for adopting AE practices
  HouseSys$N[i] <- sum(Dumm$SW_Weight)
  HouseSys$PerCentHousehold[i] <- HouseSys$N[i] / sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == HouseSys$Country[i]])
  for (k in 1:ncol(AEP)){
    Dum <- paste0(AEP[1,k],"-",AEP[2,k],"-PerCentHouseholds")
    if (!(Dum %in% names(HouseSys))) {
      HouseSys <- add_column(HouseSys, !!Dum := NA_real_)
    }
    HouseSys[[Dum]] <- 
      ifelse(HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i][i],
             round(sum(Dumm$SW_Weight[!is.na(Dumm[[AEP[3,k]]]) & Dumm[[AEP[3,k]]] != ""], na.rm = T) /
                                        sum(Dumm$SW_Weight),
                                        digits = 3), HouseSys[[Dum]])
    for (j in c("To save money","To save labor/time","To improve yields",
                "To improve soil/plant health","Other")){
      #Loop for different AEP practices with associated categories
      Dum <- paste0(AEP[1,k],"-",AEP[2,k],"-",j)
      if (!(Dum %in% names(HouseSys))) {
        HouseSys <- add_column(HouseSys, !!Dum := NA_real_)
      }
      HouseSys[[Dum]] <- 
        ifelse(HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i][i],
               round(sum(Dumm$SW_Weight[Dumm[[AEP[3,k]]] == j], na.rm = T) /
                                          sum(Dumm$SW_Weight[!is.na(Dumm[[AEP[3,k]]]) & Dumm[[AEP[3,k]]] != ""], na.rm = T),
                                          digits = 3), HouseSys[[Dum]])
      }
  }
  #Isolated information about practices
  #Soil conservation practice
  #%households mentionning a reason for not implementing AE soil conservation practice
  HouseSys$`PerCentH - ReasonForNotImplAEPSC`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Reason for not implementing AE soil conservation practice - "I do not know them"
  HouseSys$`ReasonForNotImplAEPSC - I do not know them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "I do not know them"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil conservation practice - "They are too costly"
  HouseSys$`ReasonForNotImplAEPSC - They are too costly`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil conservation practice - "I have no time to implement them"
  HouseSys$`ReasonForNotImplAEPSC - I have no time to implement them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil conservation practice - "I don't want to do things differently from my neighbors"
  HouseSys$`ReasonForNotImplAEPSC - I don't want to do things differently from my neighbors`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Soil fertility practice
  #%households mentionning a reason for not implementing AE soil fertility practice
  HouseSys$`PerCentH - ReasonForNotImplAEPSF`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Reason for not implementing AE soil fertility practice - "I do not know them"
  HouseSys$`ReasonForNotImplAEPSF - I do not know them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "I do not know them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil fertility practice - "They are too costly"
  HouseSys$`ReasonForNotImplAEPSF - They are too costly`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil fertility practice - "I have no time to implement them"
  HouseSys$`ReasonForNotImplAEPSF - I have no time to implement them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil fertility practice - "I don't want to do things differently from my neighbors"
  HouseSys$`ReasonForNotImplAEPSF - I don't want to do things differently from my neighbors`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Weed management practice
  #%households mentionning a reason for not implementing AE weed management practice
  HouseSys$`PerCentH - ReasonForNotImplAEPWM`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Reason for not implementing AE weed management practice - "I do not know them"
  HouseSys$`ReasonForNotImplAEPWM - I do not know them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "I do not know them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "They are too costly"
  HouseSys$`ReasonForNotImplAEPWM - They are too costly`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "I have no time to implement them"
  HouseSys$`ReasonForNotImplAEPWM - I have no time to implement them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "I don't want to do things differently from my neighbors"
  HouseSys$`ReasonForNotImplAEPWM - I don't want to do things differently from my neighbors`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Other method used for weed management
  #%households mentioning another method for AE weed management practice
  HouseSys$`PerCentH - WMOtherMethod`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Other method used for weed management - "Synthetic herbicide without mechanical weeding"
  HouseSys$`OtherPracticeWM - Synthetic herbicide without mechanical weeding`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d23 == "Synthetic herbicide without mechanical weeding"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "Frequent mechanical weeding (more than three times per year) without synthetic herbicide"
  HouseSys$`OtherPracticeWM - Frequent mechanical weeding (more than three times per year) without synthetic herbicide`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d23 == "Frequent mechanical weeding (more than three times per year) without synthetic herbicide"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "Mixed management using herbicide and mechanical weeding"
  HouseSys$`OtherPracticeWM - Mixed management using herbicide and mechanical weeding`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d23 == "Mixed management using herbicide and mechanical weeding"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T), digits = 3)
 #Pest&DiseaseManagement
  #NbHouseholdsConcerned
  HouseSys$`PestDisease - PerCentHouseholdsProblemCropPest`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <- 
    round(sum(Dumm$SW_Weight[Dumm$d24 == "Yes"], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #%households mentionning a reason for not implementing AE pest&disease management practice
  HouseSys$`PerCentH - ReasonForNotImplAEPPDM`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Reason for not implementing AE pest&disease management practice - "I do not know them"
  HouseSys$`ReasonForNotImplAEPPDM - I do not know them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "I do not know them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE pest&disease management practice - "They are too costly"
  HouseSys$`ReasonForNotImplAEPPDM - They are too costly`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE pest&disease management practice - "I have no time to implement them"
  HouseSys$`ReasonForNotImplAEPPDM - I have no time to implement them`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE pest&disease management practice - "I don't want to do things differently from my neighbors"
  HouseSys$`ReasonForNotImplAEPPDM - I don't want to do things differently from my neighbors`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management
  #%households mentioning another method for AE pest and disease management practice
  HouseSys$`PerCentH - PDMOtherMethod`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Other method used for pest and disease management - "Synthetic insecticide and fungicide are used regularly and no other system is used"
  HouseSys$`OtherPracticePDM - Synthetic insecticide and fungicide are used regularly and no other system is used`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Synthetic insecticide and fungicide are used regularly and no other system is used"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management - "Mixed use of synthetic and biological/natural pesticides"
  HouseSys$`OtherPracticePDM - Mixed use of synthetic and biological/natural pesticides`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Mixed use of synthetic and biological/natural pesticides"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management - "Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used"
  HouseSys$`OtherPracticePDM - Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management - "Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide
  HouseSys$`OtherPracticePDM - Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Provenance of the seeds
  #%households who conserve and use own seeds
  HouseSys$`PerCentH - SeedProvenance`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm[,9959]) & Dumm[,9959] != ""], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Seed source 1: From the village seller
  HouseSys$`SeedProvenance - From the village seller`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From the village seller"], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Seed source 2: From the cooperative
  HouseSys$`SeedProvenance - From the cooperative`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From the cooperative"], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Seed source 3: From a trader in town
  HouseSys$`SeedProvenance - From a trader in town`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From a trader in town"], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Seed source 4: From family and friends
  HouseSys$`SeedProvenance - From family & friends`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From family & friends"], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  #Seed source 5: Other
  HouseSys$`SeedProvenance - Other`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "Other"], na.rm = T) /
            HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
  
  #INFORMATION ABOUT HOUSEHOLD MEMBERS
  #Preparation
  Dumm$NbhMembers <- rowSums(!is.na(Dumm[,grep("a2.", names(Dumm))]))
  Dumm$NbhActiveMembers <- rowSums(!is.na(Dumm[,grep("a6.", names(Dumm))]) &
                                      Dumm[,grep("a6.", names(Dumm))] != "Study/training" &
                                      Dumm[,grep("a6.", names(Dumm))] != "Unable to work")
  Dumm$NbhFarmMembersPrim <- rowSums(!is.na(Dumm[,grep("a6.", names(Dumm))]) &
                                     Dumm[,grep("a6.", names(Dumm))] == "Agricultural work on their own farm (including livestock management)")
  Dumm$NbhFarmMembersSec <- rowSums(!is.na(Dumm[,grep("a7.", names(Dumm))]) &
                                     Dumm[,grep("a7.", names(Dumm))] == "Agricultural work on their own farm (including livestock management)")
  Dumm$AgeHH <- NA
  #Loop for average age of household head
  for (l in 0:19){
    Dumm$AgeHH <- ifelse(!is.na(Dumm[,9962+l*36]) & Dumm[,9962+l*36] == "Household head", 
                          Dumm[,9964+l*36], Dumm$AgeHH)
  }
    #Nb of migrating members
  Dumm$NbMigratingMembers <- rowSums(!is.na(Dumm[,grep("a10.", names(Dumm))]) &
                                      Dumm[,grep("a10.", names(Dumm))] == "Yes")
  #Nb of month durign which some members are migrating
  Dumm$YearlyRatemigrationperMigrant <- ifelse(Dumm$NbMigratingMembers > 0, (rowSums(!is.na(Dumm[,grep("a13", names(Dumm))]) &
                                       Dumm[,grep("a13", names(Dumm))] == "1") / 12) / 
                                       Dumm$NbMigratingMembers, NA)
  #Nb of month during which members can see animals ribs/bones
  Dumm$MonthRibBones <- rowSums(as.matrix(Dumm[,11231:11242]) == 1)
  #Nb of month during which members can see animals lack of feed
  Dumm$MonthLackFeed <- rowSums(as.matrix(Dumm[,11244:11255]) == 1)
  
  #Svy design implementation
  survey_design <- svydesign(ids = ~1, weights = ~SW_Weight, data = Dumm)
  #Nb of household members
  HouseSys$NbHouseholdMembers[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(svymean(~ NbhMembers, design = survey_design, na.rm = TRUE), digits = 2)
  #Nb of active household members (depending on main occupation)
  HouseSys$NbActiveMembers[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(svymean(~ NbhActiveMembers, design = survey_design, na.rm = TRUE), digits = 2)
  #Nb of household members working on the farm (main activity)
  HouseSys$NbFarmFirstOcc[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(svymean(~ NbhFarmMembersPrim, design = survey_design, na.rm = TRUE), digits = 2)
  #Nb of households members working on the farm (secondary activity)
  HouseSys$NbFarmSecOcc[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(svymean(~ NbhFarmMembersSec, design = survey_design, na.rm = TRUE), digits = 2)
  #Age of the household head
  HouseSys$AgeHH[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(svymean(~ AgeHH, design = survey_design, na.rm = TRUE), digits = 0)
  #Nb of people migrating in the household
  HouseSys$NbMigratingMembers[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(svymean(~ NbMigratingMembers, design = survey_design, na.rm = TRUE), digits = 2)
  #Average nb of months in migration
  HouseSys$YearlyRatemigrationperMigrant[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
    round(svymean(~ YearlyRatemigrationperMigrant, design = survey_design, na.rm = TRUE), digits = 2)

  #INFORMATION ABOUT HOUSEHOLD COLLABORATION
  #% of households collaborating for: Share labor (mutual help, working together on each other farm)
HouseSys$`Collab - Share labor (mutual help, working together on each other farm)`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c131 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c131) & Dumm$c131 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Manage water/irrigation systems
HouseSys$`Collab - Manage water/irrigation systems`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c132 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c132) & Dumm$c132 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Raise livestock
HouseSys$`Collab - Raise livestock`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c133 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c133) & Dumm$c133 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Buy agricultural inputs
HouseSys$`Collab - Buy agricultural inputs`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c134 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c134) & Dumm$c134 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Selling products to the markets for other farmers
HouseSys$`Collab - Selling products to the markets for other farmers`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c135 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c135) & Dumm$c135 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Experiment new farming practices
HouseSys$`Collab - Experiment new farming practices`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c136 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c136) & Dumm$c136 != ""], na.rm = T), digits = 3)
#% of households collaborating for: No collaboration with other people on these issues
HouseSys$`Collab - No collaboration with other people on these issues`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c130 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c130) & Dumm$c130 != ""], na.rm = T), digits = 3)
#% of households with hired workers
HouseSys$SHareHouseholdsHiringWorkers[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d35 > 0], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average cost of workers hired per year
HouseSys$`ValueHiredWorkersPerYear`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d35, design = survey_design, na.rm = TRUE), digits = 2)

#INFORMATION ABOUT LABOR
#Land preparation - Do not have
HouseSys$`Laborkind-Land preparation-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_10 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Land preparation - Family members
HouseSys$`Laborkind-Land preparation-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_11 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Land preparation - Hired people
HouseSys$`Laborkind-Land preparation-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_12 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Land preparation - Mutual help
HouseSys$`Laborkind-Land preparation-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_13 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Sowing - Do not have
HouseSys$`Laborkind-Sowing-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_20 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Sowing - Family members
HouseSys$`Laborkind-Sowing-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_21 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Sowing - Hired people
HouseSys$`Laborkind-Sowing-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_22 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Sowing - Mutual help
HouseSys$`Laborkind-Sowing-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_23 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Fertilization - Do not have
HouseSys$`Laborkind-Fertilization-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_30 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Fertilization - Family members
HouseSys$`Laborkind-Fertilization-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_31 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Fertilization - Hired people
HouseSys$`Laborkind-Fertilization-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_32 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Fertilization - Mutual help
HouseSys$`Laborkind-Fertilization-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_33 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Weed management - Do not have
HouseSys$`Laborkind-Weed management-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_40 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Weed management - Family members
HouseSys$`Laborkind-Weed management-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_41 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Weed management - Hired people
HouseSys$`Laborkind-Weed management-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_42 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Weed management - Mutual help
HouseSys$`Laborkind-Weed management-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_43 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pest and disease management - Do not have
HouseSys$`Laborkind-Pest and disease management-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_50 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pest and disease management - Family members
HouseSys$`Laborkind-Pest and disease management-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_51 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pest and disease management - Hired people
HouseSys$`Laborkind-Pest and disease management-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_52 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pest and disease management - Mutual help
HouseSys$`Laborkind-Pest and disease management-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_53 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Crop stimulation/ growth regulation - Do not have
HouseSys$`Laborkind-Crop stimulation/ growth regulation-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_60 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Crop stimulation/ growth regulation - Family members
HouseSys$`Laborkind-Crop stimulation/ growth regulation-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_61 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Crop stimulation/ growth regulation - Hired people
HouseSys$`Laborkind-Crop stimulation/ growth regulation-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_62 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Crop stimulation/ growth regulation - Mutual help
HouseSys$`Laborkind-Crop stimulation/ growth regulation-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_63 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pruning - Do not have
HouseSys$`Laborkind-Pruning-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_70 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pruning - Family members
HouseSys$`Laborkind-Pruning-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_71 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pruning - Hired people
HouseSys$`Laborkind-Pruning-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_72 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pruning - Mutual help
HouseSys$`Laborkind-Pruning-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_73 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Water/irrigation management - Do not have
HouseSys$`Laborkind-Water/irrigation management-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_80 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Water/irrigation management - Family members
HouseSys$`Laborkind-Water/irrigation management-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_81 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Water/irrigation management - Hired people
HouseSys$`Laborkind-Water/irrigation management-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_82 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Water/irrigation management - Mutual help
HouseSys$`Laborkind-Water/irrigation management-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_83 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Harvest - Do not have
HouseSys$`Laborkind-Harvest-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_90 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Harvest - Family members
HouseSys$`Laborkind-Harvest-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_91 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Harvest - Hired people
HouseSys$`Laborkind-Harvest-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_92 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Harvest - Mutual help
HouseSys$`Laborkind-Harvest-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_93 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Transportation - Do not have
HouseSys$`Laborkind-Transportation-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_100 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Transportation - Family members
HouseSys$`Laborkind-Transportation-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_101 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Transportation - Hired people
HouseSys$`Laborkind-Transportation-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_102 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Transportation - Mutual help
HouseSys$`Laborkind-Transportation-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_103 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Post-harvest processing - Do not have
HouseSys$`Laborkind-Post-harvest processing-Do not have`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_110 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Post-harvest processing - Family members
HouseSys$`Laborkind-Post-harvest processing-Family members`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_111 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Post-harvest processing - Hired people
HouseSys$`Laborkind-Post-harvest processing-Hired people`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_112 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Post-harvest processing - Mutual help
HouseSys$`Laborkind-Post-harvest processing-Mutual help`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_113 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#EXTERNAL-MECHANIZED SERVICE USED by hh
#Land preparation
HouseSys$`ExtMechaServices-Land preparation`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d361 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Sowing
HouseSys$`ExtMechaServices-Sowing`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d362 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Fertilization
HouseSys$`ExtMechaServices-Fertilization`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d363 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Weed management
HouseSys$`ExtMechaServices-Weed management`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d364 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pest and disease management
HouseSys$`ExtMechaServices-Pest and disease management`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d365 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Pruning
HouseSys$`ExtMechaServices-Pruning`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d366 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Water/irrigation management
HouseSys$`ExtMechaServices-Water/irrigation management`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d367 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Harvest
HouseSys$`ExtMechaServices-Harvest`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d368 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Transportation
HouseSys$`ExtMechaServices-Transportation`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d369 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Post-harvest processing
HouseSys$`ExtMechaServices-Post-harvest processing`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d3610 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#None
HouseSys$`ExtMechaServices-None of above`[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d360 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#EQUIPMENTS
#Motorcycle
HouseSys$NbMotorcycle[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_1, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Wheel hand tractor
HouseSys$NbWheelHandTractor[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_2, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Wheel tractor
HouseSys$NbWheelTractor[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_3, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Car
HouseSys$NbCar[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_4, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Truck
HouseSys$NbTruck[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_5, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Combine harvester
HouseSys$NbCombineHarvester[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_6, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Land leveler
HouseSys$NbLandLeveler[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_7, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Rice planter
HouseSys$NbRicePlanter[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_8, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Maize planter
HouseSys$NbMaizePlanter[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_9, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Cassava disc ridging tool
HouseSys$NbCassavaDiscRidgingTool[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_10, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Cassava harvesting tool
HouseSys$NbCassavaHarvestingTool[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_11, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Rice thresher
HouseSys$NbRiceThresher[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_12, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Rice mill
HouseSys$NbRiceMill[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_13, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Backpack sprayer
HouseSys$NbBackpackSprayer[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_14, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Motorpump sprayer
HouseSys$NbMotorPumpSprayer[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_15, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Grass cutter
HouseSys$NbGrassCutter[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_16, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Grass chopping machine
HouseSys$NbGrassChoppingMachine[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_17, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Water pump
HouseSys$NbWaterPump[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_18, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Irrigation system
HouseSys$NbIrrigationSystem[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_19, na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#ADDITIONNAL INFORMATION ABOUT FISHES
#% of households who are raising fishes
HouseSys$PerCentHRaisingFish[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e8 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average size of the fish pond (m2)
HouseSys$AvSizeFishPond[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ e9, design = survey_design, na.rm = TRUE), digits = 2)
#Average nb of fish breeds raised
HouseSys$NbFishBreed[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ e10, design = survey_design, na.rm = TRUE), digits = 2)

#ADDITIONNAL INFORMATION ABOUT ANIMALS
#% of households who raise animals (% of households with animals)
HouseSys$PerCentHAnimals[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#% of households who are growing forage (% of households with animals)
HouseSys$PerCentHGrowingForage[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e12_1 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average size of forage fields (m2) (% of households with animals)
HouseSys$AvSizeForage[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ e12_2, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who have natural pasture land (% of households with animals)
HouseSys$PerCentHNaturePasture[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e12_3 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average size of natural pastureland (m2) (% of households with animals)
HouseSys$AvSizeNaturePasture[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ e12_4, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who have improved pasture land (% of households with animals)
HouseSys$PerCentHImprovedPasture[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e12_5 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average size of improved pasture land (m2) (% of households with animals)
HouseSys$AvSizeImprovedPasture[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ e12_6, design = survey_design, na.rm = TRUE), digits = 2)
#% of household who fertilize the forage or improved pasture (% of households with animals)
HouseSys$PerCentHFertilizeForage[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e13 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of household who fertilize the forage or improved pasture - Synthetic fertilizer (% of households with animals)
HouseSys$'PerCentHFertForage - Synthetic fertilizer'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e13_1 == "Synthetic fertilizer"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of household who fertilize the forage or improved pasture - Organic manure (% of households with animals)
HouseSys$'PerCentHFertForage - Organic manure'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e13_1 == "Organic manure"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of household who fertilize the forage or improved pasture - Both above (% of households with animals)
HouseSys$'PerCentHFertForage - Both above'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e13_1 == "Both above"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who use grass-legume mix for improved pasture/forage (% of households with animals)
HouseSys$PerCentHGrassLegumePasture[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e14 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who use a public/collective pastureland (% of households with animals)
HouseSys$PerCentHGrassLegumePasture[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e15 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - Total (at any part of the year - % of households with animals)
HouseSys$PerCentHRibsBonesAnimals[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average nb of month during which rib/bones are seen (including only households with this issue)
HouseSys$AvNbMonthRibsBonesAnimals[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ MonthRibBones, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who see rib/bones of the animals - January (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - January'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_11 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - February (% of households with animals))
HouseSys$'PerCentHRibsBonesAnimals - February'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_12 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - March (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - March'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_13 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - April (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - April'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_14 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - May (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - May'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_15 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - June (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - June'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_16 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - July (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - July'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_17 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - August (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - August'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_18 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - September (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - September'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_19 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - October (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - October'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_110 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - November (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - November'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_111 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - December (% of households with animals)
HouseSys$'PerCentHRibsBonesAnimals - December'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_112 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)

#% of households who declared a lack of feed for the animals - Total (at any part of the year - % of households with animals)
HouseSys$PerCentHLackFeedAnimals[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average nb of month during which there is a lack of feed for animals (including only households with this issue)
HouseSys$AvNbMonthLackFeedAnimals[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ MonthLackFeed, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who declared a lack of feed for the animals - January (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - January'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_11 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - February (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - February'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_12 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - March (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - March'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_13 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - April (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - April'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_14 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - May (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - May'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_15 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - June (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - June'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_16 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - July (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - July'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_17 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - August (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - August'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_18 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - September (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - September'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_19 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - October (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - October'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_110 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - November (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - November'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_111 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - December (% of households with animals)
HouseSys$'PerCentHLackFeedAnimals - December'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_112 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households for which animals have access to water (% of households with animals)
HouseSys$PerCentHWaterAccessAnimals[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i][i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$e60 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)

#SOURCES OF INCOME FOR THE HOUSEHOLD
#Main source of household income - Financial support / gift
HouseSys$'MainIncomeSource - Financial support / gift'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i]] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Financial support / gift"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i]], digits = 3)
#Main source of household income - Non-farm income (own business: shop, trader/collector etc.) 
HouseSys$'MainIncomeSource - Non-farm income (own business: shop, trader/collector etc.)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Non-farm income (own business: shop, trader/collector etc.)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i]], digits = 3)
#Main source of household income - Non-farm wages (salaried work in private or public company) 
HouseSys$'MainIncomeSource - Non-farm wages (salaried work in private or public company)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Non-farm wages (salaried work in private or public company)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Other  ${b3_2oth}
HouseSys$'MainIncomeSource - Other  ${b3_2oth}'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Other  ${b3_2oth}"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Pension
HouseSys$'MainIncomeSource - Pension'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Pension"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Remittances
HouseSys$'MainIncomeSource - Remittances'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Remittances"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Rented land
HouseSys$'MainIncomeSource - Rented land'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Rented land"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Sell derived/processed products
HouseSys$'MainIncomeSource - Sell derived/processed products'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Sell derived/processed products"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling cashew from the farm
HouseSys$'MainIncomeSource - Selling cashew from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling cashew from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling cassava from the farm
HouseSys$'MainIncomeSource - Selling cassava from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling cassava from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling cattle and buffalo
HouseSys$'MainIncomeSource - Selling cattle and buffalo'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling cattle and buffalo"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling labor
HouseSys$'MainIncomeSource - Selling labor'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling labor"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling maize from the farm
HouseSys$'MainIncomeSource - Selling maize from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling maize from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling other crops from the farm
HouseSys$'MainIncomeSource - Selling other crops from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling other crops from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling other farm products: ${b3_1oth}
HouseSys$'MainIncomeSource - Selling other farm products: ${b3_1oth}'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling other farm products: ${b3_1oth}"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling own fruits
HouseSys$'MainIncomeSource - Selling own fruits'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling own fruits"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling own vegetables
HouseSys$'MainIncomeSource - Selling own vegetables'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling own vegetables"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling pigs
HouseSys$'MainIncomeSource - Selling pigs'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling pigs"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling poultry
HouseSys$'MainIncomeSource - Selling poultry'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling poultry"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling rice from the farm
HouseSys$'MainIncomeSource - Selling rice from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling rice from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income - Selling soybean from the farm
HouseSys$'MainIncomeSource - Selling soybean from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling soybean from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#Second source of household income - Financial support / gift
HouseSys$'SecondIncomeSource - Financial support / gift'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Financial support / gift"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Non-farm income (own business: shop, trader/collector etc.) 
HouseSys$'SecondIncomeSource - Non-farm income (own business: shop, trader/collector etc.)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Non-farm income (own business: shop, trader/collector etc.)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Non-farm wages (salaried work in private or public company) 
HouseSys$'SecondIncomeSource - Non-farm wages (salaried work in private or public company)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Non-farm wages (salaried work in private or public company)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Other  ${b3_2oth}
HouseSys$'SecondIncomeSource - Other  ${b3_2oth}'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Other  ${b3_2oth}"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Pension
HouseSys$'SecondIncomeSource - Pension'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Pension"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Sell derived/processed products
HouseSys$'SecondIncomeSource - Sell derived/processed products'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Sell derived/processed products"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling cashew from the farm
HouseSys$'SecondIncomeSource - Selling cashew from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling cashew from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling cassava from the farm
HouseSys$'SecondIncomeSource - Selling cassava from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling cassava from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling cattle and buffalo
HouseSys$'SecondIncomeSource - Selling cattle and buffalo'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling cattle and buffalo"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling labor
HouseSys$'SecondIncomeSource - Selling labor'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling labor"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling maize from the farm
HouseSys$'SecondIncomeSource - Selling maize from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling maize from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling other crops from the farm
HouseSys$'SecondIncomeSource - Selling other crops from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling other crops from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling other farm products: ${b3_1oth}
HouseSys$'SecondIncomeSource - Selling other farm products: ${b3_1oth}'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling other farm products: ${b3_1oth}"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling own fruits
HouseSys$'SecondIncomeSource - Selling own fruits'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling own fruits"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling own vegetables
HouseSys$'SecondIncomeSource - Selling own vegetables'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling own vegetables"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling pigs
HouseSys$'SecondIncomeSource - Selling pigs'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling pigs"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling poultry
HouseSys$'SecondIncomeSource - Selling poultry'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling poultry"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling rice from the farm
HouseSys$'SecondIncomeSource - Selling rice from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling rice from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Second source of household income - Selling soybean from the farm
HouseSys$'SecondIncomeSource - Selling soybean from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling soybean from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#Third source of household income - Financial support / gift
HouseSys$'ThirdIncomeSource - Financial support / gift'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Financial support / gift"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Non-farm income (own business: shop, trader/collector etc.) 
HouseSys$'ThirdIncomeSource - Non-farm income (own business: shop, trader/collector etc.)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Non-farm income (own business: shop, trader/collector etc.)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Non-farm wages (salaried work in private or public company) 
HouseSys$'ThirdIncomeSource - Non-farm wages (salaried work in private or public company)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Non-farm wages (salaried work in private or public company)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Other  ${b3_2oth}
HouseSys$'ThirdIncomeSource - Other  ${b3_2oth}'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Other  ${b3_2oth}"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Pension
HouseSys$'ThirdIncomeSource - Pension'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Pension"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Sell derived/processed products
HouseSys$'ThirdIncomeSource - Sell derived/processed products'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Sell derived/processed products"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling cashew from the farm
HouseSys$'ThirdIncomeSource - Selling cashew from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling cashew from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling cassava from the farm
HouseSys$'ThirdIncomeSource - Selling cassava from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling cassava from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling cattle and buffalo
HouseSys$'ThirdIncomeSource - Selling cattle and buffalo'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling cattle and buffalo"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling labor
HouseSys$'ThirdIncomeSource - Selling labor'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling labor"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling maize from the farm
HouseSys$'ThirdIncomeSource - Selling maize from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling maize from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling other crops from the farm
HouseSys$'ThirdIncomeSource - Selling other crops from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling other crops from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling other farm products: ${b3_1oth}
HouseSys$'ThirdIncomeSource - Selling other farm products: ${b3_1oth}'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling other farm products: ${b3_1oth}"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling own fruits
HouseSys$'ThirdIncomeSource - Selling own fruits'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling own fruits"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling own vegetables
HouseSys$'ThirdIncomeSource - Selling own vegetables'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling own vegetables"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling pigs
HouseSys$'ThirdIncomeSource - Selling pigs'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling pigs"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling poultry
HouseSys$'ThirdIncomeSource - Selling poultry'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling poultry"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling rice from the farm
HouseSys$'ThirdIncomeSource - Selling rice from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling rice from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Third source of household income - Selling soybean from the farm
HouseSys$'ThirdIncomeSource - Selling soybean from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling soybean from the farm"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#Average % of income from 1st source of income
HouseSys$PerCentIncomeFirstSource[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ b5_1, design = survey_design, na.rm = TRUE), digits = 2) / 100
#Average % of income from 2nd source of income
HouseSys$PerCentIncomeSecondSource[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ b5_2, design = survey_design, na.rm = TRUE), digits = 2) / 100
#Average % of income from 3rd source of income
HouseSys$PerCentIncomeThirdSource[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ b5_3, design = survey_design, na.rm = TRUE), digits = 2) / 100

#% of households for which 3 main sources of income changed during the last 3 years
HouseSys$PerCentHIncomeSourceChange[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Main source of household income before change - Selling own vegetables
HouseSys$'IncomeSourceBefore - Selling own vegetables'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b71 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling own fruits
HouseSys$'IncomeSourceBefore - Selling own fruits'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b72 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling rice from the farm
HouseSys$'IncomeSourceBefore - Selling rice from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b73 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cassava from the farm
HouseSys$'IncomeSourceBefore - Selling cassava from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b719 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cashew from the farm
HouseSys$'IncomeSourceBefore - Selling cashew from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b720 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling soybean from the farm
HouseSys$'IncomeSourceBefore - Selling soybean from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b76 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cover crop seed from the farm
HouseSys$'IncomeSourceBefore - Selling cover crop seed from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b77 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling other crops from the farm
HouseSys$'IncomeSourceBefore - Selling other crops from the farm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b78 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cattle and buffalo
HouseSys$'IncomeSourceBefore - Selling cattle and buffalo'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b79 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling pigs
HouseSys$'IncomeSourceBefore - Selling pigs'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b710 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling poultry
HouseSys$'IncomeSourceBefore - Selling poultry'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b711 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling other farm products
HouseSys$'IncomeSourceBefore - Selling other farm products'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b712 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling other farm products
HouseSys$'IncomeSourceBefore - Selling derived/processed products'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b713 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Non-farm wages (salaried work in private or public company)
HouseSys$'IncomeSourceBefore - Non-farm wages (salaried work in private or public company)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b714 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Non-farm income (own business: shop, trader/collector etc.)
HouseSys$'IncomeSourceBefore - Non-farm income (own business: shop, trader/collector etc.)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b715 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling labor
HouseSys$'IncomeSourceBefore - Selling labor'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b716 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Remittances
HouseSys$'IncomeSourceBefore - Remittances'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b717 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Pension
HouseSys$'IncomeSourceBefore - Pension'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b718 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)

#%Households earning most of their income during specific months
HouseSys$'PerCentHMostIncomeSpecificMonth'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - January
HouseSys$'PerCentHMostIncomeSpecificMonth - January'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_11 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - February
HouseSys$'PerCentHMostIncomeSpecificMonth - February'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_12 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - March
HouseSys$'PerCentHMostIncomeSpecificMonth - March'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_13 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - April
HouseSys$'PerCentHMostIncomeSpecificMonth - April'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_14 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - May
HouseSys$'PerCentHMostIncomeSpecificMonth - May'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_15 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - June
HouseSys$'PerCentHMostIncomeSpecificMonth - June'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_16 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - July
HouseSys$'PerCentHMostIncomeSpecificMonth - July'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_17 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - August
HouseSys$'PerCentHMostIncomeSpecificMonth - August'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_18 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - September
HouseSys$'PerCentHMostIncomeSpecificMonth - September'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_19 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - October
HouseSys$'PerCentHMostIncomeSpecificMonth - October'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_110 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - November
HouseSys$'PerCentHMostIncomeSpecificMonth - November'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_111 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households earning most of their income during specific months - December
HouseSys$'PerCentHMostIncomeSpecificMonth - December'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_112 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#%Households facing financial difficulties during specific months
HouseSys$'PerCentHFinancialDifficulties'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - January
HouseSys$'PerCentHFinancialDifficulties - January'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_11 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - February
HouseSys$'PerCentHFinancialDifficulties - February'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_12 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - March
HouseSys$'PerCentHFinancialDifficulties - March'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_13 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - April
HouseSys$'PerCentHFinancialDifficulties - April'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_14 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - May
HouseSys$'PerCentHFinancialDifficulties - May'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_15 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - June
HouseSys$'PerCentHFinancialDifficulties - June'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_16 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - July
HouseSys$'PerCentHFinancialDifficulties - July'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_17 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - August
HouseSys$'PerCentHFinancialDifficulties - August'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_18 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - September
HouseSys$'PerCentHFinancialDifficulties - September'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_19 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - October
HouseSys$'PerCentHFinancialDifficulties - October'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_110 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - November
HouseSys$'PerCentHFinancialDifficulties - November'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_111 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households facing financial difficulties during specific months - December
HouseSys$'PerCentHFinancialDifficulties - December'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_112 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) -	Non-working members went out to look for work
HouseSys$'PerCentHOvercomeDifficulties - Non-working members went out to look for work'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_21 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) -	Working members increased work hours
HouseSys$'PerCentHOvercomeDifficulties - Working members increased work hours'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_22 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Reducing / postpone own farm work and sale labor to the others
HouseSys$'PerCentHOvercomeDifficulties - Reducing / postpone own farm work and sale labor to the others'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_23 == "1" | Dumm$b9_210 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - One or more members changes residence
HouseSys$'PerCentHOvercomeDifficulties - One or more members changes residence'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_24 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Spent savings
HouseSys$'PerCentHOvercomeDifficulties - Spent savings'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_25 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Taking loan
HouseSys$'PerCentHOvercomeDifficulties - Taking loan'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_26 == "1" | Dumm$b9_211 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) -Increase loan
HouseSys$'PerCentHOvercomeDifficulties - Increase loan'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_27 == "1" | Dumm$b9_212 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Went into debt
HouseSys$'PerCentHOvercomeDifficulties - Went into debt'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_27 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Sold property or assets
HouseSys$'PerCentHOvercomeDifficulties - Sold property or assets'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_28 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)

#Action to overcome these difficulties (%of household facing difficulties) - Sold animal (cattle,buffalo)
HouseSys$'PerCentHOvercomeDifficulties - Sold animal (cattle,buffalo)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_213 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Did nothing
HouseSys$'PerCentHOvercomeDifficulties - Did nothing'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_20 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Other
HouseSys$'PerCentHOvercomeDifficulties - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_299 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)

#%Household involvment in additionnal activities - Community-based tourism or agroecological tourism
HouseSys$'PerCentHAdditionnalActivities - Community-based tourism or agroecological tourism'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_31 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household involvment in additionnal activities - Hosting events (e.g. for projects from NGOs, research)
HouseSys$'PerCentHAdditionnalActivities - Hosting events (e.g. for projects from NGOs, research)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_32 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household involvment in additionnal activities - Education and training of others (e.g. training of other farmers, school visits)
HouseSys$'PerCentHAdditionnalActivities - Education and training of others (e.g. training of other farmers, school visits)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_33 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household involvment in additionnal activities - Food processing
HouseSys$'PerCentHAdditionnalActivities - Food processing'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_34 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household involvment in additionnal activities - Restaurant
HouseSys$'PerCentHAdditionnalActivities - Restaurant'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_35 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household involvment in additionnal activities - Selling products from other farms
HouseSys$'PerCentHAdditionnalActivities - Selling products from other farms'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_36 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#%Households who are selling crops,fruits,vegetables
HouseSys$PerCentHSellingCropsVegeFruits[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Export (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruit - Export'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "Export"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The local market (lower or equal to district level market) (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruit - The local market ( lower or equal to district level market)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "The local market ( lower or equal to district level market)"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The provincial or national market (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruit - The provincial or national market'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "The provincial or national market"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Do not know (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruit - Do not know'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "Do not know"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Other (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruit - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "Other"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - Less than 25% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruitRaw - <25%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "Less than 25%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 25-50% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruitRaw - 25-50%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "25-50%" | Dumm$b11 == "25%-50%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruitRaw - 50-75%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "50-75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingCropsVegeFruitRaw - Over 75%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "Over 75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)


#%Households who are selling livestock products
HouseSys$PerCentHSellingLivestock[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Export (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestock - Export'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "Export"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The local market (lower or equal to district level market) (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestock - The local market ( lower or equal to district level market)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "The local market ( lower or equal to district level market)"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The provincial or national market (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestock - The provincial or national market'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "The provincial or national market"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Do not know (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestock - Do not know'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "Do not know"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Other (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestock - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "Other"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - Less than 25% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestockRaw - <25%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "Less than 25%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 25-50% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestockRaw - 25-50%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "25-50%" | Dumm$b11 == "25%-50%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestockRaw - 50-75%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "50-75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseSys$'PerCentHSellingLivestockRaw - Over 75%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "Over 75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)

#%Household selling certified crops - Vegetables (% of total households)
HouseSys$'PerCentHSellingCertifiedCrops - Vegetables'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_11 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household selling certified crops - Rice (% of total households)
HouseSys$'PerCentHSellingCertifiedCrops - Rice'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_12 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household selling certified crops - Cassava (% of total households)
HouseSys$'PerCentHSellingCertifiedCrops - Cassava'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_13 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household selling certified crops - Cashew nut (% of total households)
HouseSys$'PerCentHSellingCertifiedCrops - Cashew nut'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_14 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household selling certified crops - Tea (% of total households)
HouseSys$'PerCentHSellingCertifiedCrops - Tea'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_15 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#%Household quality or control test on crops 
HouseSys$'PerCentHQualityTestCrops'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b25 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household premium prices on crops 
HouseSys$'PerCentHPremiumPricesCrops'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b26 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household communicating with buyers to getting their feedback
HouseSys$'PerCentHBuyersFeedback'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b28 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household participating in forum or fair
HouseSys$'PerCentHForumFair'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b29 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household selling product online/on social media
HouseSys$'PerCentHOnlineSelling'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$b30 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#%Household with members being active in one or several unions (% of all households) - Women Union
HouseSys$'PerCentHActiveMember - Women Union'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_1 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Youth Union
HouseSys$'PerCentHActiveMember - Youth Union'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_2 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Veteran group
HouseSys$'PerCentHActiveMember - Veteran group'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_3 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Farmer Union
HouseSys$'PerCentHActiveMember - Farmer Union'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_4 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Elderly group
HouseSys$'PerCentHActiveMember - Elderly group'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_5 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Religious group
HouseSys$'PerCentHActiveMember - Religious group'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_6 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Local government
HouseSys$'PerCentHActiveMember - Local government'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_7 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Agricultural cooperative
HouseSys$'PerCentHActiveMember - Agricultural cooperative'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_8 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Farmer association
HouseSys$'PerCentHActiveMember - Farmer association'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_9 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Community forestry
HouseSys$'PerCentHActiveMember - Community forestry'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_10 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Community Fisheries
HouseSys$'PerCentHActiveMember - Community Fisheries'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_11 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Farmer Water User Community
HouseSys$'PerCentHActiveMember - Farmer Water User Community'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_12 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Producer group / cluster
HouseSys$'PerCentHActiveMember - Producer group / cluster'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_13 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#%Household membership in farmers organization - Yes, one
HouseSys$'PerCentHFarmerOrganization - One'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c2 == "Yes, one"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household membership in farmers organization - Yes, more than one
HouseSys$'PerCentHFarmerOrganization - More than one'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Household membership in farmers organization - Farmer organization on Crop (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on Crop'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c31 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on fruits (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on fruits'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c32 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on livestock (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on livestock'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c33 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on honey (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on honey'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c34 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on water (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on water'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c35 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on forest (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on forest'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c36 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on market (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on market'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c37 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on credit (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on credit'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c38 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on any type of mutual help (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on any type of mutual help'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c39 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on diversified activities (% of households involved into farmer organization)
HouseSys$'PerCentHFarmerOrganization - Farmer organization on diversified activities'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$c310 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)

#%Household NTFP - Hunting
HouseSys$'PerCentH-Hunting'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d71 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for hunting
HouseSys$'NbDaysSpent-Hunting'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_11, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Hunting (% of concerned households)
HouseSys$'PerCentHSellingProducts-Hunting'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_12 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d71 == "1"], na.rm = T), digits = 3)
#Average income from Hunting
HouseSys$'AvIncome-Hunting'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_13, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Fishing
HouseSys$'PerCentH-Fishing'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d72 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for fishing
HouseSys$'NbDaysSpent-Fishing'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_21, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Fishing (% of concerned households)
HouseSys$'PerCentHSellingProducts-Fishing'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_22 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d72 == "1"], na.rm = T), digits = 3)
#Average income from Fishing
HouseSys$'AvIncome-Fishing'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_23, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Fuelwood
HouseSys$'PerCentH-Fuelwood'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d73 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for fuelwood
HouseSys$'NbDaysSpent-Fuelwood'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_31, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Fuelwood (% of concerned households)
HouseSys$'PerCentHSellingProducts-Fuelwood'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_32 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d73 == "1"], na.rm = T), digits = 3)
#Average income from Fuelwood
HouseSys$'AvIncome-Fuelwood'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_33, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Mushrooms
HouseSys$'PerCentH-Mushrooms'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d74 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for mushrooms
HouseSys$'NbDaysSpent-Mushrooms'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_41, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Mushrooms (% of concerned households)
HouseSys$'PerCentHSellingProducts-Mushrooms'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_42 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d74 == "1"], na.rm = T), digits = 3)
#Average income from Mushrooms
HouseSys$'AvIncome-Mushrooms'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_43, design = survey_design, na.rm = TRUE), digits = 2)


#%Household NTFP - Bamboo shoots
HouseSys$'PerCentH-BambooShoots'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d75 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Bamboo shoots
HouseSys$'NbDaysSpent-Bamboo shoots'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_51, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Bamboo shoots (% of concerned households)
HouseSys$'PerCentHSellingProducts-Bamboo shoots'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_52 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d75 == "1"], na.rm = T), digits = 3)
#Average income from Bamboo shoots
HouseSys$'AvIncome-Bamboo shoots'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_53, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Bamboo poles
HouseSys$'PerCentH-BambooPoles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d76 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Bamboo poles
HouseSys$'NbDaysSpent-Bamboo poles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_61, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Bamboo poles (% of concerned households)
HouseSys$'PerCentHSellingProducts-Bamboo poles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_62 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d76 == "1"], na.rm = T), digits = 3)
#Average income from Bamboo poles
HouseSys$'AvIncome-Bamboo poles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_63, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Broom Grass
HouseSys$'PerCentH-BroomGrass'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d77 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Broom Grass
HouseSys$'NbDaysSpent-Broom Grass'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_71, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Broom Grass (% of concerned households)
HouseSys$'PerCentHSellingProducts-Broom grass'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_72 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d77 == "1"], na.rm = T), digits = 3)
#Average income from Broom Grass
HouseSys$'AvIncome-Broom Grass'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_73, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Honey
HouseSys$'PerCentH-Honey'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d78 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Honey
HouseSys$'NbDaysSpent-Honey'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_81, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Honey (% of concerned households)
HouseSys$'PerCentHSellingProducts-Honey'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_82 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d78 == "1"], na.rm = T), digits = 3)
#Average income from Honey
HouseSys$'AvIncome-Honey'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_83, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Rattan
HouseSys$'PerCentH-Rattan'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d79 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Rattan
HouseSys$'NbDaysSpent-Rattan'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_91, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Rattan (% of concerned households)
HouseSys$'PerCentHSellingProducts-Rattan'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_92 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d79 == "1"], na.rm = T), digits = 3)
#Average income from Rattan
HouseSys$'AvIncome-Rattan'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_93, design = survey_design, na.rm = TRUE), digits = 2)


#%Household NTFP - Cardamom
HouseSys$'PerCentH-Cardamom'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d710 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Cardamom
HouseSys$'NbDaysSpent-Cardamom'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_101, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Cardamom (% of concerned households)
HouseSys$'PerCentHSellingProducts-Cardamom'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_102 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d710 == "1"], na.rm = T), digits = 3)
#Average income from Cardamom
HouseSys$'AvIncome-Cardamom'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_103, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Galangal
HouseSys$'PerCentH-Galangal'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d711 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Galangal
HouseSys$'NbDaysSpent-Galangal'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_111, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Galangal (% of concerned households)
HouseSys$'PerCentHSellingProducts-Galangal'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_112 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d711 == "1"], na.rm = T), digits = 3)
#Average income from Galangal
HouseSys$'AvIncome-Galangal'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_113, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Wild pepper
HouseSys$'PerCentH-WildPepper'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d713 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Wild pepper
HouseSys$'NbDaysSpent-Wild pepper'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_131, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Wild pepper (% of concerned households)
HouseSys$'PerCentHSellingProducts-Wild pepper'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_132 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d713 == "1"], na.rm = T), digits = 3)
#Average income from Wild pepper
HouseSys$'AvIncome-Wild pepper'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_133, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Medicinal plants
HouseSys$'PerCentH-Medicinal plants'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d714 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Medicinal plants
HouseSys$'NbDaysSpent-Medicinal plants'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_141, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Medicinal plants (% of concerned households)
HouseSys$'PerCentHSellingProducts-Medicinal plants'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_142 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d714 == "1"], na.rm = T), digits = 3)
#Average income from Medicinal plants
HouseSys$'AvIncome-Medicinal plants'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_143, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Wooden poles
HouseSys$'PerCentH-WoodenPoles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d716 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Wooden poles
HouseSys$'NbDaysSpent-Wooden poles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_161, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Wooden poles (% of concerned households)
HouseSys$'PerCentHSellingProducts-Wooden poles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_162 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d716 == "1"], na.rm = T), digits = 3)
#Average income from Wooden poles
HouseSys$'AvIncome-Wooden poles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_163, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Leave for thatch roof
HouseSys$'PerCentH-LeaveThatchRoof'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d718 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average nb of days spent for Leave for thatch roof
HouseSys$'NbDaysSpent-LeaveThatchRoof'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_181, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Leave for thatch roof (% of concerned households)
HouseSys$'PerCentHSellingProducts-LeaveThatchRoof'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_182 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d718 == "1"], na.rm = T), digits = 3)
#Average income from Leave for thatch roof
HouseSys$'AvIncome-LeaveThatchRoof'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d7_183, design = survey_design, na.rm = TRUE), digits = 2)

#%Households having rented-in plots
HouseSys$'PerCentHRentingPlots'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d9_1 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average area of rented-in plots
HouseSys$'RentedInPlot-AvArea'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d9_3, design = survey_design, na.rm = TRUE), digits = 2)
#%Households having rented-out plots
HouseSys$'PerCentHRentingOutPlots'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d10_1 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average area of rented-out plots
HouseSys$'RentedOutPlot-AvArea'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d10_3, design = survey_design, na.rm = TRUE), digits = 2)
#%Households having owned plots
HouseSys$'PerCentHOwningPlots'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#Average area of owned plots
HouseSys$'OwnedPlot-AvArea'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(svymean(~ d11_3, design = survey_design, na.rm = TRUE), digits = 2)
#%Households having property documents for owned plots - Title deed (% of households owning lands)
HouseSys$'PerCentHOwningPlots - Title deed'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_41 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Certificate of customary tenure (% of households owning lands)
HouseSys$'PerCentHOwningPlots - Certificate of customary tenure'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_42 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Certificate of occupancy (% of households owning lands)
HouseSys$'PerCentHOwningPlots - Certificate of occupancy'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_43 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Registered will or registered certificate of hereditary acquisition (% of households owning lands)
HouseSys$'PerCentHOwningPlots - Registered will or registered certificate of hereditary acquisition'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_44 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Other (% of households owning lands)
HouseSys$'PerCentHOwningPlots - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_45 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)


#%Households feeling their work/occupation is stressful
HouseSys$'PerCentHWorkStressful'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f1 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for family and social relationship - No time
HouseSys$'PerCentHTimeAvailSocial - No time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "No time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for family and social relationship - Very little time
HouseSys$'PerCentHTimeAvailSocial - Very little time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Very little time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for family and social relationship - Moderate amount of time
HouseSys$'PerCentHTimeAvailSocial - Moderate amount of time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Moderate amount of time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for family and social relationship - Almost enough time
HouseSys$'PerCentHTimeAvailSocial - Almost enough time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Almost enough time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for family and social relationship - Sufficient amount of time
HouseSys$'PerCentHTimeAvailSocial - Sufficient amount of time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Sufficient amount of time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - No time
HouseSys$'PerCentHTimeAvailKnowledge - No time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "No time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Very little time
HouseSys$'PerCentHTimeAvailKnowledge - Very little time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Very little time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Moderate amount of time
HouseSys$'PerCentHTimeAvailKnowledge - Moderate amount of time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Moderate amount of time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Almost enough time
HouseSys$'PerCentHTimeAvailKnowledge - Almost enough time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Almost enough time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Sufficient amount of time
HouseSys$'PerCentHTimeAvailKnowledge - Sufficient amount of time'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Sufficient amount of time"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#%Households where XX makes the decision on what and how to produce - Myself alone
HouseSys$'PerCentHDecisionProduction - Myself alone'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "Myself alone"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on what and how to produce - Me in consultation with spouse/other family members
HouseSys$'PerCentHDecisionProduction - Me in consultation with spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on what and how to produce - My spouse/other family members
HouseSys$'PerCentHDecisionProduction - My spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "My spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on what and how to produce - Do not know
HouseSys$'PerCentHDecisionProduction - Do not know'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "Do not know"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - Myself alone
HouseSys$'PerCentHDecisionAssets - Myself alone'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "Myself alone"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - Me in consultation with spouse/other family members
HouseSys$'PerCentHDecisionAssets - Me in consultation with spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - My spouse/other family members
HouseSys$'PerCentHDecisionAssets - My spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "My spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - Do not know
HouseSys$'PerCentHDecisionAssets - Do not know'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "Do not know"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - Myself alone
HouseSys$'PerCentHDecisionBorrowingLending - Myself alone'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "Myself alone"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - Me in consultation with spouse/other family members
HouseSys$'PerCentHDecisionBorrowingLending - Me in consultation with spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - My spouse/other family members
HouseSys$'PerCentHDecisionBorrowingLending - My spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "My spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - Do not know
HouseSys$'PerCentHDecisionBorrowingLending - Do not know'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "Do not know"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on  how the household income is used - Myself alone
HouseSys$'PerCentHDecisionUsingIncome - Myself alone'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "Myself alone"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on  how the household income is used - Me in consultation with spouse/other family members
HouseSys$'PerCentHDecisionUsingIncome - Me in consultation with spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on  how the household income is used - My spouse/other family members
HouseSys$'PerCentHDecisionUsingIncome - My spouse/other family members'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "My spouse/other family members"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households where XX makes the decision on  how the household income is used - Do not know
HouseSys$'PerCentHDecisionUsingIncome - Do not know'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "Do not know"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)

#%Households thinking working hours are evenly distributed across family members
HouseSys$'PerCentHWorkingHoursDistributionPerception'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g5 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households thinking they face challenges in their daylife?
HouseSys$'PerCentHDaylifeChallenge'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$g6 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
          
#%Households wanting their children to be farmers too - Yes, strongly
HouseSys$'PerCentHWantingChildrenAsFarmers - Yes, strongly'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "Yes, strongly"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households wanting their children to be farmers too - Yes, maybe
HouseSys$'PerCentHWantingChildrenAsFarmers - Yes, maybe'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "Yes, maybe"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households wanting their children to be farmers too - They should emigrate if they had the chance
HouseSys$'PerCentHWantingChildrenAsFarmers - They should emigrate if they had the chance'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "They should emigrate if they had the chance"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households wanting their children to be farmers too - No, agriculture is not a good job
HouseSys$'PerCentHWantingChildrenAsFarmers - No, agriculture is not a good job'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "No, agriculture is not a good job"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households wanting their children to be farmers too - Do not know
HouseSys$'PerCentHWantingChildrenAsFarmers - Do not know'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "Do not know"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having someone in the family to takeover the farm if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHFamilyMemberTakeoverFarm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h3 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$h3 != ""], na.rm = T), digits = 3)
#%Households having someone outside the family to takeover the farm if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHOutsidePeopleTakeoverFarm'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h4 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$h4 != ""], na.rm = T), digits = 3)

#%Households aware of ecological agriculture concept - Other farmers/farmer group/cooperative if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHAwareEcologicalAgriculture - Other farmers/farmer group/cooperative'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_11 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_11)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Technical advisors if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHAwareEcologicalAgriculture - Technical advisors'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_12 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_12)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Researchers if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHAwareEcologicalAgriculture - Researchers'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_13 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_13)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Buyers if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHAwareEcologicalAgriculture - Buyers'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_14 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_14)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Other if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHAwareEcologicalAgriculture - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_199 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_199)], na.rm = T), digits = 3)
#%Households applying ecological agriculture concept if HH > 50 (% of households for which HH is older than 50)
HouseSys$'PerCentHApplyingEcologicalAgriculture'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$h5 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h5)], na.rm = T), digits = 3)


#%Households proportion of the food consumed from own farm or homegarden - Less than 25%
HouseSys$'PerCentHSelfConsumption - Less than 25%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "Less than 25%"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households proportion of the food consumed from own farm or homegarden - 25-50%
HouseSys$'PerCentHSelfConsumption - 25-50%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "25-50%"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households proportion of the food consumed from own farm or homegarden - 50-75%
HouseSys$'PerCentHSelfConsumption - 50-75%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "50-75%"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households proportion of the food consumed from own farm or homegarden - Over 75%
HouseSys$'PerCentHSelfConsumption - Over 75%'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "Over 75%"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year
HouseSys$'PerCentHLackFood'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - January (% of total households)
HouseSys$'PerCentHLackFood - January'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i31 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - February (% of total households)
HouseSys$'PerCentHLackFood - February'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i32 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - March (% of total households)
HouseSys$'PerCentHLackFood - March'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i33 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - April (% of total households)
HouseSys$'PerCentHLackFood - April'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i34 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - May (% of total households)
HouseSys$'PerCentHLackFood - May'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i35 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - June (% of total households)
HouseSys$'PerCentHLackFood - June'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i36 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - July (% of total households)
HouseSys$'PerCentHLackFood - July'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i37 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - August (% of total households)
HouseSys$'PerCentHLackFood - August'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i38 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - September (% of total households)
HouseSys$'PerCentHLackFood - September'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i39 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - October (% of total households)
HouseSys$'PerCentHLackFood - October'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i310 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - November (%of total households)
HouseSys$'PerCentHLackFood - November'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i311 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year - December (% of total households)
HouseSys$'PerCentHLackFood - December'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i312 == "1"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having lack of food during past year, reason - Climate (drought, floods) (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - Climate (drought, floods)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i41 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Pest damages (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - Pest damages'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i42 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Animal disease (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - Animal disease'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i43 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - No buyers for your produce (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - No buyers for your produce'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i44 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Declining selling prices for your produce (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - Declining selling prices for your produce'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i45 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Need to reimburse credits (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - Need to reimburse credits'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i46 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Increasing prices of food (rice…) (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - Increasing prices of food (rice…)'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i47 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Other (%of households facing lack of food)
HouseSys$'PerCentHLackFoodReason - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i499 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food for each event frequency  (%of households facing lack of food)
HouseSys$'PerCentHLackFood - Happens every year or most years'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i5 == "Happens every year or most years"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food for each event frequency - Happens sometimes but not regularly (%of households facing lack of food)
HouseSys$'PerCentHLackFood - Happens sometimes but not regularly'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i5 == "Happens sometimes but not regularly"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food for each event frequency - It was exceptional (%of households facing lack of food)
HouseSys$'PerCentHLackFood - It was exceptional'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$i5 == "It was exceptional"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)

#%Households House tenure - Owned
HouseSys$'PerCentHHouse - Owned'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j1 == "Owned"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households House tenure - Share cropping
HouseSys$'PerCentHHouse - Share cropping'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j1 == "Share cropping"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Wall material of the house - Brick wall
HouseSys$'PerCentHWallMaterial - Brick wall'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Brick wall"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Wall material of the house - Concrete wall
HouseSys$'PerCentHWallMaterial - Concrete wall'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Concrete wall"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Wall material of the house - Wooden wall
HouseSys$'PerCentHWallMaterial - Wooden wall'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Wooden wall"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Wall material of the house - Bamboo, Thatch/leaves, Grass
HouseSys$'PerCentHWallMaterial - Bamboo, Thatch/leaves, Grass'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Bamboo, Thatch/leaves, Grass"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Wall material of the house - Galvanized iron or aluminium or other metal sheets
HouseSys$'PerCentHWallMaterial - Galvanized iron or aluminium or other metal sheets'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Galvanized iron or aluminium or other metal sheets"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Wall material of the house - Wood or logs
HouseSys$'PerCentHWallMaterial - Wood or logs'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Wood or logs"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Wall material of the house - Other
HouseSys$'PerCentHWallMaterial - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Other"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Brick tile roof
HouseSys$'PerCentHRoofMaterial - Brick tile roof'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Brick tile roof"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Concrete, cement
HouseSys$'PerCentHRoofMaterial - Concrete, cement'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Concrete, cement"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Fibrous cement
HouseSys$'PerCentHRoofMaterial - Fibrous cement'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Fibrous cement"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Metal/ tin roof
HouseSys$'PerCentHRoofMaterial - Metal/ tin roof'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Metal/ tin roof"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Other
HouseSys$'PerCentHRoofMaterial - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Other"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Stone tile roof
HouseSys$'PerCentHRoofMaterial - Stone tile roof'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Stone tile roof"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Thatch/leaves/grass
HouseSys$'PerCentHRoofMaterial - Thatch/leaves/grass'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Thatch/leaves/grass"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households Roof material of the house - Wood
HouseSys$'PerCentHRoofMaterial - Wood'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Wood"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3) 
#%Households having a latrine
HouseSys$'PerCentHLatrine'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j4 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households having a latrine shared with another household
HouseSys$'PerCentHLatrineShared'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j5 == "Yes"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3) 
#%Households source of water used in the family - Drill
HouseSys$'PerCentHWaterSource - Drill'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Drill"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households source of water used in the family - Natural stream
HouseSys$'PerCentHWaterSource - Natural stream'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Natural stream"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households source of water used in the family - Other Private tap water
HouseSys$'PerCentHWaterSource - Other Private tap water'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Other Private tap water"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households source of water used in the family - Public water
HouseSys$'PerCentHWaterSource - Public water'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Public water"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)             
#%Households source of water used in the family - Rain water
HouseSys$'PerCentHWaterSource - Rain water'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Rain water"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)             
#%Households source of water used in the family - Water delivery
HouseSys$'PerCentHWaterSource - Water delivery'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Water delivery"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)    
#%Households source of water used in the family - Well
HouseSys$'PerCentHWaterSource - Well'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Well"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)    
#%Households source of electricity used in the family - Battery
HouseSys$'PerCentHElectricitySource - Battery'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Battery"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)    
#%Households source of electricity used in the family - Grid electricity
HouseSys$'PerCentHElectricitySource - Grid electricity'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Grid electricity"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)    
#%Households source of electricity used in the family - None, using kerosene/ candles
HouseSys$'PerCentHElectricitySource - None, using kerosene/ candles'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "None, using kerosene/ candles"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households source of electricity used in the family - Other
HouseSys$'PerCentHElectricitySource - Other'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Other"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)   
#%Households source of electricity used in the family - Private electricity
HouseSys$'PerCentHElectricitySource - Private electricity'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Private electricity"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)
#%Households source of electricity used in the family - Public electricity
HouseSys$'PerCentHElectricitySource - Public electricity'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Public electricity"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)   
#%Households source of electricity used in the family - Solar panel
HouseSys$'PerCentHElectricitySource - Solar panel'[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Solar panel"], na.rm = T) /
          HouseSys$N[HouseSys$Country == HouseSys$Country[i] & HouseSys$Code == HouseSys$Code[i] ], digits = 3)   
 
}


#We remove NaN
for (i in 1:ncol(HouseSys)){
  HouseSys[is.nan(HouseSys[,i]),i] <- 0
}
#We'll converthired workers values into $
#We use currencies values for April 2023:
#Cambodia: Already dollar value obviously
#Laos: 1$ = 17110 kips
#Vietnam: 1$ = 23452 dongs
HouseSys$ValueHiredWorkersPerYear <- ifelse(HouseSys$Country == "Cambodia",
                                             HouseSys$ValueHiredWorkersPerYear,
                                             ifelse(HouseSys$Country == "Lao",
                                             round(HouseSys$ValueHiredWorkersPerYear / 17110, digits = 2),
                                             round(HouseSys$ValueHiredWorkersPerYear / 23452, digits = 2)))
#Same for income from NTFP collection
#Cambodia: #Cambodia: 1$ = 4075 riels
for (i in 0:14){
  HouseSys[,710+i*4]  <- ifelse(HouseSys$Country == "Cambodia",
                                 round(HouseSys[,710+i*4] / 4075, digits = 2),
                                 ifelse(HouseSys$Country == "Lao",
                                        round(HouseSys[,710+i*4] / 17110, digits = 2),
                                        round(HouseSys[,710+i*4] / 23452, digits = 2)))
}
                                              

#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(HouseSys, "HouseSys.rds")

#Pense-bête
#Crops: 7 -> 9889 OK
#Reasons AEP: 9890 -> 9948 OK
#Additionnal info Practices: 9949 -> 9960 OK
#HHmembers: 9961 -> 10680 
#Labor general: 10681 -> 10690, 10735
#Labor Tasks: 10691 -> 10734,
#Services: 10736 -> 10747
#Equipments: 10748 -> 10766
#Animals specific: 10767 -> 11229
#Information at household scale: 11230 -> 11540


