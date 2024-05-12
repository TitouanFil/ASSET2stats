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
#We create a table first
HouseDesc <- data.frame(Column1 = Sarealist,
                        Column2 = c(sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Cambodia"]),
                                                         sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Lao"]),
                                                         sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Dien Bien province"]),
                                                         sum(CropSystems3C_TF$SW_Weight[CropSystems3C_TF$S_Area == "Son La province"])))
HouseDesc$Column2 <- round(HouseDesc$Column2, digits = 0)
colnames(HouseDesc) <- c("Sarea","N")

i = "Lao"
#Loop to fulfil the table
for (i in Sarealist){
  Dumm <- CropSystems3C_TF[CropSystems3C_TF$S_Area == i,]
  #Loop for reasons for adopting AE practices
  for (k in 1:ncol(AEP)){
    Dum <- paste0(AEP[1,k],"-",AEP[2,k],"-PerCentHouseholds")
    if (!(Dum %in% names(HouseDesc))) {
      HouseDesc <- add_column(HouseDesc, !!Dum := NA_real_)
    }
    HouseDesc[[Dum]] <- 
      ifelse(HouseDesc$Sarea == i,round(sum(Dumm$SW_Weight[!is.na(Dumm[[AEP[3,k]]]) & Dumm[[AEP[3,k]]] != ""], na.rm = T) /
                                        sum(Dumm$SW_Weight),
                                        digits = 3), HouseDesc[[Dum]])
    for (j in c("To save money","To save labor/time","To improve yields",
                "To improve soil/plant health","Other")){
      #Loop for different AEP practices with associated categories
      Dum <- paste0(AEP[1,k],"-",AEP[2,k],"-",j)
      if (!(Dum %in% names(HouseDesc))) {
        HouseDesc <- add_column(HouseDesc, !!Dum := NA_real_)
      }
      HouseDesc[[Dum]] <- 
        ifelse(HouseDesc$Sarea == i,round(sum(Dumm$SW_Weight[Dumm[[AEP[3,k]]] == j], na.rm = T) /
                                          sum(Dumm$SW_Weight[!is.na(Dumm[[AEP[3,k]]]) & Dumm[[AEP[3,k]]] != ""], na.rm = T),
                                          digits = 3), HouseDesc[[Dum]])
      }
  }
  #Isolated information about practices
  #Soil conservation practice
  #%households mentionning a reason for not implementing AE soil conservation practice
  HouseDesc$`PerCentH - ReasonForNotImplAEPSC`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Reason for not implementing AE soil conservation practice - "I do not know them"
  HouseDesc$`ReasonForNotImplAEPSC - I do not know them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "I do not know them"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil conservation practice - "They are too costly"
  HouseDesc$`ReasonForNotImplAEPSC - They are too costly`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil conservation practice - "I have no time to implement them"
  HouseDesc$`ReasonForNotImplAEPSC - I have no time to implement them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil conservation practice - "I don't want to do things differently from my neighbors"
  HouseDesc$`ReasonForNotImplAEPSC - I don't want to do things differently from my neighbors`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d16 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d16) & Dumm$d16 != ""], na.rm = T), digits = 3)
  #Soil fertility practice
  #%households mentionning a reason for not implementing AE soil fertility practice
  HouseDesc$`PerCentH - ReasonForNotImplAEPSF`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Reason for not implementing AE soil fertility practice - "I do not know them"
  HouseDesc$`ReasonForNotImplAEPSF - I do not know them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "I do not know them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil fertility practice - "They are too costly"
  HouseDesc$`ReasonForNotImplAEPSF - They are too costly`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil fertility practice - "I have no time to implement them"
  HouseDesc$`ReasonForNotImplAEPSF - I have no time to implement them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE soil fertility practice - "I don't want to do things differently from my neighbors"
  HouseDesc$`ReasonForNotImplAEPSF - I don't want to do things differently from my neighbors`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d19 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d19) & Dumm$d19 != ""], na.rm = T), digits = 3)
  #Weed management practice
  #%households mentionning a reason for not implementing AE weed management practice
  HouseDesc$`PerCentH - ReasonForNotImplAEPWM`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Reason for not implementing AE weed management practice - "I do not know them"
  HouseDesc$`ReasonForNotImplAEPWM - I do not know them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "I do not know them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "They are too costly"
  HouseDesc$`ReasonForNotImplAEPWM - They are too costly`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "I have no time to implement them"
  HouseDesc$`ReasonForNotImplAEPWM - I have no time to implement them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "I don't want to do things differently from my neighbors"
  HouseDesc$`ReasonForNotImplAEPWM - I don't want to do things differently from my neighbors`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d22 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d22) & Dumm$d22 != ""], na.rm = T), digits = 3)
  #Other method used for weed management
  #%households mentioning another method for AE weed management practice
  HouseDesc$`PerCentH - WMOtherMethod`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Other method used for weed management - "Synthetic herbicide without mechanical weeding"
  HouseDesc$`OtherPracticeWM - Synthetic herbicide without mechanical weeding`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d23 == "Synthetic herbicide without mechanical weeding"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "Frequent mechanical weeding (more than three times per year) without synthetic herbicide"
  HouseDesc$`OtherPracticeWM - Frequent mechanical weeding (more than three times per year) without synthetic herbicide`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d23 == "Frequent mechanical weeding (more than three times per year) without synthetic herbicide"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE weed management practice - "Mixed management using herbicide and mechanical weeding"
  HouseDesc$`OtherPracticeWM - Mixed management using herbicide and mechanical weeding`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d23 == "Mixed management using herbicide and mechanical weeding"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d23) & Dumm$d23 != ""], na.rm = T), digits = 3)
 #Pest&DiseaseManagement
  #NbHouseholdsConcerned
  HouseDesc$`PestDisease - PerCentHouseholdsProblemCropPest`[HouseDesc$Sarea == i] <- 
    round(sum(Dumm$SW_Weight[Dumm$d24 == "Yes"], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #%households mentionning a reason for not implementing AE pest&disease management practice
  HouseDesc$`PerCentH - ReasonForNotImplAEPPDM`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Reason for not implementing AE pest&disease management practice - "I do not know them"
  HouseDesc$`ReasonForNotImplAEPPDM - I do not know them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "I do not know them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE pest&disease management practice - "They are too costly"
  HouseDesc$`ReasonForNotImplAEPPDM - They are too costly`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "They are too costly"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE pest&disease management practice - "I have no time to implement them"
  HouseDesc$`ReasonForNotImplAEPPDM - I have no time to implement them`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "I have no time to implement them"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Reason for not implementing AE pest&disease management practice - "I don't want to do things differently from my neighbors"
  HouseDesc$`ReasonForNotImplAEPPDM - I don't want to do things differently from my neighbors`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d28 == "I don't want to do things differently from my neighbors"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d28) & Dumm$d28 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management
  #%households mentioning another method for AE pest and disease management practice
  HouseDesc$`PerCentH - PDMOtherMethod`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Other method used for pest and disease management - "Synthetic insecticide and fungicide are used regularly and no other system is used"
  HouseDesc$`OtherPracticePDM - Synthetic insecticide and fungicide are used regularly and no other system is used`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Synthetic insecticide and fungicide are used regularly and no other system is used"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management - "Mixed use of synthetic and biological/natural pesticides"
  HouseDesc$`OtherPracticePDM - Mixed use of synthetic and biological/natural pesticides`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Mixed use of synthetic and biological/natural pesticides"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management - "Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used"
  HouseDesc$`OtherPracticePDM - Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Other method used for pest and disease management - "Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide
  HouseDesc$`OtherPracticePDM - Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d29 == "Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide"], na.rm = T) /
            sum(Dumm$SW_Weight[!is.na(Dumm$d29) & Dumm$d29 != ""], na.rm = T), digits = 3)
  #Provenance of the seeds
  #%households who conserve and use own seeds
  HouseDesc$`PerCentH - SeedProvenance`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[!is.na(Dumm[,9959]) & Dumm[,9959] != ""], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Seed source 1: From the village seller
  HouseDesc$`SeedProvenance - From the village seller`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From the village seller"], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Seed source 2: From the cooperative
  HouseDesc$`SeedProvenance - From the cooperative`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From the cooperative"], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Seed source 3: From a trader in town
  HouseDesc$`SeedProvenance - From a trader in town`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From a trader in town"], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Seed source 4: From family and friends
  HouseDesc$`SeedProvenance - From family & friends`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "From family & friends"], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  #Seed source 5: Other
  HouseDesc$`SeedProvenance - Other`[HouseDesc$Sarea == i] <-
    round(sum(Dumm$SW_Weight[Dumm$d32_2 == "Other"], na.rm = T) /
            HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
  
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
  HouseDesc$NbHouseholdMembers[HouseDesc$Sarea == i] <-
    round(svymean(~ NbhMembers, design = survey_design, na.rm = TRUE), digits = 2)
  #Nb of active household members (depending on main occupation)
  HouseDesc$NbActiveMembers[HouseDesc$Sarea == i] <-
    round(svymean(~ NbhActiveMembers, design = survey_design, na.rm = TRUE), digits = 2)
  #Nb of household members working on the farm (main activity)
  HouseDesc$NbFarmFirstOcc[HouseDesc$Sarea == i] <-
    round(svymean(~ NbhFarmMembersPrim, design = survey_design, na.rm = TRUE), digits = 2)
  #Nb of households members working on the farm (secondary activity)
  HouseDesc$NbFarmSecOcc[HouseDesc$Sarea == i] <-
    round(svymean(~ NbhFarmMembersSec, design = survey_design, na.rm = TRUE), digits = 2)
  #Age of the household head
  HouseDesc$AgeHH[HouseDesc$Sarea == i] <-
    round(svymean(~ AgeHH, design = survey_design, na.rm = TRUE), digits = 0)
  #Nb of people migrating in the household
  HouseDesc$NbMigratingMembers[HouseDesc$Sarea == i] <-
    round(svymean(~ NbMigratingMembers, design = survey_design, na.rm = TRUE), digits = 2)
  #Average nb of months in migration
  HouseDesc$YearlyRatemigrationperMigrant[HouseDesc$Sarea == i] <-
    round(svymean(~ YearlyRatemigrationperMigrant, design = survey_design, na.rm = TRUE), digits = 2)

  #INFORMATION ABOUT HOUSEHOLD COLLABORATION
  #% of households collaborating for: Share labor (mutual help, working together on each other farm)
HouseDesc$`Collab - Share labor (mutual help, working together on each other farm)`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c131 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c131) & Dumm$c131 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Manage water/irrigation systems
HouseDesc$`Collab - Manage water/irrigation systems`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c132 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c132) & Dumm$c132 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Raise livestock
HouseDesc$`Collab - Raise livestock`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c133 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c133) & Dumm$c133 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Buy agricultural inputs
HouseDesc$`Collab - Buy agricultural inputs`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c134 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c134) & Dumm$c134 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Selling products to the markets for other farmers
HouseDesc$`Collab - Selling products to the markets for other farmers`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c135 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c135) & Dumm$c135 != ""], na.rm = T), digits = 3)
#% of households collaborating for: Experiment new farming practices
HouseDesc$`Collab - Experiment new farming practices`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c136 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c136) & Dumm$c136 != ""], na.rm = T), digits = 3)
#% of households collaborating for: No collaboration with other people on these issues
HouseDesc$`Collab - No collaboration with other people on these issues`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c130 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$c130) & Dumm$c130 != ""], na.rm = T), digits = 3)
#% of households with hired workers
HouseDesc$SHareHouseholdsHiringWorkers[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d35 > 0], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average cost of workers hired per year
HouseDesc$`ValueHiredWorkersPerYear`[HouseDesc$Sarea == i] <-
  round(svymean(~ d35, design = survey_design, na.rm = TRUE), digits = 2)

#INFORMATION ABOUT LABOR
#Land preparation - Do not have
HouseDesc$`Laborkind-Land preparation-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_10 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Land preparation - Family members
HouseDesc$`Laborkind-Land preparation-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_11 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Land preparation - Hired people
HouseDesc$`Laborkind-Land preparation-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_12 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Land preparation - Mutual help
HouseDesc$`Laborkind-Land preparation-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_13 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Sowing - Do not have
HouseDesc$`Laborkind-Sowing-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_20 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Sowing - Family members
HouseDesc$`Laborkind-Sowing-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_21 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Sowing - Hired people
HouseDesc$`Laborkind-Sowing-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_22 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Sowing - Mutual help
HouseDesc$`Laborkind-Sowing-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_23 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Fertilization - Do not have
HouseDesc$`Laborkind-Fertilization-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_30 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Fertilization - Family members
HouseDesc$`Laborkind-Fertilization-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_31 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Fertilization - Hired people
HouseDesc$`Laborkind-Fertilization-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_32 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Fertilization - Mutual help
HouseDesc$`Laborkind-Fertilization-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_33 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Weed management - Do not have
HouseDesc$`Laborkind-Weed management-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_40 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Weed management - Family members
HouseDesc$`Laborkind-Weed management-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_41 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Weed management - Hired people
HouseDesc$`Laborkind-Weed management-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_42 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Weed management - Mutual help
HouseDesc$`Laborkind-Weed management-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_43 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pest and disease management - Do not have
HouseDesc$`Laborkind-Pest and disease management-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_50 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pest and disease management - Family members
HouseDesc$`Laborkind-Pest and disease management-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_51 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pest and disease management - Hired people
HouseDesc$`Laborkind-Pest and disease management-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_52 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pest and disease management - Mutual help
HouseDesc$`Laborkind-Pest and disease management-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_53 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Crop stimulation/ growth regulation - Do not have
HouseDesc$`Laborkind-Crop stimulation/ growth regulation-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_60 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Crop stimulation/ growth regulation - Family members
HouseDesc$`Laborkind-Crop stimulation/ growth regulation-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_61 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Crop stimulation/ growth regulation - Hired people
HouseDesc$`Laborkind-Crop stimulation/ growth regulation-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_62 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Crop stimulation/ growth regulation - Mutual help
HouseDesc$`Laborkind-Crop stimulation/ growth regulation-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_63 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pruning - Do not have
HouseDesc$`Laborkind-Pruning-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_70 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pruning - Family members
HouseDesc$`Laborkind-Pruning-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_71 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pruning - Hired people
HouseDesc$`Laborkind-Pruning-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_72 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pruning - Mutual help
HouseDesc$`Laborkind-Pruning-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_73 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Water/irrigation management - Do not have
HouseDesc$`Laborkind-Water/irrigation management-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_80 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Water/irrigation management - Family members
HouseDesc$`Laborkind-Water/irrigation management-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_81 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Water/irrigation management - Hired people
HouseDesc$`Laborkind-Water/irrigation management-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_82 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Water/irrigation management - Mutual help
HouseDesc$`Laborkind-Water/irrigation management-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_83 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Harvest - Do not have
HouseDesc$`Laborkind-Harvest-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_90 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Harvest - Family members
HouseDesc$`Laborkind-Harvest-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_91 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Harvest - Hired people
HouseDesc$`Laborkind-Harvest-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_92 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Harvest - Mutual help
HouseDesc$`Laborkind-Harvest-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_93 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Transportation - Do not have
HouseDesc$`Laborkind-Transportation-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_100 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Transportation - Family members
HouseDesc$`Laborkind-Transportation-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_101 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Transportation - Hired people
HouseDesc$`Laborkind-Transportation-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_102 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Transportation - Mutual help
HouseDesc$`Laborkind-Transportation-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_103 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Post-harvest processing - Do not have
HouseDesc$`Laborkind-Post-harvest processing-Do not have`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_110 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Post-harvest processing - Family members
HouseDesc$`Laborkind-Post-harvest processing-Family members`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_111 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Post-harvest processing - Hired people
HouseDesc$`Laborkind-Post-harvest processing-Hired people`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_112 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Post-harvest processing - Mutual help
HouseDesc$`Laborkind-Post-harvest processing-Mutual help`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d33_113 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#EXTERNAL-MECHANIZED SERVICE USED by hh
#Land preparation
HouseDesc$`ExtMechaServices-Land preparation`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d361 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Sowing
HouseDesc$`ExtMechaServices-Sowing`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d362 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Fertilization
HouseDesc$`ExtMechaServices-Fertilization`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d363 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Weed management
HouseDesc$`ExtMechaServices-Weed management`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d364 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pest and disease management
HouseDesc$`ExtMechaServices-Pest and disease management`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d365 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Pruning
HouseDesc$`ExtMechaServices-Pruning`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d366 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Water/irrigation management
HouseDesc$`ExtMechaServices-Water/irrigation management`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d367 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Harvest
HouseDesc$`ExtMechaServices-Harvest`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d368 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Transportation
HouseDesc$`ExtMechaServices-Transportation`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d369 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Post-harvest processing
HouseDesc$`ExtMechaServices-Post-harvest processing`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d3610 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#None
HouseDesc$`ExtMechaServices-None of above`[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d360 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#EQUIPMENTS
#Motorcycle
HouseDesc$NbMotorcycle[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_1, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Wheel hand tractor
HouseDesc$NbWheelHandTractor[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_2, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Wheel tractor
HouseDesc$NbWheelTractor[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_3, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Car
HouseDesc$NbCar[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_4, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Truck
HouseDesc$NbTruck[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_5, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Combine harvester
HouseDesc$NbCombineHarvester[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_6, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Land leveler
HouseDesc$NbLandLeveler[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_7, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Rice planter
HouseDesc$NbRicePlanter[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_8, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Maize planter
HouseDesc$NbMaizePlanter[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_9, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Cassava disc ridging tool
HouseDesc$NbCassavaDiscRidgingTool[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_10, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Cassava harvesting tool
HouseDesc$NbCassavaHarvestingTool[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_11, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Rice thresher
HouseDesc$NbRiceThresher[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_12, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Rice mill
HouseDesc$NbRiceMill[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_13, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Backpack sprayer
HouseDesc$NbBackpackSprayer[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_14, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Motorpump sprayer
HouseDesc$NbMotorPumpSprayer[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_15, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Grass cutter
HouseDesc$NbGrassCutter[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_16, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Grass chopping machine
HouseDesc$NbGrassChoppingMachine[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_17, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Water pump
HouseDesc$NbWaterPump[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_18, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Irrigation system
HouseDesc$NbIrrigationSystem[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight*Dumm$k2_19, na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#ADDITIONNAL INFORMATION ABOUT FISHES
#% of households who are raising fishes
HouseDesc$PerCentHRaisingFish[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e8 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average size of the fish pond (m2)
HouseDesc$AvSizeFishPond[HouseDesc$Sarea == i] <-
  round(svymean(~ e9, design = survey_design, na.rm = TRUE), digits = 2)
#Average nb of fish breeds raised
HouseDesc$NbFishBreed[HouseDesc$Sarea == i] <-
  round(svymean(~ e10, design = survey_design, na.rm = TRUE), digits = 2)

#ADDITIONNAL INFORMATION ABOUT ANIMALS
#% of households who raise animals (% of households with animals)
HouseDesc$PerCentHAnimals[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#% of households who are growing forage (% of households with animals)
HouseDesc$PerCentHGrowingForage[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e12_1 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average size of forage fields (m2) (% of households with animals)
HouseDesc$AvSizeForage[HouseDesc$Sarea == i] <-
  round(svymean(~ e12_2, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who have natural pasture land (% of households with animals)
HouseDesc$PerCentHNaturePasture[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e12_3 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average size of natural pastureland (m2) (% of households with animals)
HouseDesc$AvSizeNaturePasture[HouseDesc$Sarea == i] <-
  round(svymean(~ e12_4, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who have improved pasture land (% of households with animals)
HouseDesc$PerCentHImprovedPasture[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e12_5 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average size of improved pasture land (m2) (% of households with animals)
HouseDesc$AvSizeImprovedPasture[HouseDesc$Sarea == i] <-
  round(svymean(~ e12_6, design = survey_design, na.rm = TRUE), digits = 2)
#% of household who fertilize the forage or improved pasture (% of households with animals)
HouseDesc$PerCentHFertilizeForage[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e13 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of household who fertilize the forage or improved pasture - Synthetic fertilizer (% of households with animals)
HouseDesc$'PerCentHFertForage - Synthetic fertilizer'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e13_1 == "Synthetic fertilizer"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of household who fertilize the forage or improved pasture - Organic manure (% of households with animals)
HouseDesc$'PerCentHFertForage - Organic manure'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e13_1 == "Organic manure"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of household who fertilize the forage or improved pasture - Both above (% of households with animals)
HouseDesc$'PerCentHFertForage - Both above'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e13_1 == "Both above"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who use grass-legume mix for improved pasture/forage (% of households with animals)
HouseDesc$PerCentHGrassLegumePasture[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e14 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who use a public/collective pastureland (% of households with animals)
HouseDesc$PerCentHGrassLegumePasture[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e15 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - Total (at any part of the year - % of households with animals)
HouseDesc$PerCentHRibsBonesAnimals[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average nb of month during which rib/bones are seen (including only households with this issue)
HouseDesc$AvNbMonthRibsBonesAnimals[HouseDesc$Sarea == i] <-
  round(svymean(~ MonthRibBones, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who see rib/bones of the animals - January (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - January'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_11 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - February (% of households with animals))
HouseDesc$'PerCentHRibsBonesAnimals - February'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_12 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - March (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - March'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_13 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - April (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - April'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_14 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - May (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - May'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_15 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - June (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - June'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_16 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - July (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - July'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_17 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - August (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - August'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_18 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - September (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - September'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_19 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - October (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - October'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_110 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - November (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - November'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_111 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who see rib/bones of the animals - December (% of households with animals)
HouseDesc$'PerCentHRibsBonesAnimals - December'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e58_112 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)

#% of households who declared a lack of feed for the animals - Total (at any part of the year - % of households with animals)
HouseDesc$PerCentHLackFeedAnimals[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#Average nb of month during which there is a lack of feed for animals (including only households with this issue)
HouseDesc$AvNbMonthLackFeedAnimals[HouseDesc$Sarea == i] <-
  round(svymean(~ MonthLackFeed, design = survey_design, na.rm = TRUE), digits = 2)
#% of households who declared a lack of feed for the animals - January (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - January'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_11 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - February (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - February'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_12 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - March (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - March'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_13 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - April (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - April'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_14 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - May (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - May'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_15 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - June (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - June'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_16 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - July (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - July'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_17 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - August (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - August'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_18 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - September (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - September'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_19 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - October (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - October'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_110 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - November (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - November'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_111 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households who declared a lack of feed for the animals - December (% of households with animals)
HouseDesc$'PerCentHLackFeedAnimals - December'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e59_112 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)
#% of households for which animals have access to water (% of households with animals)
HouseDesc$PerCentHWaterAccessAnimals[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$e60 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[rowSums(!is.na(Dumm[,11042:11058]) & Dumm[,11042:11058] != 0) > 0], na.rm = T), digits = 3)

#SOURCES OF INCOME FOR THE HOUSEHOLD
#Main source of household income - Financial support / gift
HouseDesc$'MainIncomeSource - Financial support / gift'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Financial support / gift"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Non-farm income (own business: shop, trader/collector etc.) 
HouseDesc$'MainIncomeSource - Non-farm income (own business: shop, trader/collector etc.)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Non-farm income (own business: shop, trader/collector etc.)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Non-farm wages (salaried work in private or public company) 
HouseDesc$'MainIncomeSource - Non-farm wages (salaried work in private or public company)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Non-farm wages (salaried work in private or public company)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Other  ${b3_2oth}
HouseDesc$'MainIncomeSource - Other  ${b3_2oth}'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Other  ${b3_2oth}"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Pension
HouseDesc$'MainIncomeSource - Pension'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Pension"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Remittances
HouseDesc$'MainIncomeSource - Remittances'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Remittances"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Rented land
HouseDesc$'MainIncomeSource - Rented land'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Rented land"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Sell derived/processed products
HouseDesc$'MainIncomeSource - Sell derived/processed products'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Sell derived/processed products"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling cashew from the farm
HouseDesc$'MainIncomeSource - Selling cashew from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling cashew from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling cassava from the farm
HouseDesc$'MainIncomeSource - Selling cassava from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling cassava from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling cattle and buffalo
HouseDesc$'MainIncomeSource - Selling cattle and buffalo'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling cattle and buffalo"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling labor
HouseDesc$'MainIncomeSource - Selling labor'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling labor"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling maize from the farm
HouseDesc$'MainIncomeSource - Selling maize from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling maize from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling other crops from the farm
HouseDesc$'MainIncomeSource - Selling other crops from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling other crops from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling other farm products: ${b3_1oth}
HouseDesc$'MainIncomeSource - Selling other farm products: ${b3_1oth}'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling other farm products: ${b3_1oth}"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling own fruits
HouseDesc$'MainIncomeSource - Selling own fruits'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling own fruits"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling own vegetables
HouseDesc$'MainIncomeSource - Selling own vegetables'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling own vegetables"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling pigs
HouseDesc$'MainIncomeSource - Selling pigs'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling pigs"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling poultry
HouseDesc$'MainIncomeSource - Selling poultry'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling poultry"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling rice from the farm
HouseDesc$'MainIncomeSource - Selling rice from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling rice from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income - Selling soybean from the farm
HouseDesc$'MainIncomeSource - Selling soybean from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_1 == "Selling soybean from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#Second source of household income - Financial support / gift
HouseDesc$'SecondIncomeSource - Financial support / gift'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Financial support / gift"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Non-farm income (own business: shop, trader/collector etc.) 
HouseDesc$'SecondIncomeSource - Non-farm income (own business: shop, trader/collector etc.)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Non-farm income (own business: shop, trader/collector etc.)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Non-farm wages (salaried work in private or public company) 
HouseDesc$'SecondIncomeSource - Non-farm wages (salaried work in private or public company)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Non-farm wages (salaried work in private or public company)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Other  ${b3_2oth}
HouseDesc$'SecondIncomeSource - Other  ${b3_2oth}'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Other  ${b3_2oth}"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Pension
HouseDesc$'SecondIncomeSource - Pension'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Pension"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Sell derived/processed products
HouseDesc$'SecondIncomeSource - Sell derived/processed products'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Sell derived/processed products"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling cashew from the farm
HouseDesc$'SecondIncomeSource - Selling cashew from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling cashew from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling cassava from the farm
HouseDesc$'SecondIncomeSource - Selling cassava from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling cassava from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling cattle and buffalo
HouseDesc$'SecondIncomeSource - Selling cattle and buffalo'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling cattle and buffalo"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling labor
HouseDesc$'SecondIncomeSource - Selling labor'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling labor"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling maize from the farm
HouseDesc$'SecondIncomeSource - Selling maize from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling maize from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling other crops from the farm
HouseDesc$'SecondIncomeSource - Selling other crops from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling other crops from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling other farm products: ${b3_1oth}
HouseDesc$'SecondIncomeSource - Selling other farm products: ${b3_1oth}'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling other farm products: ${b3_1oth}"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling own fruits
HouseDesc$'SecondIncomeSource - Selling own fruits'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling own fruits"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling own vegetables
HouseDesc$'SecondIncomeSource - Selling own vegetables'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling own vegetables"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling pigs
HouseDesc$'SecondIncomeSource - Selling pigs'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling pigs"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling poultry
HouseDesc$'SecondIncomeSource - Selling poultry'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling poultry"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling rice from the farm
HouseDesc$'SecondIncomeSource - Selling rice from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling rice from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Second source of household income - Selling soybean from the farm
HouseDesc$'SecondIncomeSource - Selling soybean from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_2 == "Selling soybean from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#Third source of household income - Financial support / gift
HouseDesc$'ThirdIncomeSource - Financial support / gift'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Financial support / gift"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Non-farm income (own business: shop, trader/collector etc.) 
HouseDesc$'ThirdIncomeSource - Non-farm income (own business: shop, trader/collector etc.)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Non-farm income (own business: shop, trader/collector etc.)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Non-farm wages (salaried work in private or public company) 
HouseDesc$'ThirdIncomeSource - Non-farm wages (salaried work in private or public company)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Non-farm wages (salaried work in private or public company)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Other  ${b3_2oth}
HouseDesc$'ThirdIncomeSource - Other  ${b3_2oth}'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Other  ${b3_2oth}"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Pension
HouseDesc$'ThirdIncomeSource - Pension'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Pension"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Sell derived/processed products
HouseDesc$'ThirdIncomeSource - Sell derived/processed products'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Sell derived/processed products"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling cashew from the farm
HouseDesc$'ThirdIncomeSource - Selling cashew from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling cashew from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling cassava from the farm
HouseDesc$'ThirdIncomeSource - Selling cassava from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling cassava from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling cattle and buffalo
HouseDesc$'ThirdIncomeSource - Selling cattle and buffalo'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling cattle and buffalo"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling labor
HouseDesc$'ThirdIncomeSource - Selling labor'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling labor"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling maize from the farm
HouseDesc$'ThirdIncomeSource - Selling maize from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling maize from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling other crops from the farm
HouseDesc$'ThirdIncomeSource - Selling other crops from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling other crops from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling other farm products: ${b3_1oth}
HouseDesc$'ThirdIncomeSource - Selling other farm products: ${b3_1oth}'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling other farm products: ${b3_1oth}"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling own fruits
HouseDesc$'ThirdIncomeSource - Selling own fruits'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling own fruits"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling own vegetables
HouseDesc$'ThirdIncomeSource - Selling own vegetables'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling own vegetables"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling pigs
HouseDesc$'ThirdIncomeSource - Selling pigs'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling pigs"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling poultry
HouseDesc$'ThirdIncomeSource - Selling poultry'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling poultry"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling rice from the farm
HouseDesc$'ThirdIncomeSource - Selling rice from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling rice from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Third source of household income - Selling soybean from the farm
HouseDesc$'ThirdIncomeSource - Selling soybean from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b4_3 == "Selling soybean from the farm"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#Average % of income from 1st source of income
HouseDesc$PerCentIncomeFirstSource[HouseDesc$Sarea == i] <-
  round(svymean(~ b5_1, design = survey_design, na.rm = TRUE), digits = 2) / 100
#Average % of income from 2nd source of income
HouseDesc$PerCentIncomeSecondSource[HouseDesc$Sarea == i] <-
  round(svymean(~ b5_2, design = survey_design, na.rm = TRUE), digits = 2) / 100
#Average % of income from 3rd source of income
HouseDesc$PerCentIncomeThirdSource[HouseDesc$Sarea == i] <-
  round(svymean(~ b5_3, design = survey_design, na.rm = TRUE), digits = 2) / 100

#% of households for which 3 main sources of income changed during the last 3 years
HouseDesc$PerCentHIncomeSourceChange[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Main source of household income before change - Selling own vegetables
HouseDesc$'IncomeSourceBefore - Selling own vegetables'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b71 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling own fruits
HouseDesc$'IncomeSourceBefore - Selling own fruits'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b72 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling rice from the farm
HouseDesc$'IncomeSourceBefore - Selling rice from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b73 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cassava from the farm
HouseDesc$'IncomeSourceBefore - Selling cassava from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b719 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cashew from the farm
HouseDesc$'IncomeSourceBefore - Selling cashew from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b720 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling soybean from the farm
HouseDesc$'IncomeSourceBefore - Selling soybean from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b76 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cover crop seed from the farm
HouseDesc$'IncomeSourceBefore - Selling cover crop seed from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b77 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling other crops from the farm
HouseDesc$'IncomeSourceBefore - Selling other crops from the farm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b78 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling cattle and buffalo
HouseDesc$'IncomeSourceBefore - Selling cattle and buffalo'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b79 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling pigs
HouseDesc$'IncomeSourceBefore - Selling pigs'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b710 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling poultry
HouseDesc$'IncomeSourceBefore - Selling poultry'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b711 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling other farm products
HouseDesc$'IncomeSourceBefore - Selling other farm products'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b712 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling other farm products
HouseDesc$'IncomeSourceBefore - Selling derived/processed products'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b713 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Non-farm wages (salaried work in private or public company)
HouseDesc$'IncomeSourceBefore - Non-farm wages (salaried work in private or public company)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b714 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Non-farm income (own business: shop, trader/collector etc.)
HouseDesc$'IncomeSourceBefore - Non-farm income (own business: shop, trader/collector etc.)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b715 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Selling labor
HouseDesc$'IncomeSourceBefore - Selling labor'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b716 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Remittances
HouseDesc$'IncomeSourceBefore - Remittances'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b717 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)
#Main source of household income before change - Pension
HouseDesc$'IncomeSourceBefore - Pension'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b718 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b6 == "Yes"], na.rm = T), digits = 3)

#%Households earning most of their income during specific months
HouseDesc$'PerCentHMostIncomeSpecificMonth'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - January
HouseDesc$'PerCentHMostIncomeSpecificMonth - January'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_11 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - February
HouseDesc$'PerCentHMostIncomeSpecificMonth - February'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_12 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - March
HouseDesc$'PerCentHMostIncomeSpecificMonth - March'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_13 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - April
HouseDesc$'PerCentHMostIncomeSpecificMonth - April'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_14 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - May
HouseDesc$'PerCentHMostIncomeSpecificMonth - May'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_15 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - June
HouseDesc$'PerCentHMostIncomeSpecificMonth - June'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_16 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - July
HouseDesc$'PerCentHMostIncomeSpecificMonth - July'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_17 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - August
HouseDesc$'PerCentHMostIncomeSpecificMonth - August'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_18 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - September
HouseDesc$'PerCentHMostIncomeSpecificMonth - September'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_19 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - October
HouseDesc$'PerCentHMostIncomeSpecificMonth - October'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_110 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - November
HouseDesc$'PerCentHMostIncomeSpecificMonth - November'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_111 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households earning most of their income during specific months - December
HouseDesc$'PerCentHMostIncomeSpecificMonth - December'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b8_112 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#%Households facing financial difficulties during specific months
HouseDesc$'PerCentHFinancialDifficulties'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - January
HouseDesc$'PerCentHFinancialDifficulties - January'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_11 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - February
HouseDesc$'PerCentHFinancialDifficulties - February'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_12 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - March
HouseDesc$'PerCentHFinancialDifficulties - March'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_13 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - April
HouseDesc$'PerCentHFinancialDifficulties - April'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_14 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - May
HouseDesc$'PerCentHFinancialDifficulties - May'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_15 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - June
HouseDesc$'PerCentHFinancialDifficulties - June'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_16 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - July
HouseDesc$'PerCentHFinancialDifficulties - July'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_17 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - August
HouseDesc$'PerCentHFinancialDifficulties - August'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_18 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - September
HouseDesc$'PerCentHFinancialDifficulties - September'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_19 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - October
HouseDesc$'PerCentHFinancialDifficulties - October'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_110 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - November
HouseDesc$'PerCentHFinancialDifficulties - November'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_111 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households facing financial difficulties during specific months - December
HouseDesc$'PerCentHFinancialDifficulties - December'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_112 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) -	Non-working members went out to look for work
HouseDesc$'PerCentHOvercomeDifficulties - Non-working members went out to look for work'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_21 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) -	Working members increased work hours
HouseDesc$'PerCentHOvercomeDifficulties - Working members increased work hours'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_22 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Reducing / postpone own farm work and sale labor to the others
HouseDesc$'PerCentHOvercomeDifficulties - Reducing / postpone own farm work and sale labor to the others'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_23 == "1" | Dumm$b9_210 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - One or more members changes residence
HouseDesc$'PerCentHOvercomeDifficulties - One or more members changes residence'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_24 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Spent savings
HouseDesc$'PerCentHOvercomeDifficulties - Spent savings'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_25 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Taking loan
HouseDesc$'PerCentHOvercomeDifficulties - Taking loan'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_26 == "1" | Dumm$b9_211 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) -Increase loan
HouseDesc$'PerCentHOvercomeDifficulties - Increase loan'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_27 == "1" | Dumm$b9_212 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Went into debt
HouseDesc$'PerCentHOvercomeDifficulties - Went into debt'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_27 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Sold property or assets
HouseDesc$'PerCentHOvercomeDifficulties - Sold property or assets'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_28 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)

#Action to overcome these difficulties (%of household facing difficulties) - Sold animal (cattle,buffalo)
HouseDesc$'PerCentHOvercomeDifficulties - Sold animal (cattle,buffalo)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_213 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Did nothing
HouseDesc$'PerCentHOvercomeDifficulties - Did nothing'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_20 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)
#Action to overcome these difficulties (%of household facing difficulties) - Other
HouseDesc$'PerCentHOvercomeDifficulties - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_299 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b9 == "Yes"], na.rm = T), digits = 3)

#%Household involvment in additionnal activities - Community-based tourism or agroecological tourism
HouseDesc$'PerCentHAdditionnalActivities - Community-based tourism or agroecological tourism'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_31 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household involvment in additionnal activities - Hosting events (e.g. for projects from NGOs, research)
HouseDesc$'PerCentHAdditionnalActivities - Hosting events (e.g. for projects from NGOs, research)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_32 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household involvment in additionnal activities - Education and training of others (e.g. training of other farmers, school visits)
HouseDesc$'PerCentHAdditionnalActivities - Education and training of others (e.g. training of other farmers, school visits)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_33 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household involvment in additionnal activities - Food processing
HouseDesc$'PerCentHAdditionnalActivities - Food processing'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_34 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household involvment in additionnal activities - Restaurant
HouseDesc$'PerCentHAdditionnalActivities - Restaurant'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_35 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household involvment in additionnal activities - Selling products from other farms
HouseDesc$'PerCentHAdditionnalActivities - Selling products from other farms'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b9_36 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#%Households who are selling crops,fruits,vegetables
HouseDesc$PerCentHSellingCropsVegeFruits[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Export (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruit - Export'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "Export"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The local market (lower or equal to district level market) (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruit - The local market ( lower or equal to district level market)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "The local market ( lower or equal to district level market)"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The provincial or national market (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruit - The provincial or national market'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "The provincial or national market"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Do not know (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruit - Do not know'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "Do not know"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Other (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruit - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b10 == "Other"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - Less than 25% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruitRaw - <25%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "Less than 25%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 25-50% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruitRaw - 25-50%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "25-50%" | Dumm$b11 == "25%-50%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruitRaw - 50-75%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "50-75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingCropsVegeFruitRaw - Over 75%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b11 == "Over 75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b10 != "" & !is.na(Dumm$b10) & Dumm$b10 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)


#%Households who are selling livestock products
HouseDesc$PerCentHSellingLivestock[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Export (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestock - Export'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "Export"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The local market (lower or equal to district level market) (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestock - The local market ( lower or equal to district level market)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "The local market ( lower or equal to district level market)"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - The provincial or national market (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestock - The provincial or national market'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "The provincial or national market"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Do not know (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestock - Do not know'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "Do not know"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households who are selling crops,fruits,vegetables, final destination - Other (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestock - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b16 == "Other"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - Less than 25% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestockRaw - <25%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "Less than 25%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 25-50% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestockRaw - 25-50%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "25-50%" | Dumm$b11 == "25%-50%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestockRaw - 50-75%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "50-75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)
#%Households - % of the production of crops, vegetables and fruits sold raw - 50-75% (% of households selling crops,vege,fruits)
HouseDesc$'PerCentHSellingLivestockRaw - Over 75%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b17 == "Over 75%"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$b16 != "" & !is.na(Dumm$b16) & Dumm$b16 != "I do not sell (crops/vegetables/fruits or livestock)"], na.rm = T), digits = 3)

#%Household selling certified crops - Vegetables (% of total households)
HouseDesc$'PerCentHSellingCertifiedCrops - Vegetables'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_11 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household selling certified crops - Rice (% of total households)
HouseDesc$'PerCentHSellingCertifiedCrops - Rice'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_12 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household selling certified crops - Cassava (% of total households)
HouseDesc$'PerCentHSellingCertifiedCrops - Cassava'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_13 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household selling certified crops - Cashew nut (% of total households)
HouseDesc$'PerCentHSellingCertifiedCrops - Cashew nut'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_14 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household selling certified crops - Tea (% of total households)
HouseDesc$'PerCentHSellingCertifiedCrops - Tea'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b22_15 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#%Household quality or control test on crops 
HouseDesc$'PerCentHQualityTestCrops'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b25 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household premium prices on crops 
HouseDesc$'PerCentHPremiumPricesCrops'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b26 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household communicating with buyers to getting their feedback
HouseDesc$'PerCentHBuyersFeedback'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b28 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household participating in forum or fair
HouseDesc$'PerCentHForumFair'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b29 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household selling product online/on social media
HouseDesc$'PerCentHOnlineSelling'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$b30 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#%Household with members being active in one or several unions (% of all households) - Women Union
HouseDesc$'PerCentHActiveMember - Women Union'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_1 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Youth Union
HouseDesc$'PerCentHActiveMember - Youth Union'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_2 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Veteran group
HouseDesc$'PerCentHActiveMember - Veteran group'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_3 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Farmer Union
HouseDesc$'PerCentHActiveMember - Farmer Union'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_4 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Elderly group
HouseDesc$'PerCentHActiveMember - Elderly group'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_5 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Religious group
HouseDesc$'PerCentHActiveMember - Religious group'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_6 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Local government
HouseDesc$'PerCentHActiveMember - Local government'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_7 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Agricultural cooperative
HouseDesc$'PerCentHActiveMember - Agricultural cooperative'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_8 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Farmer association
HouseDesc$'PerCentHActiveMember - Farmer association'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_9 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Community forestry
HouseDesc$'PerCentHActiveMember - Community forestry'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_10 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Community Fisheries
HouseDesc$'PerCentHActiveMember - Community Fisheries'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_11 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Farmer Water User Community
HouseDesc$'PerCentHActiveMember - Farmer Water User Community'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_12 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household with members being active in one or several unions (% of all households) - Producer group / cluster
HouseDesc$'PerCentHActiveMember - Producer group / cluster'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c1_13 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#%Household membership in farmers organization - Yes, one
HouseDesc$'PerCentHFarmerOrganization - One'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c2 == "Yes, one"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household membership in farmers organization - Yes, more than one
HouseDesc$'PerCentHFarmerOrganization - More than one'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Household membership in farmers organization - Farmer organization on Crop (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on Crop'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c31 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on fruits (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on fruits'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c32 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on livestock (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on livestock'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c33 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on honey (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on honey'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c34 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on water (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on water'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c35 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on forest (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on forest'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c36 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on market (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on market'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c37 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on credit (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on credit'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c38 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on any type of mutual help (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on any type of mutual help'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c39 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)
#%Household membership in farmers organization - Farmer organization on diversified activities (% of households involved into farmer organization)
HouseDesc$'PerCentHFarmerOrganization - Farmer organization on diversified activities'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$c310 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$c2 == "Yes, more than one" | Dumm$c2 == "Yes, one"]), digits = 3)

#%Household NTFP - Hunting
HouseDesc$'PerCentH-Hunting'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d71 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for hunting
HouseDesc$'NbDaysSpent-Hunting'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_11, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Hunting (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Hunting'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_12 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d71 == "1"], na.rm = T), digits = 3)
#Average income from Hunting
HouseDesc$'AvIncome-Hunting'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_13, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Fishing
HouseDesc$'PerCentH-Fishing'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d72 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for fishing
HouseDesc$'NbDaysSpent-Fishing'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_21, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Fishing (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Fishing'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_22 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d72 == "1"], na.rm = T), digits = 3)
#Average income from Fishing
HouseDesc$'AvIncome-Fishing'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_23, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Fuelwood
HouseDesc$'PerCentH-Fuelwood'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d73 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for fuelwood
HouseDesc$'NbDaysSpent-Fuelwood'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_31, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Fuelwood (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Fuelwood'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_32 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d73 == "1"], na.rm = T), digits = 3)
#Average income from Fuelwood
HouseDesc$'AvIncome-Fuelwood'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_33, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Mushrooms
HouseDesc$'PerCentH-Mushrooms'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d74 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for mushrooms
HouseDesc$'NbDaysSpent-Mushrooms'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_41, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Mushrooms (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Mushrooms'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_42 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d74 == "1"], na.rm = T), digits = 3)
#Average income from Mushrooms
HouseDesc$'AvIncome-Mushrooms'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_43, design = survey_design, na.rm = TRUE), digits = 2)


#%Household NTFP - Bamboo shoots
HouseDesc$'PerCentH-BambooShoots'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d75 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Bamboo shoots
HouseDesc$'NbDaysSpent-Bamboo shoots'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_51, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Bamboo shoots (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Bamboo shoots'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_52 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d75 == "1"], na.rm = T), digits = 3)
#Average income from Bamboo shoots
HouseDesc$'AvIncome-Bamboo shoots'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_53, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Bamboo poles
HouseDesc$'PerCentH-BambooPoles'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d76 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Bamboo poles
HouseDesc$'NbDaysSpent-Bamboo poles'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_61, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Bamboo poles (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Bamboo poles'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_62 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d76 == "1"], na.rm = T), digits = 3)
#Average income from Bamboo poles
HouseDesc$'AvIncome-Bamboo poles'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_63, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Broom Grass
HouseDesc$'PerCentH-BroomGrass'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d77 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Broom Grass
HouseDesc$'NbDaysSpent-Broom Grass'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_71, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Broom Grass (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Broom grass'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_72 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d77 == "1"], na.rm = T), digits = 3)
#Average income from Broom Grass
HouseDesc$'AvIncome-Broom Grass'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_73, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Honey
HouseDesc$'PerCentH-Honey'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d78 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Honey
HouseDesc$'NbDaysSpent-Honey'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_81, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Honey (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Honey'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_82 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d78 == "1"], na.rm = T), digits = 3)
#Average income from Honey
HouseDesc$'AvIncome-Honey'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_83, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Rattan
HouseDesc$'PerCentH-Rattan'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d79 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Rattan
HouseDesc$'NbDaysSpent-Rattan'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_91, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Rattan (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Rattan'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_92 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d79 == "1"], na.rm = T), digits = 3)
#Average income from Rattan
HouseDesc$'AvIncome-Rattan'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_93, design = survey_design, na.rm = TRUE), digits = 2)


#%Household NTFP - Cardamom
HouseDesc$'PerCentH-Cardamom'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d710 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Cardamom
HouseDesc$'NbDaysSpent-Cardamom'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_101, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Cardamom (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Cardamom'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_102 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d710 == "1"], na.rm = T), digits = 3)
#Average income from Cardamom
HouseDesc$'AvIncome-Cardamom'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_103, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Galangal
HouseDesc$'PerCentH-Galangal'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d711 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Galangal
HouseDesc$'NbDaysSpent-Galangal'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_111, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Galangal (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Galangal'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_112 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d711 == "1"], na.rm = T), digits = 3)
#Average income from Galangal
HouseDesc$'AvIncome-Galangal'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_113, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Wild pepper
HouseDesc$'PerCentH-WildPepper'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d713 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Wild pepper
HouseDesc$'NbDaysSpent-Wild pepper'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_131, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Wild pepper (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Wild pepper'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_132 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d713 == "1"], na.rm = T), digits = 3)
#Average income from Wild pepper
HouseDesc$'AvIncome-Wild pepper'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_133, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Medicinal plants
HouseDesc$'PerCentH-Medicinal plants'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d714 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Medicinal plants
HouseDesc$'NbDaysSpent-Medicinal plants'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_141, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Medicinal plants (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Medicinal plants'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_142 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d714 == "1"], na.rm = T), digits = 3)
#Average income from Medicinal plants
HouseDesc$'AvIncome-Medicinal plants'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_143, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Wooden poles
HouseDesc$'PerCentH-WoodenPoles'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d716 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Wooden poles
HouseDesc$'NbDaysSpent-Wooden poles'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_161, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Wooden poles (% of concerned households)
HouseDesc$'PerCentHSellingProducts-Wooden poles'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_162 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d716 == "1"], na.rm = T), digits = 3)
#Average income from Wooden poles
HouseDesc$'AvIncome-Wooden poles'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_163, design = survey_design, na.rm = TRUE), digits = 2)

#%Household NTFP - Leave for thatch roof
HouseDesc$'PerCentH-LeaveThatchRoof'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d718 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average nb of days spent for Leave for thatch roof
HouseDesc$'NbDaysSpent-LeaveThatchRoof'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_181, design = survey_design, na.rm = TRUE), digits = 2)
#%Household NTFP - Selling products from Leave for thatch roof (% of concerned households)
HouseDesc$'PerCentHSellingProducts-LeaveThatchRoof'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d7_182 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d718 == "1"], na.rm = T), digits = 3)
#Average income from Leave for thatch roof
HouseDesc$'AvIncome-LeaveThatchRoof'[HouseDesc$Sarea == i] <-
  round(svymean(~ d7_183, design = survey_design, na.rm = TRUE), digits = 2)

#%Households having rented-in plots
HouseDesc$'PerCentHRentingPlots'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d9_1 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average area of rented-in plots
HouseDesc$'RentedInPlot-AvArea'[HouseDesc$Sarea == i] <-
  round(svymean(~ d9_3, design = survey_design, na.rm = TRUE), digits = 2)
#%Households having rented-out plots
HouseDesc$'PerCentHRentingOutPlots'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d10_1 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average area of rented-out plots
HouseDesc$'RentedOutPlot-AvArea'[HouseDesc$Sarea == i] <-
  round(svymean(~ d10_3, design = survey_design, na.rm = TRUE), digits = 2)
#%Households having owned plots
HouseDesc$'PerCentHOwningPlots'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#Average area of owned plots
HouseDesc$'OwnedPlot-AvArea'[HouseDesc$Sarea == i] <-
  round(svymean(~ d11_3, design = survey_design, na.rm = TRUE), digits = 2)
#%Households having property documents for owned plots - Title deed (% of households owning lands)
HouseDesc$'PerCentHOwningPlots - Title deed'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_41 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Certificate of customary tenure (% of households owning lands)
HouseDesc$'PerCentHOwningPlots - Certificate of customary tenure'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_42 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Certificate of occupancy (% of households owning lands)
HouseDesc$'PerCentHOwningPlots - Certificate of occupancy'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_43 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Registered will or registered certificate of hereditary acquisition (% of households owning lands)
HouseDesc$'PerCentHOwningPlots - Registered will or registered certificate of hereditary acquisition'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_44 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)
#%Households having property documents for owned plots - Other (% of households owning lands)
HouseDesc$'PerCentHOwningPlots - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$d11_45 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$d11_1 == "Yes"], na.rm = T), digits = 3)


#%Households feeling their work/occupation is stressful
HouseDesc$'PerCentHWorkStressful'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f1 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for family and social relationship - No time
HouseDesc$'PerCentHTimeAvailSocial - No time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "No time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for family and social relationship - Very little time
HouseDesc$'PerCentHTimeAvailSocial - Very little time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Very little time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for family and social relationship - Moderate amount of time
HouseDesc$'PerCentHTimeAvailSocial - Moderate amount of time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Moderate amount of time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for family and social relationship - Almost enough time
HouseDesc$'PerCentHTimeAvailSocial - Almost enough time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Almost enough time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for family and social relationship - Sufficient amount of time
HouseDesc$'PerCentHTimeAvailSocial - Sufficient amount of time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f3 == "Sufficient amount of time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - No time
HouseDesc$'PerCentHTimeAvailKnowledge - No time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "No time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Very little time
HouseDesc$'PerCentHTimeAvailKnowledge - Very little time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Very little time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Moderate amount of time
HouseDesc$'PerCentHTimeAvailKnowledge - Moderate amount of time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Moderate amount of time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Almost enough time
HouseDesc$'PerCentHTimeAvailKnowledge - Almost enough time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Almost enough time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households feeling about time availability for new knowledge and improve skills - Sufficient amount of time
HouseDesc$'PerCentHTimeAvailKnowledge - Sufficient amount of time'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$f4 == "Sufficient amount of time"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#%Households where XX makes the decision on what and how to produce - Myself alone
HouseDesc$'PerCentHDecisionProduction - Myself alone'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "Myself alone"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on what and how to produce - Me in consultation with spouse/other family members
HouseDesc$'PerCentHDecisionProduction - Me in consultation with spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on what and how to produce - My spouse/other family members
HouseDesc$'PerCentHDecisionProduction - My spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "My spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on what and how to produce - Do not know
HouseDesc$'PerCentHDecisionProduction - Do not know'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g1 == "Do not know"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - Myself alone
HouseDesc$'PerCentHDecisionAssets - Myself alone'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "Myself alone"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - Me in consultation with spouse/other family members
HouseDesc$'PerCentHDecisionAssets - Me in consultation with spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - My spouse/other family members
HouseDesc$'PerCentHDecisionAssets - My spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "My spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on purchasing, selling major assets - Do not know
HouseDesc$'PerCentHDecisionAssets - Do not know'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g2 == "Do not know"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - Myself alone
HouseDesc$'PerCentHDecisionBorrowingLending - Myself alone'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "Myself alone"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - Me in consultation with spouse/other family members
HouseDesc$'PerCentHDecisionBorrowingLending - Me in consultation with spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - My spouse/other family members
HouseDesc$'PerCentHDecisionBorrowingLending - My spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "My spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on borrowing or lending money - Do not know
HouseDesc$'PerCentHDecisionBorrowingLending - Do not know'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g3 == "Do not know"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on  how the household income is used - Myself alone
HouseDesc$'PerCentHDecisionUsingIncome - Myself alone'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "Myself alone"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on  how the household income is used - Me in consultation with spouse/other family members
HouseDesc$'PerCentHDecisionUsingIncome - Me in consultation with spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "Me in consultation with spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on  how the household income is used - My spouse/other family members
HouseDesc$'PerCentHDecisionUsingIncome - My spouse/other family members'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "My spouse/other family members"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households where XX makes the decision on  how the household income is used - Do not know
HouseDesc$'PerCentHDecisionUsingIncome - Do not know'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g4 == "Do not know"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)

#%Households thinking working hours are evenly distributed across family members
HouseDesc$'PerCentHWorkingHoursDistributionPerception'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g5 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households thinking they face challenges in their daylife?
HouseDesc$'PerCentHDaylifeChallenge'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$g6 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
          
#%Households wanting their children to be farmers too - Yes, strongly
HouseDesc$'PerCentHWantingChildrenAsFarmers - Yes, strongly'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "Yes, strongly"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households wanting their children to be farmers too - Yes, maybe
HouseDesc$'PerCentHWantingChildrenAsFarmers - Yes, maybe'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "Yes, maybe"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households wanting their children to be farmers too - They should emigrate if they had the chance
HouseDesc$'PerCentHWantingChildrenAsFarmers - They should emigrate if they had the chance'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "They should emigrate if they had the chance"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households wanting their children to be farmers too - No, agriculture is not a good job
HouseDesc$'PerCentHWantingChildrenAsFarmers - No, agriculture is not a good job'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "No, agriculture is not a good job"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households wanting their children to be farmers too - Do not know
HouseDesc$'PerCentHWantingChildrenAsFarmers - Do not know'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h1 == "Do not know"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having someone in the family to takeover the farm if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHFamilyMemberTakeoverFarm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h3 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$h3 != ""], na.rm = T), digits = 3)
#%Households having someone outside the family to takeover the farm if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHOutsidePeopleTakeoverFarm'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h4 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$h4 != ""], na.rm = T), digits = 3)

#%Households aware of ecological agriculture concept - Other farmers/farmer group/cooperative if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHAwareEcologicalAgriculture - Other farmers/farmer group/cooperative'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_11 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_11)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Technical advisors if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHAwareEcologicalAgriculture - Technical advisors'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_12 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_12)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Researchers if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHAwareEcologicalAgriculture - Researchers'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_13 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_13)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Buyers if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHAwareEcologicalAgriculture - Buyers'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_14 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_14)], na.rm = T), digits = 3)
#%Households aware of ecological agriculture concept - Other if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHAwareEcologicalAgriculture - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h4_199 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h4_199)], na.rm = T), digits = 3)
#%Households applying ecological agriculture concept if HH > 50 (% of households for which HH is older than 50)
HouseDesc$'PerCentHApplyingEcologicalAgriculture'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$h5 == "Yes"], na.rm = T) /
          sum(Dumm$SW_Weight[!is.na(Dumm$h5)], na.rm = T), digits = 3)


#%Households proportion of the food consumed from own farm or homegarden - Less than 25%
HouseDesc$'PerCentHSelfConsumption - Less than 25%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "Less than 25%"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households proportion of the food consumed from own farm or homegarden - 25-50%
HouseDesc$'PerCentHSelfConsumption - 25-50%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "25-50%"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households proportion of the food consumed from own farm or homegarden - 50-75%
HouseDesc$'PerCentHSelfConsumption - 50-75%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "50-75%"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households proportion of the food consumed from own farm or homegarden - Over 75%
HouseDesc$'PerCentHSelfConsumption - Over 75%'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i1 == "Over 75%"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year
HouseDesc$'PerCentHLackFood'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - January (% of total households)
HouseDesc$'PerCentHLackFood - January'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i31 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - February (% of total households)
HouseDesc$'PerCentHLackFood - February'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i32 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - March (% of total households)
HouseDesc$'PerCentHLackFood - March'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i33 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - April (% of total households)
HouseDesc$'PerCentHLackFood - April'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i34 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - May (% of total households)
HouseDesc$'PerCentHLackFood - May'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i35 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - June (% of total households)
HouseDesc$'PerCentHLackFood - June'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i36 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - July (% of total households)
HouseDesc$'PerCentHLackFood - July'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i37 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - August (% of total households)
HouseDesc$'PerCentHLackFood - August'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i38 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - September (% of total households)
HouseDesc$'PerCentHLackFood - September'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i39 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - October (% of total households)
HouseDesc$'PerCentHLackFood - October'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i310 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - November (%of total households)
HouseDesc$'PerCentHLackFood - November'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i311 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year - December (% of total households)
HouseDesc$'PerCentHLackFood - December'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i312 == "1"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having lack of food during past year, reason - Climate (drought, floods) (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - Climate (drought, floods)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i41 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Pest damages (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - Pest damages'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i42 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Animal disease (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - Animal disease'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i43 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - No buyers for your produce (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - No buyers for your produce'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i44 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Declining selling prices for your produce (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - Declining selling prices for your produce'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i45 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Need to reimburse credits (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - Need to reimburse credits'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i46 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Increasing prices of food (rice…) (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - Increasing prices of food (rice…)'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i47 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food during past year, reason - Other (%of households facing lack of food)
HouseDesc$'PerCentHLackFoodReason - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i499 == "1"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food for each event frequency  (%of households facing lack of food)
HouseDesc$'PerCentHLackFood - Happens every year or most years'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i5 == "Happens every year or most years"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food for each event frequency - Happens sometimes but not regularly (%of households facing lack of food)
HouseDesc$'PerCentHLackFood - Happens sometimes but not regularly'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i5 == "Happens sometimes but not regularly"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)
#%Households having lack of food for each event frequency - It was exceptional (%of households facing lack of food)
HouseDesc$'PerCentHLackFood - It was exceptional'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$i5 == "It was exceptional"], na.rm = T) /
          sum(Dumm$SW_Weight[Dumm$i2 == "Yes"], na.rm = T), digits = 3)

#%Households House tenure - Owned
HouseDesc$'PerCentHHouse - Owned'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j1 == "Owned"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households House tenure - Share cropping
HouseDesc$'PerCentHHouse - Share cropping'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j1 == "Share cropping"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Wall material of the house - Brick wall
HouseDesc$'PerCentHWallMaterial - Brick wall'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Brick wall"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Wall material of the house - Concrete wall
HouseDesc$'PerCentHWallMaterial - Concrete wall'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Concrete wall"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Wall material of the house - Wooden wall
HouseDesc$'PerCentHWallMaterial - Wooden wall'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Wooden wall"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Wall material of the house - Bamboo, Thatch/leaves, Grass
HouseDesc$'PerCentHWallMaterial - Bamboo, Thatch/leaves, Grass'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Bamboo, Thatch/leaves, Grass"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Wall material of the house - Galvanized iron or aluminium or other metal sheets
HouseDesc$'PerCentHWallMaterial - Galvanized iron or aluminium or other metal sheets'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Galvanized iron or aluminium or other metal sheets"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Wall material of the house - Wood or logs
HouseDesc$'PerCentHWallMaterial - Wood or logs'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Wood or logs"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Wall material of the house - Other
HouseDesc$'PerCentHWallMaterial - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j2 == "Other"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Brick tile roof
HouseDesc$'PerCentHRoofMaterial - Brick tile roof'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Brick tile roof"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Concrete, cement
HouseDesc$'PerCentHRoofMaterial - Concrete, cement'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Concrete, cement"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Fibrous cement
HouseDesc$'PerCentHRoofMaterial - Fibrous cement'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Fibrous cement"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Metal/ tin roof
HouseDesc$'PerCentHRoofMaterial - Metal/ tin roof'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Metal/ tin roof"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Other
HouseDesc$'PerCentHRoofMaterial - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Other"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Stone tile roof
HouseDesc$'PerCentHRoofMaterial - Stone tile roof'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Stone tile roof"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Thatch/leaves/grass
HouseDesc$'PerCentHRoofMaterial - Thatch/leaves/grass'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Thatch/leaves/grass"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households Roof material of the house - Wood
HouseDesc$'PerCentHRoofMaterial - Wood'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j3 == "Wood"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3) 
#%Households having a latrine
HouseDesc$'PerCentHLatrine'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j4 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households having a latrine shared with another household
HouseDesc$'PerCentHLatrineShared'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j5 == "Yes"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3) 
#%Households source of water used in the family - Drill
HouseDesc$'PerCentHWaterSource - Drill'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Drill"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households source of water used in the family - Natural stream
HouseDesc$'PerCentHWaterSource - Natural stream'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Natural stream"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households source of water used in the family - Other Private tap water
HouseDesc$'PerCentHWaterSource - Other Private tap water'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Other Private tap water"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households source of water used in the family - Public water
HouseDesc$'PerCentHWaterSource - Public water'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Public water"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)             
#%Households source of water used in the family - Rain water
HouseDesc$'PerCentHWaterSource - Rain water'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Rain water"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)             
#%Households source of water used in the family - Water delivery
HouseDesc$'PerCentHWaterSource - Water delivery'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Water delivery"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)    
#%Households source of water used in the family - Well
HouseDesc$'PerCentHWaterSource - Well'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j6 == "Well"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)    
#%Households source of electricity used in the family - Battery
HouseDesc$'PerCentHElectricitySource - Battery'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Battery"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)    
#%Households source of electricity used in the family - Grid electricity
HouseDesc$'PerCentHElectricitySource - Grid electricity'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Grid electricity"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)    
#%Households source of electricity used in the family - None, using kerosene/ candles
HouseDesc$'PerCentHElectricitySource - None, using kerosene/ candles'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "None, using kerosene/ candles"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households source of electricity used in the family - Other
HouseDesc$'PerCentHElectricitySource - Other'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Other"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)   
#%Households source of electricity used in the family - Private electricity
HouseDesc$'PerCentHElectricitySource - Private electricity'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Private electricity"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)
#%Households source of electricity used in the family - Public electricity
HouseDesc$'PerCentHElectricitySource - Public electricity'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Public electricity"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)   
#%Households source of electricity used in the family - Solar panel
HouseDesc$'PerCentHElectricitySource - Solar panel'[HouseDesc$Sarea == i] <-
  round(sum(Dumm$SW_Weight[Dumm$j7 == "Solar panel"], na.rm = T) /
          HouseDesc$N[HouseDesc$Sarea == i], digits = 3)   
 
}

Null <- HouseDesc[,586:878]
Null$PerCentHIncomeSourceChange
Null[,23]

#We remove NaN
for (i in 1:ncol(HouseDesc)){
  HouseDesc[is.nan(HouseDesc[,i]),i] <- 0
}
#We'll converthired workers values into $
#We use currencies values for April 2023:
#Cambodia: Already dollar value obviously
#Laos: 1$ = 17110 kips
#Vietnam: 1$ = 23452 dongs
HouseDesc$ValueHiredWorkersPerYear <- ifelse(HouseDesc$Sarea == "Cambodia",
                                             HouseDesc$ValueHiredWorkersPerYear,
                                             ifelse(HouseDesc$Sarea == "Lao",
                                             round(HouseDesc$ValueHiredWorkersPerYear / 17110, digits = 2),
                                             round(HouseDesc$ValueHiredWorkersPerYear / 23452, digits = 2)))
#Same for income from NTFP collection
#Cambodia: #Cambodia: 1$ = 4075 riels
for (i in 0:14){
  HouseDesc[,710+i*4]  <- ifelse(HouseDesc$Sarea == "Cambodia",
                                 round(HouseDesc[,710+i*4] / 4075, digits = 2),
                                 ifelse(HouseDesc$Sarea == "Lao",
                                        round(HouseDesc[,710+i*4] / 17110, digits = 2),
                                        round(HouseDesc[,710+i*4] / 23452, digits = 2)))
}
                                              

#Export database
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(HouseDesc, "HouseDesc.rds")

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


