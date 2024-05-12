###1. Prerequisites

#Package loading
library(labelled)
library(stringr)
library(dplyr)
library(vctrs)
library(plyr)
library(reshape2)
library(tidyverse)
library(purrr)
library(doBy)
library(survey)
#Options
options(warn=1)


#Work directory and data loading (for the 3  countries)
#We use the output datasets which were displayed during previous mission
#to check if the datasets are clean
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_cleaning&Analysis-Titouan2023/ASSETOutputs/ASSET_db")
#a. Cambodia
HouseholdCambodia_TF <- readRDS("HouseholdCambodia_TF.rds")
ClowlandCambodia_TF <- readRDS("ClowlandCambodia_TF.rds")
CuplandCambodia_TF <- readRDS("CuplandCambodia_TF.rds")
HouMemberCambodia_TF <- readRDS("HouMemberCambodia_TF.rds")
HomegardenCambodia_TF <- readRDS("HomegardenCambodia_TF.rds")
#One small correction
HouseholdCambodia_TF$b5_1 <- ifelse(HouseholdCambodia_TF$b5_1 == 7, 70,HouseholdCambodia_TF$b5_1)


#b. Laos
HouseholdLaos_TF <- readRDS("HouseholdLaos_TF.rds")
ClowlandLaos_TF <- readRDS("ClowlandLaos_TF.rds")
CuplandLaos_TF <- readRDS("CuplandLaos_TF.rds")
HouMemberLaos_TF <- readRDS("HouMemberLaos_TF.rds")
#c. Vietnam
HouseholdVietnam_TF <- readRDS("HouseholdVietnam_TF.rds")
ClowlandVietnam_TF <- readRDS("ClowlandVietnam_TF.rds")
CuplandVietnam_TF <- readRDS("CuplandVietnam_TF.rds")
HouMemberVietnam_TF <- readRDS("HouMemberVietnam_TF.rds")
#Import of sampling weight file
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/ASSET2stats")
SWeight <- read.csv2("SWeight.csv")


###2. Data preparation
#Let's create a common database for each countries with all the data we
#will need for the analyses

##2.1. Crops relative information - Crops main information table preparation
{
## - a. Preparation of the data for each country
# - Cambodia
#We select the useful data among lowland dataset - Cambodia
dlowC <- ClowlandCambodia_TF[,c(2:4,9,11:13,16,19:22)]
#We change column names to fit with other datasets
colnames(dlowC)[8:10] <- c("d2_135","d2_136","d2_137")
#We select the useful data among upland dataset - Cambodia
dupC <- CuplandCambodia_TF[,c(2:4,9,11:13,16,19:22)]
colnames(dupC) <- colnames(dlowC)
# - Laos
#We select the useful data among lowland dataset - Laos
dlowL <- ClowlandLaos_TF[,c(1:3,9,11:17)]
#We add one column (which will be empty) to fit with dlowC
dlowL$d2_137b <- NA
dlowL <- dlowL %>% relocate(d2_137b , .after = d2_137)
#We select the useful data among upland dataset - Laos
dupL <- CuplandLaos_TF[,c(1:3,8,10:16)]
dupL$d2_137b <- NA
dupL <- dupL %>% relocate(d2_137b , .after = d2_237)
colnames(dupL) <- colnames(dlowL)
# - Vietnam
#Lowland
#We select the useful data among lowland dataset - Vietnam
dlowV <- ClowlandVietnam_TF[,c(1:3,8,10:16)]
#We add one column (which will be empty) to fit with dlowC
dlowV$d2_137b <- NA
dlowV <- dlowV %>% relocate(d2_137b , .after = d2_137)
#We select the useful data among upland dataset - Vietnam
dupV <- CuplandVietnam_TF[,c(1:3,8,10:16)]
dupV$d2_137b <- NA
dupV <- dupV %>% relocate(d2_137b , .after = d2_237)
colnames(dupV) <- colnames(dlowV)

## - b. Merging and post-processings
#We create first a lowland and a upland database in order to correct some crop names
#We also want to indicate next to crop name if it is lowland or upland crop for some specific crops (vegetables)
#First for lowland crops
dlowTot <- rbind(dlowC,dlowL,dlowV)
dlowTot$d2_13e <- str_replace_all(dlowTot$d2_13e, paste0("^","Cucumber","$"), "lowlCucumber")
dlowTot$d2_13e <- str_replace_all(dlowTot$d2_13e, paste0("^","Cabbages","$"), "lowlCabbages")
dlowTot$d2_13e <- str_replace_all(dlowTot$d2_13e, c(
  "Chilli" = "lowlChili", "Chinese lettuce" = "lowlChinese lettuce",
  "Coriander" = "lowlCoriander","coriander" = "lowlCoriander",
  "Garlic" = "lowlGarlic", "ginger" = "lowlGinger",
  "Peanut" = "lowlPeanut", "groundnut" = "lowlPeanut",
  "jobs_tears" = "lowljobs_tears", "kale_flower" = "lowlkale_flower",
  "long_bean" = "lowlLong bean", "long bean, Chinese" = "lowlLong bean",
  "Mustard greens" = "lowlMustard greens","Mustasa" = "lowlMustard greens",
  "Other vegetable crop" = "lowlOther vegetable crop","Strawberry" = "lowlStrawberry",
  "Eggplant, Thai" = "lowlEggplant","French bean" = "lowlFrench bean",
  "Potatoes" = "lowlPotatoes", "Pumpkin" = "lowlPumpkin",
  "Sweet potatoes, tuber" = "lowlSweet potatoes, tuber",
  "Winter melon" = "lowlWinter melon"))
#Then for upland crops
dupTot <- rbind(dupC,dupL,dupV)
dupTot$d2_13e <- str_replace_all(dupTot$d2_13e, paste0("^","Cucumber","$"), "uplnCucumber")
dupTot$d2_13e <- str_replace_all(dupTot$d2_13e, paste0("^","Cabbages","$"), "uplnCabbages")
dupTot$d2_13e <- str_replace_all(dupTot$d2_13e, c(
"chili" = "uplnChili", "Chinese lettuce" = "uplnChinese lettuce",
"coriander" = "uplnCoriander","Garlic" = "uplnGarlic",
"Ginger, rhizome" = "uplnGinger", "groundnut" =  "uplnPeanut",
"jobs_tears" = "uplnjobs_tears", "kale_flower" = "uplnkale_flower",
"long_bean" = "uplnLong bean",
"Other vegetable crop" = "uplnOther vegetable crop", "Strawberry" = "uplnStrawberry", 
"eggplant" = "uplnEggplant", "Eggplant, Thai" = "uplnEggplant", 
"French bean" = "uplnFrench bean", 
"Mustard greens" = "uplnMustard greens","Mustasa" = "uplnMustard greens",
"Potatoes" = "uplnPotatoes", "Pumpkin" = "uplnPumpkin", 
"Sweet potatoes, tuber" = "uplnSweet potatoes, tuber", "Winter melon" = "uplnWinter melon"))
#We create the complete database with all countries and all kind of lands
dcrop <- rbind(dlowTot,dupTot)
#We change values in the columns to make it match between different datasets
# - Crops name homogenization
dcrop$d2_13e <- as.character(dcrop$d2_13e)
dcrop$d2_13e <- str_replace_all(dcrop$d2_13e, paste0("^","Mustard","$"), "Mustard2")
dcrop$d2_13e <- str_replace_all(dcrop$d2_13e, paste0("^","Onions","$"), "Onions2")
dcrop$d2_13e <- str_replace_all(dcrop$d2_13e, c("Advocado"="Avocado",
                                                "Cassava"="Cassava2",
                                                "cassava"="Cassava2", 
                                                "Chinese flowering cabbage/ choysum" = "Chinese flowering Cabbages/ choysum",
                                                "Coco" = "Coconuts",
                                                "Coconutsnuts"= "Coconuts",
                                                "Green mustard/ Choysum" = "Chinese flowering Cabbages/ choysum",
                                                "Kindey bean" = "Kidney bean",
                                                "Upland rice" = "Upland rice2",
                                                "Moutainous rice" = "Upland rice2",
                                                "mung_bean" = "Mung bean",
                                                "Mung Bean" = "Mung bean",
                                                "Other orchards crop" = "Other fruit crop",
                                                "salad/lacture" = "Salad/lettuce",
                                                "Soybean"="Soybean2",
                                                "Spring onion" = "Spring Onions"))
dcrop$d2_13e <- ifelse(dcrop$d2_13e == "Cashew nut + cassava / others", "Cashew - mixed cropping",
                ifelse(dcrop$d2_13e == "Cashew nut + Cassava2 / others", "Cashew - mixed cropping",
                ifelse(dcrop$d2_13e == "Cassava2 + cashew", "Cassava - mixed cropping",
                ifelse(dcrop$d2_13e == "Upland rice2 + cashew / mango", "Upland rice - mixed cropping",
                ifelse(dcrop$d2_13e == "Sunhemp + cashew", "Sunhemp - mixed cropping",
                ifelse(dcrop$d2_13e == "Soybean2 + cashew", "Soybean - mixed cropping", dcrop$d2_13e))))))
x <- data.frame(unique(as.factor(dcrop$d2_13e)))

# - Units homogenization
dcrop$d2_133 <- as.character(dcrop$d2_133)
dcrop$d2_133 <- str_replace_all(dcrop$d2_133, c("1" = "Kg of seed",
                                                "2" = "Gr of seed",
                                                "3" = "Number of seedlings",
                                                "Gram of seed" = "Gr of seed",
                                                "gr of seed" = "Gr of seed",
                                                "Kilogram of seed" = "Kg of seed",
                                                "kg of seed" = "Kg of seed",
                                                "number of seedlings" = "Number of seedlings"))

#And correction
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Cashew - mixed cropping", "Number of seedlings",ifelse(
                       dcrop$d2_13e == "Wet season rice broadcast", "Kg of seed", ifelse(
                       dcrop$d2_13e == "Wet season rice broadcast", "Kg of seed", dcrop$d2_133)))

# - Blank fulfiling
dcrop$d2_133 <- ifelse(dcrop$d2_133 == '' & (dcrop$d2_13e == "lowlGarlic" | dcrop$d2_13e == "uplnGarlic" |
                        dcrop$d2_13e =="lowljobs_tears" | dcrop$d2_13e =="uplnjobs_tears" |
                        dcrop$d2_13e == "lowlkale_flower" | dcrop$d2_13e == "uplnkale_flower" |
                        dcrop$d2_13e == "maize_hybrid" |
                        dcrop$d2_13e == "lowlOther vegetable crop" | dcrop$d2_13e == "uplnOther vegetable crop" |
                        dcrop$d2_13e == "Summer-autumn season rice" |
                        dcrop$d2_13e == "Tea leaves" | dcrop$d2_13e == "Upland rice2"|
                        dcrop$d2_13e == "Wet season rice broadcast"),
                        'Kg of seed',dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_133 == '' & (dcrop$d2_13e == "lowlChinese lettuce" | dcrop$d2_13e == "uplnChinese lettuce" |
                       dcrop$d2_13e == "uplnCoriander" | dcrop$d2_13e == "lowlCoriander" |
                       dcrop$d2_13e == "Napa Cabbages" | dcrop$d2_13e == "Onions2" | 
                       dcrop$d2_13e == "lowlPumpkin" | dcrop$d2_13e == "uplnPumpkin"), 'Gr of seed', dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_133 == '' & (dcrop$d2_13e == "lowlStrawberry" | dcrop$d2_13e == "uplnStrawberry"),
                       'Number of seedlings',dcrop$d2_133)

# - Real duplicates removal
#pid: "3299 2", "3206 1", "3219 2", "3493 3"
dcrop <- dcrop[!dcrop$pid == "1 4" &
               !dcrop$pid == "18 4" &
               !dcrop$pid == "34 4" &
               !(dcrop$pid == "126 1" & dcrop$d2_132 == 1600) &
               !(dcrop$pid == "126 2" & dcrop$d2_132 == 1200) &
               !dcrop$pid == "231 4" &
               !dcrop$pid == "677 2" &
               !dcrop$pid == "709 2" &
               !dcrop$pid == "2679 3" & 
               !dcrop$pid == "2712 4" &
               !dcrop$pid == "3299 2" &
               !dcrop$pid == "3206 1" &
               !dcrop$pid == "3219 2" &
               !dcrop$pid == "3493 3" &
               !dcrop$pid == "3621 2",]

# - Crops merging
#First let's check for households having several times a similar crop
x <- dcrop %>% 
  dplyr::count(hhid_re2, d2_13e) %>% 
  filter(n > 1)
#And then gather similar crops in a same row
dcrop$id <- 1:nrow(dcrop)
dcrop <- dcrop %>% relocate(id , .before = crop1_now)
dcrop$d2_132 <- as.numeric(dcrop$d2_132)
dcrop$d2_137 <- as.numeric(dcrop$d2_137)
dcrop$d2_138 <- as.numeric(dcrop$d2_138)
for (i in c(x$hhid_re2)){
  for (j in c(x$d2_13e[x$hhid_re2 == i])){
    n = 1
    y <- list()
    for (k in (dcrop$id[dcrop$hhid_re2 == i & dcrop$d2_13e == j])){
      y[n]<- (dcrop$d2_136[dcrop$hhid_re2 == i & dcrop$d2_13e == j & dcrop$id == k]*
                as.numeric(dcrop$d2_137[dcrop$hhid_re2 == i & dcrop$d2_13e == j & dcrop$id == k]))
      n = n+1
    }
    dcrop[dcrop$hhid_re2 == i & dcrop$d2_13e == j & dcrop$id == 
            max(as.numeric(dcrop$id[dcrop$hhid_re2 == i & dcrop$d2_13e == j])),11] <-
      (sum(unlist(y), na.rm = T)/sum(dcrop$d2_136[dcrop$hhid_re2 == i & dcrop$d2_13e == j & !is.na(dcrop$d2_137)]))
    dcrop[dcrop$hhid_re2 == i & dcrop$d2_13e == j & dcrop$id == 
            max(as.numeric(dcrop$id[dcrop$hhid_re2 == i & dcrop$d2_13e == j])),c(6,8:10,13)] <- 
      as.numeric(c(sum(dcrop$d2_132[dcrop$hhid_re2 == i & dcrop$d2_13e == j], na.rm = T),
                   sum(dcrop$d2_134[dcrop$hhid_re2 == i & dcrop$d2_13e == j], na.rm = T),
                   sum(dcrop$d2_135[dcrop$hhid_re2 == i & dcrop$d2_13e == j], na.rm = T),
                   sum(dcrop$d2_136[dcrop$hhid_re2 == i & dcrop$d2_13e == j], na.rm = T),
                   sum(dcrop$d2_138[dcrop$hhid_re2 == i & dcrop$d2_13e == j], na.rm = T)))
    dcrop <- dcrop[!(dcrop$hhid_re2 == i & dcrop$d2_13e == j & dcrop$id != 
                       max(as.numeric(dcrop$id[dcrop$hhid_re2 == i & dcrop$d2_13e == j]))),]
  }
}

#Correction #2 (Misc. corrections, mainly about area and yield stuffs)
x <- rbind(HouseholdCambodia_TF[,c(1,18)], HouseholdLaos_TF[,c(1,17)], HouseholdVietnam_TF[,c(1,18)])
colnames(x)[1] <- "hhid_re2"
dcrop <- left_join(dcrop,x, by = "hhid_re2")
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "lowlMustard greens", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Cashew nut", "Number of seedlings", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$country_eng_preload == "Cambodia" &
                         dcrop$d2_13e == "Cassava2", "Number of stems", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Upland rice2", "Kg of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Cassava - mixed cropping", "Number of stems", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Wet season rice transplant", "Kg of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Longan", "Number of seedlings", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Long bean, Chinese", "Kg of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "lowlCucumber", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Bananas", "Number of seedlings", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Summer-autumn season rice", "Kg of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Tea leaves" &  dcrop$country_eng_preload == "Lao", "Number of seedlings", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "lowlChinese lettuce" & dcrop$d2_133 == "Kg of seed", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "lowlChili" & dcrop$d2_133 == "Kg of seed", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "lowlCoriander" & dcrop$country_eng_preload == "Lao", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Salad/lettuce", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Chinese flowering Cabbages/ choysum", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "uplnChinese lettuce", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Napa Cabbages", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "uplnGarlic", "Number of seedlings", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "uplnPeanut", "Kg of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Basil, sweet, leaves", "Gr of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Rice bean", "Kg of seed", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Arrowroot, root", "Number of seedlings", dcrop$d2_133)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "Chayote", "Number of seedlings", dcrop$d2_133)

dcrop$d2_134 <- ifelse(dcrop$d2_13e == "lowlCucumber" & dcrop$country_eng_preload == "Lao" &
                         dcrop$d2_134 < 10, NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "Summer-autumn season rice" & dcrop$country_eng_preload == "Lao" &
                         dcrop$d2_134 == 30000, NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "Summer-autumn season rice" & dcrop$country_eng_preload == "Lao" &
                         dcrop$d2_134 > 9000, NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "Summer-autumn season rice" & dcrop$country_eng_preload == "Lao" &
                         dcrop$d2_134 > 1000, NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "maize_hybrid" & dcrop$d2_133 == "Number of seedlings",
                       NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "maize_hybrid" & dcrop$d2_133 == "Kg of seed" & dcrop$d2_134 > 200,
                       NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "Mung bean" & dcrop$country_eng_preload == "Lao",
                       dcrop$d2_134*10, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "lowlGarlic" & dcrop$country_eng_preload == "Lao" &
                         dcrop$d2_134 > 1000, NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "lowlGinger", dcrop$d2_134*1000, dcrop$d2_134)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "lowlWinter melon" & dcrop$d2_133 == "Gr of seed",
                       NA, dcrop$d2_134)
dcrop$d2_133 <- ifelse(dcrop$d2_13e == "lowlWinter melon", "Kg of seed", dcrop$d2_133)
dcrop$d2_134 <- ifelse(dcrop$d2_13e == "lowlWinter melon", dcrop$d2_134/10, dcrop$d2_134)
dcrop$d2_135 <- ifelse(dcrop$pid == "3552 2", NA, dcrop$d2_135) 
dcrop$d2_135 <- ifelse(dcrop$pid == "589 3", NA, dcrop$d2_135) 
dcrop$d2_135 <- ifelse(dcrop$pid == "642 3", NA, dcrop$d2_135) 
dcrop$d2_132 <- ifelse(dcrop$d2_13e == "lowlCoriander" & dcrop$country_eng_preload == "Vietnam",
                       dcrop$d2_132*10, dcrop$d2_132)
dcrop$d2_132 <- ifelse(dcrop$d2_13e == "uplnPeanut" &
                         dcrop$d2_132 == 50000, NA, dcrop$d2_132)
dcrop$d2_132 <- ifelse(dcrop$d2_13e == "uplnPeanut" &
                         dcrop$d2_132 < 1000, NA, dcrop$d2_132)
dcrop$d2_132 <- ifelse(dcrop$d2_13e == "Hmong mustard" &
                         dcrop$d2_132 < 100, NA, dcrop$d2_132)
dcrop$d2_136 <- ifelse(dcrop$d2_136 > dcrop$d2_135, dcrop$d2_135, dcrop$d2_136)
#Correction #3
dcrop$d2_135 <- ifelse((dcrop$d2_13e == "Summer-autumn season rice"| dcrop$d2_13e == "Winter-Spring season rice" | dcrop$d2_13e == "maize_hybrid") & dcrop$d2_135 / dcrop$d2_132 > 4,
                       NA, dcrop$d2_135)
dcrop$d2_135 <- ifelse((dcrop$d2_13e == "Summer-autumn season rice"| dcrop$d2_13e == "Winter-Spring season rice" | dcrop$d2_13e == "Upland rice2") & dcrop$d2_135 / dcrop$d2_132 > 1,
                       NA, dcrop$d2_135)
dcrop$d2_136 <- ifelse((dcrop$d2_13e == "Summer-autumn season rice"| dcrop$d2_13e == "Winter-Spring season rice" | dcrop$d2_13e == "Upland rice2") & dcrop$d2_136 > dcrop$d2_135,
                       dcrop$d2_135, dcrop$d2_136)
dcrop$d2_135 <- ifelse(dcrop$d2_13e == "maize_hybrid" & dcrop$d2_135 / dcrop$d2_132 > 1,
                       NA, dcrop$d2_135)
dcrop$d2_136 <- ifelse(dcrop$d2_13e == "maize_hybrid" & dcrop$d2_136 > dcrop$d2_135,
                       dcrop$d2_135, dcrop$d2_136)
dcrop$d2_134 <- ifelse((dcrop$d2_13e == "Summer-autumn season rice"| dcrop$d2_13e == "Winter-Spring season rice") & dcrop$d2_134 / dcrop$d2_132 > 0.07,
                       NA, dcrop$d2_134)
dcrop$d2_134 <- ifelse((dcrop$d2_13e == "Summer-autumn season rice"| dcrop$d2_13e == "Winter-Spring season rice") & dcrop$d2_134 / dcrop$d2_132 > 0.02,
                       NA, dcrop$d2_134)
dcrop$d2_135 <- ifelse(dcrop$d2_13e == "Maize (corn)" & dcrop$d2_135 / dcrop$d2_132 > 7,
                       NA, dcrop$d2_135)
dcrop$d2_135 <- ifelse(dcrop$d2_13e == "Maize (corn)" & dcrop$d2_135 / dcrop$d2_132 > 3,
                       NA, dcrop$d2_135)
dcrop$d2_136 <- ifelse(dcrop$d2_13e == "Maize (corn)" & dcrop$d2_136 > dcrop$d2_135,
                       dcrop$d2_135, dcrop$d2_136)

max(dcrop$d2_134[dcrop$d2_13e == "Summer-autumn season rice"], na.rm = T)
#We remove country column (will be re-added later)
dcrop <- dcrop[,-14]

#We change numeric variables to facilitate subsequent analysis
#First we correct some area values that are unlikely
dcrop$d2_132 <- ifelse(dcrop$d2_132 == 1, 10000, dcrop$d2_132)
#First we'll convert selling price into dollar
#We use currencies values for April 2023:
#Cambodia: 1$ = 4075 riels
#Laos: 1$ = 17110 kips
#Vietnam: 1$ = 23452 dongs
Dumm <- rbind(HouseholdCambodia_TF[,c(1,18)], HouseholdLaos_TF[,c(1,17)], HouseholdVietnam_TF[,c(1,18)])
colnames(Dumm)[1] <- "hhid_re2"
dcrop <- join(Dumm, dcrop, by = "hhid_re2")
dcrop$d2_137 <- ifelse(dcrop$country_eng_preload == "Cambodia",round(dcrop$d2_137/4075, digits = 2),
                ifelse(dcrop$country_eng_preload == "Lao", round(dcrop$d2_137/17110, digits = 2),
                       round(dcrop$d2_137/23452, digits = 2)))
#Then we'll add absolute numerical variables recalculated so that their values correspond to 1ha.
dcrop <- dcrop %>%
  mutate(d2_ha134 = round(d2_134 / (d2_132/10000), digits = 0),
         d2_ha135 = round(d2_135 / (d2_132/10000), digits = 0),
         d2_ha136 = round(d2_136 / (d2_132/10000), digits = 0),
         GrossProductRaw = round(d2_136*d2_137, digits = 0),
         GrossProductha = round((d2_136*d2_137)/(d2_132/10000), digits = 0))
#To facilitate the addition of subsequent crops, we move the upland/lowland info (for certain crops) to a different column.
patterns <- c("lowl", "upln")
dcrop$loworup <- str_extract(dcrop$d2_13e, paste(patterns, collapse = "|"))
dcrop$d2_13e <- str_replace_all(dcrop$d2_13e, c("lowl" = "", "upln" = ""))
}

#2.2. Crops relative information - Motivation, constraints & practices (part 1)
#
  ## - a. First we create an index list that will help to translate crop names
  {  
  #Cambodia
  #In the original database,  crop names are still in Khmer, we translate it
  TranslistLo <- rbind(ClowlandCambodia_TF[,c(8,9)],ClowlandLaos_TF[,c(8,9)],
                      ClowlandVietnam_TF[,c(7,8)]) %>% distinct(d2_13v, .keep_all = TRUE)
  colnames(TranslistLo) <- c("Foreign crop name", "English crop name")
  TranslistUp <- rbind(CuplandCambodia_TF[,c(8,9)],CuplandLaos_TF[,c(7,8)],
                       CuplandVietnam_TF[,c(7,8)]) %>% distinct(d2_23v, .keep_all = TRUE)
  colnames(TranslistUp) <- c("Foreign crop name", "English crop name")
  Translist <- rbind(TranslistLo,TranslistUp)
  #Homogenization of crops names in translation list
  Translist$`English crop name` <- as.character(Translist$`English crop name`)
  Translist$`English crop name` <- str_replace_all(Translist$`English crop name`, paste0("^","Mustard","$"), "Mustard2")
  Translist$`English crop name` <- str_replace_all(Translist$`English crop name`, paste0("^","Onions","$"), "Onions2")
  Translist$`English crop name` <- str_replace_all(Translist$`English crop name`, c("Advocado"="Avocado",
                                                  "Cassava"="Cassava2",
                                                  "cassava"="Cassava2",
                                                  "chili" = "Chili",
                                                  "Chilli" = "Chili",
                                                  "Chinese flowering cabbage/ choysum" = "Chinese flowering Cabbages/ choysum",
                                                  "coriander" = "Coriander",
                                                  "Coco" = "Coconuts",
                                                  "Coconutsnuts"= "Coconuts",
                                                  "eggplant" = "Eggplant",
                                                  "Eggplant, Thai" = "Eggplant",
                                                  "Green mustard/ Choysum" = "Chinese flowering Cabbages/ choysum",
                                                  "Ginger, rhizome" = "Ginger",
                                                  "ginger" = "Ginger",
                                                  "groundnut" = "Peanut",
                                                  "Kindey bean" = "Kidney bean",
                                                  "long_bean" = "Long bean",
                                                  "Upland rice" = "Upland rice2",
                                                  "Moutainous rice" = "Upland rice2",
                                                  "mung_bean" = "Mung bean",
                                                  "Mung Bean" = "Mung bean",
                                                  "Mustasa" = "Mustard greens",
                                                  "Other orchards crop" = "Other fruit crop",
                                                  "salad/lacture" = "Salad/lettuce",
                                                  "Soybean"="Soybean2",
                                                  "Spring onion" = "Spring Onions"))
  Translist$`English crop name` <- ifelse(Translist$`English crop name` == "Cashew nut + cassava / others", "Cashew - mixed cropping",
                         ifelse(Translist$`English crop name` == "Cashew nut + Cassava2 / others", "Cashew - mixed cropping",
                                ifelse(Translist$`English crop name` == "Cassava2 + cashew", "Cassava - mixed cropping",
                                       ifelse(Translist$`English crop name` == "Upland rice2 + cashew / mango", "Upland rice - mixed cropping",
                                              ifelse(Translist$`English crop name` == "Sunhemp + cashew", "Sunhemp - mixed cropping",
                                                     ifelse(Translist$`English crop name` == "Soybean2 + cashew", "Soybean - mixed cropping", Translist$`English crop name`))))))

  #Adding of some necesary missing translation
  x <- data.frame(c("Khan Na Daem","Trab","Nor Norng","Pot ( Haub)","Banle Phsaengtiet","Srov Vossa"),
             c("Chinese kale/ Gailan","Eggplant","gourd","Maize (corn)","Other vegetable crop","Wet season rice broadcast"))
  colnames(x) <- colnames(Translist)
  Translist <- rbind(Translist,x)
  }

 ## - b. Extraction of additionnal data - Reasons & constraints
  {
 #Extraction of additionnal data
 AddC <- HouseholdCambodia_TF[,c(1,756:759,761,763,765,767,769)]
 AddL <- HouseholdLaos_TF[,c(1,780:783,785,787,789,791,793)]
 AddV <- HouseholdVietnam_TF[,c(1,782:785,787,789,791,793,795)]
 colnames(AddC) <- colnames(AddL)
 colnames(AddV) <- colnames(AddL)
 Addcrops <- rbind(AddC,AddL, AddV)
 colnames(Addcrops)[2:4] <- c("Crop1","Crop2","Crop3")
 #Now we replace foreign names by english names
 for (i in 2:4){
   i1 <- with(Addcrops, match(Addcrops[,i], Translist$`Foreign crop name`))
   Addcrops[,i] <- with(Addcrops, ifelse(is.na(i1), Addcrops[,i], Translist$`English crop name`[i1]))
 }
 # - Datasets merging
 #Table reorganization
 colnames(Addcrops) <- 1
 AddcropsM <- rbind(Addcrops[,c(1:2,5:6)],Addcrops[,c(1,3,7:8)],Addcrops[,c(1,4,9:10)])
 colnames(AddcropsM) <- c("hhid_re2","d2_13e","d81_a - Reason", "d81_b - Constraint")
 #Specific case of replacing broadcasted by transplanted
 x <- c(2253, 2238, 2272, 2227, 2277, 2260, 2256, 2263, 2239, 2228, 2248, 2269, 2291)
 for (i in x){
   AddcropsM$d2_13e[AddcropsM$hhid_re2 == i & AddcropsM$d2_13e == "Wet season rice broadcast"] <- "Wet season rice transplant"
 }
 #Homogenization of reasons and constraints
 AddcropsM$`d81_a - Reason` <- as.character(AddcropsM$`d81_a - Reason`)
 AddcropsM$`d81_a - Reason` <- str_replace_all(AddcropsM$`d81_a - Reason`, c("Household consumption preferences;" = "Household consumption preferences",
                                                                            "1" = "Market price and demand",
                                                                            "2" = "Household consumption preferences",
                                                                            "3" = "Well adapted to local conditions (soil, climate, …)",
                                                                            "88" = "Do not know", "99" = "Other"))
                               
    
 #Constraints
 AddcropsM$`d81_b - Constraint` <- str_replace_all(AddcropsM$`d81_b - Constraint`,
                                  c("Diseases;" = "Diseases",
                                    "Insects;" = "Insects",
                                    "Other" = "Other agronomic constraints",
                                    "Other agronmic constraints;" = "Other agronmic constraints",
                                    "Soil fertility;" = "Soil fertility",
                                    "0" = "No constraint",
                                    "1" = "Water management",
                                    "2" = "Soil fertility",
                                    "3" = "Other agronomic constraints",
                                    "4" = "Insects",
                                    "5" = "Diseases",
                                    "6" = "Market price",
                                    "7" = "Product quality",
                                    "88" = "Do not know",
                                    "99" = "Other agronomic constraints",
                                    "Other agronomic constraints agronomic constraints;" = "Other agronomic constraints"))
 #Fulfilling of blank cases according to other crops dataset
 #First we remove useless cells
 AddcropsM <- AddcropsM[!((AddcropsM$d2_13e == '' | is.na(AddcropsM$d2_13e)) &
                           (AddcropsM$'d81_a - Reason' == '' | is.na(AddcropsM$'d81_a - Reason')) &
                           (AddcropsM$'d81_b - Constraint' == '' | is.na(AddcropsM$'d81_a - Reason'))),]
 #Then we fulfill blank cells with information from other dataset
 hhid_re2 <- AddcropsM$hhid_re2[AddcropsM$d2_13e == '']
 crops <- c("Summer-autumn season rice","Summer-autumn season rice","Winter-Spring season rice",
           "Maize (corn)","Summer-autumn season rice","Winter-Spring season rice",
           "Summer-autumn season rice","Summer-autumn season rice","Summer-autumn season rice",
           "","Winter-Spring season rice","Onions2",
           "Garlic","maize_hybrid","Cucumber",
           "Summer-autumn season rice","Winter-Spring season rice","Summer-autumn season rice",
           "Winter-Spring season rice","Winter-Spring season rice","Winter-Spring season rice",
           "","","maize_hybrid",
           "Chinese lettuce","Coriander","Ginger",
           "Honeydew","Winter-Spring season rice","Spring Onions",
           "Maize (corn)","Maize (corn)","Maize (corn)",
           "Maize (corn)","Maize (corn)","",
           "Other vegetable crop")
 AddcropsM$id <- 1:nrow(AddcropsM)
 for (i in 1:length(hhid_re2)){
     AddcropsM$d2_13e <- ifelse(AddcropsM$hhid_re2 == hhid_re2[i] & AddcropsM$d2_13e == '' &
                                AddcropsM$id == min(AddcropsM$id[AddcropsM$hhid_re2 == hhid_re2[i] & AddcropsM$d2_13e == '']),
                                crops[i], AddcropsM$d2_13e)
 }
 #Duplicates removal
 AddcropsM <- AddcropsM %>% dplyr::distinct(hhid_re2,d2_13e, .keep_all = TRUE)
 # - Inclusion in the previous dataset
 dcrop2 <- full_join(dcrop, AddcropsM, by = c("hhid_re2","d2_13e"))
  }


 ## - c. Extraction of additionnal data - Practices part 1
 {
   ## - a. Data preparation for each country
   # - Cambodia
   AEPc <- HouseholdCambodia_TF[,c(1,576:580,585:590,
                                   #d13 = AE water conservation practice (9 "practices")
                                   802:812,816,819:829,833,836:846,850,853:863,867,870:880,884,
                                   887:897,901,904:914,918,921:931,935,
                                   #d15 = AE soil conservation practices (8 "practices")
                                   949:959,963,966:976,980,983:993,997,1000:1010,1014,1017:1027,1031,
                                   1034:1044,1048,1051:1061,1065,1068:1078,1082,
                                   #AE maintaining/enhancing soil fertility practices (12 "practices)
                                   1101:1111,1115,1118:1128,1132,1135:1145,1149,1152:1162,1166,1169:1179,1183,
                                   1186:1196,1200,1203:1213,1217,1220:1230,1234,1237:1247,1251,1254:1264,1268,
                                   1271:1281,1285,1288:1298,1302,
                                   #AE weed control practices (15 "practices")
                                   1324:1334,1338,1341:1351,1355,1358:1368,1372,1375:1385,1389,1392:1402,1406,
                                   1409:1419,1423,1426:1436,1440,1443:1453,1457,1460:1470,1474,1477:1487,1491,
                                   1494:1504,1508,1511:1521,1525,1528:1538,1542,1545:1555,1559,1562:1572,1576,
                                   #AE pest and disease control practices (16 "practices")
                                   1602:1612,1616,1619:1629,1633,1636:1646,1650,1653:1663,1667,1670:1680,1684,1687:1697,1701,
                                   1704:1714,1718,1721:1731,1735,1738:1748,1752,1755:1765,1769,1772:1782,1786,1789:1799,1803,
                                   1806:1816,1820,1823:1833,1837,1840:1850,1854,1857:1867,1871,
                                   #Crops requiring more water than others
                                   1877:1887,
                                   #Crops requiring heavy fertilization to thrive
                                   1893:1903,
                                   #Crops requiring heavy use of pesticides to thrive
                                   1909:1919,
                                   #Conservation of traditionnal, local seeds
                                   1925:1935)]
   #Laos
   AEPl <- HouseholdLaos_TF[,c(1,620:624,628:633,
                               #d13 = AE water conservation practice (8 "practices")
                               822:833,836:847,850:861,864:875,878:889,892:903,906:917,920:931,
                               #d15 = AE soil conservation practices (8 "practices")
                               945:956,959:970,973:984,987:998,1001:1012,1015:1026,1029:1040,1043:1054,
                               #AE maintaining/enhancing soil fertility practices (12 "practices)
                               1073:1084,1087:1098,1101:1112,1115:1126,1129:1140,1143:1154,
                               1157:1168,1171:1182,1185:1196,1199:1210,1213:1224,1227:1238,
                               #AE weed control practices (15 "practices")
                               1260:1271,1274:1285,1288:1299,1302:1313,1316:1327,1330:1341,
                               1344:1355,1358:1369,1372:1383,1386:1397,1400:1411,1414:1425,
                               1428:1439,1442:1453,1456:1467,
                               #AE pest and disease control practices (15 "practices")
                               1492:1503,1506:1517,1520:1531,1534:1545,1548:1559,1562:1573,
                               1576:1587,1590:1601,1604:1615,1618:1629,1632:1643,1646:1657,
                               1660:1671,1674:1685,1688:1699,
                               #Crops requiring more water than others
                               1705:1715,
                               #Crops requiring heavy fertilization to thrive
                               1718:1728,
                               #Crops requiring heavy use of pesticides to thrive
                               1731:1741,
                               #Conservation of traditionnal, local seeds
                               1744:1754)]
   #Vietnam
   AEPv <- HouseholdVietnam_TF[,c(1,623:627,630:635,
                                  #d13 = AE water conservation practice (8 "practices")
                                  824:835,838:849,852:863,866:877,880:891,894:905,908:919,922:933,
                                  #d15 = AE soil conservation practices (8 "practices")
                                  947:958,961:972,975:986,989:1000,1003:1014,1017:1028,
                                  1031:1042,1045:1056,
                                  #AE maintaining/enhancing soil fertility practices (12 "practices)
                                  1075:1086,1089:1100,1103:1114,1117:1128,1131:1142,1145:1156,
                                  1159:1170,1173:1184,1187:1198,1201:1212,1215:1226,1229:1240,
                                  #AE weed control practices (14 "practices")
                                  #Some are missing
                                  1263:1274,1277:1288,1291:1302,1305:1316,1319:1330,1333:1344,
                                  1347:1358,1361:1372,1375:1386,1389:1400,1403:1414,1417:1428,
                                  1431:1442,1445:1456,
                                  #AE pest and disease control practices (15 "practices")
                                  1481:1492,1495:1506,1509:1520,1523:1534,1537:1548,1551:1562,
                                  1565:1576,1579:1590,1593:1604,1607:1618,1621:1632,1635:1646,
                                  1649:1660,1663:1674,1677:1688,
                                  #Crops requiring more water than others
                                  1694:1704,
                                  #Crops requiring heavy fertilization to thrive
                                  1707:1717,
                                  #Crops requiring heavy use of pesticides to thrive
                                  1720:1730,
                                  #Conservation of traditionnal, local seeds
                                  1733:1743)]
   
   ## - b. Data preparation
     #Now we'll add columns and change names to make it similar for each country
   {
     Camb <- colnames(AEPc)
     Laos <- colnames(AEPl)
     Viet <- colnames(AEPv)
     Tot <- as.data.frame(t(rbind(Camb,Laos,Viet)))
     Tot$check <- ifelse(Tot$Camb == Tot$Laos & Tot$Camb == Tot$Viet, "OK", "NNN")
   }
   {
       var_label(HouseholdCambodia_TF[,c(790:799,938:946,1087:1098,1307:1321,1584:1599)])
       var_label(HouseholdLaos_TF[,c(810:819,934:942,1059:1070,1243:1257,1475:1489)])
       var_label(HouseholdVietnam_TF[,c(812:821,936:944,1061:1072,1245:1259,1464:1478)])
    #Water conservation practices
      #Cambodia - #d121"Rainwater collection/conservation" - #d122"Greywater recycling" - #d123"Ponds (for water conservation)" - #d124"Terraces building" - #d125"Swales digging" - #d126"Land levelling" - #d127"Mulching" - #d1299"Other"
      #Laos     - #d121"Rainwater collection/conservation" - #d122"Greywater recycling" - #d123"Ponds (for water conservation)" - #d124"Terraces building" - #d125"Swales digging" - #d126"Land levelling" - #d127"Mulching" - #d1299"Other"
      #Vietnam  - #d121"Rainwater collection/conservation" - #d122"Greywater recycling" - #d123"Ponds (for water conservation)" - #d124"Terraces building" - #d125"Swales digging" - #d126"Land levelling" - #d127"Mulching" - #d1299"Other"
    #Soil conservation practices
      #Cambodia - #d140"No soil conservation practice" - #d141"Sowing in contour lines" - #d142"Natural or planted grass strips" - #d143"Trees conservation in agricultural plots" - #d144"Agroforestry (trees + crops)" - #d145"Crop residues maintained to cover the soil" - #d146"Use of cover crops" - #d147"Reduced to no-tillage" - #d1499"Other"
      #Laos     - #d140"No soil conservation practice" - #d141"Sowing in contour lines" - #d142"Natural or planted grass strips" - #d143"Trees conservation in agricultural plots" - #d144"Agroforestry (trees + crops)" - #d145"Crop residues maintained to cover the soil" - #d146"Use of cover crops" - #d147"Reduced to no-tillage" - #d1499"Other"
      #Vietnam  - #d140"No soil conservation practice" - #d141"Sowing in contour lines" - #d142"Natural or planted grass strips" - #d143"Trees conservation in agricultural plots" - #d144"Agroforestry (trees + crops)" - #d145"Crop residues maintained to cover the soil" - #d146"Use of cover crops" - #d147"Reduced to no-tillage" - #d1499"Other"
    #AE maintaining/enhancing soil fertility practices
      #Cambodia - #d181"Animal manure" - #d182"Compost (heap)" - #d183"Bokashi (fermented organic matter)" - #d184"Legume-based green manure" - #d185"Pulses in association and/or rotation with main crop" - #d186"Cover crops in association and/or rotation with main crop" - #d187"Biochar" - #d188"Crop residue maintenance" - #d189"Recycling crop waste" - #d1810"Ramial Wood Chip (RWC) or other wood chips" - #d1811"Organic agro-industrial waste" - #d1899"Other methods"
      #Laos     - #d181"Animal manure" - #d182"Compost (heap)" - #d183"Bokashi (fermented organic matter)" - #d184"Legume-based green manure" - #d185"Pulses in association and/or rotation with main crop" - #d186"Cover crops in association and/or rotation with main crop" - #d187"Biochar" - #d188"Crop residue maintenance" - #d189"Recycling crop waste" - #d1810"Ramial Wood Chip (RWC) or other wood chips" - #d1811"Organic agro-industrial waste" - #d1899"Other methods" 
      #Vietnam  - #d181"Animal manure" - #d182"Compost (heap)" - #d183"Bokashi (fermented organic matter)" - #d184"Legume-based green manure" - #d185"Pulses in association and/or rotation with main crop" - #d186"Cover crops in association and/or rotation with main crop" - #d187"Biochar" - #d188"Crop residue maintenance" - #d189"Recycling crop waste" - #d1810"Ramial Wood Chip (RWC) or other wood chips" - #d1811"Organic agro-industrial waste" - #d1899"Other methods"
    #AE weed control practices
      #Cambodia - #d211"Crop rotation / intercropping" - #d212"Cover crops" - #d213"Mulching / shading" - #d214"Sowing date / rate / depth" - #d215"Crop spatial arrangement" - #d216"Seed cleaning before sowing" - #d217"Cultivar choice" - #d218"Crop mixtures" - #d219"Nutrient placement" - #d2110"Patch/ban spraying" - #d2111"Bioherbicide" - #d2112"Mowing / slashing" - #d2113"Grazing" - #d2114"Post harvest weed seed destruction in field" - #d2199"Any other methods"
      #Laos     - #d211"Crop rotation / intercropping" - #d212"Cover crops" - #d213"Mulching / shading" - #d214"Sowing date / rate / depth" - #d215"Crop spatial arrangement" - #d216"Seed cleaning before sowing" - #d217"Cultivar choice" - #d218"Crop mixtures" - #d219"Nutrient placement" - #d2110"Patch/ban spraying" - #d2111"Bioherbicide" - #d2112"Mowing / slashing" - #d2113"Grazing" - #d2114"Post harvest weed seed destruction in field" - #d2199"Any other methods"
      #Vietnam  - #d211"Crop rotation / intercropping" - #d212"Cover crops" - #d213"Mulching / shading" - #d214"Sowing date / rate / depth" - #d215"Crop spatial arrangement" - #d216"Seed cleaning before sowing" - #d217"Cultivar choice" - #d218"Crop mixtures" - #d219"Nutrient placement" - #d2110"Patch/ban spraying" - #d2111"Bioherbicide" - #d2112"Mowing / slashing" - #d2113"Grazing" - #d2114"Post harvest weed seed destruction in field" - #d2199"Any other methods"
    #AE pest and disease control practices
      #Cambodia - #d271"Crop rotation / intercropping" - #d272"Flower strips" - #d273"Hedgerows" - #d274"Soil health maintenance/improvement" - #d275"Sanitation practices (removal of damaged/infected plants and fruits)" - #d276"Planting date" - #d277"Water and nutrient management" - #d278"Cultivar choice (tolerant/resistant) / cultivar mixture" - #d279"Biopesticide / organic pesticide" - #d2710"Commercial biological control agents (BCAs)" - #d2711"Home-made efficient microorganism (EM)" - #d2712"Commercial efficient microorganism (EM)" - #d2713"Pheromone traps" - #d2714"Protein baits" - #d2715"Weaver ant" - #d2799"Any other methods" 
      #Laos     - #d271"Crop rotation / intercropping" - #d272"Flower strips" - #d273"Hedgerows" - #d274"Soil health maintenance/improvement" - #d275"Sanitation practices (removal of damaged/infected plants and fruits)" - #d276"Planting date" - #d277"Water and nutrient management" - #d278"Cultivar choice (tolerant/resistant) / cultivar mixture" - #d279"Biopesticide / organic pesticide" - #d2710"Commercial biological control agents (BCAs)" - #d2711"Home-made efficient microorganism (EM)" - #d2712"Commercial efficient microorganism (EM)" - #d2713"Pheromone traps" - #d2714"Protein baits" - #d2715"Weaver ant" - #d2799"Any other methods"
      #Vietnam  - #d271"Crop rotation / intercropping" - #d272"Flower strips" - #d273"Hedgerows" - #d274"Soil health maintenance/improvement" - #d275"Sanitation practices (removal of damaged/infected plants and fruits)" - #d276"Planting date" - #d277"Water and nutrient management" - #d278"Cultivar choice (tolerant/resistant) / cultivar mixture" - #d279"Biopesticide / organic pesticide" - #d2710"Commercial biological control agents (BCAs)" - #d2711"Home-made efficient microorganism (EM)" - #d2712"Commercial efficient microorganism (EM)" - #d2713"Pheromone traps" - #d2714"Protein baits" - #d2715"Weaver ant" - #d2799"Any other methods"      
     }
     
     #Cambodia
     #Laos
     #We add columns of additionnal practices (Weaver ant, 
     AEPl <- AEPl %>% add_column("d27_1511" = NA, "d27_1512" = NA, "d27_1513" = NA,
                                 "d27_1514" = NA, "d27_1515" = NA, "d27_1516" = NA,
                                 "d27_1517" = NA, "d27_1518" = NA, "d27_1519" = NA,
                                 "d27_15110" = NA, "d27_15111" = NA, "d27_152" = NA,
                                 .after = "d27_142")
     
     #Vietnam
     #We add columns of additionnal practices (Weaver ant)
     AEPv <- AEPv %>% add_column("d27_1511" = NA, "d27_1512" = NA, "d27_1513" = NA,
                                 "d27_1514" = NA, "d27_1515" = NA, "d27_1516" = NA,
                                 "d27_1517" = NA, "d27_1518" = NA, "d27_1519" = NA,
                                 "d27_15110" = NA, "d27_15111" = NA, "d27_152" = NA,
                                 .after = "d27_142")
    # Missing practice (crop rotation intercropping - values for this practcie will still be NA)
     AEPv <- AEPv %>% add_column("d21_121" = NA, "d21_122" = NA, "d21_123" = NA,
                                 "d21_124" = NA, "d21_125" = NA, "d21_126" = NA,
                                 "d21_127" = NA, "d21_128" = NA, "d21_129" = NA,
                                 "d21_1210" = NA, "d21_1211" = NA,"d21_13" = NA,
                                 .after = "d18_992")
     
     #Now we'll merge it together
     colnames(AEPl) <- colnames(AEPc)
     colnames(AEPv) <- colnames(AEPc)
     AEPcrops <- rbind(AEPc,AEPl,AEPv)
     #Some preliminary corrections
     AEPcrops$crop1_name1 <- str_replace(AEPcrops$crop1_name1, "Srov Vossa Pongrous", "Srov Vossa Pongros")
     AEPcrops$crop1_name2 <- str_replace(AEPcrops$crop1_name2, "Srov Vossa Pongrous", "Srov Vossa Pongros")
     for (i in 1:60){
       AEPcrops[,(i*12+1)] <- as.character(AEPcrops[,(i*12+1)])
       AEPcrops[,(i*12+2)] <- as.character(AEPcrops[,(i*12+2)])
       AEPcrops[,(i*12+2)] <- ifelse(AEPcrops$crop1_name1 == "Srov Vossa Pongros, Srov Vossa Santoung" &
                                       AEPcrops[,(i*12+1)] == "1", "1",AEPcrops[,(i*12+2)] )
     }
     for (i in c(732,743,754)){
       AEPcrops[,i] <- as.character(AEPcrops[,i])
       AEPcrops[,(i+1)] <- as.character(AEPcrops[,(i+1)])
       AEPcrops[,(i+1)] <- ifelse(AEPcrops$crop1_name1 == "Srov Vossa Pongros, Srov Vossa Santoung" &
                                       AEPcrops[,i] == "1", "1",AEPcrops[,(i+1)] )
     }
     AEPcrops$crop1_name2 <- ifelse(AEPcrops$crop1_name1 == "Srov Vossa Pongros, Srov Vossa Santoung", "Srov Vossa Santoung", AEPcrops$crop1_name2)
     AEPcrops$crop1_name1 <- str_replace(AEPcrops$crop1_name1, "Srov Vossa Pongros, Srov Vossa Santoung", "Srov Vossa Pongros")
     #Now we replace foreign names by english names
     for (i in 2:12){
       i1 <- with(AEPcrops, match(AEPcrops[,i], Translist$`Foreign crop name`))
       AEPcrops[,i] <- with(AEPcrops, ifelse(is.na(i1), AEPcrops[,i], Translist$`English crop name`[i1]))
     }
     {summary(as.factor(rbind(AEPcrops$crop1_name1,AEPcrops$crop1_name2,AEPcrops$crop1_name3,
                             AEPcrops$crop1_name4,AEPcrops$crop1_name5,AEPcrops$crop2_name1,
                             AEPcrops$crop2_name2,AEPcrops$crop2_name3,AEPcrops$crop2_name4,
                             AEPcrops$crop2_name5,AEPcrops$crop2_name6)))}
     #Now we re-arrange the table
     #We create a list with sub dataframes inside 
     x <- list() 
     for (i in 0:10){
       x[[i+1]] <- AEPcrops[,c(1,2+i,13+i,25+i,37+i,49+i,61+i,73+i,85+i,97+i,109+i,121+i,
                    133+i,145+i,157+i,169+i,181+i,193+i,205+i,217+i,229+i,241+i,
                    253+i,265+i,277+i,289+i,301+i,313+i,325+i,337+i,349+i,361+i,
                    373+i,385+i,397+i,409+i,421+i,433+i,445+i,457+i,469+i,481+i,
                    493+i,505+i,517+i,529+i,541+i,553+i,565+i,577+i,589+i,601+i,
                    613+i,625+i,637+i,649+i,661+i,673+i,685+i,697+i,709+i,721+i,
                    732+i,743+i,754+i)]
       colnames(x[[i+1]]) <- colnames(x[[1]])
     }
     #We merge the dataframes into 1
     AEPcrops2 <- rbind(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]],x[[8]],
                        x[[9]],x[[10]],x[[11]])
     #We remove useless rows and correct few missing information
     AEPcrops2$check <- apply(AEPcrops2[,c(3:65)], 1, function(x) length(which(x=="1")))
     AEPcrops2 <- AEPcrops2[!AEPcrops2$crop1_name1 == '',]
     #CHange 1st columns names to merge it with dcrops table
     colnames(AEPcrops2)[1:2] <- c("hhid_re2", "d2_13e")
     #Check for duplicates
     {
     y <- AEPcrops2[,c(1,2,66)]
     AEPcrops2[AEPcrops2$hhid_re2 ==  2150 & AEPcrops2$d2_13e == "Cashew - mixed cropping",]
     # 2146 - Cashew - mixed cropping 7 vs 6 = OK
     # 2211 - Cassava - 3 vs 0 = Add information: "0.5" to "2211 2 - Cassava"
     # 2679 - Cashew - mixed cropping -  2 vs 1 = OK
     # 3004 - Other vegetable crop = Add information: "0.25" to "3004 2 - Other vegetable crop"
     # 3005 - Tea leaves - 1 vs 0 = Add information: "0.5" to "3005 2 - Tea leaves"
     # 3143 - Garlic - 1 vs 0 = Add information: "0.5" to "3143 2 - Tea leaves"
     # 3206 - Summer-autumn season rice  - 4 vs 0 = OK
     # 3219 - Summer-autumn season rice - 1 vs 0 = OK
     # 3367 - Summer-autumn season rice - 8 vs 0 = Add information: "0.5" to "3367 2 - Summer-autumn season rice"
     # 3433 - maize_hybrid - 3 vs 2 = OK
     # 3438 - maize_hybrid - 2 vs 0 = Add information: "0.5" to "3438 1 - maize_hybrid"
     # 3440 - maize_hybrid - 2 vs 0 = Add information: "0.5" to "3440 1 - maize_hybrid"
     # 3443 - maize_hybrid - 3 vs 0 = Add information: "0.1" to "3443 2 - maize_hybrid"
     # 3493 - Ginger, rhizome - 1 vs 0 = OK
     # 3493 - Napa Cabbages - 3 vs 0 = OK
     # 3513 - maize_hybrid - 2 vs 1 = OK
     # 3526 - Upland rice - 3 vs 2 = OK
     # 3574 - Upland rice - 3 vs 0 = Add information: "0.4" to "3574 2 - Upland rice"
     # 1 - Maize (corn) - 3 vs 0 = Add information: "0.5" to "1 3 - Maize (corn)"
     # 18 - Maize (corn) - 3 vs 0 = OK
     # 34 - Maize (corn) - 4 vs 0 = OK
     # 231 - Maize (corn) - 5 vs 0 = OK
     # 279 - Winter-Spring season rice - 5 vs 4 = OK
     # 551 - Maize (corn) - 4 vs 0 = Add information: "0.5" to "551 2 - Maize (corn)"
     # 598 - Longan - 8 vs 0 = OK
     # 622 - Sweetsop -  3 vs 0 = Add information: "0.5" to "622 1 - Maize (corn)"
     # 629 - Summer-autumn season rice - 2 vs 1 = OK
     # 677 - Cabbages - 5 vs 0 = OK
     # 709 - Maize(corn) - 5 vs 0 = OK
     # 714 - Other vegetable crop - 7 vs 6 = OK
     # 769 - Other vegetable crop - 3 vs 2 = OK
     }
     #Addition of relevant information about AE practices
     Info <- data.frame(hhid_re2  = c("2211","3004","3005","3143","3367","3438","3440","3443","3574",
                                          "1","551","622"),
                       d2_13e = c("Cassava", "Other vegetable crop", "Tea leaves", "Tea leaves", "Summer-autumn season rice",
     "maize_hybrid", "maize_hybrid", "maize_hybrid", "Upland rice", "Maize (corn)", "Maize (corn)", "Maize (corn)"),
                       PercentageAEP =c(0.5,0.25,0.5,0.5,0.5,0.5,0.5,0.1,0.4,0.5,0.5,0.5))
     AEPcrops2$hhid_re2 <- as.character(AEPcrops2$hhid_re2)
     AEPcrops2$PercentageAEP <- 1
     for (i in 1:nrow(Info)){
       AEPcrops2$PercentageAEP <- ifelse((AEPcrops2$hhid_re2 == Info[i,1] &
                                         AEPcrops2$d2_13e == Info[i,2]), as.numeric(Info[i,3]), AEPcrops2$PercentageAEP)
     }
     x <- AEPcrops2 %>% 
       dplyr::count(hhid_re2, d2_13e) %>% 
       filter(n > 1)
    AEPcrops2$check <- as.numeric(AEPcrops2$check)
    for (i in 1:nrow(x)){
      AEPcrops2 <- AEPcrops2[!(AEPcrops2$hhid_re2 == x[i,1] & AEPcrops2$d2_13e == x[i,2] &
                               AEPcrops2$check != max(AEPcrops2$check[AEPcrops2$hhid_re2 == x[i,1] & AEPcrops2$d2_13e == x[i,2]])),]
    }
     AEPcrops2 <- AEPcrops2[,-66]
     
     # - Inclusion in the previous dataset
     dcrop2 <- full_join(dcrop2, AEPcrops2, by = c("hhid_re2","d2_13e"), relationship = "many-to-many")
     }
     
 ## - d. Table reorganization to have 1 row -> 1 household
 {# - Wide to long and labels adding
  #We change name of a column for further use
  colnames(dcrop2)[13] <- "d2_139"
  #We include again lowland and upland information into the crop cell
  dcrop2$d2_13e <- ifelse(!is.na(dcrop2$loworup), paste(dcrop2$d2_13e, "-",dcrop2$loworup), dcrop2$d2_13e)
  #We remove useless columns
  dcrop2 <- dcrop2[,c(1:2,4:19,21:22,24:87)]
  #We check for duplicates
  x <- dcrop2 %>% 
    dplyr::count(hhid_re2, d2_13e) %>% 
    filter(n > 1)
  #Duplicates removal:
  dcrop2$id <- 1:nrow(dcrop2)
  for (i in 1:nrow(x)){
    dcrop2 <- dcrop2[!(dcrop2$hhid_re2 == x[i,1] & dcrop2$d2_13e == x[i,2] &
                               dcrop2$id != max(dcrop2$id[dcrop2$hhid_re2 == x[i,1] & dcrop2$d2_13e == x[i,2]])),]
  }
  #NA and no crops name removal
  dcrop2 <- dcrop2[!is.na(dcrop2$d2_13e) & dcrop2$d2_13e != "",]
  #Small final corrections
  dcrop2$d2_13e <- gsub(
    "^(Cabbages|Chinese lettuce|Cucumber|Eggplant|French bean|Garlic|Ginger|jobs_tears|Long bean|Mustard greens|Other vegetable crop|Peanut|Pumpkin|Strawberry|Sweet potatoes, tuber|Winter melon)$", 
    "\\1 - lowl", 
    dcrop2$d2_13e
  )
  
  #We export the croplist
  dcrop2$d2_13e <- as.character(dcrop2$d2_13e)
  dcrop2$d2_13e <- gsub("[()]", "", dcrop2$d2_13e)
  croplist <- unique(dcrop2$d2_13e)
  x <- as.data.frame(croplist)
  #Change columns order
  dcrop2 <- dcrop2 %>% relocate(d2_139, .before =  d2_136)
  #We change the lowland data from long format to wide format
  dcropwide <- reshape(dcrop2[,c(1:84)], direction = "wide", timevar = "d2_13e", idvar = c("hhid_re2","country_eng_preload"))
  #We add labels with useful information
  for (i in 0:121){
    var_label(dcropwide[,3+81*i]) <- "Crop n° (in the household)"
    var_label(dcropwide[,4+81*i]) <- "household id + Crop n°"
    var_label(dcropwide[,5+81*i]) <- "Area (m2)"
    var_label(dcropwide[,6+81*i]) <- "Seed unit (SU) - sowing"
    var_label(dcropwide[,7+81*i]) <- "Amount of SU sowed"
    var_label(dcropwide[,8+81*i]) <- "Amount of CU (kg) harvested"
    var_label(dcropwide[,9+81*i]) <- "Form when sold"
    var_label(dcropwide[,10+81*i]) <- "Amount of CU (kg) sold"
    var_label(dcropwide[,11+81*i]) <- "Selling price (Dol/CU)"
    var_label(dcropwide[,12+81*i]) <- "Number of species"
    var_label(dcropwide[,13+81*i]) <- "PER HA Amount of SU sowed"
    var_label(dcropwide[,14+81*i]) <- "PER HA Amount of CU (kg) harvested"
    var_label(dcropwide[,15+81*i]) <- "PER HA Amount of CU (kg) sold"
    var_label(dcropwide[,16+81*i]) <- "Gross product Raw"
    var_label(dcropwide[,17+81*i]) <- "PER HA Gross product"
    var_label(dcropwide[,18+81*i]) <- "Reason for planting"
    var_label(dcropwide[,19+81*i]) <- "Constraints faced"
    var_label(dcropwide[,20+81*i]) <- "Rainwater collection/conservation"
    var_label(dcropwide[,21+81*i]) <- "Greywater recycling"
    var_label(dcropwide[,22+81*i]) <- "Ponds (for water conservation)"
    var_label(dcropwide[,23+81*i]) <- "Terraces building"
    var_label(dcropwide[,24+81*i]) <- "Swales digging"
    var_label(dcropwide[,25+81*i]) <- "Land levelling"
    var_label(dcropwide[,26+81*i]) <- "Mulching"
    var_label(dcropwide[,27+81*i]) <- "Other"
    var_label(dcropwide[,28+81*i]) <- "Sowing in contour lines"
    var_label(dcropwide[,29+81*i]) <- "Natural or planted grass strips"
    var_label(dcropwide[,30+81*i]) <- "Trees conservation in agricultural plots"
    var_label(dcropwide[,31+81*i]) <- "Agroforestry (trees + crops)"
    var_label(dcropwide[,32+81*i]) <- "Crop residues maintained to cover the soil"
    var_label(dcropwide[,33+81*i]) <- "Use of cover crops"
    var_label(dcropwide[,34+81*i]) <- "Reduced to no-tillage"
    var_label(dcropwide[,35+81*i]) <- "Other"
    var_label(dcropwide[,36+81*i]) <- "Animal manure"
    var_label(dcropwide[,37+81*i]) <- "Compost (heap)"
    var_label(dcropwide[,38+81*i]) <- "Bokashi (fermented organic matter)"
    var_label(dcropwide[,39+81*i]) <- "Legume-based green manure"
    var_label(dcropwide[,40+81*i]) <- "Pulses in association and/or rotation with main crop"
    var_label(dcropwide[,41+81*i]) <- "Cover crops in association and/or rotation with main crop"
    var_label(dcropwide[,42+81*i]) <- "Biochar"
    var_label(dcropwide[,43+81*i]) <- "Crop residue maintenance"
    var_label(dcropwide[,44+81*i]) <- "Recycling crop waste"
    var_label(dcropwide[,45+81*i]) <- "Ramial Wood Chip (RWC) or other wood chips"
    var_label(dcropwide[,46+81*i]) <- "Organic agro-industrial waste"
    var_label(dcropwide[,47+81*i]) <- "Other methods"
    var_label(dcropwide[,48+81*i]) <- "Crop rotation / intercropping"
    var_label(dcropwide[,49+81*i]) <- "Cover crops"
    var_label(dcropwide[,50+81*i]) <- "Mulching / shading"
    var_label(dcropwide[,51+81*i]) <- "Sowing date / rate / depth"
    var_label(dcropwide[,52+81*i]) <- "Crop spatial arrangement"
    var_label(dcropwide[,53+81*i]) <- "Seed cleaning before sowing"
    var_label(dcropwide[,54+81*i]) <- "Cultivar choice"
    var_label(dcropwide[,55+81*i]) <- "Crop mixtures"
    var_label(dcropwide[,56+81*i]) <- "Nutrient placement"
    var_label(dcropwide[,57+81*i]) <- "Patch/ban spraying"
    var_label(dcropwide[,58+81*i]) <- "Bioherbicide"
    var_label(dcropwide[,59+81*i]) <- "Mowing / slashing"
    var_label(dcropwide[,60+81*i]) <- "Grazing"
    var_label(dcropwide[,61+81*i]) <- "Post harvest weed seed destruction in field"
    var_label(dcropwide[,62+81*i]) <- "Any other methods"
    var_label(dcropwide[,63+81*i]) <- "Crop rotation / intercropping"
    var_label(dcropwide[,64+81*i]) <- "Flower strips"
    var_label(dcropwide[,65+81*i]) <- "Hedgerows"
    var_label(dcropwide[,66+81*i]) <- "Soil health maintenance/improvement"
    var_label(dcropwide[,67+81*i]) <- "Sanitation practices (removal of damaged/infected plants and fruits)"
    var_label(dcropwide[,68+81*i]) <- "Planting date"
    var_label(dcropwide[,69+81*i]) <- "Water and nutrient management"
    var_label(dcropwide[,70+81*i]) <- "Cultivar choice (tolerant/resistant) / cultivar mixture"
    var_label(dcropwide[,71+81*i]) <- "Biopesticide / organic pesticide"
    var_label(dcropwide[,72+81*i]) <- "Commercial biological control agents (BCAs)"
    var_label(dcropwide[,73+81*i]) <- "Home-made efficient microorganism (EM)"
    var_label(dcropwide[,74+81*i]) <- "Commercial efficient microorganism (EM)"
    var_label(dcropwide[,75+81*i]) <- "Pheromone traps"
    var_label(dcropwide[,76+81*i]) <- "Protein baits"
    var_label(dcropwide[,77+81*i]) <- "Weaver ant"
    var_label(dcropwide[,78+81*i]) <- "Any other methods"
    var_label(dcropwide[,79+81*i]) <- "Crops requiring more water than others"
    var_label(dcropwide[,80+81*i]) <- "Crops requiring heavy fertilization to thrive"
    var_label(dcropwide[,81+81*i]) <- "Crops requiring heavy use of pesticides to thrive"
    var_label(dcropwide[,82+81*i]) <- "Conservation of traditionnal, local seeds"
    var_label(dcropwide[,83+81*i]) <- "AEP percentage"
  }
  
 }

                     
#2.3. Crops relative information - Practices (part 2), equipment and work organization

## - Variables related to hhid and not to crops
## - a. Justification related to AE practices
{
#First we'll clean the data we want to add
for (i in c(24,48,60,72,84,96,108,120,132,144,156,168,180,192,204,
         216,228,240,252,264,276,288,300,312,324,336,348,360,372,384,
         396,408,420,432,444,456,468,480,492,504,516,528,540,564,
         576,588,600,612,624,636,684,696,708,720)){
    AEPcrops[,i] <- as.character(AEPcrops[,i])
    AEPcrops[,i] <- str_replace_all(AEPcrops[,i], c("1" = "To save money",
                                    "2" = "To save labor/time",
                                    "3" = "To improve yields",
                                    "4" = "To improve soil/plant health",
                                    "99" = "Other"))
  }

#Justifications of practices adoption are still missing, we'll add it
colnames(AEPcrops)[1] <- "hhid_re2"
dcropwide2 <- join(dcropwide, AEPcrops[,c(1,24,36,48,60,72,84,96,108,120,132,144,156,168,180,192,204,
                                         216,228,240,252,264,276,288,300,312,324,336,348,360,372,384,
                                         396,408,420,432,444,456,468,480,492,504,516,528,540,552,564,
                                         576,588,600,612,624,636,648,660,672,684,696,708,720)], by = "hhid_re2")

## - b. Other variables related to AE practices
Temp <- rbind(HouseholdCambodia_TF[,c(1,1084,1304,1578:1580,1873:1875,1891,1907,1923,1939)],
        HouseholdLaos_TF[,c(1,1056,1240,1469:1471,1701:1703,1716,1729,1742,1755)],
        HouseholdVietnam_TF[,c(1,1058,1242,1458:1460,1690:1692,1705,1718,1731,1744)])
#Again we'll clean the data we want to add
for (i in c(2:4,7)){
  Temp[,i] <- as.character(Temp[,i])
  Temp[,i] <- str_replace_all(Temp[,i], c("1" = "I do not know them",
                                  "2" = "They are too costly",
                                  "3" = "I have no time to implement them",
                                  "4" = "I don't want to do things differently from my neighbors",
                                  "99" = "Other"))
}
 Temp[,5] <- as.character(Temp[,5]) 
 Temp[,5] <- str_replace_all(Temp[,5], c("1" = "Synthetic herbicide without mechanical weeding",
                                  "2" = "Frequent mechanical weeding (more than three times per year) without synthetic herbicide",
                                  "3" = "Mixed management using herbicide and mechanical weeding",
                                  "88" = "Do not know",
                                  "99" = "Other"))
  for (i in c(6,9:12)){
  Temp[,i] <- as.character(Temp[,i])
  Temp[,i] <- str_replace_all(Temp[,i], c("1" = "Yes",
                                          "0" = "No",
                                          "88" = "Do not know"))
  }
  Temp[,8] <- as.character(Temp[,8])
  Temp[,8] <- str_replace_all(Temp[,8], c("1" = "Synthetic insecticide and fungicide are used regularly and no other system is used",
                                          "2" = "Mixed use of synthetic and biological/natural pesticides",
                                          "3" = "Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used",
                                          "4" = "Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide",
                                          "88" = "Do not know"))
  Temp[,13] <- as.character(Temp[,13])
  Temp[,13] <- str_replace_all(Temp[,13], c("1" = "From the village seller",
                                            "2" = "From the cooperative",
                                            "3" = "From a trader in town",
                                            "4" = "From family & friends",
                                            "88" = "Do not know",
                                            "99" = "Other"))

colnames(Temp)[1] <- "hhid_re2"
dcropwide3 <- full_join(dcropwide2,Temp,by = "hhid_re2")
}

## - b. Other variables related to work organization from Household members db
{
#1st we prepare each database
#Cambodia
HMc <- HouMemberCambodia_TF[,c(2:4,7,9,11,13,15,18:33,37,39,41:52)]
#Laos
HMl <- HouMemberLaos_TF[,c(1:3,6,8,10,12,14,17:26,30,32,34:45)]
 colnames(HMl)[12:16] <- c("a811","a812","a813","a814","a84")
 HMl <- HMl %>% relocate(a84 , .after = a83)
 HMl <- HMl %>% add_column("a85" = NA, "a86" = NA, "a87" = NA,
                             "a88" = NA, "a89" = NA, "a810" = NA, .after = "a84")
#Vietnam
HMv <- HouMemberVietnam_TF[,c(1:3,6,8,10,12,14,17:26,30,32,34:45)]
colnames(HMv)[12:16] <- c("a811","a812","a813","a814","a84")
HMv <- HMv %>% relocate(a84 , .after = a83)
HMv <- HMv %>% add_column("a85" = NA, "a86" = NA, "a87" = NA,
                          "a88" = NA, "a89" = NA, "a810" = NA, .after = "a84")
#Now we merge data from 3 countries on one dataframe
HMtot <- rbind(HMc,HMl,HMv)
#We homogenize some of the answers
#a2
HMtot$a2 <- as.character(HMtot$a2)
HMtot$a2 <- str_replace_all(HMtot$a2, c("10" = "Sibling","11" = "Other relative","12" = "Other non-relative",
                                        "88" = "Do not know", "99" = "Other", 
                                        "1" = "Household head","2" = "Spouse/partner of the household head",
                                        "3" = "Son/daughter","4" = "Son/daughter in law","5" = "Grandchild","6" = "Parent",
                                        "7" = "Parent-in-law","8" = "Grandparent","9" = "Grandparent-in-law"))
                             
                                                       
#a3
HMtot$a3 <- as.character(HMtot$a3)
HMtot$a3 <- str_replace_all(HMtot$a3, c("1" = "Male", "2" = "Female"))
#a6
HMtot$a6 <- as.character(HMtot$a6)
HMtot$a6 <- str_replace_all(HMtot$a6, c("10" = "Unable to work","11" = "Other","12" = "Do not know",
                                        "88" = "Do not know", "99" = "Other",
                                        "1" = "Agricultural work on their own farm (including livestock management)",
                                        "5" = "Business (owner)","9" = "Housewife/ caregiver",
                                        "2" = "Permanent salaried employment on someone else’s farm (including livestock management",
                                        "6" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with company" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with NGOs" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with government" = "Government",
                                        "4" = "Government","3" = "Seasonal salaried employment on someone else’s farm (including livestock management)",
                                        "Student/Study at university/training" = "Study/training", "8" = "Study/training",
                                         "7" = "Junk collector"))
HMtot$a6 <- ifelse(HMtot$a6 == "Business (owner) (please specify)", "Business (owner)",HMtot$a6)
#a7
HMtot$a7 <- as.character(HMtot$a7)
HMtot$a7 <- str_replace_all(HMtot$a7, c("10" = "Unable to work","11" = "Other","12" = "Do not know",
                                        "88" = "Do not know", "99" = "Other",
                                        "0" = "No secondary occupation", "1" = "Agricultural work on their own farm (including livestock management)",
                                        "5" = "Business (owner)","9" = "Housewife/ caregiver",
                                        "2" = "Permanent salaried employment on someone else’s farm (including livestock management",
                                        "6" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with company" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with NGOs" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with government" = "Government",
                                        "4" = "Government","3" = "Seasonal salaried employment on someone else’s farm (including livestock management)",
                                        "Student/Study at university/training" = "Study/training", "8" = "Study/training",
                                        "7" = "Junk collector"))
HMtot$a7 <- ifelse(HMtot$a7 == "Business (owner) (please specify)", "Business (owner)",HMtot$a7)
#a10
HMtot$a10 <- as.character(HMtot$a10)
HMtot$a10 <- str_replace_all(HMtot$a10, c("0" = "No", "1" = "Yes"))
#a12
HMtot$a12 <- as.character(HMtot$a12)
HMtot$a12 <- str_replace_all(HMtot$a12, c("10" = "Unable to work","11" = "Other","12" = "Do not know",
                                        "88" = "Do not know", "99" = "Other",
                                        "0" = "No secondary occupation", "1" = "Agricultural work on their own farm (including livestock management)",
                                        "5" = "Business (owner)","9" = "Housewife/ caregiver",
                                        "2" = "Permanent salaried employment on someone else’s farm (including livestock management",
                                        "6" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with company" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with NGOs" = "Salaried employment (non-agricultural work)",
                                        "Salaried employment with government" = "Government",
                                        "4" = "Government","3" = "Seasonal salaried employment on someone else’s farm (including livestock management)",
                                        "Student/Study at university/training" = "Study/training", "8" = "Study/training",
                                        "7" = "Junk collector"))
HMtot$a12 <- ifelse(HMtot$a12 == "Business (owner) (please specify)", "Business (owner)",HMtot$a12)
summary(as.factor(HMtot$a12))
#p_no
summary(as.factor(HMtot$p_no))
HMtot$p_no <- str_replace_all(HMtot$p_no, c("10" = "Ten","11" = "Eleven","12" = "Twelve","13" = "Thirteen",
                                            "14" = "Forteen", "15" = "Fifteen", "16" = "Sixteen", "17" = "Seventeen",
                                            "18" = "Eighteen", "19" ="Nineteen", "20" = "Twenty",
                                            "1" = "One", "2" = "Two", "3" = "Three", "4" = "Four", "5" = "Five",
                                            "6" = "Six2", "7" = "Seven2", "8" = "Eight2", "9" = "Nine2"))
                                          
#Now we need to "wide to long" part of household members table
dHMwide <- reshape(HMtot, direction = "wide", timevar = "p_no", idvar = "hhid_re1")
#And we merge previous Table (including ALL the information) with this new one
colnames(dHMwide)[1] <- "hhid_re2"
dcropwide4 <- full_join(dcropwide3,dHMwide, by = "hhid_re2")
#We add labels
for (i in 0:19){
var_label(dcropwide4[,9956+36*i]) <- "Member id"
var_label(dcropwide4[,9957+36*i]) <- "Relationship to household head"
var_label(dcropwide4[,9958+36*i]) <- "Gender"
var_label(dcropwide4[,9959+36*i]) <- "Age (years)"
var_label(dcropwide4[,9960+36*i]) <- "Main occupation"
var_label(dcropwide4[,9961+36*i]) <- "Secondary occupation"
var_label(dcropwide4[,9962+36*i]) <- "Weaving"
var_label(dcropwide4[,9963+36*i]) <- "Handicraft"
var_label(dcropwide4[,9964+36*i]) <- "Commerce"
var_label(dcropwide4[,9965+36*i]) <- "Agricultural sector"
var_label(dcropwide4[,9966+36*i]) <- "Agri. Service on soil preparation and harvesting"
var_label(dcropwide4[,9967+36*i]) <- "Agri. Service on spraying"
var_label(dcropwide4[,9968+36*i]) <- "Agri. Service on threshing"
var_label(dcropwide4[,9969+36*i]) <- "Services/employment in tourism"
var_label(dcropwide4[,9970+36*i]) <- "Services/employment in restaurant"
var_label(dcropwide4[,9971+36*i]) <- "Services/employment in shops"
var_label(dcropwide4[,9972+36*i]) <- "Transportation"
var_label(dcropwide4[,9973+36*i]) <- "Blacksmithing"
var_label(dcropwide4[,9974+36*i]) <- "Construction work"
var_label(dcropwide4[,9975+36*i]) <- "Factory work"
var_label(dcropwide4[,9976+36*i]) <- "Other"
var_label(dcropwide4[,9977+36*i]) <- "Do not know"
var_label(dcropwide4[,9978+36*i]) <- "Migration Y-N"
var_label(dcropwide4[,9979+36*i]) <- "Main occupation when migrating"
var_label(dcropwide4[,9980+36*i]) <- "Jan"
var_label(dcropwide4[,9981+36*i]) <- "Feb"
var_label(dcropwide4[,9982+36*i]) <- "Mar"
var_label(dcropwide4[,9983+36*i]) <- "Apr"
var_label(dcropwide4[,9984+36*i]) <- "May"
var_label(dcropwide4[,9985+36*i]) <- "Jun"
var_label(dcropwide4[,9986+36*i]) <- "Jul"
var_label(dcropwide4[,9987+36*i]) <- "Aug"
var_label(dcropwide4[,9988+36*i]) <- "Sep"
var_label(dcropwide4[,9989+36*i]) <- "Oct"
var_label(dcropwide4[,9990+36*i]) <- "Nov"
var_label(dcropwide4[,9991+36*i]) <- "Dec"
}

}

## - c. Other variables related to work organization and equipments from Household general DB
{
#Cambodia
WOc <- HouseholdCambodia_TF[,c(1,553:560,562:563,1942:1945,1947:1950,1952:1955,1957:1960,
                                      1962:1965,1967:1970,1972:1975,1977:1980,1982:1985,1987:1990,
                                      1992:1996,1999:2009,2440:2458)]
colnames(WOc)[56] <- "d35"
WOc <- WOc %>% add_column("d3610" = NA, .after = "d369")
#Laos
WOl <- HouseholdLaos_TF[,c(1,598:605,607:608,1758:1761,1763:1766,1768:1771,1773:1776,1778:1781,
                                  1783:1786,1788:1791,1793:1796,1798:1801,1803:1806,1808:1811,1813,
                                  1815:1826,2233:2251)]

#Vietnam
WOv <- HouseholdVietnam_TF[,c(1,602:609,611:612,1747:1750,1752:1755,1757:1760,
                                  1762:1765,1767:1770,1772:1775,1777:1780,1782:1785,
                                  1787:1790,1792:1795,1797:1800,1802,1804:1814,2217:2235)]
WOv <- WOv %>% add_column("d3610" = NA, .after = "d369")
#Now we merge the 3 dataframes
WOtot <- rbind(WOc,WOl,WOv)
summary(as.factor(WOtot[,10]))
for (i in 10:11){
  WOtot[,i] <- as.character(WOtot[,i])
  WOtot[,i] <- str_replace_all(WOtot[,i], c("1" = "Yes",
                                            "0" = "No",
                                            "88" = "Do not know"))
}
colnames(WOtot)[1] <- "hhid_re2"
dcropwide5 <- full_join(dcropwide4,WOtot,by = "hhid_re2")

}

#2.4. Raising systems relative information
#a. Animal scale
{
#First we'll prepare a wide table with information concerning the 3 most important animals
#Cambodia
AnimC <- HouseholdCambodia_TF[,c(1,18,2045,2047,2049,2051:2064,2067,2069:2076,2079:2093,2096:2097,2099:2112)]
columns_to_convert <- c("e5_c", "e5_51","e6_c")
# Convert selected columns to numeric
AnimC <- AnimC %>%
  mutate_at(columns_to_convert, as.numeric)
#Laos
AnimL <- HouseholdLaos_TF[,c(1,17,1863,1865,1867,1869:1880,1883,1885:1892,1895:1907,1910:1911,1913:1924)]
AnimL <- AnimL %>% add_column("e5_42" =  NA, .after = "e5_41")
AnimL <- AnimL %>% add_column("e5_52" =  NA, .after = "e5_51")
AnimL <- AnimL %>% add_column("e6_42" =  NA, .after = "e6_41")
AnimL <- AnimL %>% add_column("e6_52" =  NA, .after = "e6_51")
AnimL <- AnimL %>% add_column("e7_42" =  NA, .after = "e7_41")
AnimL <- AnimL %>% add_column("e7_52" =  NA, .after = "e7_51")
AnimL[,30] <- as.numeric(AnimL[,30])
#Vietnam
AnimV <- HouseholdVietnam_TF[,c(1,18,1851,1853,1855,1857:1868,1871,1873:1880,1883:1895,1898:1899,1901:1912)]
AnimV <- AnimV %>% add_column("e5_42" =  NA, .after = "e5_41")
AnimV <- AnimV %>% add_column("e5_52" =  NA, .after = "e5_51")
AnimV <- AnimV %>% add_column("e6_42" =  NA, .after = "e6_41")
AnimV <- AnimV %>% add_column("e6_52" =  NA, .after = "e6_51")
AnimV <- AnimV %>% add_column("e7_42" =  NA, .after = "e7_41")
AnimV <- AnimV %>% add_column("e7_52" =  NA, .after = "e7_51")
#Merging of all country databases
AnimTot <- rbind(AnimC,AnimL,AnimV)
#We now Re-arrange the table to have all the animals in One column
#First we add columns to permit the data merging
AnimTot <- AnimTot %>% add_column("aa" = NA, "ab" = NA, "ac" = NA, "ad" = NA,
                                  "ae" = NA, "af" = NA, "ag" = NA, "ah" = NA, .after = "b20_2")
AnimTot$b20_2 <- as.character(AnimTot$b20_2)
AnimTot$b20_2 <- str_replace_all(AnimTot$b20_2, c("Regular and reliable / honest sales/purchase" = "Regular sales",
                                                  "0" = "Nothing",
                                                  "1" = "Inputs (sold)",
                                                  "5" = "Market information",
                                                  "6" = "Regular sales"))
Dummy <- c("Nothing","Inputs (sold)","Inputs on credit","Cash credit",
           "Technical advice/training","Market information","Regular sales",
           "Other")
for (i in 1:8){
  AnimTot[,45+i] <- ifelse(AnimTot$b20_2 == Dummy[i], "1", NA)
}
AnimTot <- AnimTot[,-45]
AnimTot <- AnimTot %>% add_column("ba" = NA, "bb" = NA, "bc" = NA, "bd" = NA,
                                  "be" = NA, "bf" = NA, "bg" = NA, "bh" = NA,
                                  "bi" = NA, "bj" = NA, "bk" = NA, .after = "e7_52")
#Now we split and merge the data
Ania <- AnimTot[,c(1:3,6:29)]
Anib <- AnimTot[,c(1:2,4,30:53)]
Anic <- AnimTot[,c(1:2,5,54:77)]
colnames(Anib) <- colnames(Ania)
colnames(Anic) <- colnames(Ania)
AnimTotN <- rbind(Ania,Anib,Anic)
#And before adding to the previous data, we check column values to be homogeneous and then turn data to wide
#e4_1
AnimTotN$e4_1 <- as.character(AnimTotN$e4_1)
AnimTotN$e4_1 <- str_replace_all(AnimTotN$e4_1, paste0("^","Duck","$"), "Duck or/and Muscovy")
AnimTotN$e4_1 <- str_replace_all(AnimTotN$e4_1, paste0("^","Muscovy","$"), "Duck or/and Muscovy")
AnimTotN$e4_1 <- str_replace_all(AnimTotN$e4_1, c("10" = "Duck or/and Muscovy",
                                                  "98" = "Other cattle",
                                                  "99" = "Other poultry",
                                                  "1" = "Buffalo",
                                                  "2" = "Cattle",
                                                  "3" = "Pig",
                                                  "4" = "Goat",
                                                  "6" = "Horse",
                                                  "7" = "Rabbit",
                                                  "8" = "Chicken",
                                                  "9" = "Duck or/and Muscovy",
                                                  "Other poutry" = "Other poultry"))
#Yes/no answers
for (i in c(5,7)){
  AnimTotN[,i] <- as.character(AnimTotN[,i])
  AnimTotN[,i] <- str_replace_all(AnimTotN[,i], c("1" = "Yes",
                                                "0" = "No"))
}
#b19_1
AnimTotN$b18_1 <- as.character(AnimTotN$b18_1)
AnimTotN$b18_1 <- str_replace_all(AnimTotN$b18_1, c( "10" =	"Consumers through online sales",
                  "11" = "Foreign trader (e.g. from China)",
                  "12" = "Processors",
                  "99" = "Other",
                  "88" = "Do not know/ no the second outlet",
                  "1" = "Village collector",
                  "2" = "Collector outside the village",
                  "3" = "Trader in the district",
                  "4" = "Trader from the province",
                  "5" = "Trader from another province",
                  "6" = "Cooperative of which you are a member",
                  "7" =	"Cooperative of which you are not a member",
                  "8" =	"Consumers on the farm",
                  "9" = "Local markets where you sell your products directly to final consumers",
                  "Do not know/ No selling/ No the second outlet" = "Do not know/ no the second outlet"))
#b19_1
AnimTotN$b19_1 <- as.character(AnimTotN$b19_1)
AnimTotN$b19_1 <- str_replace_all(AnimTotN$b19_1, paste0("^","2","$"), "25-50%")
AnimTotN$b19_1 <- str_replace_all(AnimTotN$b19_1, c("1" = "Less than 25%",
                                                    "3" = "50-75%",
                                                    "4" = "Over 75%",
                                                    "88" = "Do not know"))
#b21_1
AnimTotN$b21_1 <- as.character(AnimTotN$b21_1)
AnimTotN$b21_1 <- str_replace_all(AnimTotN$b21_1,c("1" =	"Formal contract", "2" =	"Informal contract",
"3" = "No contract/ no prior arrangements", "4" = "Spot relations",
"88" = "Do not know"))
#Removal of NA and ""
AnimTotN <- AnimTotN %>% filter(!is.na(e4_1) & e4_1 != "")
#We'll homogeneize the content of some numeric columns and add some that will be useful later
#We use currencies values for April 2023:
#Cambodia: 1$ = 4075 riels
#Laos: 1$ = 17110 kips
#Vietnam: 1$ = 23452 dongs
AnimTotN$e5_41 <- ifelse(AnimTotN$country_eng_preload == "Cambodia",round(AnimTotN$e5_41/4075, digits = 2),
                       ifelse(AnimTotN$country_eng_preload == "Lao", round(AnimTotN$e5_41/17110, digits = 2),
                              round(AnimTotN$e5_41/23452, digits = 2)))
AnimTotN$e5_51 <- ifelse(AnimTotN$country_eng_preload == "Cambodia",round(AnimTotN$e5_51/4075, digits = 2),
                         ifelse(AnimTotN$country_eng_preload == "Lao", round(AnimTotN$e5_51/17110, digits = 2),
                                round(AnimTotN$e5_51/23452, digits = 2)))
#We try to correct prices based on kind of animal to evaluate gross product
#summaryBy(e5_42 ~ e4_1+country_eng_preload, data = AnimTotN, FUN = c(mean, median, min, max, quantile), digits = 2, na.rm = T)
#Dumm <- AnimTotN[AnimTotN$e4_1 == "Buffalo",c(1:2,11:13)]
AnimTotN <- AnimTotN %>%
  add_column("e5_43" = AnimTotN$e5_4 * AnimTotN$e5_41, .after = "e5_41")
  
#Data long to wide
Animwide <- reshape(AnimTotN, direction = "wide", timevar = "e4_1", idvar = c("o9","country_eng_preload"))
colnames(Animwide)[1] <- "hhid_re2"
Animwide <- Animwide[,-2]
#We include these data in the previous table
dcropwide6 <- full_join(dcropwide5,Animwide, by = "hhid_re2")
#We add proper variable labels
for (i in 0:10){
  var_label(dcropwide6[,10762+25*i]) <- "Number of breeds"
  var_label(dcropwide6[,10763+25*i]) <- "Local breeds"
  var_label(dcropwide6[,10764+25*i]) <- "Number of local breeds"
  var_label(dcropwide6[,10765+25*i]) <- "Do you cross local breeds with other breeds"
  var_label(dcropwide6[,10766+25*i]) <- "Nb dead animals - past year"
  var_label(dcropwide6[,10767+25*i]) <- "Nb slaughter and self-consumed - past year"
  var_label(dcropwide6[,10768+25*i]) <- "Nb given - past year"
  var_label(dcropwide6[,10769+25*i]) <- "Nb sold - past year"
  var_label(dcropwide6[,10770+25*i]) <- "Average selling price $/kg"
  var_label(dcropwide6[,10771+25*i]) <- "Gross product $/year"
  var_label(dcropwide6[,10772+25*i]) <- "Amount kg/head sold - past year"
  var_label(dcropwide6[,10773+25*i]) <- "Animals bought - past year"
  var_label(dcropwide6[,10774+25*i]) <- "Average buying price $/kg"
  var_label(dcropwide6[,10775+25*i]) <- "Amount kg/head bought - past year"
  var_label(dcropwide6[,10776+25*i]) <- "Main outlet"
  var_label(dcropwide6[,10777+25*i]) <- "Proportion sold to the main outlet"
  var_label(dcropwide6[,10778+25*i]) <- "Provided by buyer - Nothing"
  var_label(dcropwide6[,10779+25*i]) <- "Provided by buyer - Inputs (sold)"
  var_label(dcropwide6[,10780+25*i]) <- "Provided by buyer - Inputs on credit"
  var_label(dcropwide6[,10781+25*i]) <- "Provided by buyer - Cash credit"
  var_label(dcropwide6[,10782+25*i]) <- "Provided by buyer - Technical advice/training"
  var_label(dcropwide6[,10783+25*i]) <- "Provided by buyer - Market information"
  var_label(dcropwide6[,10784+25*i]) <- "Provided by buyer - Regular sales"
  var_label(dcropwide6[,10785+25*i]) <- "Provided by buyer - Other"
  var_label(dcropwide6[,10786+25*i]) <- "Kind of contract"}
}

#b. Household scale
{
  #Other information at household scale
#Cambodia
AnAddC <- HouseholdCambodia_TF[,c(1,2028:2044,2113:2128,2130:2142,2146:2148,2150:2163,2166,2168:2169,
                          2171:2173,2175:2178,2180,2182,2184:2188,2190,2192,2194:2196,
                          2198:2211,2213:2215,2217:2225,2227:2231,2233,2235:2236,2238:2240,
                          2242:2245,2247,2249,2251:2255,2257,2259,2261:2263,2265:2273,
                          2275:2282,2284:2292,2294:2298,2300,2302:2303,2305,2307,
                          2309:2311,2313,2315,2317,2319:2322,2325,2327,2329:2341,2343:2355)]
AnAddC <- AnAddC %>% add_column("e26_11" = NA, "e26_12" = NA, "e26_13" = NA,
                                  "e26_199" = NA, .after = "e26")
Dummy <- c("Internal parasites,", "Tick,", "Worms", "Others")
for (i in 1:length(Dummy)){
  AnAddC[,69+i] <- as.character(AnAddC[,69+i])
  AnAddC[,69+i] <- ifelse(AnAddC[,74] == Dummy[i], "1", NA)
}
AnAddC <- AnAddC[,-74]
AnAddC <- AnAddC %>% relocate(e430, .before =  "e431")
#Laos
AnAddL <- HouseholdLaos_TF[,c(1,1845:1862,1925:1940,1942:1950,1952,1954:1956,1958:1966,1968,1970,
                      1972,1973,1975:1976,1978:1980,1982,1984:1987,1989,1991,1994:1997,
                      1999,2001,2003:2005,2007:2015,2017:2020,2022:2030,2032,2034,
                      2036:2037,2039:2041,2043:2046,2048,2050,2053:2056,2058,2060,
                      2062:2064,2066:2074,2076:2079,2081:2089,2091,2093,2095:2096,
                      2098,2100,2102:2105,2107,2109,2112:2115,2117,2119,2121:2133,
                      2135:2147)]
AnAddL$e3_14 <- rowSums(AnAddL[, c("e3_14", "e3_15")], na.rm = TRUE)
AnAddL <- AnAddL[,-c(16)]
AnAddL <- AnAddL %>% add_column("e1910" = NA, "e1911" = NA, "e1912" = NA,
                                .after = "e199")
AnAddL <- AnAddL %>% add_column("e2410" = NA, "e2411" = NA, "e2412" = NA,
                                "e2413" = NA, .after = "e249")
AnAddL <- AnAddL %>% add_column("e3410" = NA, "e3411" = NA, "e3412" = NA,
                                "e3413" = NA, .after = "e349")
AnAddL <- AnAddL %>% add_column("e3810" = NA, "e3811" = NA, "e3812" = NA,
                                "e3813" = NA, .after = "e3899")
AnAddL <- AnAddL %>% relocate(e29_10, .before =  "e29_2")
colnames(AnAddL)[80:83] <- c("e290","e291","e292","e293")
AnAddL <- AnAddL %>% add_column("e29_1" = NA, .after = "e293")
AnAddL <- AnAddL %>% relocate(e43_10, .before =  "e43_11")
colnames(AnAddL)[133:136] <- c("e430","e431","e432","e433")
AnAddL <- AnAddL %>% add_column("e43_1" = NA, .after = "e433")
AnAddL <- AnAddL %>% add_column("e4810" = NA, "e4811" = NA, "e4812" = NA,
                                "e4813" = NA, .after = "e4899")
AnAddL <- AnAddL %>% add_column("e5210" = NA, "e5211" = NA, "e5212" = NA,
                                "e5213" = NA, .after = "e5299")
colnames(AnAddL)[185:188] <- c("e571","e572","e573","e570")
#Vietnam
AnAddV <- HouseholdVietnam_TF[,c(1,1833:1850,1913:1928,1930:1938,1940,1942:1944,1946:1954,
                         1956,1958,1960:1961,1963:1965,1967:1970,1972,1974,1976:1980,
                         1982,1984,1986:1988,1990:1998,2000:2003,2005:2014,2017,2019:2020,
                         2022:2024,2026:2029,2031,2033,2035:2039,2041,2043,2045:2047,2049:2057,
                         2059:2062,2064:2072,2074,2076,2078:2079,2081,2083,
                         2085:2087,2089,2091,2093,2095:2098,2101,2103,2105:2117,2119:2131)]
AnAddV$e3_14 <- rowSums(AnAddV[, c("e3_14", "e3_15")], na.rm = TRUE)
AnAddV <- AnAddV[,-16]
AnAddV <- AnAddV %>% add_column("e1910" = NA, "e1911" = NA, "e1912" = NA,
                                .after = "e199")
AnAddV <- AnAddV %>% add_column("e2410" = NA, "e2411" = NA, "e2412" = NA,
                                "e2413" = NA, .after = "e249")
AnAddV <- AnAddV %>% add_column("e26_11" = NA, "e26_12" = NA, "e26_13" = NA,
                                "e26_199" = NA, .after = "e26")
for (i in 1:length(Dummy)){
  AnAddV[,69+i] <- as.character(AnAddV[,69+i])
  AnAddV[,69+i] <- ifelse(AnAddV[,74] == Dummy[i], "1", NA)
}
AnAddV <- AnAddV[,-74]
AnAddV <- AnAddV %>% add_column("e3410" = NA, "e3411" = NA, "e3412" = NA,
                                "e3413" = NA, .after = "e349")
AnAddV <- AnAddV %>% add_column("e3810" = NA, "e3811" = NA, "e3812" = NA,
                                "e3813" = NA, .after = "e3899")
AnAddV <- AnAddV %>% add_column("e4810" = NA, "e4811" = NA, "e4812" = NA,
                                "e4813" = NA, .after = "e4899")
AnAddV <- AnAddV %>% add_column("e5210" = NA, "e5211" = NA, "e5212" = NA,
                                "e5213" = NA, .after = "e5299")
AnAddV <- AnAddV %>% relocate(e570, .after =  "e573")
#Now we merge all the data
AnAddtot <- rbind(AnAddC,AnAddL,AnAddV)
#And no we'll check the homogeneity of the data in columns
#yesno  correction
for (i in c(19,22,24,26,28,30:31,34,50,65,69,79,89,106,121,125,
            132,152,159,174,184,190,203,216)){
  AnAddtot[,i] <- as.character(AnAddtot[,i])
  AnAddtot[,i] <- str_replace_all(AnAddtot[,i], c("1" = "Yes",
                                          "0" = "No"))
}
#e13_1
  AnAddtot[,29] <- as.character(AnAddtot[,29])
  AnAddtot[,29] <- str_replace_all(AnAddtot[,29], c("1" = "Synthetic fertilizer",
                                                    "2" = "Organic manure",
                                                    "3" = "Both above"))
#e16
for (i in c(32:33,48:49,87:88,104:105,140:141,157:158)){
  AnAddtot[,i] <- as.character(AnAddtot[,i])
  AnAddtot[,i] <- str_replace_all(AnAddtot[,i], paste0("^","Attached in the grazing area,","$"), "Attached in the grazing area, home yard without fence")
  AnAddtot[,i] <- str_replace_all(AnAddtot[,i], paste0("^","Confined in a shelter in the grazing area,","$"), "Confined in a shelter in the grazing area, or home yard with fence")
  AnAddtot[,i] <- str_replace_all(AnAddtot[,i], c("1" = "Confined in a barn,",
                                                  "2" = "Confined in a shelter in the grazing area, or home yard with fence",
                                                  "3" = "Attached in the grazing area, home yard without fence",
                                                  "4" = "Grazing with shepherd,",
                                                  "5" = "Free grazing without Shepherd",
                                                  "99" = "Other"))
                                                  
}
#e27a_1
for (i in c(77,84,130,137,182)){
  AnAddtot[,i] <- as.character(AnAddtot[,i])
  AnAddtot[,i] <- str_replace_all(AnAddtot[,i], c("1" = "Better for environment/ AE",
                                                    "2" = "Human health",
                                                    "88" = "Do not know",
                                                    "99" = "Others"))
  
}
  #e27a_2
for (i in c(78,85,131,138,183,189)){
  AnAddtot[,i] <- as.character(AnAddtot[,i])
  AnAddtot[,i] <- str_replace_all(AnAddtot[,i], c("1" = "Not available on the market,",
                                                    "2" = "Too expensive",
                                                    "3" = "Already include in the feed",
                                                    "88" = "Do not know",
                                                    "99" = "Others",
                                                    "No disease" = "No need, not severe disease"))
}
#e30
AnAddtot[,86] <- as.character(AnAddtot[,86])
AnAddtot[,86] <- str_replace_all(AnAddtot[,86], c("1" = "Fattening family raising using only local feed stuffs and kitchen waste",
                                                    "2" = "Fattening family raising, but buying feed from market",
                                                    "3" = "Piglet family raising using only local feed stuffs and kitchen waste",
                                                    "4" = "Piglet family raising, but buying feed from market",
                                                    "99" = "Other"))
#e40_1
for (i in c(126,178)){
  AnAddtot[,i] <- as.character(AnAddtot[,i])
  AnAddtot[,i] <- str_replace_all(AnAddtot[,i], c("1" = "Internal parasites,",
                                                      "2" = "Tick,",
                                                      "3" = "Worms",
                                                      "88" = "Do not know",
                                                      "99" = "Others"))
}

#e44
AnAddtot[,139] <- as.character(AnAddtot[,139])
AnAddtot[,139] <- str_replace_all(AnAddtot[,139], c("1" = "Family raising for consumption",
                                                    "2" = "Family raising but larger scale for selling meat",
                                                    "3" = "Family raising but larger scale for selling chick",
                                                    "4" = "Family raising but larger scale for selling eggs",
                                                    "5" = "Industrial (semi-industrial) broiler chicken",
                                                    "6" = "Industrial (semi-industrial) layer chicken",
                                                    "99" = "Other"))
#We add this table to the previous one
colnames(AnAddtot)[1] <- "hhid_re2"
dcropwide7 <- full_join(dcropwide6,AnAddtot, by = "hhid_re2")
}

#2.5. Adding of Remaining information at household scale (Technical performance, Socio-economic relative information...)
#Cambodia
{
SuppC <- HouseholdCambodia_TF[,c(1,105,107,109,111,113,115:116,118:138,
                                 142,144:156,158:169,171:185,188:194,196,199:204,
                                 206:209,213,215,217:218,447:448,450,461,
                                 463:464,466:479,482,484:492,494,496,498,500,
                                 503:513,516:519,691:706,708:752,
                                 771,773:774,776:777,779,781:784,786,2356,
                                 2358:2369,2371:2375,2378,2381:2382,2384:2395,
                                 2397:2404,2406:2407,2409,2411,2413:2415,2417)]
SuppC <- SuppC %>% relocate(c(b74,b75,b76,b77), .after = b720)
colnames(SuppC)[12:28] <- c("b76","b77","b78","b79","b710",
                            "b711","b712","b713","b714","b715",
                            "b716","b717","b718","b719","b720",
                            "b721","b722")
SuppC <- SuppC %>% add_column("b74" = NA, "b75" = NA, .after = "b73")
SuppC <- SuppC %>% relocate(c(b9_23,b9_26,b9_27), .after = b9_212)
colnames(SuppC)[60:70] <- c("b9_23","b9_24","b9_25","b9_26","b9_27",
                            "b9_28","b9_29","b9_210","b9_211","b9_212",
                            "b9_213")
colnames(SuppC)[91:94] <- c("b22_11","b22_12","b22_13","b22_14")
SuppC <- SuppC %>% add_column("b22_110" = NA, .after = "b22_14")
SuppC <- SuppC %>% relocate(c(c1_1,c1_2,c1_3,c1_4,c1_5,c1_6)
                              , .after = c1_13)
colnames(SuppC)[102:114] <- c("c1_1","c1_2","c1_3","c1_4","c1_5",
                              "c1_6","c1_7","c1_8","c1_9","c1_10",
                              "c1_11","c1_12","c1_13")
SuppC <- SuppC %>% add_column("c3x" = NA, .after = "c33")
colnames(SuppC)[120:126] <- c("c34","c35","c36","c37","c38",
                              "c39","c310")
SuppC <- SuppC %>% add_column("d710" = NA, .after = "d79")
SuppC <- SuppC %>% add_column("d712" = NA, "d713" = NA, .after = "d711")
SuppC <- SuppC %>% add_column("d715" = NA, .after = "d714")
SuppC <- SuppC %>% add_column("d7_101" = NA,"d7_102" = NA,
                              "d7_103" = NA, .after = "d7_93")
SuppC <- SuppC %>% add_column("d7_121" = NA, "d7_122" = NA,
                              "d7_123" = NA, "d7_131" = NA,
                              "d7_132" = NA, "d7_133" = NA,
                               .after = "d7_113")
SuppC <- SuppC %>% add_column("d7_151" = NA,"d7_152" = NA,
                              "d7_153" = NA, .after = "d7_143")

#Laos
SuppL <- HouseholdLaos_TF[,c(1,100,102,104,106,108,110,112,114:132,136,
                             138:150,152:163,165:175,178:184,186,189:194,
                             195:198,201,210,518:520,531,533:534,537:543,
                             545,547:553,555,557,559,562:571,574:577,
                             711:727,729:776,795,797,799,802:805,807,2148,
                             2150:2157,2159:2162,2164:2168,2171,2174:2175,
                             2177:2188,2190:2197,2199:2200,2202,2204,2206:2208,2210)]
SuppL <- SuppL %>% add_column("b719" = NA, "b720" = NA, "b721" = NA, "b722" = NA, .after = "b718")
SuppL <- SuppL %>% add_column("b9_210" = NA, "b9_211" = NA, "b9_212" = NA, "b9_213" = NA, .after = "b9_29")
SuppL <- SuppL %>% add_column("b22_12" = NA, "b22_13" = NA,"b22_14" = NA, .after = "b22_11")
SuppL <- SuppL %>% add_column("c1_7" = NA, "c1_8" = NA,"c1_9" = NA,"c1_10" = NA,
                              "c1_11" = NA, "c1_12" = NA, "c1_13" = NA, .after = "c1_6")
SuppL <- SuppL %>% add_column("c37" = NA, "c38" = NA,"c39" = NA,"c310" = NA,
                              .after = "c36")
SuppL <- SuppL %>% add_column("c910" = NA, .after = "c99")
SuppL <- SuppL %>% add_column("d716" = NA, "d717" = NA, "d718" = NA,
                              .after = "d715")
SuppL <- SuppL %>% add_column("d7_161" = NA,"d7_162" = NA,
                              "d7_163" = NA,"d7_171" = NA,
                              "d7_172" = NA,"d7_173" = NA,
                              "d7_181" = NA, "d7_182" = NA,
                              "d7_183" = NA, .after = "d7_153")
SuppL <- SuppL %>% add_column("d9_3" = NA, .after = "d9_1")
SuppL <- SuppL %>% add_column("d10_3" = NA, .after = "d10_1")
SuppL <- SuppL %>% add_column("d11_3" = NA, .after = "d11_1")

#Vietnam
SuppV <- HouseholdVietnam_TF[,c(1,105,107,109,111,113,115:116,118:136,140,142:154,
                                156:167,169:178,179,182:188,190,193:202,205,214,
                                522:524,535,537:538,540:546,549,551:557,
                                559,561,563,566:575,578:581,713:729,
                                731:778,797,799,801,804:807,809,2132,2134:2135,
                                2137:2146,2148:2152,2155,2158:2159,2161:2172,2174:2181,
                                2183:2184,2186,2188,2190:2192,2194)]
SuppV <- SuppV %>% add_column("b719" = NA, "b720" = NA, "b721" = NA, "b722" = NA, .after = "b718")
SuppV <- SuppV %>% add_column("b9_210" = NA, "b9_211" = NA, "b9_212" = NA, "b9_213" = NA, .after = "b9_29")
SuppV <- SuppV %>% add_column("b22_12" = NA, "b22_13" = NA,"b22_14" = NA, .after = "b22_11")
SuppV <- SuppV %>% add_column("c1_7" = NA, "c1_8" = NA,"c1_9" = NA,"c1_10" = NA,
                              "c1_11" = NA, "c1_12" = NA, "c1_13" = NA, .after = "c1_6")
SuppV <- SuppV %>% add_column("c37" = NA, "c38" = NA,"c39" = NA,"c310" = NA,
                              .after = "c36")
SuppV <- SuppV %>% add_column("c910" = NA, .after = "c99")
SuppV <- SuppV %>% add_column("d716" = NA, "d717" = NA, "d718" = NA,
                              .after = "d715")
SuppV <- SuppV %>% add_column("d7_161" = NA,"d7_162" = NA,
                              "d7_163" = NA,"d7_171" = NA,
                              "d7_172" = NA,"d7_173" = NA,
                              "d7_181" = NA, "d7_182" = NA,
                              "d7_183" = NA, .after = "d7_153")
SuppV <- SuppV %>% add_column("d9_3" = NA, .after = "d9_1")
SuppV <- SuppV %>% add_column("d10_3" = NA, .after = "d10_1")
SuppV <- SuppV %>% add_column("d11_3" = NA, .after = "d11_1")
#Now we merge the table of each country into one
SuppTot <- rbind(SuppC,SuppL,SuppV)
#Corrections and homogenization of columns content
#b4_1,b4_2,b4_3
for (i in 2:4){
  SuppTot[,i] <- as.character(SuppTot[,i])
  SuppTot[,i] <- str_replace_all(SuppTot[,i], paste0("^","1","$"), "Selling own vegetables")
  SuppTot[,i] <- str_replace_all(SuppTot[,i], paste0("^","2","$"), "Selling own fruits")
  SuppTot[,i] <- str_replace_all(SuppTot[,i], paste0("^","3","$"), "Selling rice from the farm")
  SuppTot[,i] <- str_replace_all(SuppTot[,i], c("10" = "Selling other crops from the farm",
                                                "11" = "Sell derived/processed products",
                                                "12" = "Non-farm wages (salaried work in private or public company)",
                                                "13" = "Non-farm income (own business: shop, trader/collector etc.)",
                                                "14" = "Selling labor",
                                                "15" = "Remittances",
                                                "16" = "Pension",
                                                "17" = "Rented land",
                                                "18" = "Financial support / gift",
                                                "99" = "Other  ${b3_2oth}",
                                                "4" = "Selling coffee from the farm",
                                                "5" = "Selling maize from the farm",
                                                "6" = "Selling other crops from the farm",
                                                "7" = "Selling cattle and buffalo",
                                                "8" = "Selling pigs",
                                                "9" = "Selling poultry"))
  SuppTot[,i] <- ifelse(SuppTot[,i] == "Non-farm wages/salary (salaried work in private or public company)",
                        "Non-farm wages (salaried work in private or public company)", SuppTot[,i])
}
#yes/no
for (i in c(8,32,45,96:101,142:145,173,176,179,182,185,188,194,
            197,206,221,223,225,227,234,241:242,244:246,252,
            254,279:280)){
  SuppTot[,i] <- as.character(SuppTot[,i])
  SuppTot[,i] <- str_replace_all(SuppTot[,i], c("1" = "Yes",
                                                "0" = "No",
                                                "no" = "No",
                                                "yes" = "Yes"))   
}
#b10
for (i in c(80,88)){
  SuppTot[,i] <- as.character(SuppTot[,i])
  SuppTot[,i] <- str_replace_all(SuppTot[,i], c("0" = "I do not sell (crops/vegetables/fruits or livestock)",
                                                  "1" = "The local market ( lower or equal to district level market)",
                                                  "2" = "The provincial or national market", 
                                                  "3" = "Export",
                                                  "88" = "Do not know",
                                                  "99" = "Other"))
}

#b11
for (i in c(87,90,253)){
  SuppTot[,i] <- as.character(SuppTot[,i])
  SuppTot[,i] <- str_replace_all(SuppTot[,i], paste0("^","2","$"), "25%-50%")
  SuppTot[,i] <- str_replace_all(SuppTot[,i], c("1" = "Less than 25%",
                                                "3" = "50-75%",
                                                "4" = "Over 75%",
                                                "88" = "Do not know"))   
}
#c2
  SuppTot[,116] <- as.character(SuppTot[,116])
  SuppTot[,116] <- str_replace_all(SuppTot[,116], c("1" = "Yes, one",
                                                "2" = "Yes, more than one",
                                                "0" = "No"))   
#c3a
  SuppTot[,128] <- as.character(SuppTot[,128])
  SuppTot[,128] <- str_replace_all(SuppTot[,128], c("1" = "Farmer organization on crops",
                                                "2" = "Farmer organization on fruits",
                                                "3" = "Farmer organization on livestock",
                                                "4" = "Farmer organization on honey",
                                                "5" = "Farmer organization on water",
                                                "6" = "Farmer organization on forest",
                                                "99" = "Other"))
  SuppTot[,128] <- ifelse(SuppTot[,128] == "${cFarmer organization on livestock_oth}", "Farmer organization on livestock", SuppTot[,128])

#c4
  SuppTot[,129] <- as.character(SuppTot[,129])
  SuppTot[,129] <- str_replace_all(SuppTot[,129], c("1" = "Farmer cooperative old style (under 2003 law)",
                                                "2" = "Farmer cooperative new style (under 2012 law)",
                                                "3" = "Farmer group",
                                                "4" = "President",
                                                "5" = "Treasurer",
                                                "6" = "Internal control person",
                                                "7" = "Trainer",
                                                "99" = "Other",
                                                "88" = "Do not know",
                                                "8" = "Collector",
                                                "9" = "Ordinary member"))

#c5
  SuppTot[,130] <- as.character(SuppTot[,130])
  SuppTot[,130] <- str_replace_all(SuppTot[,130], c("1" = "President",
                                                "2" = "Treasurer",
                                                "3" = "Internal control person",
                                                "4" = "Trainer",
                                                "5" = "Collector",
                                                "6" = "Ordinary member",
                                                "99" = "Other",
                                                "88" = "Do not know"))

#f3-f4
for (i in c(235,236)){
  SuppTot[,i] <- as.character(SuppTot[,i])
  SuppTot[,i] <- str_replace_all(SuppTot[,i], c("1" = "No time",
                                                "2" = "Very little time",
                                                "3" = "Moderate amount of time",
                                                "4" = "Almost enough time",
                                                "5" = "Sufficient amount of time",
                                                "88" = "Do not know"))
}
#g1-g2-g3
for (i in c(237:240)){
  SuppTot[,i] <- as.character(SuppTot[,i])
  SuppTot[,i] <- str_replace_all(SuppTot[,i], c("1" = "Myself alone",
                                                "2" = "Me in consultation with spouse/other family members",
                                                "3" = "My spouse/other family members",
                                                "88" = "Do not know"))
}
#h1
  SuppTot[,243] <- as.character(SuppTot[,243])
  SuppTot[,243] <- str_replace_all(SuppTot[,243], c("1" = "Yes, strongly",
                                                "2" = "Yes, maybe",
                                                "3" = "They should emigrate if they had the chance",
                                                "4" = "No, agriculture is not a good job",
                                                "88" = "Do not know"))
  
#i1
  SuppTot[,275] <- as.character(SuppTot[,275])
  SuppTot[,275] <- str_replace_all(SuppTot[,275], c("1" = "Happens every year or most years",
                                                "2" = "Happens sometimes but not regularly",
                                                "8" = "It was exceptional"))

#j1
  SuppTot[,276] <- as.character(SuppTot[,276])
  SuppTot[,276] <- str_replace_all(SuppTot[,276], c("1" = "Owned",
                                                "2" = "Rented",
                                                "3" = "Share cropping",
                                                "99" = "Other",
                                                "88" = "Do not know"))

#j2
  SuppTot[,277] <- as.character(SuppTot[,277])
  SuppTot[,277] <- str_replace_all(SuppTot[,277], c("1" = "Brick wall",
                                                "2" = "Concrete wall",
                                                "3" = "Wooden wall",
                                                "4" = "Bamboo, Thatch/leaves, Grass",
                                                "5" = "Galvanized iron or aluminium or other metal sheets",
                                                "6" = "Wood or logs",
                                                "99" = "Other"))

#j3
  SuppTot[,278] <- as.character(SuppTot[,278])
  SuppTot[,278] <- str_replace_all(SuppTot[,278], c("1" = "Thatch/leaves/grass",
                                                "2" = "Wood",
                                                "3" = "Fibrous cement",
                                                "4" = "Concrete, cement",
                                                "5" = "Brick tile roof",
                                                "6" = "Stone tile roof",
                                                "7" = "Metal/ tin roof",
                                                "99" = "Other"))

#j6
  SuppTot[,281] <- as.character(SuppTot[,281])
  SuppTot[,281] <- str_replace_all(SuppTot[,281], c("1" = "Private tap water",
                                                "2" = "Public water",
                                                "3" = "Drill",
                                                "4" = "Well",
                                                "5" = "Rain water",
                                                "6" = "Natural stream",
                                                "7" = "Water delivery",
                                                "8" = "Rain water",
                                                "99" = "Other"))

#j7
  SuppTot[,282] <- as.character(SuppTot[,282])
  SuppTot[,282] <- str_replace_all(SuppTot[,282], c("1" = "Grid electricity",
                                                "2" = "Private electricity",
                                                "3" = "Small hydroelectricity",
                                                "4" = "Own generator",
                                                "5" = "Battery",
                                                "6" = "Solar panel",
                                                "7" = "None, using kerosene/ candles",
                                                "99" = "Other"))

#We add labels 
  SuppTot <- labelled::copy_labels(HouseholdCambodia_TF,SuppTot)
  var_label(SuppTot[,c(27:30,67:70,91:95,102:114,117:126,146:164,
                       193:195,199:204,208:210)]) <-
                          c("Selling cassava from the farm","Selling cashew from the farm",
                          "Selling soybean from the farm","Selling cover crop seed from the farm",
                          "Reducing / postpone own farm work and sale labor to the others",
                          "Taking loan","Increase loan","Sold animal (cattle, buffalos…)",
                          "Vegetables","Rice","Cassava","Cashew nut","Tea","Women Union",
                          "Youth Union","Veteran group","Farmer union","Elderly group",
                          "Religious group","Local government","Agricultural Cooperative",
                          "Farmer association","Community forestry","Community Fisheries",
                          "Farmer Water User Community","Producer group / cluster",
                          "Farmer organization on crops","Farmer organization on fruits",
                          "Farmer organization on livestock","Farmer organization on honey", "Farmer organization on water",
                          "Farmer organization on forest", "Farmer organization on market",
                          "Farmer organization on credit", "Farmer organization on any type of mutual help",
                          "Farmer organization on diversified activities: crop, livestock, market, credit…etc.",
                          "None of forest product","Hunting","Fishing","Fuelwood","Mushrooms",
                          "Bamboo shoots","Bamboo poles","Broom grass","Honey","Rattan","Cardamom",
                          "Galangal","Damar Gum","Wild pepper","Medicinal plants","Paper mulberry black",
                          "Wooden poles","Collect wood oil","Leave for thatch roof",
                          "how many days did your household spend to collect Cardamom over 12 month",
                          "did your household sell collect Cardamom product in the last 12 months?",
                          "what was your household income from collecting Cardamom in the last 12 m",
                          "how many days did your household spend to collect Damar gum over 12 month",
                          "did your household sell collect Damar gum product in the last 12 months?",
                          "what was your household income from collecting Damar gum in the last 12 m",
                          "how many days did your household spend to collect Wild pepper over 12 month",
                          "did your household sell collect Wild pepper product in the last 12 months?",
                          "what was your household income from collecting Wild pepper in the last 12 m",
                          "how many days did your household spend to collect Pepper mulberry black over 12 month",
                          "did your household sell collect Pepper mulberry black product in the last 12 months?",
                          "what was your household income from collecting Pepper mulberry black in the last 12 m")
                          
  #We add this table to the previous one
  colnames(SuppTot)[1] <- "hhid_re2"
  dcropwide8 <- full_join(dcropwide7,SuppTot, by = "hhid_re2")
  }


#2.6. Database export
#Finally we add information about village, etc
dcropwide8 <- dcropwide8[,-2]
Couc <- HouseholdCambodia_TF[,c(1,18,21,24,27,30)]
Coul <- HouseholdLaos_TF[,c(1,17,20,23,26)]
Coul <- Coul %>% add_column("commune_eng_preload" = NA, .after = "district_eng_preload")
Couv <- HouseholdVietnam_TF[,c(1,18,21,24,27,30)]
Country <- rbind(Couc,Coul,Couv)
for (i in 2:6){
  Country[,i] <- as.character(Country[,i])
  Country[,i] <- str_replace_all(Country[,i], c("Xeingkhouang" = "Xeingkhouang province",
                                                "KHAM" = "Kham dist.", "PEK" = "Pek dist.",
                                                "Phoukoud" = "Phoukoud dist.", "Reaksmei" = "Reaksmei com.",
                                                "Rik Reay" = "Rik Reay com.", "Rohas" = "Rohas com.",
                                                "Rous Ran" = "Rous Ran com."))
  colnames(Country)[1] <- "hhid_re2"
}
dcropwide9 <- full_join(Country,dcropwide8, by = "hhid_re2")

#We add a "Study area" variable
dcropwide9$country_eng_preload <- as.character(dcropwide9$country_eng_preload)
dcropwide9$province_eng_preload <- as.character(dcropwide9$province_eng_preload)
dcropwide9 <- dcropwide9 %>% add_column("S_Area" = ifelse(dcropwide9$country_eng_preload == "Vietnam",
                                                                      dcropwide9$province_eng_preload,
                                                                      dcropwide9$country_eng_preload), .after = "hhid_re2")

#We add the sampling weights
dcropwide9 <- join(dcropwide9,SWeight, by = "village_eng_preload")
dcropwide9$SW_Weight <- as.character(dcropwide9$SW_Weight)
dcropwide9$SW_Weight <- as.numeric(dcropwide9$SW_Weight)

#We remove hhid without area specified
dcropwide9 <- dcropwide9[!is.na(dcropwide9$S_Area),]
#We check data type for each variable
dcropwide9 <- dcropwide9 %>% mutate_if(is.character, as.factor)
#And labels
dcropwide9 <- labelled::copy_labels(dcropwide8, dcropwide9)
dcropwide9 <- labelled::copy_labels(dcropwide7, dcropwide9)
dcropwide9 <- labelled::copy_labels(dcropwide6, dcropwide9)
dcropwide9 <- labelled::copy_labels(dcropwide5, dcropwide9)
dcropwide9 <- labelled::copy_labels(dcropwide4, dcropwide9)
dcropwide9 <- labelled::copy_labels(dcropwide3, dcropwide9)
dcropwide9 <- labelled::copy_labels(dcropwide2, dcropwide9)
dcropwide9 <- labelled::copy_labels(dcropwide, dcropwide9)
dcropwide9 <- labelled::copy_labels(HouseholdCambodia_TF, dcropwide9)
dcropwide9 <- labelled::copy_labels(SuppTot, dcropwide9)

## - d. Database export (Temporary)
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/2.ASSET_practices&Perf-Titouan2024/Data")
saveRDS(dcropwide9, "Newdata_TF.rds")
saveRDS(croplist, "croplist.rds")


