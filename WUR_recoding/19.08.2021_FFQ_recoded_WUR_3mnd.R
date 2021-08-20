###     SCRIPT: RECODING OF INFANT FFQ VARIABLE HEADERS FOR WUR - 3MND CODEBOOK
###     AUTHOR(S): SIOBHAN BRUSHETT 
###     DESCRIPTION: RECODE INFANT FFQ VARIABLE HEADERS FOR DOWNSTREAM WUR PROCESSING
###     PROJECT: NEXT-INFANT-FFQ
###     NOTE(S): 


##Contents of this file

## 0. LOAD DATA
## 1. M3 FFQs RECODING
## 2. CLEAN DATASET

library(tidyverse)

  ## =============== 0. LOAD DATA   ================ ##

m3 <-  read.csv("qs_data_M3_Baby_2021-07-23_15_18_03.csv", header=T, as.is=T, sep=",")
dim(m3)
#429 200

#CONTINUE FROM HERE AFTER CONFIRMATION

  ## =============== 1. M3 FFQs RECODING ================ ##

c("CHFFQ1",
  "CHFFQ1TXT",
  "CHFFQ2",
  "CHFFQ3",
  "CHFFQ4",
  "CHFFQ5",
  "CHFFQ5TXT",
  "CHFFQ12",
  "CHFFQ12A",
  "CHFFQ12A.1",
  "CHFFQ12A.2",
  "CHFFQ12A.3",
  "CHFFQ12A.4",
  "CHFFQ12A.5",
  "CHFFQ12A.6",
  "CHFFQ12A.7",
  "CHFFQ12A.8",
  "CHFFQ12A1TXT",
  "CHFFQ12A.9",
  "CHFFQ12A2TXT",
  "CHFFQ13",
  "CHFFQ14",
  "CHFFQ15",
  "XCHFFQ16",
  "XCHFFQ17",
  "XCHFFQ18",
  "CHFFQ6",
  "CHFFQ7",
  "XCHFFQ8",
  "CHFFQ9",
  "CHFFQ10",
  "XCHFFQ11") %in% colnames(m3)

m3_wur <- m3 [ , c("NEXT_NR", 
                   "CHFFQ1", #feeding mode method
                   "CHFFQ1TXT", #feeding mode method - other
                   "CHFFQ2", #how much milk - frequency
                   "CHFFQ3", #EBF
                   "CHFFQ4", #BF frequency
                   "CHFFQ5", #formula milk type
                   "CHFFQ5TXT", ##how much milk - frequency
                   "CHFFQ12", #follow-on milk - addition
                   "CHFFQ12A", #follow-on milk - rice flour
                   "CHFFQ12A.1", #follow-on milk - cereals/grains
                   "CHFFQ12A.2", #follow-on milk - glucose
                   "CHFFQ12A.3", #follow-on milk - sugar
                   "CHFFQ12A.4", #follow-on milk - calcium
                   "CHFFQ12A.5", #follow-on milk - biscuits
                   "CHFFQ12A.6", #follow-on milk - fruit/fruit juice
                   "CHFFQ12A.7", #follow-on milk - iron droplets
                   "CHFFQ12A.8", #follow-on milk - vegetables
                   "CHFFQ12A1TXT", #follow-on milk - vegetables (string)
                   "CHFFQ12A.9", #follow-on milk - other
                   "CHFFQ12A2TXT", #follow-on milk - other (string)
                   "CHFFQ13", #complementary food introduction
                   "CHFFQ14", #complementary food introduction - infant age (weeks)
                   "CHFFQ15", #complementary food - frequency
                   "XCHFFQ16", #first foods
                   "XCHFFQ17", #first foods - reasoning
                   "XCHFFQ18", #first foods - behaviour
                   "CHFFQ6", #vitamin D
                   "CHFFQ7", #vitamin D - amount
                   "XCHFFQ8", #vitamin D - brand
                   "CHFFQ9", #vitamin K
                   "CHFFQ10", #vitamin K - amount
                   "XCHFFQ11")] #vitamin K - brand


dim(m3_wur)
#429  33

#EXPANSION OF CHFFQ5 VARIABLE
#CHFFQ5 needs to be expanded into variables v05a1 - v05a19 to coincide with WUR codebook
table(m3_wur$CHFFQ5, useNA = "ifany")

#4    5    6    7   10   11   13   14   15   16   19 <NA>
#2   12    5    1    6    4    4    7    8   74   89  217

#SUM of NonNAs: 212


m3_wur$v05a1 <- NA
m3_wur$v05a1 [!is.na(m3_wur$CHFFQ5)] <- 0 #not filled in
m3_wur$v05a1 [m3_wur$CHFFQ5 == 1] <- 1 #filled in
table(m3_wur$v05a1)
#0
#212

m3_wur$v05a2 <- NA
m3_wur$v05a2 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a2 [m3_wur$CHFFQ5 == 2] <- 1 
table(m3_wur$v05a2)

m3_wur$v05a3 <- NA
m3_wur$v05a3 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a3 [m3_wur$CHFFQ5 == 3] <- 1 
table(m3_wur$v05a3)

m3_wur$v05a4 <- NA
m3_wur$v05a4 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a4 [m3_wur$CHFFQ5 == 4] <- 1 
table(m3_wur$v05a4)

m3_wur$v05a5 <- NA
m3_wur$v05a5 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a5 [m3_wur$CHFFQ5 == 5] <- 1 
table(m3_wur$v05a5)

m3_wur$v05a6 <- NA
m3_wur$v05a6 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a6 [m3_wur$CHFFQ5 == 6] <- 1 
table(m3_wur$v05a6)

m3_wur$v05a7 <- NA
m3_wur$v05a7 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a7 [m3_wur$CHFFQ5 == 7] <- 1 
table(m3_wur$v05a7)

m3_wur$v05a8 <- NA
m3_wur$v05a8 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a8 [m3_wur$CHFFQ5 == 8] <- 1 
table(m3_wur$v05a8)

m3_wur$v05a9 <- NA
m3_wur$v05a9 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a9 [m3_wur$CHFFQ5 == 9] <- 1 
table(m3_wur$v05a9)

m3_wur$v05a10 <- NA
m3_wur$v05a10 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a10 [m3_wur$CHFFQ5 == 10] <- 1 
table(m3_wur$v05a10)

m3_wur$v05a11 <- NA
m3_wur$v05a11 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a11 [m3_wur$CHFFQ5 == 11] <- 1 
table(m3_wur$v05a11)

m3_wur$v05a12 <- NA
m3_wur$v05a12 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a12 [m3_wur$CHFFQ5 == 12] <- 1 
table(m3_wur$v05a12)

m3_wur$v05a13 <- NA
m3_wur$v05a13 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a13 [m3_wur$CHFFQ5 == 13] <- 1 
table(m3_wur$v05a13)

m3_wur$v05a14 <- NA
m3_wur$v05a14 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a14 [m3_wur$CHFFQ5 == 14] <- 1 
table(m3_wur$v05a14)

m3_wur$v05a15 <- NA
m3_wur$v05a15 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a15 [m3_wur$CHFFQ5 == 15] <- 1 
table(m3_wur$v05a15)

m3_wur$v05a16 <- NA
m3_wur$v05a16 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a16 [m3_wur$CHFFQ5 == 16] <- 1 
table(m3_wur$v05a16)

m3_wur$v05a17 <- NA
m3_wur$v05a17 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a17 [m3_wur$CHFFQ5 == 17] <- 1 
table(m3_wur$v05a17)

m3_wur$v05a18 <- NA
m3_wur$v05a18 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a18 [m3_wur$CHFFQ5 == 18] <- 1 
table(m3_wur$v05a18)

m3_wur$v05a19 <- NA
m3_wur$v05a19 [!is.na(m3_wur$CHFFQ5)] <- 0
m3_wur$v05a19 [m3_wur$CHFFQ5 == 19] <- 1 
table(m3_wur$v05a19)

m3_wur <- m3_wur %>% rename("v01_M3"="CHFFQ1",
                            "v01_anders_M3"="CHFFQ1TXT",
                            "v02_M3"="CHFFQ2",
                            "v03_M3"="CHFFQ3",
                            "v04_M3"="CHFFQ4",
                            "v05a1_M3"="v05a1",
                            "v05a2_M3"="v05a2",
                            "v05a3_M3"="v05a3",
                            "v05a4_M3"="v05a4",
                            "v05a5_M3"="v05a5",
                            "v05a6_M3"="v05a6",
                            "v05a7_M3"="v05a7",
                            "v05a8_M3"="v05a8",
                            "v05a9_M3"="v05a9",
                            "v05a10_M3"="v05a10",
                            "v05a11_M3"="v05a11",
                            "v05a12_M3"="v05a12",
                            "v05a13_M3"="v05a13",
                            "v05a14_M3"="v05a14",
                            "v05a15_M3"="v05a15",
                            "v05a16_M3"="v05a16",
                            "v05a17_M3"="v05a17",
                            "v05a18_M3"="v05a18",
                            "v05a19_M3"="v05a19",
                            "v05a19_anders_M3"="CHFFQ5TXT",
                            "v06_M3"="CHFFQ12",
                            "v06_namelijk1_M3"="CHFFQ12A",
                            "v06_namelijk2_M3"="CHFFQ12A.1",
                            "v06_namelijk3_M3"="CHFFQ12A.2",
                            "v06_namelijk4_M3"="CHFFQ12A.3",
                            "v06_namelijk5_M3"="CHFFQ12A.4",
                            "v06_namelijk7_M3"="CHFFQ12A.5",
                            "v06_namelijk8_M3"="CHFFQ12A.6",
                            "v06_namelijk6_M3"="CHFFQ12A.7",
                            "v06_namelijk9_M3"="CHFFQ12A.8",
                            "v06_namelijk9_groenten_M3"="CHFFQ12A1TXT",
                            "v06_namelijk_anders_M3"="CHFFQ12A.9",
                            "v06_namelijk_anders_namelijk_M3"="CHFFQ12A2TXT",
                            "v13_M3"="CHFFQ13",
                            "v13_weken_M3"="CHFFQ14",
                            "v14_M3"="CHFFQ15",
                            "v15_M3"="XCHFFQ16",
                            "v16_M3"="XCHFFQ17",
                            "v17_M3"="XCHFFQ18",
                            "v07_M3"="CHFFQ6",
                            "v08_M3"="CHFFQ7",
                            "v09_M3"="XCHFFQ8",
                            "v10_M3"="CHFFQ9",
                            "v11_M3"="CHFFQ10",
                            "v12_M3"="XCHFFQ11")
colnames(m3_wur)

m3_wur <- subset(m3_wur, select=-CHFFQ5)
dim(m3_wur)
#429  51

m3_wur <- select(m3_wur, NEXT_NR, v01_M3,
                 v01_anders_M3,
                 v02_M3,
                 v03_M3,
                 v04_M3,
                 v05a1_M3,
                 v05a2_M3,
                 v05a3_M3,
                 v05a4_M3,
                 v05a5_M3,
                 v05a6_M3,
                 v05a7_M3,
                 v05a8_M3,
                 v05a9_M3,
                 v05a10_M3,
                 v05a11_M3,
                 v05a12_M3,
                 v05a13_M3,
                 v05a14_M3,
                 v05a15_M3,
                 v05a16_M3,
                 v05a17_M3,
                 v05a18_M3,
                 v05a19_M3,
                 v05a19_anders_M3,
                 v06_M3,
                 v06_namelijk1_M3,
                 v06_namelijk2_M3,
                 v06_namelijk3_M3,
                 v06_namelijk4_M3,
                 v06_namelijk5_M3,
                 v06_namelijk6_M3,
                 v06_namelijk7_M3,
                 v06_namelijk8_M3,
                 v06_namelijk9_M3,
                 v06_namelijk9_groenten_M3,
                 v06_namelijk_anders_M3,
                 v06_namelijk_anders_namelijk_M3,
                 v07_M3,
                 v08_M3,
                 v09_M3,
                 v10_M3,
                 v11_M3,
                 v12_M3,
                 v13_M3,
                 v13_weken_M3,
                 v14_M3,
                 v15_M3,
                 v16_M3,
                 v17_M3)
str(m3_wur)

write.csv(m3_wur, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/FFQs_by_time_point/19.08.2021_M3_WUR.csv")

  ## =============== 2. CLEAN AND MERGE DATASETS   ================ ##

m3 <-  read.csv("19.08.2021_M3_WUR.csv", header=T, as.is=T, sep=",", row.names = 1)
dim(m3)

#ADD LEADING ZEROS BACK TO NEXT_NR 
m3$NEXT_NR = formatC(m3$NEXT_NR, width = 6, format = "d", flag = "0")

write.csv(m3, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/files_for_WUR/20.08.2021_WUR_3mnd.csv")

