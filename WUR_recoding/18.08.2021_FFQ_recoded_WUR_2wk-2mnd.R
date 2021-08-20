###     SCRIPT: RECODING OF INFANT FFQ VARIABLE HEADERS FOR WUR - 2WK-2MND CODEBOOK
###     AUTHOR(S): SIOBHAN BRUSHETT 
###     DESCRIPTION: RECODE INFANT FFQ VARIABLE HEADERS FOR DOWNSTREAM WUR PROCESSING
###     PROJECT: NEXT-INFANT-FFQ
###     NOTE(S): 


##Contents of this file

## 0. LOAD DATA
## 1. W2 FFQs RECODING
## 2. M1 FFQs RECODING
## 3. M2 FFQs RECODING
## 4. CLEAN AND MERGE DATASETS

library(tidyverse)

        ## =============== 0. LOAD DATA   ================ ##

w2 <-  read.csv("qs_data_W2_Baby_2021-07-26_09_29_25.csv", header=T, as.is=T, sep=",")
dim(w2)
#252 107

m1 <-  read.csv("qs_data_M1_Baby_2021-07-26_09_32_03.csv", header=T, as.is=T, sep=",")
dim(m1)
#523 185

m2 <-  read.csv("qs_data_M2_Baby_2021-07-26_09_33_18.csv", header=T, as.is=T, sep=",")
dim(m2)
#455 168

        ## =============== 1. W2 FFQs RECODING ================ ##

w2_wur <- w2 [ , c("NEXT_NR", 
                   "v_78", #feeding mode method
                   "v_79", #feeding mode method - other
                   "v_87", #how much milk - frequency
                   "v_88", #EBF
                   "v_89", #BF frequency
                   "v_90", #formula milk type
                   "v_91", #formula milk type - other
                   "v_92", #vitamin D 
                   "v_293",#vitamin D - amount
                   "v_98", #vitamin D -brand
                   "v_99", #vitamin K
                   "v_298",#vitamin K - amount
                   "v_101")] #vitamin K - brand

dim(w2_wur)
#252 14

#EXPANSION OF V_90 VARIABLE
#v_90 needs to be expanded into variables v05a1 - v05a19 to coincide with WUR codebook
table(w2_wur$v_90, useNA = "ifany")

# 5    6   10   12   13   15   16   19 <NA>
#8    1    4    1    5    1   48   25  159

#SUM of NonNAs: 93

w2_wur$v05a1 <- NA
w2_wur$v05a1 [!is.na(w2_wur$v_90)] <- 0 #not filled in
w2_wur$v05a1 [w2_wur$v_90 == 1] <- 1 #filled in
table(w2_wur$v05a1)
#0
#93

w2_wur$v05a2 <- NA
w2_wur$v05a2 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a2 [w2_wur$v_90 == 2] <- 1 
table(w2_wur$v05a2)

w2_wur$v05a3 <- NA
w2_wur$v05a3 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a3 [w2_wur$v_90 == 3] <- 1 
table(w2_wur$v05a3)

w2_wur$v05a4 <- NA
w2_wur$v05a4 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a4 [w2_wur$v_90 == 4] <- 1 
table(w2_wur$v05a4)

w2_wur$v05a5 <- NA
w2_wur$v05a5 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a5 [w2_wur$v_90 == 5] <- 1 
table(w2_wur$v05a5)
#0  1
#85  8

w2_wur$v05a6 <- NA
w2_wur$v05a6 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a6 [w2_wur$v_90 == 6] <- 1 
table(w2_wur$v05a6)

w2_wur$v05a7 <- NA
w2_wur$v05a7 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a7 [w2_wur$v_90 == 7] <- 1 
table(w2_wur$v05a7)

w2_wur$v05a8 <- NA
w2_wur$v05a8 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a8 [w2_wur$v_90 == 8] <- 1 
table(w2_wur$v05a8)
                  
w2_wur$v05a9 <- NA
w2_wur$v05a9 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a9 [w2_wur$v_90 == 9] <- 1 
table(w2_wur$v05a9)

w2_wur$v05a10 <- NA
w2_wur$v05a10 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a10 [w2_wur$v_90 == 10] <- 1 
table(w2_wur$v05a10)

w2_wur$v05a11 <- NA
w2_wur$v05a11 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a11 [w2_wur$v_90 == 11] <- 1 
table(w2_wur$v05a11)

w2_wur$v05a12 <- NA
w2_wur$v05a12 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a12 [w2_wur$v_90 == 12] <- 1 
table(w2_wur$v05a12)

w2_wur$v05a13 <- NA
w2_wur$v05a13 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a13 [w2_wur$v_90 == 13] <- 1 
table(w2_wur$v05a13)

w2_wur$v05a14 <- NA
w2_wur$v05a14 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a14 [w2_wur$v_90 == 14] <- 1 
table(w2_wur$v05a14)

w2_wur$v05a15 <- NA
w2_wur$v05a15 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a15 [w2_wur$v_90 == 15] <- 1 
table(w2_wur$v05a15)

w2_wur$v05a16 <- NA
w2_wur$v05a16 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a16 [w2_wur$v_90 == 16] <- 1 
table(w2_wur$v05a16)

w2_wur$v05a17 <- NA
w2_wur$v05a17 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a17 [w2_wur$v_90 == 17] <- 1 
table(w2_wur$v05a17)

w2_wur$v05a18 <- NA
w2_wur$v05a18 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a18 [w2_wur$v_90 == 18] <- 1 
table(w2_wur$v05a18)

w2_wur$v05a19 <- NA
w2_wur$v05a19 [!is.na(w2_wur$v_90)] <- 0
w2_wur$v05a19 [w2_wur$v_90 == 19] <- 1 
table(w2_wur$v05a19)

w2_wur <- w2_wur %>% rename("v01_W2"="v_78",
                            "v01_anders_W2"="v_79",
                            "v02_W2"="v_87",
                            "v03_W2"="v_88",
                            "v04_W2"="v_89",
                            "v05a1_W2"="v05a1",
                            "v05a2_W2"="v05a2",
                            "v05a3_W2"="v05a3",
                            "v05a4_W2"="v05a4",
                            "v05a5_W2"="v05a5",
                            "v05a6_W2"="v05a6",
                            "v05a7_W2"="v05a7",
                            "v05a8_W2"="v05a8",
                            "v05a9_W2"="v05a9",
                            "v05a10_W2"="v05a10",
                            "v05a11_W2"="v05a11",
                            "v05a12_W2"="v05a12",
                            "v05a13_W2"="v05a13",
                            "v05a14_W2"="v05a14",
                            "v05a15_W2"="v05a15",
                            "v05a16_W2"="v05a16",
                            "v05a17_W2"="v05a17",
                            "v05a18_W2"="v05a18",
                            "v05a19_W2"="v05a19",
                            "v05a19_anders_W2"="v_91",
                            "v06_W2"="v_92",
                            "v07_W2"="v_293",
                            "v_08_W2"="v_98",
                            "v_09_W2"="v_99",
                            "v_10_W2"="v_298",
                            "v_11_W2"="v_101")
colnames(w2_wur)

w2_wur <- subset(w2_wur, select=-v_90)
dim(w2_wur)
#252  32

w2_wur <- select(w2_wur, NEXT_NR, v01_W2, v01_anders_W2, v02_W2, v03_W2, v04_W2,
                 v05a1_W2,
                 v05a2_W2,
                 v05a3_W2,
                 v05a4_W2,
                 v05a5_W2,
                 v05a6_W2,
                 v05a7_W2,
                 v05a8_W2,
                 v05a9_W2,
                 v05a10_W2,
                 v05a11_W2,
                 v05a12_W2,
                 v05a13_W2,
                 v05a14_W2,
                 v05a15_W2,
                 v05a16_W2,
                 v05a17_W2,
                 v05a18_W2,
                 v05a19_W2,
                 everything())

str(w2_wur)

write.csv(w2_wur, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/FFQs_by_time_point/18.08.2021_W2_WUR.csv")

        ## =============== 2. M1 FFQs RECODING ================ ##

c("CHFFQ1",
  "CHFFQ1TXT",
  "CHFFQ2", 
  "CHFFQ3", 
  "CHFFQ4", 
  "CHFFQ5",
  "CHFFQ5TXT",
  "CHFFQ6", 
  "CHFFQ7",
  "XCHFFQ8",
  "CHFFQ9", 
  "CHFFQ10",
  "XCHFFQ11") %in% colnames(m1)


m1_wur <- m1 [ , c("NEXT_NR", 
                   "CHFFQ1", #feeding mode method
                   "CHFFQ1TXT", #feeding mode method - other
                   "CHFFQ2", #how much milk - frequency
                   "CHFFQ3", #EBF
                   "CHFFQ4", #BF frequency
                   "CHFFQ5", #formula milk type
                   "CHFFQ5TXT", #formula milk type - other
                   "CHFFQ6", #vitamin D
                   "CHFFQ7", #vitamin D - amount
                   "XCHFFQ8", #vitamin D - brand
                   "CHFFQ9",  #vitamin K 
                   "CHFFQ10", #vitamin K - amount
                   "XCHFFQ11")] #vitamin K - brand

dim(m1_wur)
#523  14

#EXPANSION OF CHFFQ5 VARIABLE
#CHFFQ5 needs to be expanded into variables v05a1 - v05a19 to coincide with WUR codebook
table(m1_wur$CHFFQ5, useNA = "ifany")

#2    5    6   10   11   12   13   14   15   16   19 <NA>
#1   20    5    6    5    3    8    5    4   75  140  251

#SUM of NonNAs: 272


m1_wur$v05a1 <- NA
m1_wur$v05a1 [!is.na(m1_wur$CHFFQ5)] <- 0 #not filled in
m1_wur$v05a1 [m1_wur$CHFFQ5 == 1] <- 1 #filled in
table(m1_wur$v05a1)
#0
#272

m1_wur$v05a2 <- NA
m1_wur$v05a2 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a2 [m1_wur$CHFFQ5 == 2] <- 1 
table(m1_wur$v05a2)
#0   1
#271   1

m1_wur$v05a3 <- NA
m1_wur$v05a3 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a3 [m1_wur$CHFFQ5 == 3] <- 1 
table(m1_wur$v05a3)

m1_wur$v05a4 <- NA
m1_wur$v05a4 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a4 [m1_wur$CHFFQ5 == 4] <- 1 
table(m1_wur$v05a4)

m1_wur$v05a5 <- NA
m1_wur$v05a5 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a5 [m1_wur$CHFFQ5 == 5] <- 1 
table(m1_wur$v05a5)

m1_wur$v05a6 <- NA
m1_wur$v05a6 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a6 [m1_wur$CHFFQ5 == 6] <- 1 
table(m1_wur$v05a6)

m1_wur$v05a7 <- NA
m1_wur$v05a7 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a7 [m1_wur$CHFFQ5 == 7] <- 1 
table(m1_wur$v05a7)

m1_wur$v05a8 <- NA
m1_wur$v05a8 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a8 [m1_wur$CHFFQ5 == 8] <- 1 
table(m1_wur$v05a8)

m1_wur$v05a9 <- NA
m1_wur$v05a9 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a9 [m1_wur$CHFFQ5 == 9] <- 1 
table(m1_wur$v05a9)

m1_wur$v05a10 <- NA
m1_wur$v05a10 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a10 [m1_wur$CHFFQ5 == 10] <- 1 
table(m1_wur$v05a10)

m1_wur$v05a11 <- NA
m1_wur$v05a11 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a11 [m1_wur$CHFFQ5 == 11] <- 1 
table(m1_wur$v05a11)

m1_wur$v05a12 <- NA
m1_wur$v05a12 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a12 [m1_wur$CHFFQ5 == 12] <- 1 
table(m1_wur$v05a12)

m1_wur$v05a13 <- NA
m1_wur$v05a13 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a13 [m1_wur$CHFFQ5 == 13] <- 1 
table(m1_wur$v05a13)

m1_wur$v05a14 <- NA
m1_wur$v05a14 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a14 [m1_wur$CHFFQ5 == 14] <- 1 
table(m1_wur$v05a14)

m1_wur$v05a15 <- NA
m1_wur$v05a15 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a15 [m1_wur$CHFFQ5 == 15] <- 1 
table(m1_wur$v05a15)

m1_wur$v05a16 <- NA
m1_wur$v05a16 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a16 [m1_wur$CHFFQ5 == 16] <- 1 
table(m1_wur$v05a16)

m1_wur$v05a17 <- NA
m1_wur$v05a17 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a17 [m1_wur$CHFFQ5 == 17] <- 1 
table(m1_wur$v05a17)

m1_wur$v05a18 <- NA
m1_wur$v05a18 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a18 [m1_wur$CHFFQ5 == 18] <- 1 
table(m1_wur$v05a18)

m1_wur$v05a19 <- NA
m1_wur$v05a19 [!is.na(m1_wur$CHFFQ5)] <- 0
m1_wur$v05a19 [m1_wur$CHFFQ5 == 19] <- 1 
table(m1_wur$v05a19)

m1_wur <- m1_wur %>% rename("v01_M1"="CHFFQ1",
                            "v01_anders_M1"="CHFFQ1TXT",
                            "v02_M1"="CHFFQ2",
                            "v03_M1"="CHFFQ3",
                            "v04_M1"="CHFFQ4",
                            "v05a1_M1"="v05a1",
                            "v05a2_M1"="v05a2",
                            "v05a3_M1"="v05a3",
                            "v05a4_M1"="v05a4",
                            "v05a5_M1"="v05a5",
                            "v05a6_M1"="v05a6",
                            "v05a7_M1"="v05a7",
                            "v05a8_M1"="v05a8",
                            "v05a9_M1"="v05a9",
                            "v05a10_M1"="v05a10",
                            "v05a11_M1"="v05a11",
                            "v05a12_M1"="v05a12",
                            "v05a13_M1"="v05a13",
                            "v05a14_M1"="v05a14",
                            "v05a15_M1"="v05a15",
                            "v05a16_M1"="v05a16",
                            "v05a17_M1"="v05a17",
                            "v05a18_M1"="v05a18",
                            "v05a19_M1"="v05a19",
                            "v05a19_anders_M1"="CHFFQ5TXT",
                            "v06_M1"="CHFFQ6",
                            "v07_M1"="CHFFQ7",
                            "v_08_M1"="XCHFFQ8",
                            "v_09_M1"="CHFFQ9",
                            "v_10_M1"="CHFFQ10",
                            "v_11_M1"="XCHFFQ11")
colnames(m1_wur)

m1_wur <- subset(m1_wur, select=-CHFFQ5)
dim(m1_wur)
#523  32

m1_wur <- select(m1_wur, NEXT_NR, v01_M1, v01_anders_M1, v02_M1, v03_M1, v04_M1,
                 v05a1_M1,
                 v05a2_M1,
                 v05a3_M1,
                 v05a4_M1,
                 v05a5_M1,
                 v05a6_M1,
                 v05a7_M1,
                 v05a8_M1,
                 v05a9_M1,
                 v05a10_M1,
                 v05a11_M1,
                 v05a12_M1,
                 v05a13_M1,
                 v05a14_M1,
                 v05a15_M1,
                 v05a16_M1,
                 v05a17_M1,
                 v05a18_M1,
                 v05a19_M1,
                 everything())
str(m1_wur)

write.csv(m1_wur, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/FFQs_by_time_point/18.08.2021_M1_WUR.csv")

        ## =============== 3. M2 FFQs RECODING ================ ##

c("CHFFQ1",
  "CHFFQ1TXT",
  "CHFFQ2", 
  "CHFFQ3", 
  "CHFFQ4", 
  "CHFFQ5",
  "CHFFQ5TXT",
  "CHFFQ6", 
  "CHFFQ7",
  "XCHFFQ8",
  "CHFFQ9", 
  "CHFFQ10",
  "XCHFFQ11") %in% colnames(m2)


m2_wur <- m2 [ , c("NEXT_NR", 
                   "CHFFQ1", #feeding mode method
                   "CHFFQ1TXT", #feeding mode method - other
                   "CHFFQ2", #how much milk - frequency
                   "CHFFQ3", #EBF
                   "CHFFQ4", #BF frequency
                   "CHFFQ5", #formula milk type
                   "CHFFQ5TXT", #formula milk type - other
                   "CHFFQ6", #vitamin D
                   "CHFFQ7", #vitamin D - amount
                   "XCHFFQ8", #vitamin D - brand
                   "CHFFQ9",  #vitamin K 
                   "CHFFQ10", #vitamin K - amount
                   "XCHFFQ11")] #vitamin K - brand

dim(m2_wur)
#455  14

#EXPANSION OF CHFFQ5 VARIABLE
#CHFFQ5 needs to be expanded into variables v05a1 - v05a19 to coincide with WUR codebook
table(m2_wur$CHFFQ5, useNA = "ifany")

#5    6   10   11   13   14   15   16   19 <NA>
#16    7    6    6    6   10    3   63   77  261

#SUM of NonNAs: 194


m2_wur$v05a1 <- NA
m2_wur$v05a1 [!is.na(m2_wur$CHFFQ5)] <- 0 #not filled in
m2_wur$v05a1 [m2_wur$CHFFQ5 == 1] <- 1 #filled in
table(m2_wur$v05a1)
#0
#194

m2_wur$v05a2 <- NA
m2_wur$v05a2 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a2 [m2_wur$CHFFQ5 == 2] <- 1 
table(m2_wur$v05a2)

m2_wur$v05a3 <- NA
m2_wur$v05a3 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a3 [m2_wur$CHFFQ5 == 3] <- 1 
table(m2_wur$v05a3)

m2_wur$v05a4 <- NA
m2_wur$v05a4 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a4 [m2_wur$CHFFQ5 == 4] <- 1 
table(m2_wur$v05a4)

m2_wur$v05a5 <- NA
m2_wur$v05a5 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a5 [m2_wur$CHFFQ5 == 5] <- 1 
table(m2_wur$v05a5)
#0   1
#178  16

m2_wur$v05a6 <- NA
m2_wur$v05a6 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a6 [m2_wur$CHFFQ5 == 6] <- 1 
table(m2_wur$v05a6)

m2_wur$v05a7 <- NA
m2_wur$v05a7 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a7 [m2_wur$CHFFQ5 == 7] <- 1 
table(m2_wur$v05a7)

m2_wur$v05a8 <- NA
m2_wur$v05a8 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a8 [m2_wur$CHFFQ5 == 8] <- 1 
table(m2_wur$v05a8)

m2_wur$v05a9 <- NA
m2_wur$v05a9 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a9 [m2_wur$CHFFQ5 == 9] <- 1 
table(m2_wur$v05a9)

m2_wur$v05a10 <- NA
m2_wur$v05a10 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a10 [m2_wur$CHFFQ5 == 10] <- 1 
table(m2_wur$v05a10)

m2_wur$v05a11 <- NA
m2_wur$v05a11 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a11 [m2_wur$CHFFQ5 == 11] <- 1 
table(m2_wur$v05a11)

m2_wur$v05a12 <- NA
m2_wur$v05a12 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a12 [m2_wur$CHFFQ5 == 12] <- 1 
table(m2_wur$v05a12)

m2_wur$v05a13 <- NA
m2_wur$v05a13 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a13 [m2_wur$CHFFQ5 == 13] <- 1 
table(m2_wur$v05a13)

m2_wur$v05a14 <- NA
m2_wur$v05a14 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a14 [m2_wur$CHFFQ5 == 14] <- 1 
table(m2_wur$v05a14)

m2_wur$v05a15 <- NA
m2_wur$v05a15 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a15 [m2_wur$CHFFQ5 == 15] <- 1 
table(m2_wur$v05a15)

m2_wur$v05a16 <- NA
m2_wur$v05a16 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a16 [m2_wur$CHFFQ5 == 16] <- 1 
table(m2_wur$v05a16)

m2_wur$v05a17 <- NA
m2_wur$v05a17 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a17 [m2_wur$CHFFQ5 == 17] <- 1 
table(m2_wur$v05a17)

m2_wur$v05a18 <- NA
m2_wur$v05a18 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a18 [m2_wur$CHFFQ5 == 18] <- 1 
table(m2_wur$v05a18)

m2_wur$v05a19 <- NA
m2_wur$v05a19 [!is.na(m2_wur$CHFFQ5)] <- 0
m2_wur$v05a19 [m2_wur$CHFFQ5 == 19] <- 1 
table(m2_wur$v05a19)

m2_wur <- m2_wur %>% rename("v01_M2"="CHFFQ1",
                            "v01_anders_M2"="CHFFQ1TXT",
                            "v02_M2"="CHFFQ2",
                            "v03_M2"="CHFFQ3",
                            "v04_M2"="CHFFQ4",
                            "v05a1_M2"="v05a1",
                            "v05a2_M2"="v05a2",
                            "v05a3_M2"="v05a3",
                            "v05a4_M2"="v05a4",
                            "v05a5_M2"="v05a5",
                            "v05a6_M2"="v05a6",
                            "v05a7_M2"="v05a7",
                            "v05a8_M2"="v05a8",
                            "v05a9_M2"="v05a9",
                            "v05a10_M2"="v05a10",
                            "v05a11_M2"="v05a11",
                            "v05a12_M2"="v05a12",
                            "v05a13_M2"="v05a13",
                            "v05a14_M2"="v05a14",
                            "v05a15_M2"="v05a15",
                            "v05a16_M2"="v05a16",
                            "v05a17_M2"="v05a17",
                            "v05a18_M2"="v05a18",
                            "v05a19_M2"="v05a19",
                            "v05a19_anders_M2"="CHFFQ5TXT",
                            "v06_M2"="CHFFQ6",
                            "v07_M2"="CHFFQ7",
                            "v_08_M2"="XCHFFQ8",
                            "v_09_M2"="CHFFQ9",
                            "v_10_M2"="CHFFQ10",
                            "v_11_M2"="XCHFFQ11")
colnames(m2_wur)

m2_wur <- subset(m2_wur, select=-CHFFQ5)
dim(m2_wur)
#455  32

m2_wur <- select(m2_wur, NEXT_NR, v01_M2, v01_anders_M2, v02_M2, v03_M2, v04_M2,
                 v05a1_M2,
                 v05a2_M2,
                 v05a3_M2,
                 v05a4_M2,
                 v05a5_M2,
                 v05a6_M2,
                 v05a7_M2,
                 v05a8_M2,
                 v05a9_M2,
                 v05a10_M2,
                 v05a11_M2,
                 v05a12_M2,
                 v05a13_M2,
                 v05a14_M2,
                 v05a15_M2,
                 v05a16_M2,
                 v05a17_M2,
                 v05a18_M2,
                 v05a19_M2,
                 everything())
str(m2_wur)

write.csv(m2_wur, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/FFQs_by_time_point/18.08.2021_M2_WUR.csv")

        ## =============== 4. CLEAN AND MERGE DATASETS   ================ ##

w2 <-  read.csv("18.08.2021_W2_WUR.csv", header=T, as.is=T, sep=",", row.names = 1)
m1 <-  read.csv("18.08.2021_M1_WUR.csv", header=T, as.is=T, sep=",", row.names = 1)
m2 <-  read.csv("18.08.2021_M2_WUR.csv", header=T, as.is=T, sep=",", row.names = 1)

dim(w2)
dim(m1)
dim(m2)


#ADD LEADING ZEROS BACK TO NEXT_NR 
w2$NEXT_NR = formatC(w2$NEXT_NR, width = 6, format = "d", flag = "0")
m1$NEXT_NR = formatC(m1$NEXT_NR, width = 6, format = "d", flag = "0")
m2$NEXT_NR = formatC(m2$NEXT_NR, width = 6, format = "d", flag = "0")

w2_m1 <- full_join(w2, m1, by="NEXT_NR")
WUR_cdbk1 <- full_join(w2_m1, m2, by = "NEXT_NR")
dim(WUR_cdbk1)
#594  94

write.csv(WUR_cdbk1, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/files_for_WUR/20.08.2021_WUR_2wk-2mnd.csv")
