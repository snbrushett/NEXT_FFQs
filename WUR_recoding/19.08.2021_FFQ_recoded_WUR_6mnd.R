###     SCRIPT: RECODING OF INFANT FFQ VARIABLE HEADERS FOR WUR - 6MND CODEBOOK
###     AUTHOR(S): SIOBHAN BRUSHETT 
###     DESCRIPTION: RECODE INFANT FFQ VARIABLE HEADERS FOR DOWNSTREAM WUR PROCESSING
###     PROJECT: NEXT-INFANT-FFQ
###     NOTE(S): 


##Contents of this file

## 0. LOAD DATA
## 1. M6 FFQs RECODING
## 2. CLEAN DATASET

library(tidyverse)

        ## =============== 0. LOAD DATA   ================ ##

m6 <-  read.csv("qs_data_M6_Baby_2021-07-23_15_19_43.csv", header=T, as.is=T, sep=",")
dim(m6)
#328 686

        ## =============== 1. M6 FFQs RECODING ================ ##

x = c("CHALLERGY6",
  "CHALLERGY6A",
  "CHALLERGY6B",
  "CHALLERGY6C",
  "CHALLERGY6D",
  "CHALLERGY6E",
  "CHALLERGY6F",
  "CHALLERGY6FTXT",
  "CHFOOD8",
  "CHFOOD8A",
  "CHFOOD8B",
  "CHFOOD8C",
  "CHFOOD8CTXT",
  "CHFOOD9A",
  "CHFOOD9B",
  "CHFOOD10A",
  "CHFOOD10B",
  "CHFOOD10C",
  "CHFOOD10D1",
  "CHFOOD10D2",
  "CHFOOD10D3",
  "CHFOOD10D4",
  "CHFOOD10D5",
  "CHFOOD10D6",
  "CHFOOD10D7",
  "CHFOOD10D8",
  "CHFOOD10D9",
  "CHFOOD10D10",
  "CHFOOD10D11",
  "CHFOOD10D12",
  "CHFOOD10D13",
  "CHFOOD10D14",
  "CHFOOD10D15",
  "CHFOOD10D16",
  "CHFOOD10D17",
  "CHFOOD10D18",
  "CHFOOD10D19",
  "CHFOOD10D19TXT",
  "CHFFQ13",
  "CHFFQ14",
  "CHFFQ15",
  "CHFOOD11A",
  "CHFOOD11B",
  "CHFOOD12A",
  "CHFOOD12B",
  "CHFOOD13A",
  "CHFOOD13B",
  "CHFOOD13C1",
  "CHFOOD13C2",
  "CHFOOD13C3",
  "CHFOOD14A",
  "CHFOOD14B1",
  "CHFOOD14B2",
  "CHFOOD14B3",
  "CHFOOD14B4",
  "CHFOOD14B5",
  "CHFOOD14B6",
  "CHFOOD14B7",
  "CHFOOD14B8",
  "CHFOOD14B9",
  "CHFOOD14B9TXT",
  "CHFOOD14C",
  "CHFOOD15A",
  "CHFOOD15B",
  "CHFOOD15C",
  "CHFOOD16A",
  "CHFOOD16B",
  "CHFOOD17A",
  "CHFOOD17B",
  "CHFOOD18A",
  "CHFOOD18B",
  "CHFOOD19A",
  "CHFOOD19B",
  "CHFOOD19C1",
  "CHFOOD19C2",
  "CHFOOD20A",
  "CHFOOD20B",
  "CHFOOD21A",
  "CHFOOD21B",
  "CHFOOD22A",
  "CHFOOD22B",
  "CHFOOD23A",
  "CHFOOD23B",
  "CHFOOD24A",
  "CHFOOD24B",
  "CHFOOD25A",
  "CHFOOD25B",
  "CHFOOD25C1",
  "CHFOOD25C2",
  "CHFOOD25C3",
  "CHFOOD25C4",
  "CHFOOD26A",
  "CHFOOD26B",
  "CHFOOD27A",
  "CHFOOD27B",
  "CHFOOD28A",
  "CHFOOD28B",
  "CHFOOD29A",
  "CHFOOD29B",
  "CHFOOD29C",
  "CHFOOD29D",
  "CHFOOD29E",
  "CHFOOD29F",
  "CHFOOD29G",
  "CHFOOD29H",
  "CHFOOD29I",
  "CHFOOD29J",
  "CHFOOD29K",
  "CHFOOD29L",
  "CHFOOD29M",
  "CHFOOD29N",
  "CHFOOD29O",
  "CHFOOD29P",
  "CHFOOD29PTXT",
  "CHFOOD30A",
  "CHFOOD30B",
  "CHFOOD30C1",
  "CHFOOD30C2",
  "CHFOOD30C3",
  "CHFOOD30C4",
  "CHFOOD31A",
  "CHFOOD31B",
  "CHFOOD32A",
  "CHFOOD32B",
  "CHFOOD33A",
  "CHFOOD33B",
  "CHFOOD34TXT",
  "CHFOOD35A",
  "CHFOOD35C",
  "XCHFOOD35D") %in% colnames(m6)


m6_wur <- m6 [ , c("NEXT_NR", 
                   "CHALLERGY6", #allergies/intolerance
                   "CHALLERGY6A", #allergy - cow's milk
                   "CHALLERGY6B", #allergy - nuts
                   "CHALLERGY6C", #allergy - gluten
                   "CHALLERGY6D", #allergy - egg
                   "CHALLERGY6E", #lactose intolerant
                   "CHALLERGY6F", #allery/intolerances - other
                   "CHALLERGY6FTXT", #allery/intolerances - other (string)
                   "CHFOOD8", #diet type
                   "CHFOOD8A", #diet type - vegetarian
                   "CHFOOD8B", #diet type - vegan
                   "CHFOOD8C", #diet type - other
                   "CHFOOD8CTXT", #diet type - other (string)
                   "CHFOOD9A", #breast milk - frequency
                   "CHFOOD9B", #breast feeding cessation - infant age
                   "CHFOOD10A",#formula/follow-on - frequency
                   "CHFOOD10B", #formula/follow-on - amount (cups)
                   "CHFOOD10C", #formula/follow-on - amount (ml)
                   "CHFOOD10D1", #formula type
                   "CHFOOD10D2", #formula type
                   "CHFOOD10D3", #formula type
                   "CHFOOD10D4", #formula type
                   "CHFOOD10D5", #formula type
                   "CHFOOD10D6", #formula type
                   "CHFOOD10D7", #formula type
                   "CHFOOD10D8", #formula type
                   "CHFOOD10D9", #formula type
                   "CHFOOD10D10", #formula type
                   "CHFOOD10D11", #formula type
                   "CHFOOD10D12", #formula type
                   "CHFOOD10D13", #formula type
                   "CHFOOD10D14", #formula type
                   "CHFOOD10D15", #formula type
                   "CHFOOD10D16", #formula type
                   "CHFOOD10D17", #formula type
                   "CHFOOD10D18", #formula type
                   "CHFOOD10D19", #formula type - other
                   "CHFOOD10D19TXT", #formula type - other (string)
                   "CHFFQ13", #complementary food introduction
                   "CHFFQ14", #complementary food introduction - infant age (weeks)
                   "CHFFQ15", #complementary food - frequency
                   "CHFOOD11A", #formula/follow-on: rice flour frequency
                   "CHFOOD11B", #formula/follow-on: rice flour amount
                   "CHFOOD12A", #biscuits - frequency
                   "CHFOOD12B", #biscuits - amount
                   "CHFOOD13A", #bread - frequency
                   "CHFOOD13B", #bread - amount
                   "CHFOOD13C1", #bread - white bread
                   "CHFOOD13C2", #bread - brown bread
                   "CHFOOD13C3", #bread - whole wheat bread
                   "CHFOOD14A", #spreads - frequency
                   "CHFOOD14B1", #spreads - dairy butter 
                   "CHFOOD14B2", #spreads - half-full diary butter
                   "CHFOOD14B3", #spreads - halvarine
                   "CHFOOD14B4", #spreads - light halvarine
                   "CHFOOD14B5", #spreads - margarine in a tub
                   "CHFOOD14B6", #spreads - margarine in a packet
                   "CHFOOD14B7", #spreads - light margarine
                   "CHFOOD14B8", #spreads - unknown
                   "CHFOOD14B9", #spreads - other
                   "CHFOOD14B9TXT", #spreads - other (string)
                   "CHFOOD14C", #spreads - amount
                   "CHFOOD15A", #cheese/cheese spread - frequency
                   "CHFOOD15B", #cheese/cheese spread - amount
                   "CHFOOD15C", #cheese/cheese spread - amount
                   "CHFOOD16A", #meat spread - frequency
                   "CHFOOD16B", #meat spread - amount
                   "CHFOOD17A", #peanut butter - frequency
                   "CHFOOD17B", #peanut butter - amount
                   "CHFOOD18A", #sweet toppings - frequency
                   "CHFOOD18B", #sweet toppngs - amount
                   "CHFOOD19A", #egg - frequency
                   "CHFOOD19B", #egg - amount
                   "CHFOOD19C1", #egg - preparation: boiled
                   "CHFOOD19C2", #egg - preparation: fried
                   "CHFOOD20A", #tea - frequency
                   "CHFOOD20B", #tea - amount
                   "CHFOOD21A", #pasta - frequency
                   "CHFOOD21B", #pasta - amount
                   "CHFOOD22A", #rice/grains - frequency
                   "CHFOOD22B", #rice/grains - amount
                   "CHFOOD23A", #legumes - frequency
                   "CHFOOD23B", #legumes - amount
                   "CHFOOD24A", #potatoes - frequency
                   "CHFOOD24B", #potatoes - amount
                   "CHFOOD25A", #cooked vegetables - frequency
                   "CHFOOD25B", #cooked vegetables - amount
                   "CHFOOD25C1", #cooked vegetables - onion and leek
                   "CHFOOD25C2", #cooked vegetables - cauliflower and broccoli
                   "CHFOOD25C3", #cooked vegetables - cabbage varieties
                   "CHFOOD25C4", #cooked vegetables - other
                   "CHFOOD26A", #fish - frequency
                   "CHFOOD26B", #fish - amount
                   "CHFOOD27A", #meat - frequency
                   "CHFOOD27B", #meat - amount
                   "CHFOOD28A", #meat substitutes - frequency
                   "CHFOOD28B", #meat substitutes - amount
                   "CHFOOD29A", #food preparation fat - cooking fat
                   "CHFOOD29B", #food preparation fat - dairy butter
                   "CHFOOD29C", #food preparation fat - margarine in a tub
                   "CHFOOD29D", #food preparation fat - margarine in a packet
                   "CHFOOD29E", #food preparation fat - light margarine
                   "CHFOOD29F", #food preparation fat - liquid margarine
                   "CHFOOD29G", #food preparation fat - light liquid margarine
                   "CHFOOD29H", #food preparation fat - solid stock
                   "CHFOOD29I", #food preparation fat - liquid stock
                   "CHFOOD29J", #food preparation fat - olive oil, peanut oil and frying oil
                   "CHFOOD29K", #food preparation fat - sunflower oil, other type of oil
                   "CHFOOD29L", #food preparation fat - solid cooking fat
                   "CHFOOD29M", #food preparation fat - liquid cooking fat
                   "CHFOOD29N", #food preparation fat - lard, bacon fat, goose fat
                   "CHFOOD29O", #food preparation fat - unknown
                   "CHFOOD29P", #food preparation fat - other
                   "CHFOOD29PTXT", #food preparation fat - other (string)
                   "CHFOOD30A", #fresh fruit - frequency
                   "CHFOOD30B", #fresh fruit - amount
                   "CHFOOD30C1", #fresh fruit - banana 
                   "CHFOOD30C2", #fresh fruit - apple
                   "CHFOOD30C3", #fresh fruit - citrus fruit
                   "CHFOOD30C4", #fresh fruit - other fruits
                   "CHFOOD31A", #fruit juice - frequency
                   "CHFOOD31B", #fruit juice - amount
                   "CHFOOD32A", #water - frequency
                   "CHFOOD32B", #water - amount
                   "CHFOOD33A", #lemonade - frequency
                   "CHFOOD33B", #lemonade - amount
                   "CHFOOD34TXT", #other products (string)
                   "CHFOOD35A", #vitamin D
                   "CHFOOD35C", #vitamin D - amount
                   "XCHFOOD35D")] #vitamin D - brand

dim(m6_wur)
#328 131


m6_wur <- m6_wur %>% rename("patrI_1_M6"="CHALLERGY6",
                            "patrI_2_M6"="CHALLERGY6A",
                            "patrI_3_M6"="CHALLERGY6B",
                            "patrI_4_M6"="CHALLERGY6C",
                            "patrI_5_M6"="CHALLERGY6D",
                            "patrI_6_M6"="CHALLERGY6E",
                            "patrI_7_M6"="CHALLERGY6F",
                            "patrI_7anders_M6"="CHALLERGY6FTXT",
                            "patrII_1_M6"="CHFOOD8",
                            "patrII_2_M6"="CHFOOD8A",
                            "patrII_3_M6"="CHFOOD8B",
                            "patrII_4_M6"="CHFOOD8C",
                            "patrII_4anders_M6"="CHFOOD8CTXT",
                            "v01a_M6"="CHFOOD9A",
                            "v01b_M6"="CHFOOD9B",
                            "v02a_M6"="CHFOOD10A",
                            "v02b_M6"="CHFOOD10B",
                            "v02c_M6"="CHFOOD10C",
                            "v02d1_M6"="CHFOOD10D1",
                            "v02d2_M6"="CHFOOD10D2",
                            "v02d3_M6"="CHFOOD10D3",
                            "v02d4_M6"="CHFOOD10D4",
                            "v02d5_M6"="CHFOOD10D5",
                            "v02d6_M6"="CHFOOD10D6",
                            "v02d7_M6"="CHFOOD10D7",
                            "v02d8_M6"="CHFOOD10D8",
                            "v02d9_M6"="CHFOOD10D9",
                            "v02d10_M6"="CHFOOD10D10",
                            "v02d11_M6"="CHFOOD10D11",
                            "v02d12_M6"="CHFOOD10D12",
                            "v02d13_M6"="CHFOOD10D13",
                            "v02d14_M6"="CHFOOD10D14",
                            "v02d15_M6"="CHFOOD10D15",
                            "v02d16_M6"="CHFOOD10D16",
                            "v02d17_M6"="CHFOOD10D17",
                            "v02d18_M6"="CHFOOD10D18",
                            "v02d19_M6"="CHFOOD10D19",
                            "v02d19_anders_M6"="CHFOOD10D19TXT",
                            "v13_M6"="CHFFQ13",
                            "v13_weken_M6"="CHFFQ14",
                            "v14_M6"="CHFFQ15",
                            "v03a_M6"="CHFOOD11A",
                            "v03b_M6"="CHFOOD11B",
                            "v04a_M6"="CHFOOD12A",
                            "v04b_M6"="CHFOOD12B",
                            "v05a_M6"="CHFOOD13A",
                            "v05b_M6"="CHFOOD13B",
                            "v05c1_M6"="CHFOOD13C1",
                            "v05c2_M6"="CHFOOD13C2",
                            "v05c3_M6"="CHFOOD13C3",
                            "v06a_M6"="CHFOOD14A",
                            "v06b1_M6"="CHFOOD14B1",
                            "v06b2_M6"="CHFOOD14B2",
                            "v06b3_M6"="CHFOOD14B3",
                            "v06b4_M6"="CHFOOD14B4",
                            "v06b5_M6"="CHFOOD14B5",
                            "v06b6_M6"="CHFOOD14B6",
                            "v06b7_M6"="CHFOOD14B7",
                            "v06b8_M6"="CHFOOD14B8",
                            "v06b9_M6"="CHFOOD14B9",
                            "v06b9_anders_M6"="CHFOOD14B9TXT",
                            "v06c_M6"="CHFOOD14C",
                            "v07a_M6"="CHFOOD15A",
                            "v07b_M6"="CHFOOD15B",
                            "v07c_M6"="CHFOOD15C",
                            "v08a_M6"="CHFOOD16A",
                            "v08b_M6"="CHFOOD16B",
                            "v09a_M6"="CHFOOD17A",
                            "v09b_M6"="CHFOOD17B",
                            "v10a_M6"="CHFOOD18A",
                            "v10b_M6"="CHFOOD18B",
                            "v11a_M6"="CHFOOD19A",
                            "v11b_M6"="CHFOOD19B",
                            "v11c1_M6"="CHFOOD19C1",
                            "v11c2_M6"="CHFOOD19C2",
                            "v12a_M6"="CHFOOD20A",
                            "v12b_M6"="CHFOOD20B",
                            "v13a_M6"="CHFOOD21A",
                            "v13b_M6"="CHFOOD21B",
                            "v14a_M6"="CHFOOD22A",
                            "v14b_M6"="CHFOOD22B",
                            "v15a_M6"="CHFOOD23A",
                            "v15b_M6"="CHFOOD23B",
                            "v16a_M6"="CHFOOD24A",
                            "v16b_M6"="CHFOOD24B",
                            "v17a_M6"="CHFOOD25A",
                            "v17b_M6"="CHFOOD25B",
                            "v17c1_M6"="CHFOOD25C1",
                            "v17c2_M6"="CHFOOD25C2",
                            "v17c3_M6"="CHFOOD25C3",
                            "v17c4_M6"="CHFOOD25C4",
                            "v18a_M6"="CHFOOD26A",
                            "v18b_M6"="CHFOOD26B",
                            "v19a_M6"="CHFOOD27A",
                            "v19b_M6"="CHFOOD27B",
                            "v20a_M6"="CHFOOD28A",
                            "v20b_M6"="CHFOOD28B",
                            "v21a1_M6"="CHFOOD29A",
                            "v21a2_M6"="CHFOOD29B",
                            "v21a3_M6"="CHFOOD29C",
                            "v21a4_M6"="CHFOOD29D",
                            "v21a5_M6"="CHFOOD29E",
                            "v21a6_M6"="CHFOOD29F",
                            "v21a7_M6"="CHFOOD29G",
                            "v21a8_M6"="CHFOOD29H",
                            "v21a9_M6"="CHFOOD29I",
                            "v21a10_M6"="CHFOOD29J",
                            "v21a11_M6"="CHFOOD29K",
                            "v21a12_M6"="CHFOOD29L",
                            "v21a13_M6"="CHFOOD29M",
                            "v21a14_M6"="CHFOOD29N",
                            "v21a15_M6"="CHFOOD29O",
                            "v21a16_M6"="CHFOOD29P",
                            "v21a16_anders_M6"="CHFOOD29PTXT",
                            "v22a_M6"="CHFOOD30A",
                            "v22b_M6"="CHFOOD30B",
                            "v22c1_M6"="CHFOOD30C1",
                            "v22c2_M6"="CHFOOD30C2",
                            "v22c3_M6"="CHFOOD30C3",
                            "v22c4_M6"="CHFOOD30C4",
                            "v23a_M6"="CHFOOD31A",
                            "v23b_M6"="CHFOOD31B",
                            "v24a_M6"="CHFOOD32A",
                            "v24b_M6"="CHFOOD32B",
                            "v25a_M6"="CHFOOD33A",
                            "v25b_M6"="CHFOOD33B",
                            "v26_M6"="CHFOOD34TXT",
                            "v27a_M6"="CHFOOD35A",
                            "v27b_M6"="CHFOOD35C",
                            "v27c_M6"="XCHFOOD35D")

m6_wur <- select(m6_wur, NEXT_NR, patrI_1_M6,
                 patrI_2_M6,
                 patrI_3_M6,
                 patrI_4_M6,
                 patrI_5_M6,
                 patrI_6_M6,
                 patrI_7_M6,
                 patrI_7anders_M6,
                 patrII_1_M6,
                 patrII_2_M6,
                 patrII_3_M6,
                 patrII_4_M6,
                 patrII_4anders_M6,
                 v01a_M6,
                 v01b_M6,
                 v02a_M6,
                 v02b_M6,
                 v02c_M6,
                 v02d1_M6,
                 v02d2_M6,
                 v02d3_M6,
                 v02d4_M6,
                 v02d5_M6,
                 v02d6_M6,
                 v02d7_M6,
                 v02d8_M6,
                 v02d9_M6,
                 v02d10_M6,
                 v02d11_M6,
                 v02d12_M6,
                 v02d13_M6,
                 v02d14_M6,
                 v02d15_M6,
                 v02d16_M6,
                 v02d17_M6,
                 v02d18_M6,
                 v02d19_M6,
                 v02d19_anders_M6,
                 v13_M6,
                 v13_weken_M6,
                 v14_M6,
                 v03a_M6,
                 v03b_M6,
                 v04a_M6,
                 v04b_M6,
                 v05a_M6,
                 v05b_M6,
                 v05c1_M6,
                 v05c2_M6,
                 v05c3_M6,
                 v06a_M6,
                 v06b1_M6,
                 v06b2_M6,
                 v06b3_M6,
                 v06b4_M6,
                 v06b5_M6,
                 v06b6_M6,
                 v06b7_M6,
                 v06b8_M6,
                 v06b9_M6,
                 v06b9_anders_M6,
                 v06c_M6,
                 v07a_M6,
                 v07b_M6,
                 v07c_M6,
                 v08a_M6,
                 v08b_M6,
                 v09a_M6,
                 v09b_M6,
                 v10a_M6,
                 v10b_M6,
                 v11a_M6,
                 v11b_M6,
                 v11c1_M6,
                 v11c2_M6,
                 v12a_M6,
                 v12b_M6,
                 v13a_M6,
                 v13b_M6,
                 v14a_M6,
                 v14b_M6,
                 v15a_M6,
                 v15b_M6,
                 v16a_M6,
                 v16b_M6,
                 v17a_M6,
                 v17b_M6,
                 v17c1_M6,
                 v17c2_M6,
                 v17c3_M6,
                 v17c4_M6,
                 v18a_M6,
                 v18b_M6,
                 v19a_M6,
                 v19b_M6,
                 v20a_M6,
                 v20b_M6,
                 v21a1_M6,
                 v21a2_M6,
                 v21a3_M6,
                 v21a4_M6,
                 v21a5_M6,
                 v21a6_M6,
                 v21a7_M6,
                 v21a8_M6,
                 v21a9_M6,
                 v21a10_M6,
                 v21a11_M6,
                 v21a12_M6,
                 v21a13_M6,
                 v21a14_M6,
                 v21a15_M6,
                 v21a16_M6,
                 v21a16_anders_M6,
                 v22a_M6,
                 v22b_M6,
                 v22c1_M6,
                 v22c2_M6,
                 v22c3_M6,
                 v22c4_M6,
                 v23a_M6,
                 v23b_M6,
                 v24a_M6,
                 v24b_M6,
                 v25a_M6,
                 v25b_M6,
                 v26_M6,
                 v27a_M6,
                 v27b_M6,
                 v27c_M6)

colnames(m6_wur)
str(m6_wur)

write.csv(m6_wur, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/FFQs_by_time_point/19.08.2021_M6_WUR.csv")

  ## =============== 2. CLEAN DATASET   ================ ##

m6 <-  read.csv("19.08.2021_M6_WUR.csv", header=T, as.is=T, sep=",", row.names = 1)
dim(m6)

#ADD LEADING ZEROS BACK TO NEXT_NR 
m6$NEXT_NR = formatC(m6$NEXT_NR, width = 6, format = "d", flag = "0")

write.csv(m6, "/groups/umcg-llnext/tmp01/umcg-snbrushett/processed/FFQs_recoded_WUR/files_for_WUR/20.08.2021_WUR_6mnd.csv")

