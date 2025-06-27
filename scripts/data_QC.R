#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Herring Island Intertidal Plot Data QAQC                                    ##
# Script created 2025-06-25                                                   ##
# Data source: Terrie Klinger / University of Washington                      ##
# R code prepared-b-y Ross Whippo                                             ##
# Last updated 2025-06-25                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# data/raw/ClearedPlots.xlsx

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(readxl)
library(janitor)
library(fuzzyjoin)
library(stringdist)


parse_xlsx <- function(path, start = 1, end = 10) {
  sheets <- c(start:end)
  final_list <- list()
  for (i in sheets) 
    {
    raw_data <- read_excel(path = path,
                       sheet = i)
    Year <- excel_sheets(path)
    Year <- Year[i]
    trans_data <- t(raw_data)
    colname_data <- data.frame(trans_data) |>
      row_to_names(row_number = 1) |>
      remove_rownames() |>
      mutate(Year = Year)
      #mutate_all(~replace_na(., "0"))
    final_list[[i]] <- colname_data
  }
  joined_data <- bind_rows(final_list)
  joined_data <- joined_data |>
    mutate_all(~replace_na(., "0"))
 # sheets <- read_excel(path = path)
#  joined_data <- map_dfr(sheets, 
#                         mutate(read_excel(path, sheet = .x), 
#                                sheetname = .x))
  joined_data <- joined_data |>
    relocate(Year, .before = TRIPLET)
  assign("joined_data", joined_data, envir = .GlobalEnv)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# read all sheets and combine into single dataframe
parse_xlsx("data/raw/ClearedPlots.xlsx", start = 1, end = 21)

# make columns numeric
numeric_data <- joined_data |>
  mutate_at(vars(`FUCUS%TOTAL`:`FUCUS SPORELINGS%`, Ulva:`ANTHOPL ARTEMESIA`), as.numeric)

# identify duplicated columns
names1 <- data.frame(colnames(numeric_data))
names2 <- names1
matched_names <- names1 |>
  stringdist_left_join(names2,method="lv", max_dist=5) |>
  mutate(lv_dist=stringdist(colnames.numeric_data..x,
                            colnames.numeric_data..y, 
                            method="lv")) |>
  filter(colnames.numeric_data..x != colnames.numeric_data..y)

# join duplicated columns
combined_cols <- numeric_data |>
  rowwise() |>
  mutate(
    Ulva = sum(Ulva, ULVA),
    Petrocelia = sum(Petrocelia, 
                          Petrocelis, 
                          PETROCELIS),
    `Crustose coralline` = sum(`Crustose coralline`, 
                                    `crustose coralliine`,
                                    `CRUSTOSE CORALLINE`),
    Porphyra = sum(Porphyra,
                        Pophyra,
                        porphyra,
                        PORPHYRA),
    Callithamnion = sum(Callithamnion,
                               `Callitham pikeanum`,
                               `Callithamnion pikeanum`,
                               CALLITHAMNION),
    Pterosiphonia = sum(Pterosiphonia,
                             PTEROSIPHONIA),
    Polysiphonia = sum(Polysiphonia,
                            POLYSIPHONIA),
    Nucella = sum(Nucella,
                       NUCELLA),
    `L sitkana` = sum(`L SITKANA`,
                           `L sitkana`),
    `L scutulata` = sum(`L SCUTULATA`,
                             `L scutulata`),
    `Barnacle spat` = sum(`Barnacle spat`,
                               `BARNACLES SPAT`),
    Barnacles = sum(Barnacles,
                         BARNACLES),
    Margarites = sum(Margarites,
                          `Margarites marg`,
                          `margarities small iridescent snail`),
    Palmaria = sum(`Palmaria callophylloides`,
                        PALMARIA),
    Elachista = sum(Elachista,
                         ELACHISTA),
    Mytilus = sum(Mytilus,
                       MYTILUS),
    Pagurus = sum(Pagurus,
                       PAGURUS),
    Siphonaria = sum(Siphonaria,
                          SIPHONARIA),
    Katharina = sum(Katharina,
                         KATHRINA),
    Cryptosiphonia = sum(Cryptosiphonia,
                              CRYPTOSIPHONIA),
    `Ralfsia/Hild` = sum(`Ralfsia/Hild`,
                              `RALFSIA/HILD`),
    Endocladia = sum(Endocladia,
                          ENDOCLADIA),
    Odonthalia = sum(Odonthalia,
                          ODONTHALIA),
    Gloiopeltis = sum(Gloiopeltis,
                           GLOIOPELTIS),
    Enteromorpha = sum(Enteromorpha,
                            ENTEROMORPHA),
    `Clad sericia` = sum(`Clad sericia`,
                              `CLAD SERICEA`),
    Soranthera = sum(Soranthera,
                          SORANTHERA),
    `Masto pap`  = sum(`Masto pap`,
                            `MASTO PAP`),
    Myelophycus = sum(MYELOPHYCUS,
                           Myelophycus),
    Halosaccion = sum(Halosaccion,
                           HALOSACCION),
    Neorhodomela = sum(Neorhodomela,
                            NEORHODOMELA),
    Colpomenia = sum(Colpomenia,
                          COLPOMENIA),
    `Erect coralline` = sum(`ERECT CORALLINE`,
                               `erect coralline`),
    Lottids = sum(Lottids,
                       LOTTIIDAE),
    Buccinum = sum(Buccinum,
                        BUCCINUM), 
    Leptasterias = sum(Leptasterias,
                            LEPTASTERIAS),
    Emplectonema = sum(Emplectonema,
                            EMPLECTONEMA),
    Amphiporous = sum(Amphiporous,
                           AMPHIPORUS),
    Onchidella = sum(Onchidella,
                          ONCHIDELLA),
    Paranemertes = sum(Paranemertes,
                            PARANEMERTES),
    Spirorbidae = sum(Spirorbidae,
                           SPIRORBIDAE),
    `Antho artemesia` = sum(`Antho artemesia`,
                                 `ANTHOPL ARTEMESIA`)
  ) |>
  select(-ULVA, -Petrocelis, -PETROCELIS, -`crustose coralliine`,
         -`CRUSTOSE CORALLINE`, -porphyra, -Pophyra, -PORPHYRA,
         -`Callitham pikeanum`, -`Callithamnion pikeanum`,
         -CALLITHAMNION, -PTEROSIPHONIA, -POLYSIPHONIA, -NUCELLA,
         -`L SITKANA`, -`L SCUTULATA`, -`BARNACLES SPAT`,
         -BARNACLES, -`margarities small iridescent snail`,
         -`Margarites marg`, -PALMARIA, -ELACHISTA, -MYTILUS,
         -PAGURUS, -SIPHONARIA, -KATHRINA, -CRYPTOSIPHONIA, -`RALFSIA/HILD`,
         -ENDOCLADIA, -ODONTHALIA, -GLOIOPELTIS, -`CLAD SERICEA`, 
         -SORANTHERA, -`MASTO PAP`, -MYELOPHYCUS, -HALOSACCION,
         -NEORHODOMELA, -COLPOMENIA, -`ERECT CORALLINE`, -`erect coralline`,
         -LOTTIIDAE, -BUCCINUM, -LEPTASTERIAS, -EMPLECTONEMA, -AMPHIPORUS,
         -ONCHIDELLA, -PARANEMERTES, -SPIRORBIDAE, -`ANTHOPL ARTEMESIA`,
         -ENTEROMORPHA,
         -ROCK,
         -`BARE ROCK`,
         -`SAND-GRAVEL`,
         -`BOULDER-COBBLE`)

# identify duplicated columns
names1 <- data.frame(colnames(combined_cols))
names2 <- names1
matched_names <- names1 |>
  stringdist_left_join(names2,method="lv", max_dist=5) |>
  mutate(lv_dist=stringdist(colnames.combined_cols..x,
                            colnames.combined_cols..y, 
                            method="lv")) |>
  filter(colnames.combined_cols..x != colnames.combined_cols..y)

sum(combined_cols$Odonthalia)
sum(numeric_data$Odonthalia) + sum(numeric_data$ODONTHALIA)

test <- numeric_data |>
  select(Gloiopeltis, GLOIOPELTIS)
test1 <- combined_cols |>
  select(Gloiopeltis) |>
  mutate(Glo1 = Gloiopeltis) |>
  select(Glo1) |>
  bind_cols(test)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####