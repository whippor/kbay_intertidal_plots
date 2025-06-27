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
library(viridis)
library(vegan)

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
parse_xlsx("data/raw/ClearedPlots.xlsx", start = 1, end = 24)

# filter out summary rows
core_data <- joined_data |>
  filter(TRIPLET != "0")

# make columns numeric
numeric_data <- core_data |>
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
rm(names1, names2)

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
                               CALLITHAMNION,
                        callithamnion),
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
                        PALMARIA,
                   `Palmaria cal.`),
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
         -ENTEROMORPHA, -PALMARIA, -`Palmaria callophylloides`,
         -ROCK,
         -`BARE ROCK`,
         -`SAND-GRAVEL`,
         -`BOULDER-COBBLE`)

plot_data_QAQC <- combined_cols

write_csv(file = "data/QAQC/all_plots_QAQC", plot_data_QAQC)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# VISUALIZATIONS                                                            ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Littorine abundance across Years
plot_data_QAQC |>
  mutate(Year = as.numeric(Year)) |>
  select(Year, `L scutulata`, `L sitkana`, QUAD) |>
  pivot_longer(`L scutulata`:`L sitkana`, names_to = "species", values_to = "count") |>
  ggplot(aes(x = Year, y = count, color = species)) +
  geom_point() +
  geom_smooth() +
  scale_color_viridis(discrete = TRUE, option = "magma", 
                      begin = 0.2, end = 0.8) +
  theme_bw()

# Richness/Diversity across years
div_mat <- plot_data_QAQC |>
  select(-`Ralfsia/Hild`, -Petrocelia, -Lepidochiton, -Acrosiphonia, -`irridescent snail (Homalopoma?)`,
         -`Hiatella arctica`, -Flatworm, -EPIACTIS, -`coralline crust`, -`Erect coralline`)
  select(`FUCUS%TOTAL`, Ulva:`Erect coralline`) 

# Richness per quad
spec_quads <- specnumber(div_mat) |>
  enframe() |>
  bind_cols(plot_data_QAQC)

# Richness by year
spec_quads |>
  mutate(Year = as.numeric(Year)) |>
  select(Year, value, QUAD) |>
  ggplot(aes(x = Year, y = value, color = QUAD)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "turbo") +
  theme_bw()
  
# nMDS of community by year
  
div_summary <- plot_data_QAQC |>
  select(Year, QUAD, `FUCUS%TOTAL`, Ulva:`Erect coralline`) |>
  pivot_longer(`FUCUS%TOTAL`:`Erect coralline`, names_to = "Species", values_to = "Count") |>
    group_by(Year, Species) |>
    summarise(Avg = mean(Count))
  
div_summary_wide <- div_summary |>
    pivot_wider(id_cols = c(Year), 
                names_from = Species, 
                values_from = Avg,
                values_fill = 0)
div_summary_wide <- replace(div_summary_wide, is.na(div_summary_wide), 0)
  
coverMDS <- metaMDS(div_summary_wide[,2:58], distance = "altGower")
  
  
# extract the 'points' from the nMDS that you will plot in ggplot2
coverMDS_points <- coverMDS$points
# turn those plot points into a dataframe that ggplot2 can read
coverMDS_points <- data.frame(coverMDS_points)
# join your plot points with your summed species observations from each habitat type
plot_data_tax <- data.frame(coverMDS_points, div_summary_wide[,1])
  
  
  
# run the ggplot
ggplot(plot_data_tax, aes(x=MDS1, y=MDS2,
                          color = Year, label = Year, group = 1)) + 
  labs(x = "nMDS1", y = "nMDS2") +
  theme_classic() + 
  geom_point(size =  4) + 
  scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.9, option = "G", name = "Year") +
  geom_text(hjust=0, vjust=-.5) +
  geom_path(arrow = arrow(angle = 15, ends = "last", type = "closed"))

  

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####