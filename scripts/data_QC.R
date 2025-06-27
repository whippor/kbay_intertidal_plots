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

parse_xlsx <- function(path, start = 1, end = 10) {
  sheets <- c(start:end)
  final_list <- list()
  for (i in sheets) 
    {
    raw_data <- read_excel(path = path,
                       sheet = i)
    trans_data <- t(raw_data)
    colname_data <- data.frame(trans_data) |>
      row_to_names(row_number = 1) |>
      remove_rownames() #|>
      #mutate_all(~replace_na(., "0"))
    final_list[[i]] <- colname_data
  }
  joined_data <- bind_rows(final_list)
  joined_data <- joined_data |>
    mutate_all(~replace_na(., "0"))
  assign("joined_data", joined_data, envir = .GlobalEnv)
}


parse_xlsx("data/raw/ClearedPlots.xlsx", start = 1, end = 21)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path <- "data/raw/ClearedPlots.xlsx"

raw_data <- path |>
  excel_sheets() |>
  set_names() |>
  map(read_excel, path = path) |>
  list_rbind()



raw_data <- read_excel(path = path, 
                       sheet = 2, 
                       skip = 0)
trans_data <- t(raw_data)
colname_data <- data.frame(trans_data) |>
  row_to_names(row_number = 1) |>
  remove_rownames() |>
  mutate_all(~replace_na(., "0"))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####