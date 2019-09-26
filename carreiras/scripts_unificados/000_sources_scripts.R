## Scripts that build candidates pannel using CepespData ##
    # Eliana Lins Morandi, CepespData/FGV, sept.2019 #

# Install cepespR if required
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Cepesp-Fgv/cepesp-r")

# Install dplyr if required
if (!require("tidyverse")) install.packages("tidyverse")

# Install stringdist if required
if (!require("stringdist")) install.packages("stringdist")

# Clearing R
rm(list=ls()); cat("\014")

# Loading packages
library(tidyverse)
library(cepespR)
library(stringr)
library(stringi)

# Calling scripts:

source('scripts_unificados/001_builds_candidates.R') # Build raw data.frame with all candidates.
# Generates cand_national.rds and cand_municipal.rds

source('scripts_unificados/002_piles_up_data.R') # Piles up data on all candidates in CepespData/FGV.

source('scripts_unificados/003_cleaning_data.R') # cleans voter ID (Titulo de eleitor), national ID (CPF), and birth date.
# Generates df.rds

source('scripts_unificados/004_treats_voter_id.R') # rescues national and voter IDs. 
# Generates df_identified.rds (data.frame with rescued national and voter IDs) and
# generates winners_novoterID.rds (data.frame with elected candidates without voter ID -- 9 observations only)

source('scripts_unificados/005_corrects_wrong_id.R')