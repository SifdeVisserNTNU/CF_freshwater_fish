# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 31/03/2023

### Calculating the contribution of each grid cell to the global extnction risk

# Working directory
setwd("~/Desktop/Fish_CF_code")

# Libraries
library(raster); library(foreach); library(sf); library(dplyr); library(matrixStats); library(ggplot2); 
library(rgdal); library(data.table); library(readr); library(viridis); library(writexl)

# Read files -------------------------------------------------------------------
RT <- read_delim("Data_revised/Barbarossa/RT_species.csv", ",", escape_double = FALSE, trim_ws = TRUE,
                 col_types = cols_only("binomial" = col_character(),
                                       "1.5C_no dispersal" = col_double(),
                                       "2.0C_no dispersal" = col_double(),
                                       "3.2C_no dispersal" = col_double(),
                                       "4.5C_no dispersal" = col_double()))
Area <- read_delim("Data_revised/Barbarossa/species_traits.csv", ",", escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols_only("id_no" = col_double(),
                                         "binomial" = col_character(),
                                         "area" = col_double()))

temp <- readRDS('proc/ssp/points_template.rds')

# Step 1. Species-specific extinction risk -------------------------------------

# Es = 1-(Anew/Aoriginal)^z
# Anew = (100-RT/100)*Aoriginal
# Substitution: Es = 1-((1-RT/100)*Aoriginal/Aoriginal)/)^z
# Es = 1-((100-RT)/100)^z
z <- 0.21
Species_E <- inner_join(Area, RT, by = c('binomial' = 'binomial')) %>%
  select(c(1,3:7)) %>%
  rename(species_ID = id_no,
         total_area_species = area,
         RT_1.5 = '1.5C_no dispersal',
         RT_2.0 = '2.0C_no dispersal',
         RT_3.2 = '3.2C_no dispersal',
         RT_4.5 = '4.5C_no dispersal') %>%
  mutate(Es_1.5 = 1 - (((100- RT_1.5)/100)^z),
         Es_2.0 = 1 - (((100- RT_2.0)/100)^z),
         Es_3.2 = 1 - (((100- RT_3.2)/100)^z),
         Es_4.5 = 1 - (((100- RT_4.5)/100)^z)) %>%
  select(-c(3:6)) 

# Step 2. Grid cells in which species occurs + area of those grid cells --------
files <- list.files(path = "proc/gfdl/modelled_occurrence/", full.names = TRUE)

read <- function(files){
  df <- readRDS(files) 
  dt <- tibble::rownames_to_column(df, "grid_id") %>%
    select(grid_id, area)
  id <- data.frame("species_ID" = basename(files)) 
  bind_cols(dt,id)
}

s <- lapply(files, read) %>% 
    bind_rows() %>%
  mutate(species_ID = gsub("(.*).rds$", "\\1", species_ID)) %>%
  mutate(species_ID = as.numeric(species_ID))

# Step 3. Partition E across grid cells ----------------------------------------

# Join Es, calculate area-weighted Eps for each grid cell
Partition <- inner_join(s, Species_E, by = 'species_ID') %>%
  transmute(
    grid_id = grid_id,
    species_id = species_ID,
    Ep_1.5 = Es_1.5*area/total_area_species,
    Ep_2.0 = Es_2.0*area/total_area_species,
    Ep_3.2 = Es_3.2*area/total_area_species,
    Ep_4.5 = Es_4.5*area/total_area_species)

# Step 4. Calculate EPs total --------------------------------------------------
Ep_1.5_tot <- sum(Partition$Ep_1.5)
Ep_2.0_tot <- sum(Partition$Ep_2.0)
Ep_3.2_tot <- sum(Partition$Ep_3.2)
Ep_4.5_tot <- sum(Partition$Ep_4.5)

# Step 5. Determine contribution, divide sum of EPs per grid cell by EPs total -
Partition2 <- Partition %>%
  group_by(grid_id) %>%
  summarise(Ep_1.5_grid = sum(Ep_1.5)/Ep_1.5_tot*100, 
            Ep_2.0_grid = sum(Ep_2.0)/Ep_2.0_tot*100,
            Ep_3.2_grid = sum(Ep_3.2)/Ep_3.2_tot*100,
            Ep_4.5_grid = sum(Ep_4.5)/Ep_4.5_tot*100,
            .groups = 'keep') %>%
  mutate(grid_id = as.integer(grid_id))

Contribution <- left_join(temp, Partition2, by = c('row_no' = 'grid_id'))

# Save files -------------------------------------------------------------------
saveRDS(Contribution, file = "data_revised/Contribution/Contribution.rds")
