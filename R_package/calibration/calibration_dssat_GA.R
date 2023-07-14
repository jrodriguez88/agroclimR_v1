# Script to Calibrate DSSAT- RICE model with Genetic algorithms (GA)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023


## Load packages
#library(GA)
#library(tictoc)
#library(parallel)
#library(doFuture)

path_proj, basedata_path

# Funcion copia inputs base en directorio de simulacion de cada setups
copy_inputs_dssat <- function(path_proj, basedata_path, crop = "rice"){
  
  CR <- tibble(
    crop_name = c("rice", "maize", "barley", "sorghum", "wheat", "bean", "fababean", "teff"),
    CR = c("RI", "MZ", "BA", "SG", "WH", "BN", "FB",  "TF")) %>% filter(crop_name==crop)%>%
    pull(CR)
  
  
  gen_files <- list.files(basedata_path, full.names = T, pattern = "ECO|SPE|CUL") %>%
    str_subset(CR)
  
  wth_files <- list.files(basedata_path, full.names = T, pattern = paste0(".WTH"))
  
  
  X_files <- list.files(basedata_path, full.names = T, pattern = paste0(".", CR, "X$"))
  
  soil_files <- list.files(basedata_path, full.names = T, pattern = ".SOL$")
  
  
  # Copy files in folder project
  file.copy(c(gen_files, setting_files, soil_files), path_proj)
  
  #  map2(.x = c("*.SPE", "*.ECO", "*.CUL"), 
  #       .y = paste0("standard", c("*.SPE", "*.ECO", "*.CUL")), 
  #       ~file.rename(
  #         from = list.files(dir_run, pattern = .x, full.names = T), 
  #         to = paste0(dir_run, .y)))
  
  
}