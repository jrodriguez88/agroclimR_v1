# Script to create Aquacrop Crop File 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2023



## Load packages

#library(tidyverse)
#library(lubridate)
#library(readxl)

## read data
#crops?? and params -- define by GDD 

## tidy data and quality control

## Evaluate params with observed data

## write files


crop_def <- read_lines("aquacrop_files/DEFAULT.CRO")



write_crp_aquacrop <- function(out_path = "", params, crop_def, crop = ""){
  
  have_point <- crop_def %>% str_sub(1,15) %>% str_detect(fixed(".")) %>% enframe(name = NULL) %>% slice(-1)
  
  params_to_crp <- params %>% drop_na()  %>%
    mutate(point = have_point$value,
           to_crp = if_else(point == T, sprintf("%1.6f", Mean), sprintf("%1.0f", Mean))) 
  
  cultivar <- unique(params$cultivar)
  
  
  sink(paste0(out_path, crop, cultivar, ".CRO"), append = F) 
  cat(paste(crop, cultivar, "AquaCrop crp  by https://github.com/jrodriguez88"))
  cat('\n')
  write.table(data.frame(params = sprintf("%-15s", params_to_crp$to_crp),
                         space = ":", 
                         name = params_to_crp$param), row.names = F, quote = F, col.names = F)
  sink()
  
  
} 