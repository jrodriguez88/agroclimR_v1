# Script to read Outputs files of DSSAT model
# Author: Rodriguez-Espinoza J. Mesa-Diez J.
# Repository: https://github.com/jrodriguez88/
# 2023

#library(data.table)
#library(tidyverse)


## Read "Evaluate.OUT" file
read_evaluate <- function(file){
  
  suppressMessages(suppressWarnings(read_table(file, skip = 2, col_types = cols())))
  
  
}

## Read "Summary.OUT" file
read_summary <- function(file){
  
 col_names <-  read_lines(file) %>% str_subset("RUNNO") %>%
    str_split_1("\\s+") %>% str_subset("@", negate = T)
  
 summary_out <- fread(file, skip = 4, na.strings = "-99") %>% 
   set_names(col_names) %>% 
    mutate(SDAT = as.Date(as.character(SDAT), format("%Y%j")), 
           PDAT = as.Date(as.character(PDAT), format("%Y%j")), 
           EDAT = as.Date(as.character(EDAT), format("%Y%j")),
           ADAT = as.Date(as.character(ADAT), format("%Y%j")),
           MDAT = as.Date(as.character(MDAT), format("%Y%j")),
           HDAT = as.Date(as.character(HDAT), format("%Y%j"))) %>%
    dplyr::select(SDAT, PDAT, everything())
  
  return(summary_out)


}


## Read PlantGro.OUT file
read_plantgro <- function(file) {
  
  # Read PlanstGro.OUT lines
  plangro_raw <- read_lines(file)
  

 # detect start data.frame
  skip <- plangro_raw  %>% str_detect("@YEAR") %>% which()-1 
  
  # experimental names 
  exp_names <- plangro_raw %>% 
    str_subset(fixed("EXPERIMENT")) %>% 
    map_chr(~str_split(., " ") %>% unlist() %>% pluck(-1))
  
 # id_name <- plantgro_raw %>% 
 #   str_subset(fixed("EXPERIMENT")) %>% 
 #   map_chr(~str_split(., " ") %>% unlist() %>% pluck(8))
  

  data_plangro <- suppressWarnings(map(skip, ~fread(file, skip = .x))) %>% 
    set_names(exp_names) %>%
    map(~.x %>% mutate(across(where(is.character), as.numeric))) %>% 
    bind_rows(.id = "exp_file") %>% 
    mutate(date = lubridate::make_date(`@YEAR`)+DOY-1)
    

  
  return(data_plangro)
    
}


## read weather dssat - "Weather.OUT" file
read_wth <- function(dir_run){
  
  
  file <- paste0(dir_run, "Weather.OUT")
  skip <- read_lines(file)  %>% str_detect("@YEAR") %>% which()-1 
  
  cal_summ <- function(data){
    
    data %>% tibble %>% mutate(across(.fns = as.numeric)) %>%
      summarise(t_max_acu = sum(TMXD), t_min_acu = sum(TMND), srad_acu = sum(SRAD))
    
  }
  
  data_wth <- suppressWarnings(map(skip, ~fread(file, skip = .x))) #%>% 
 #   map(cal_summ)
  
  
  data_wth %>% bind_rows(.id = "scenario")
  
}
