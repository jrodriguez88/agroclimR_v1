# Script to read Outputs files of DSSAT model
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023






#library(data.table)
#library(tidyverse)




read_evaluate <- function(file){
  
  suppressMessages(suppressWarnings(read_table(file, skip = 2, col_types = cols())))
  
  
}

# SDAT PDAT    EDAT    ADAT    MDAT    HDAT

read_summary <- function(file){
  
  summary_out <- suppressWarnings(read_table(file, skip = 3 , na = "*******",
                                             col_types = cols(SDAT = col_character(),
                                                              PDAT = col_character(), 
                                                              EDAT = col_character(),
                                                              ADAT = col_character(),
                                                              MDAT = col_character(),
                                                              HDAT = col_character()))) %>%
    mutate(SDAT = as.Date(SDAT, format("%Y%j")), 
           PDAT = as.Date(PDAT, format("%Y%j")), 
           EDAT = as.Date(EDAT, format("%Y%j")),
           ADAT = as.Date(ADAT, format("%Y%j")),
           MDAT = as.Date(MDAT, format("%Y%j")),
           HDAT = as.Date(HDAT, format("%Y%j"))) %>%
    dplyr::select(SDAT, PDAT, everything())
  
  
  return(summary_out)
}






read_plant_out <- function(file) {
  
  # file <- paste0(path, plant_grout_out)
  # skip <- 12
  fread_by_skip <- function(file, skip){
    
    options(warn = -1)
    fread(file, skip = skip, stringsAsFactors = F, na.strings = "NaN", header = T, sep = " ", showProgress = T) %>%
      tbl_df
    # mutate(`@YEAR` = as.character(`@YEAR`))
  }
  
  # make function that extract number of treatment
  
  ## now read the treatment 
  lplant_grout <- read_lines(file)
  
  ## treatment data frame (and show how many treatments is there)
  tn_df <- lplant_grout %>%
    str_subset('TREATMENT') %>%
    str_extract("TREATMENT\\s+([[:alnum:]]+)") %>%
    data_frame(TN = .) %>%
    separate(TN, into = c("Treatment", "TRNO"),  sep = "\\s+") %>%
    mutate(RUN = 1:length(Treatment), 
           TRNO = as.factor(TRNO))
  
  
  ## number of treatments
  run <- str_which(lplant_grout, 'TREATMENT') + 3 
  
  ## make data frame which contain line to read in plantgro.out
  
  tn_df <- tn_df %>%
    mutate(files = rep(file, length(RUN))) %>%
    bind_cols(data_frame(line= run))
  
  plant_grout_df <- tn_df %>%
    mutate(data = map2(files, run, fread_by_skip))  %>%
    unnest %>%
    mutate(DOY = as.factor(DOY)) %>%
    mutate(TRNO = as.factor(TRNO)) %>%
    rename(YEAR = `@YEAR`)
  
  
  return(plant_grout_df)
  
}
