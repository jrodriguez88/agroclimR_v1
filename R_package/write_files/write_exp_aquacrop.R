# Write experimental file AQUACROP (PRM)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023



#path_proj <- "D:/00_DEVELOPER/aquacrop_2023/F2000/"
#id_name <- "AIHU_FED2000_MADRI_S1" 
#clim_name <- "AIHU" -> soil_name
#cultivar <- "F2000"
#sowing_date <- test_data2$PDAT[[1]]
#harvest_date <- test_data2$PDAT[[1]] + 130


write_exp_aquacrop <- function(path_proj, id_name, clim_name, soil_name, cultivar, sowing_date, harvest_date, co2_name = "MaunaLoa", irri_name = "rainfed", man_agro = "none", ini_cond = "none"){
  
  ## Create sowing dates vector, use when requiere 1 date 
  #    sowing_dates  <- c(sowing_date - (5:1), sowing_date + (0:4))
  ##### add function // eval inputs 
  
  
### load aquacrop files
  clim_file <- clim_name
  co2_file <-  paste0(co2_name, ".CO2")
  crop_file <- paste0(cultivar, ".CRO")
  irri_file <- if(irri_name == "rainfed"){irri_name} else {paste0(irri_name, ".IRR")}
  man_file <-  if(man_agro == "none"){"none"} else {paste0(man_agro, ".MAN")}
  soil_file <- paste0(soil_name, ".SOL")
  ini_file <-  if(ini_cond == "none"){"none"} else {paste0(ini_cond, ".SW0")}
#  proj_file <- list.files(aquacrop_files, ".PRM")
  
  ### Default parameters,  
  def_params <- paste(
    "      4         : Evaporation decline factor for stage II
      1.10      : Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface
      5         : Threshold for green CC below which HI can no longer increase (% cover)
     70         : Starting depth of root zone expansion curve (% of Zmin)
      5.00      : Maximum allowable root zone expansion (fixed at 5 cm/day)
     -6         : Shape factor for effect water stress on root zone expansion
     20         : Required soil water content in top soil for germination (% TAW)
      1.0       : Adjustment factor for FAO-adjustment soil water depletion (p) by ETo
      3         : Number of days after which deficient aeration is fully effective
      1.00      : Exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
     12         : Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))
     10         : Thickness top soil (cm) in which soil water depletion has to be determined
     30         : Depth [cm] of soil profile affected by water extraction by soil evaporation
      0.30      : Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment
      1         : CN is adjusted to Antecedent Moisture Class
     20         : salt diffusion factor (capacity for salt diffusion in micro pores) [%]
    100         : salt solubility [g/liter]
     16         : shape factor for effect of soil water content gradient on capillary rise
     12.0       : Default minimum temperature (°C) if no temperature file is specified
     28.0       : Default maximum temperature (°C) if no temperature file is specified
      3         : Default method for the calculation of growing degree days")
  
  
  
  
  
  
  ### Create multiple combinations of params
  params <- expand.grid(path_proj,
                        clim_file,
                        co2_file,
                        crop_file,
                        irri_file, 
                        man_file,
                        soil_file,
                        ini_file,
                        sowing_date) %>% 
    as_tibble() %>%
    setNames(c("path_proj",
               "clim_file",
               "co2_file",
               "crop_file",
               "irri_file", 
               "man_file",
               "soil_file",
               "ini_file",
               "sowing_date"))
  
  
  ## Function to calculate and create crop growing cycles
  cal_cycles_project <- function(path_proj,
                                 clim_file,
                                 co2_file,
                                 crop_file,
                                 irri_file, 
                                 man_file,
                                 soil_file,
                                 ini_file,
                                 sowing_date) {
    
    # path files
    path_files <- path_proj %>% str_replace_all(pattern = "/", replacement = "\\\\")
    path_wth <- paste0(path_proj,  "WTH/") %>% str_replace_all(pattern = "/", replacement = "\\\\")
    path_soil <- paste0(path_proj,  "SOIL/") %>% str_replace_all(pattern = "/", replacement = "\\\\")
    
    #    max_crop_duration <- gdd_mt / clim_data %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>% summarise(median(HUH)) %>% pull(1)
    
    # calculate crop duration 
    crop_duration <- as.numeric(harvest_date - sowing_date)
    # Calculate numeric dates
    first_day <- as.numeric(sowing_date - make_date(1900, 12, 31))
    last_day <- first_day + crop_duration
    mat_date <- as.Date(last_day, origin = make_date(1900, 12, 31))
    
    #Write grow cycles
    path_data <- function(){
      
      cat(paste0(first_day, "    : First day of simulation period - ", format(sowing_date, "%d %b %Y")))
      cat('\n')
      cat(paste0(last_day,  "    : Last day of simulation period - ",  format(mat_date, "%d %b %Y")))
      cat('\n')
      cat(paste0(first_day, "    : First day of cropping period - " , format(sowing_date, "%d %b %Y")))
      cat('\n')
      cat(paste0(last_day,  "    : Last day of cropping period - "  , format(mat_date, "%d %b %Y")))
      cat('\n')    
      cat("-- 1. Climate (CLI) file", sep = '\n')
      cat(paste0(clim_file, ".CLI"), sep = '\n')
      cat(paste0(path_wth), sep = '\n')
      cat("1.1 Temperature (TMP) file", sep = '\n')
      cat(paste0(clim_file, ".Tnx"), sep = '\n') 
      cat(paste0(path_wth), sep = '\n')
      cat("1.2 Reference ET (ETo) file", sep = '\n')
      cat(paste0(clim_file, ".ETo"), sep = '\n')
      cat(paste0(path_wth), sep = '\n')
      cat("1.3 Rain (PLU) file", sep = '\n')
      cat(paste0(clim_file, ".PLU"), sep = '\n')
      cat(paste0(path_wth), sep = '\n')
      cat("1.4 Atmospheric CO2 (CO2) file", sep = '\n')
      cat(paste(co2_file), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("-- 2. Crop (CRO) file", sep = '\n')
      cat(paste(crop_file), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("-- 3. Irrigation (IRR) file", sep = '\n')
      if(irri_file=="rainfed"){
        cat("(None)", sep = '\n')
        cat("(None)", sep = '\n')
      } else {
        cat(paste(irri_file), sep = '\n')
        cat(paste0(path_files), sep = '\n')
      }
      cat("-- 4. Management (MAN) file", sep = '\n')
      if(man_file == "none"){
        cat("(None)", sep = '\n')
        cat("(None)", sep = '\n')
      } else {
        cat(paste(man_file), sep = '\n')
        cat(paste0(path_files), sep = '\n')
      }
      cat("-- 5. Soil profile (SOL) file", sep = '\n')
      cat(paste(soil_file), sep = '\n')
      cat(paste0(path_soil), sep = '\n')
      cat("-- 6. Groundwater (GWT) file", sep = '\n')
      cat("(None)", sep = '\n')
      cat("(None)", sep = '\n')
      cat("-- 7. Initial conditions (SW0) file", sep = '\n')
      if(ini_file == "none"){
        cat("(None)", sep = '\n')
        cat("(None)", sep = '\n')
      } else {
        cat(paste(ini_file), sep = '\n')
        cat(paste0(path_files), sep = '\n')
      }
      cat("-- 8. Off-season conditions (OFF) file", sep = '\n')
      cat("(None)", sep = '\n')
      cat("(None)", sep = '\n')
    }
    
    list(capture.output(path_data()))
    
  }
  
  
  ## Function to compute all runs for params table
  runs_cal <- function(params) {
    
    params %>% mutate(runs = cal_cycles_project(path_proj,
                                                clim_file,
                                                co2_file,
                                                crop_file,
                                                irri_file, 
                                                man_file,
                                                soil_file,
                                                ini_file,
                                                sowing_date)) 
    
  }
  
  sim_cycles <- split(params, 1:nrow(params)) %>% 
    map(., ~runs_cal(.)) %>%
    bind_rows() 
  
  
  ## Write PRM files
  write_projects <- function(sim_cycles, path, def_params, soil){
    
    #    description <-  paste(unique(sim_cycles$crop_file), 
    #                       unique(sim_cycles$clim_file),
    #                       unique(sim_cycles$soil_file),
    #                       unique(sim_cycles$irri_file), sep = " - ")
    
    prm_name <- paste0(id_name, ".PRM")
    
    suppressWarnings(dir.create(paste0(path, "/", "LIST")))
    
    sink(file = paste(path, "LIST", prm_name, sep = "/"), append = F)
    cat(paste("by https://github.com/jrodriguez88"))
    cat('\n')
    cat("6.0       : AquaCrop Version (March 2017)")
    cat('\n')
    writeLines(sim_cycles$runs[[1]][1:4])
    writeLines(def_params)
    writeLines(sim_cycles$runs[[1]][-c(1:4)])
    walk(.x=sim_cycles$runs[-1], ~writeLines(.x))
    sink()    
    
  }
  
  map(.x = split(sim_cycles, 
                 list(sim_cycles$crop_file, 
                      sim_cycles$irri_file, 
                      sim_cycles$soil_file)),
      ~write_projects(.x, path_proj, def_params, soil_name))
  
  #    toc()
  #25.57 sec elapsed by 1 crop, 
}



write_irri_aquacrop <- function(path_proj, irri_name = "irrigated", depletion = 10) {
  
  #Net irrigation requirement (allowable depletion 10 % RAW)
  #   6.1   : AquaCrop Version (May 2018)
  #   1     : Sprinkler irrigation
  # 100     : Percentage of soil surface wetted by irrigation
  #   3     : Determination of Net Irrigation requirement
  #  20     : Allowable depletion of RAW (%)
  
  
  sink(file = paste(path_proj, paste0(irri_name, ".IRR"), sep = "/"), append = F)
  cat(paste0("Net irrigation requirement (allowable depletion ", depletion, " % RAW)"))
  cat('\n')
  cat("   6.1   : AquaCrop Version (May 2018)", sep = '\n')
  cat("   1     : Sprinkler irrigation", sep = '\n')
  cat(" 100     : Percentage of soil surface wetted by irrigation", sep = '\n')
  cat("   3     : Determination of Net Irrigation requirement", sep = '\n')
  cat(paste0("  ", depletion, "     : Allowable depletion of RAW (%)"), sep = '\n')
  sink()   
  
  
  
}

#write_irri_aquacrop(path_proj, "TESTPAULA", 30)


write_man_aquacrop <- function(path_proj, man_agro = "rice", bund_height = 25, mulches = 0,  fert_stress = 7, weeds = 3){
  
  #Soil bunds, 0.25 m height
  #     6.1       : AquaCrop Version (May 2018)
  #     0         : percentage (%) of ground surface covered by mulches IN growing period
  #    50         : effect (%) of mulches on reduction of soil evaporation
  #     7         : Degree of soil fertility stress (%) - Effect is crop specific
  #     0.25      : height (m) of soil bunds
  #     1         : surface runoff affected or completely prevented by field surface practices
  #     0         : N/A (surface runoff is not affected or completely prevented)
  #     3         : relative cover of weeds at canopy closure (%)
  #     0         : increase of relative cover of weeds in mid-season (+%)
  #    -4.00      : shape factor of the CC expansion function in a weed infested field
  
  
  sink(file = paste(path_proj, paste0(man_agro, ".MAN"), sep = "/"), append = F)
  cat(paste0("Soil bunds, 0.", bund_height, " m height"))
  cat('\n')
  cat("     6.1       : AquaCrop Version (May 2018)", sep = '\n')
  cat(paste0(sprintf("%6.0f", mulches), "         : percentage (%) of ground surface covered by mulches IN growing period"), sep = '\n')
  cat("    50         : effect (%) of mulches on reduction of soil evaporation", sep = '\n')
  cat(paste0(sprintf("%6.0f", fert_stress), "         : Degree of soil fertility stress (%) - Effect is crop specific"), sep = '\n')
  cat(paste0("     0.", bund_height, "      : height (m) of soil bunds"), sep = '\n')
  cat("     1         : surface runoff affected or completely prevented by field surface practices", sep = '\n')
  cat("     0         : N/A (surface runoff is not affected or completely prevented)", sep = '\n')
  cat(paste0(sprintf("%6.0f", weeds), "         : relative cover of weeds at canopy closure (%)"), sep = '\n')
  cat("     0         : increase of relative cover of weeds in mid-season (+%)", sep = '\n')
  cat("    -4.00      : shape factor of the CC expansion function in a weed infested field", sep = '\n')
  sink() 
  
}

#write_man_aquacrop(path_proj, "TEST_PAULA")

#write_exp_aquacrop(path_proj, id_name, clim_name, clim_name, cultivar, sowing_date, harvest_date)























