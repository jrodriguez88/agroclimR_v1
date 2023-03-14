



test_data$data$input_data[[1]]

test_data$data
test_data$phen$data






## Write Aquacrop Experimental Projects


input_data <- test_data$data$input_data[[1]]


out_path


aquacrop_files, plugin_path  === base_path


tidy_to_exp_aquacrop



input


#funcion para remover separadores "_" de las variables a analizar
remove_unders <- function(var){str_replace_all(var, "_", "")}


#tabla de experimentos crea nombre de archivos experimentales == ID
exp_data <- input_data$AGRO_man %>% 
  mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
  mutate(PDAT = as.Date(PDAT), exp_file  = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) #%>% 
           #paste0(out_path, .,".exp")) 



# Extrae datos por componente del archivo experiental

# Datos  de fertilizacion
#FERT <- nest(input_data$FERT_obs, FERT_obs = - ID) 

# Datos de Fenologia
PHEN <- nest(input_data$PHEN_obs, PHEN_obs = - ID)

# Datos de crecimiento y desarrollo
PLANT <- nest(input_data$PLANT_gro, PLANT_gro = - ID)

# Datos de Rendimiento
YIELD <-  nest(input_data$YIELD_obs, YIELD_obs = - ID)

if(any(colnames(exp_data) == "SBDUR")){} else {
  exp_data <- mutate(exp_data, SBDUR  = NA)
} 

if(any(colnames(exp_data) == "TRDAT")){} else {
  exp_data <- mutate(exp_data, TRDAT  = NA)
}


to_write_exp <- purrr::reduce(list(exp_data, PHEN, PLANT, YIELD), left_join, by = "ID")











write_exp_aquacrop <- function(id_name, sowing_dates, cultivar, soil, clim_data, co2, id2, max_crop_duration = 140, aquacrop_files, ){
  
  ## Create sowing dates vector, use when requiere 1 date 
  #    sowing_dates  <- c(sowing_date - (5:1), sowing_date + (0:4))
  ##### add function // eval inputs 
  
  
  plugin_path
  
  
  
  ### load aquacrop files
  clim_file <- 
  co2_file <-  paste0(aquacrop_files, ".CO2")
  crop_file <- list.files(aquacrop_files, pattern = paste0(cultivar, ".CRO"))
  irri_file <- list.files(aquacrop_files, ".IRR") %>% c(., "rainfed")
  man_file <-  if(length(list.files(aquacrop_files, ".MAN")) == 0){"none"} else {list.files(aquacrop_files, ".MAN")}
  soil_file <- list.files(aquacrop_files, paste0(soil, ".SOL"))
  ini_file <-  if(length(list.files(aquacrop_files, ".SW0")) == 0){"none"} else {list.files(aquacrop_files, ".SW0")}
  proj_file <- list.files(aquacrop_files, ".PRM")
  
  ### Default parameters,  
  def_params <- read_lines(paste0(aquacrop_files, proj_file), skip = 6, n_max = 21) 
  
  
  ### Create multiple combinations of params
  params <- expand.grid(aquacrop_files,
                        clim_file,
                        co2_file,
                        crop_file,
                        irri_file, 
                        man_file,
                        soil_file,
                        ini_file,
                        max_crop_duration,
                        sowing_dates) %>% 
    as_tibble() %>%
    setNames(c("aquacrop_files",
               "clim_file",
               "co2_file",
               "crop_file",
               "irri_file", 
               "man_file",
               "soil_file",
               "ini_file",
               "max_crop_duration",
               "sowing_date"))
  
  
  ## Function to calculate and create crop growing cycles
  cal_cycles_project <- function(clim_data,
                                 aquacrop_files,
                                 clim_file,
                                 co2_file,
                                 crop_file,
                                 irri_file, 
                                 man_file,
                                 soil_file,
                                 ini_file,
                                 max_crop_duration,
                                 sowing_date) {
    
    # path files
    path_files <- aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\")
    
    ### extract "GDDays: from sowing to maturity" from CRO_file
    gdd_mt <- read_lines(file = paste0(aquacrop_files, crop_file)) %>%
      str_subset("GDDays: from sowing to maturity|GDDays: from transplanting to maturity") %>% 
      str_extract("[0-9]+") %>% as.numeric
    
    ### extract Base temperature 
    tbase <- read_lines(file = paste0(aquacrop_files, crop_file)) %>%
      str_subset("Base temperature") %>% 
      str_extract("[0-9]+") %>% as.numeric
    
    #    max_crop_duration <- gdd_mt / clim_data %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>% summarise(median(HUH)) %>% pull(1)
    
    # calculate crop duration 
    crop_duration <- clim_data %>% 
      dplyr::filter(date >= sowing_date,
                    date <= sowing_date + max_crop_duration - 1) %>%
      mutate(HUH = ((tmax + tmin)/2) - tbase) %>%
      mutate(sum_gdd = cumsum(HUH)) %>%
      dplyr::filter(sum_gdd<= gdd_mt) %>% 
      count() %>% pull(n)
    
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
      cat(paste0(path_files), sep = '\n')
      cat("1.1 Temperature (TMP) file", sep = '\n')
      cat(paste0(clim_file, ".Tnx"), sep = '\n') 
      cat(paste0(path_files), sep = '\n')
      cat("1.2 Reference ET (ETo) file", sep = '\n')
      cat(paste0(clim_file, ".ETo"), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("1.3 Rain (PLU) file", sep = '\n')
      cat(paste0(clim_file, ".PLU"), sep = '\n')
      cat(paste0(path_files), sep = '\n')
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
      cat(paste0(path_files), sep = '\n')
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
  runs_cal <- function(params, clim_data) {
    
    params %>% mutate(runs = cal_cycles_project(clim_data, 
                                                aquacrop_files,
                                                clim_file,
                                                co2_file,
                                                crop_file,
                                                irri_file, 
                                                man_file,
                                                soil_file,
                                                ini_file,
                                                max_crop_duration,
                                                sowing_date)) 
    
  }
  
  sim_cycles <- split(params, 1:nrow(params)) %>% 
    map(., ~runs_cal(., clim_data)) %>%
    bind_rows() 
  
  
  ## Write PRM files
  write_projects <- function(sim_cycles, path, def_params, soil){
    
    #    description <-  paste(unique(sim_cycles$crop_file), 
    #                       unique(sim_cycles$clim_file),
    #                       unique(sim_cycles$soil_file),
    #                       unique(sim_cycles$irri_file), sep = " - ")
    
    prm_name <- paste0(unique(sim_cycles$clim_file), "_",
                       unique(sim_cycles$crop_file), "_",
                       soil, "_", id2, "_", 
                       unique(sim_cycles$irri_file)) %>% 
      str_replace_all(pattern = "[.]+", replacement = "") %>%
      paste0(., ".PRM")
    
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
      ~write_projects(.x, plugin_path, def_params, soil))
  
  #    toc()
  #25.57 sec elapsed by 1 crop, 
}

