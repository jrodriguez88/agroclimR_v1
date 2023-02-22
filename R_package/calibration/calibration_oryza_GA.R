# Script to Calibrate ORYZA model with Genetic algorithms (GA)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023


## Load packages
library(GA)
library(tictoc)
library(parallel)
library(doFuture)


#Arguments
#path_proj <- "D:/00_DEVELOPER/oryza_2022/test_oryza1"
#
#params_to_cal <- test_params_oryza %>% dplyr::filter(Component == "Phenology")   #Subset of params to calibrate --> Default list 
#input_data <- test_data$data$input_data   #Ibserved data in INPUT_data file fortmat
#exp_files <- list.files(paste0(basedata_path, "/EXP/"), pattern = "\\.exp$")
#exp_files <- exp_files[str_detect(exp_files, "YOCS_FED2000_MADRI_S1.exp|VVME_FED2000_COL_S2.exp|YOCS_FED2000_MADRI_S3.exp|VVME_FED2000_COL_S4.exp", negate = T)]
#exp_files <- exp_files %>% str_subset("AIHU|SDTO|MRCO")
#
#test_params_model <- default_list
#basedata_path <- paste0(path_proj)   ##  WTH, SOIL, EXP, ORYZA3.EXE
#cultivar <- "F2000"
#calibration_path <- paste0(path_proj, "/calibration/") -> cal_path


# Load Functions
# Funcion copia inputs base en directorio de simulacion de cada setups
copy_inputs_oryza <- function(dest_path, basedata_path){
  
  dir_files <- list.files(basedata_path, full.names = T) %>%
    str_subset("ORYZA3|WTH|SOIL|EXP")
  
  file.copy(dir_files, dest_path, recursive = T)
  
  
  # walk2(.x = c(".sol", ".crp", ".exp"), 
  #       .y = paste0("standard", c(".sol", ".crp", ".exp")), 
  #       ~file.rename(
  #         from = list.files(dir_run, pattern = .x, full.names = T), 
  #         to = paste0(dir_run, .y)))
  # 
  
  
}

# Funcion para crear carpeta de simulacion en directorio raiz
make_dir_run <- function(dir_run_main, id_run){
  
  
  #  require(stringr)
  dir <- paste0(dir_run_main, id_run, '/')
  dir <- stringr::str_replace(dir, "ñ", "n")
  
  if (!dir.exists(dir)) { 
    
    dir.create(dir, showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir))
}

# Funcin para generar multiples combinaciones de tablas en un rango determinado 
generate_combinations_paramsTb <- function(params_to_cal, default_list, length_escenaries = 2000){
  
  # Función para generar combinaciones
  generate_combinations <- function(min1, max1, min2, max2, min3, max3) {
    # Crear un data frame con los rangos mínimo y máximo de cada factor
    ranges <- data.frame(min=c(min1, min2, min3), max=c(max1, max2, max3))
    
    # Generar todas las combinaciones posibles de los factores
    combinations <- expand.grid(seq(ranges[1,1], ranges[1,2], by = 0.001),
                                seq(ranges[2,1], ranges[2,2],  by = 0.001),
                                seq(ranges[3,1], ranges[3,2],  by = 0.001)) 
    
    # Filtrar combinaciones que sumen 1
    combinations <- combinations[rowSums(combinations) == 1, ]
    
    return(combinations)
    
  }
  
  
  ### Filtrar parametros en tablas
  
  ## Partition Tables
  part_tables <- default_list %>% dplyr::filter(str_detect(Parameter, "FSOTB|FSTTB|FLVTB")) 
  
  ## Single time series table   
  sing_tables <- params_to_cal %>% dplyr::filter(str_detect(Parameter, pattern = "DRLVT|SLATB|FSHTB", negate = F)) %>% 
    mutate(tables = map2(Min, Max, ~left_join(.x, .y, by = "DVS") %>% set_names(c("DVS", "min", "max")))) %>% #slice(1) %>%
    mutate(tables = map(tables, ~.x %>% mutate(table = map2(min, max, ~seq(from = .x, to =  .y, length.out = length_escenaries))))) %>%
    mutate(tables =  map(tables, ~.x %>% mutate(table = map(table, enframe, name = "id")) %>% 
                           dplyr::select(-c(min, max)) %>% unnest(table) %>% 
                           nest(data = -id))) %>% 
    mutate(Base = rep(list(as.integer(length_escenaries/2)), 3), Min = rep(list(as.integer(0)), 3), Max = rep(list(as.integer(length_escenaries)), 3))
  
  
  #a$tables[[2]] %>% slice(23) %>% pull(data)
  
  
  
  ### Genera combinaciones de tablas de los parametros de particion de biomasa
  c <- part_tables %>% 
    mutate(tables = map2(Min, Max, ~left_join(.x, .y, by = "DVS") %>% set_names(c("DVS", "min", "max"))))
  
  
  d <- c$tables  %>% reduce(left_join, by = "DVS") %>% 
    mutate(combinations = pmap(list(min1 = min.x, max1 = max.x, min2 = min.y, max2 = max.y, min3 = min, max3 = max), generate_combinations))
  
  
  number_combinations <- length_escenaries  
  
  combinations_bpf<- d %>% mutate(samples = map(combinations, ~sample_n(.x, number_combinations, replace = T) %>% mutate(id = 1:number_combinations)))  %>%
    unnest(samples) %>% dplyr::select(-c(combinations, min.x:max)) %>% rename(FLV = Var1, FST = Var2, FSO = Var3) %>% nest(data = -id) %>% 
    mutate(data = map(data, ~list(dplyr::select(.x, DVS, FLV), dplyr::select(.x, DVS, FST), dplyr::select(.x, DVS, FSO)) %>% set_names(part_tables$Parameter))) #%>%
  #dplyr::select(-data)
  
  
  
  
  ## genera fila con variable de tablas de particion de biomas
  part_table <- part_tables %>% slice(1) %>% 
    mutate(Parameter = "BFTB", Base = list(as.integer(length_escenaries/2)), Min = list(as.integer(0)), Max = list(as.integer(length_escenaries)),
           tables = list(combinations_bpf)) %>% dplyr::select(all_of(names(sing_tables)))
  
  
  #Une los resultados y devuelve la lista con la variable tables para analizar en algoritmo de optimizacion
  bind_rows(sing_tables, part_table, params_to_cal %>% dplyr::filter(str_detect(Parameter, "SLATB|FSHTB|DRLVT|FLVTB|FSTTB|FSOTB", negate = T)) %>% 
              mutate(tables = list(rep(NULL, 3))))
  
  
}



## Oryza Phenological parameters
#x1 <- 0.0008554438  #"DVRJ"
#x2 <- 0.0007576     #"DVRI"
#x3 <- 0.0005704062  #"DVRP"
#x4 <- 0.002219568   #"DVRR"

## Funcion de optimizacion de parametros fenologicas del modelo oryza
cal_phen_oryza <- function(x1, x2, x3, x4, params_to_cal, cal_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  ## Oryza Phenological parameters
  #x1 <- 0.0008554438  #"DVRJ"
  #x2 <- 0.0007576     #"DVRI"
  #x3 <- 0.0005704062  #"DVRP"
  #x4 <- 0.002219568   #"DVRR"
  
  params_to_cal <- tibble(Parameter = params_to_cal$Parameter,
                          Set_cal = list(x1, x2, x3, x4)) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_oryza <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(cal_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_ORYZA(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(c("phen"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(-mean(metrics_cal$RMSE))
  
  
  
}
# GA <- ga(type = "real-valued", fitness = function(x) -f(x[1], x[2], x[3]), lower = c(21, 9, 10), upper = c(30, 15, 23), maxiter = 4)



## Oryza "Leaf and stem growth|Growth AGB-RZ"

#x1 = 20          #"SLATB"
#x2 = 10          #"FSHTB"
#x3 = 15          #"DRLVT"
#x4 = 12          #"BFTB" 
#x5 = 0.0085      #"RGRLMX
#x6 = 0.004       #"RGRLMN
#x7 = 0.004337274 #"SLAMAX
#x8 = 0.2453043   #"FSTR"

#params_to_cal <- test_params_model %>% dplyr::filter(str_detect(Component, "Leaf and stem growth|Growth AGB-RZ")) %>%
#  dplyr::filter(str_detect(Parameter, "SPGF|WGRMX|ZRTMCD", negate = T))

#params_to_cal <- generate_combinations_paramsTb(params_to_cal, default_list
#Requiere generar multiples combinaciones de tablas de desarrollo


## Funcion de optimizacion de parametros de crecimiento del modelo oryza
cal_grow_oryza <- function(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, fenologia_params, cal_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  ## Oryza "Leaf and stem growth|Growth AGB-RZ"
  
  #x1 = 20          "SLATB"
  #x2 = 10          "FSHTB"
  #x3 = 15          "DRLVT"
  #x4 = 12          "BFTB" 
  #x5 = 0.0085      "RGRLMX
  #x6 = 0.004       "RGRLMN
  #x7 = 0.004337274 "SLAMAX
  #x8 = 0.2453043   "FSTR"
  
  
  test_params <-  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8)) %>% slice(-c(1:4)) %>%
    
    bind_rows(
      
      BPF <- params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x4)) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      SLA <- params_to_cal %>% dplyr::filter(Parameter == "SLATB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x1)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "SLATB") %>% nest(Set_cal = -Parameter),
      
      FSH <- params_to_cal %>% dplyr::filter(Parameter == "FSHTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x2)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "FSHTB") %>% nest(Set_cal = -Parameter),
      
      DRL <- params_to_cal %>% dplyr::filter(Parameter == "DRLVT") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x3)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "DRLVT") %>% nest(Set_cal = -Parameter),
      
      fenologia_params
    )
  
  
  
  
  
  
  
  params_to <- test_params %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_oryza <- params_to$to_test %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(cal_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_ORYZA(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(c("dry_matter", "lai"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
# GA <- ga(type = "real-valued", fitness = function(x) -f(x[1], x[2], x[3]), lower = c(21, 9, 10), upper = c(30, 15, 23), maxiter = 4)



# Oryza Yield and stress parameters
#x1 = 64900      #"SPGF"  
#x2 = 0.0000249  #"WGRMX"  
#x3 = 0.4        #"ZRTMCD"  
#x4 = 1.45       #"ULLE"   
#x5 = 1404       #"LLLE"   
#x6 = 0.4        #"FSWTD"    
#x7 = 21         #"COLDREP"
#x8 = 36.5       #"CTSTER"   


#params_to_cal <- anti_join(test_params_model, bind_rows(fenologia_params, grow_params))


#Funcion de optimizacion para parametros de rendimiento y estreses abioticos
cal_yield_oryza <- function(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, fenologia_params, grow_params, cal_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  
  ##Crea set de parametros calibrados
  
  test_params <-  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8)) %>%
    bind_rows(fenologia_params, grow_params)
  
  
  
  
  
  
  
  params_to <- test_params %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_oryza <- params_to$to_test %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(cal_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_ORYZA(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(c("yield"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$RMSE))
  
  
  
}
# GA <- ga(type = "real-valued", fitness = function(x) -f(x[1], x[2], x[3]), lower = c(21, 9, 10), upper = c(30, 15, 23), maxiter = 4)





