# Script to Calibrate ORYZA model with Genetic algorithms (GA)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023


## Load packages
#library(GA)
#library(tictoc)
#library(parallel)
#library(doFuture)


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
  sing_tables <- default_list %>% dplyr::filter(str_detect(Parameter, pattern = "DRLVT|SLATB|FSHTB", negate = F)) %>% 
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
  comb_final <- bind_rows(sing_tables, part_table, params_to_cal %>% dplyr::filter(str_detect(Parameter, "SLATB|FSHTB|DRLVT|FLVTB|FSTTB|FSOTB", negate = T)) %>% 
              mutate(tables = list(rep(NULL, 3))))
  
  return(comb_final)
  
  
}



## Oryza Phenological parameters
#x1 <- 0.0008554438  #"DVRJ"
#x2 <- 0.0007576     #"DVRI"
#x3 <- 0.0005704062  #"DVRP"
#x4 <- 0.002219568   #"DVRR"

## Funcion de optimizacion de parametros fenologicas del modelo oryza
cal_phen_oryza <- function(x1, x2, x3, x4, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
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
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(c("phen"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}



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
cal_growth_oryza <- function(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  safe_bind <- purrr::possibly(bind_rows, otherwise = NULL) 
  
  ## Oryza "Leaf and stem growth|Growth AGB-RZ"
  
  #x1 = 20          "SLATB"
  #x2 = 10          "FSHTB"
  #x3 = 15          "DRLVT"
  #x4 = 12          "BFTB" 
  #x5 = 0.0085      "RGRLMX
  #x6 = 0.004       "RGRLMN
  #x7 = 0.004337274 "SLAMAX
  #x8 = 0.2453043   "FSTR"
  
  pgparams <- list(
    
  otherparams =  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8)) %>% 
    dplyr::filter(str_detect(Parameter, "SLATB|FSHTB|DRLVT|BFTB", negate = T)),
  
  
  BPF =  params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x4)) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
  
  SLA = params_to_cal %>% dplyr::filter(Parameter == "SLATB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x1)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "SLATB") %>% nest(Set_cal = -Parameter),
    
  
  FSH = params_to_cal %>% dplyr::filter(Parameter == "FSHTB") %>% pull(tables) %>% pluck(1) %>%
    dplyr::filter( id == as.integer(x2)) %>% 
    pull(data) %>% pluck(1) %>% mutate(Parameter = "FSHTB") %>% nest(Set_cal = -Parameter),
  
  DRL = params_to_cal %>% dplyr::filter(Parameter == "DRLVT") %>% pull(tables) %>% pluck(1) %>%
    dplyr::filter( id == as.integer(x3)) %>% 
    pull(data) %>% pluck(1) %>% mutate(Parameter = "DRLVT") %>% nest(Set_cal = -Parameter))
 
      
     
  

  
  
  test_params_growth <-  safe_bind(pgparams) %>% bind_rows(phen_params)
  
  
  
  
  params_to <- test_params_growth %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_oryza <- params_to$to_test %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(c("dry_matter", "lai"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% 
                        bind_rows() %>% filter(var %in% c("WAGT", "LAI"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}

#cal_growth_oryza(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path)


# Oryza Yield and stress parameters
#x1 = 64900      #"SPGF"  
#x2 = 0.0000249  #"WGRMX"  
#x3 = 0.4        #"ZRTMCD"  
#x4 = 1.45       #"ULLE"   
#x5 = 1404       #"LLLE"   
#x6 = 0.4        #"FSWTD"    
#x7 = 21         #"COLDREP"
#x8 = 36.5       #"CTSTER"   

#params_to_cal <- anti_join(test_params_model, bind_rows(phen_params, grow_params))

#Funcion de optimizacion para parametros de rendimiento y estreses abioticos
cal_yield_oryza <- function(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, phen_params, growth_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  
  ##Crea set de parametros calibrados
  
  test_params <-  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8)) %>%
    bind_rows(phen_params, growth_params)
  
  
  
  
  
  
  
  params_to <- test_params %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_oryza <- params_to$to_test %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(c("yield", "dry_matter"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows() %>% 
                        dplyr::filter(var %in% c("WAGT", "YIELD"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}



#x1 <- 20          #"SLATB"
#x2 <- 10          #"FSHTB"
#x3 <- 15          #"DRLVT"
#x4 <- 12          #"BFTB" 
#x5 = 0.0008554438  #"DVRJ"
#x6 = 0.0007576     #"DVRI"
#x7 = 0.0005704062  #"DVRP"
#x8 = 0.002219568   #"DVRR"
#x9 = 0.0085      #"RGRLMX
#x10 = 0.004       #"RGRLMN
#x11 = 0.004337274 #"SLAMAX
#x12 = 0.2453043   #"FSTR"
#x13 = 64900      #"SPGF"  
#x14 = 0.0000249  #"WGRMX"  
#x15 = 0.4        #"ZRTMCD"  
#x16 = 1.45       #"ULLE"   
#x17 = 1404       #"LLLE"   
#x18 = 0.4        #"FSWTD"    
#x19 = 21         #"COLDREP"
#x20 = 36.5       #"CTSTER"  
#test_params_oryza <- tidy_to_write_crop(final_params)
#params_to_cal <- generate_combinations_paramsTb(test_params_oryza, default_list , 1000)

#phenol  <- phen_params
#Funcion de optimizacion para parametros 22 parametros de ORYZA v3 - requiere combinacion de tablas 
cal_oryza_global <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20,  params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield"), phenol = NULL){
  
  
  ##Crea set de parametros  a calibrar 
  
  

    
    pgparams <- list(
      
      otherparams =  tibble(Parameter = params_to_cal$Parameter,
                            Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)) %>% 
        dplyr::filter(str_detect(Parameter, "SLATB|FSHTB|DRLVT|BFTB", negate = T)),
      
      
      BPF =  params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x4)) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      SLA = params_to_cal %>% dplyr::filter(Parameter == "SLATB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x1)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "SLATB") %>% nest(Set_cal = -Parameter),
      
      
      FSH = params_to_cal %>% dplyr::filter(Parameter == "FSHTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x2)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "FSHTB") %>% nest(Set_cal = -Parameter),
      
      DRL = params_to_cal %>% dplyr::filter(Parameter == "DRLVT") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x3)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "DRLVT") %>% nest(Set_cal = -Parameter))
    
    
  
  
  params_to <- bind_rows(pgparams) 
  
  crop_params_oryza <- params_to$Set_cal %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(res_var, 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows() %>% 
    dplyr::filter(var %in% c("IDAT", "FDAT", "MDAT", "LAI", "WAGT", "YIELD"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}







#Arguments
#path_proj <- "D:/00_DEVELOPER/oryza_2022/test_oryza1"
#
#
#basedata_path <- paste0(path_proj)
#calibration_path <- paste0(path_proj, "/calibration/")
#dir.create(calibration_path)
##unlink(calibration_path, recursive = T)
#
#registerDoFuture()
#cl <- makeCluster(8)
#plan(future::cluster, workers = cl)
## plan(multiprocess)
#
#
#cal_stages <- c("phen", "dry_matter_lai", "yield", "global")
#cal_stages <- c("phen", "dry_matter_lai", "yield")
##cal_stages <- c("phen", "lai", "dry_matter", "yield", "global")
##cal_stages <- c("phen", "dry_matter", "lai", "yield", "global")
#cal_stages <- c("phen", "yield")    # same that : cal_stages <- c("phen", "global") *res_var = c("yield")
#cal_stages <- c("global")
#cal_stages <- c("phen")


### Funcion que realiza optimizacion de parametros de oryza basado en fases 
calibration_oryza_GA <- function(calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path, 
                                 cal_stages = c("phen", "dry_matter_lai", "yield"), 
                                 pop_iter = c(20, 5),  res_var = c("yield"), ncores = 4){
  
  
  message("This process can take some time. Go for coffee :V")
  
  
  # plan(multiprocess)
  registerDoFuture()
  cl <- makeCluster(ncores)
  plan(future::cluster, workers = cl)
  
  
  safe_bind <- purrr::possibly(bind_rows, otherwise = NULL) 
  
  
  ##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
  default_list <- tidy_to_write_crop(NULL)
  phen_pattern <- "DVRJ|DVRI|DVRP|DVRR"
  growth_pattern <- "FLVTB|FSTTB|FSOTB|SLATB|FSHTB|DRLVT|RGRLMX|RGRLMN|SLAMAX|FSTR"
  yield_pattern <- "SPGF|WGRMX|ZRTMCD|ULLE|LLLE|FSWTD|COLDREP|CTSTER"
  
  
  ### Separa tamaño de la poblacion, maximo de iteraciones y numero de escenarios a simular en tablas de particion
  pop_size <- pop_iter[1]
  max_iter <- pop_iter[2]
  n_escenarios <- pop_size*5 
  if(n_escenarios>10000){n_escenarios <- 10000}
  
  
  ### Posibles configuraciones de calibracion 
  if(all(cal_stages %in% c("global"))) {
    
    ### all parameters of oryza
    test_params_global <- tidy_to_write_crop(test_params_oryza)
    
    global_to_cal <- generate_combinations_paramsTb(test_params_global, default_list, length_escenaries =  n_escenarios)
    
    # plan(multiprocess)
#    registerDoFuture()
#    cl <- makeCluster(ncores)
#    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_oryza <<- ga(type = "real-valued", 
                    fitness = function(x) -cal_oryza_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20],
                                                            global_to_cal, calibration_path, cultivar, 
                                                            input_data, exp_files, default_list, basedata_path, res_var = res_var),
                    lower = low_min1, 
                    upper = upp_max1, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_par1)
    
    GA_oryza@solution
    toc()
    
    closeAllConnections()
    gc()
    
    
    oryza_paramsGA <- as.data.frame(GA_oryza@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
    gparams <- list(
      
      BFT = global_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter(id == as.integer(filter(oryza_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      other_t = map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(global_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
                      dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
                      pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(),
      
      other_p = tibble(Parameter = global_to_cal$Parameter,
                       Set_cal = oryza_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
    
    
    
    global_params <<-  safe_bind(gparams)
    
    
    #closeAllConnections()
    message("GA - Global calibration done!")
    return(list(params = global_params, GA_global = GA_oryza))
    
    message("Parameter Optimization Done!")
    

    }
  else if(all(cal_stages %in% c("phen"))) {
    
    ## Filtrar los parametros a Calibrar --- Debe contener las columnas Base, Min y Max
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, "DVRJ|DVRI|DVRP|DVRR"))
    
    #1. Phenological development parameters 
    tic(paste0("Phenology Calibration"))
    GA_phen <- ga(type = "real-valued", fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = params_to_cal$Min %>% unlist(), upper = params_to_cal$Max %>% unlist(), maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = params_to_cal$Parameter %>% unlist())
    
    GA_phen@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    closeAllConnections()
    
    return(list(params = phen_params, GA_phen = GA_phen))
    
    
    
    
    
  
    
    } 
  else if(all(cal_stages %in% c("phen", "yield"))) {
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenology Calibration"))
    GA_phen <- ga(type = "real-valued", fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = params_to_cal$Min %>% unlist(), upper = params_to_cal$Max %>% unlist(), maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = params_to_cal$Parameter %>% unlist())
    
    GA_phen@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    phen_params <- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- phen_params %>% right_join(test_params_oryza, by = "Parameter") %>%
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
      dplyr::select(Parameter, Set_cal = Base)
    params_to_cal <- test_params_model %>% filter(str_detect(Parameter, yield_pattern))
    
    
    
    ## Yield
    #9. Temperature and drought stress parameters
    
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          params_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_model, basedata_path),
                   lower = params_to_cal$Min %>% unlist(), 
                   upper = params_to_cal$Max %>% unlist(), 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = params_to_cal$Parameter %>% unlist())
    
    GA_yield@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    yield_params <- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
#    load("calibracion_rendimiento_test.RData")
    
    
    phen_yield_params <- bind_rows(phen_params, yield_params) # %>% deframe()
    
    closeAllConnections()
    
    return(list(params = phen_yield_params, GA_phen = GA_phen, GA_yield = GA_yield))
    
    
    
    
    
    
    
    
  
    
    
    
    
    } 
  else if(all(cal_stages %in% c("phen", "global"))) {
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenology Calibration"))
    GA_phen <- ga(type = "real-valued", fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = params_to_cal$Min %>% unlist(), upper = params_to_cal$Max %>% unlist(), maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = params_to_cal$Parameter %>% unlist())
    
    GA_phen@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    phen_params <- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- phen_params %>% right_join(test_params_oryza, by = "Parameter") %>%
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    # growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
    #   dplyr::select(Parameter, Set_cal = Base)
    params_to_cal <- generate_combinations_paramsTb(test_params_model, default_list, length_escenaries =  n_escenarios)
    
    
    tic("Global calibration")
    GA_oryza <- ga(type = "real-valued", 
                   fitness = function(x) -cal_oryza_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20],
                                                           params_to_cal, calibration_path, cultivar, 
                                                           input_data, exp_files, default_list, basedata_path, res_var = res_var),
                   lower = params_to_cal$Min %>% unlist(), 
                   upper = params_to_cal$Max %>% unlist(), 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = params_to_cal$Parameter %>% unlist())
    
    GA_oryza@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    
    oryza_paramsGA <- as.data.frame(GA_oryza@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
    oryza_params <-    bind_rows(
      
      params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(params_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
            dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
            pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(), 
      
      tibble(Parameter = params_to_cal$Parameter,
             Set_cal = oryza_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% 
        mutate(Set_cal =  map(Set_cal, ~.x)))
    
    closeAllConnections()
    
    return(list(params = oryza_params, GA_phen = GA_phen, GA_global = GA_oryza))
    
    
    
  
    
    
    
    } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield"))) {
    
    
    message("ORYZA v3.0 - Genetic Algorithm
            
            - Parameter Optimization - 3 stage:
            ")
    
    ##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
    default_list <- tidy_to_write_crop(NULL)
    phen_pattern <- "DVRJ|DVRI|DVRP|DVRR"
    growth_pattern <- "FLVTB|FSTTB|FSOTB|SLATB|FSHTB|DRLVT|RGRLMX|RGRLMN|SLAMAX|FSTR"
    yield_pattern <- "SPGF|WGRMX|ZRTMCD|ULLE|LLLE|FSWTD|COLDREP|CTSTER"
    
    phen_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- filter(test_params_oryza, str_detect(Parameter, growth_pattern))
    

    #   dplyr::select(Parameter, Set_cal = Base) %>% filter(test_params_model, str_detect(Parameter, growth_pattern))
    growth_to_cal <- generate_combinations_paramsTb(test_params_model, default_list, length_escenaries =  n_escenarios)
    
    
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                            growth_to_cal, phen_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_oryza, basedata_path),
                    lower =  low_ming, 
                    upper = upp_maxg, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_parg)
    
    GA_growth@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    grow_paramsGA <- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #  mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    pparams <- list(
      
      BFT = growth_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
           dplyr::filter(id == as.integer(filter(grow_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
           pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
         
         other_t = map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(growth_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
               dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
               pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(),
         
         other_p = tibble(Parameter = growth_to_cal$Parameter,
                           Set_cal = grow_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
         
   # safe_bind <- possibly(bind_rows, otherwise = NULL)   
    
    growth_params <<-  safe_bind(pparams)
      

    #test_params_model <- bind_rows(phen_params, growth_params) %>% right_join(test_params_oryza, by = "Parameter") %>%
      #mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    
    
    yield_to_cal <- test_params_oryza %>% filter(str_detect(Parameter, yield_pattern))
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
   low_min <- yield_to_cal$Min %>% unlist()
   upp_max <- yield_to_cal$Max %>% unlist()
   names_par <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          yield_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_oryza, basedata_path),
                   lower = low_min, 
                   upper = upp_max, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_par)
    
    GA_yield@solution
    toc()
    
    closeAllConnections()
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Yield done!")
    

    
    message("Parameter Optimization Done!")
    
    return(list(params = bind_rows(phen_params, growth_params, yield_params), 
                GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield))
    
    
  
    
    
    
    
    } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield", "global"))) {
    
    message("ORYZA v3.0 - Genetic Algorithm
            4 stages:
            ")
    
    ##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
    default_list <- tidy_to_write_crop(NULL)
    phen_pattern <- "DVRJ|DVRI|DVRP|DVRR"
    growth_pattern <- "FLVTB|FSTTB|FSOTB|SLATB|FSHTB|DRLVT|RGRLMX|RGRLMN|SLAMAX|FSTR"
    yield_pattern <- "SPGF|WGRMX|ZRTMCD|ULLE|LLLE|FSWTD|COLDREP|CTSTER"
    
    phen_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- filter(test_params_oryza, str_detect(Parameter, growth_pattern))
    
    
    #   dplyr::select(Parameter, Set_cal = Base) %>% filter(test_params_model, str_detect(Parameter, growth_pattern))
    growth_to_cal <- generate_combinations_paramsTb(test_params_model, default_list, length_escenaries =  n_escenarios)
    
    
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                            growth_to_cal, phen_params, calibration_path, cultivar, 
                                                            input_data, exp_files, test_params_oryza, basedata_path),
                    lower =  low_ming, 
                    upper = upp_maxg, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_parg)
    
    GA_growth@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    grow_paramsGA <- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #  mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    pparams <- list(
      
      BFT = growth_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter(id == as.integer(filter(grow_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      other_t = map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(growth_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
                      dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
                      pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(),
      
      other_p = tibble(Parameter = growth_to_cal$Parameter,
                       Set_cal = grow_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
    
   # safe_bind <- possibly(bind_rows, otherwise = NULL)   
    
    growth_params <<-  safe_bind(pparams)
    
    
    #test_params_model <- bind_rows(phen_params, growth_params) %>% right_join(test_params_oryza, by = "Parameter") %>%
    #mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    
    
    yield_to_cal <- test_params_oryza %>% filter(str_detect(Parameter, yield_pattern))
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_min <- yield_to_cal$Min %>% unlist()
    upp_max <- yield_to_cal$Max %>% unlist()
    names_par <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          yield_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_oryza, basedata_path),
                   lower = low_min, 
                   upper = upp_max, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_par)
    
    GA_yield@solution
    toc()
    
    closeAllConnections()
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Yield done!")
    
    
    
    message(paste0("4th Stage: GA Global - Parameters: ", phen_pattern, growth_pattern, yield_pattern))
    
    test_params_global <- bind_rows(phen_params, growth_params, yield_params) %>% tidy_to_write_crop()
    # growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
    #   dplyr::select(Parameter, Set_cal = Base)
    global_to_cal <- generate_combinations_paramsTb(test_params_global, default_list, length_escenaries =  n_escenarios)
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_oryza <<- ga(type = "real-valued", 
                   fitness = function(x) -cal_oryza_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20],
                                                           global_to_cal, calibration_path, cultivar, 
                                                           input_data, exp_files, default_list, basedata_path, res_var = res_var),
                   lower = low_min1, 
                   upper = upp_max1, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_par1)
    
    GA_oryza@solution
    toc()
    
    closeAllConnections()
    gc()
    
    
    oryza_paramsGA <- as.data.frame(GA_oryza@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    
    BFT <-  global_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
      dplyr::filter(id == as.integer(filter(oryza_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
      pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal")
    
    
    other_t <- map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(global_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
                    dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
                    pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows()
    
    other_p <- tibble(Parameter = global_to_cal$Parameter,
                     Set_cal = oryza_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    gparams <- list(BFT, other_t, other_p)
    
    global_params <<-  safe_bind(gparams)
    
    
    #closeAllConnections()
    message("GA - Global calibration done!")
    return(list(parameters_final = global_params, parameters_3stage = test_params_global, GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield, GA_global = GA_oryza))
    
    message("Parameter Optimization Done!")
    
    
  }
  else{message("Unknown Calibration Stages")}
  
  
  message("Parameter Optimization Done!")
  
  
}






# cal_stages <- c("phen", "dry_matter_lai", "yield", "global")
#cal_stages <- c("phen", "dry_matter_lai", "yield")
#cal_stages <- c("phen", "lai", "dry_matter", "yield", "global")
#cal_stages <- c("phen", "dry_matter", "lai", "yield", "global")
#cal_stages <- c("phen", "yield")    # same that : cal_stages <- c("phen", "global") *res_var = c("yield")
#cal_stages <- c("global")
#cal_stages <- c("phen")




#pop_iter  = c(15, 4)


#load("test_data_GA.RData")


#testf2000_0227 <- calibration_oryza_GA(calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path, cal_stages = cal_stages, pop_iter = pop_iter, ncores = 8)


#save(calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path, testf2000_GA_cal_3stages, file = "test_data_GA.RData")



#closeAllConnections()
#gc()


#exp_files <- list.files(paste0(basedata_path, "/EXP/"), pattern = "\\.exp$")
#exp_files <- exp_files[str_detect(exp_files, "YOCS_FED2000_MADRI_S1.exp|VVME_FED2000_COL_S2.exp|YOCS_FED2000_MADRI_S3.exp|VVME_FED2000_COL_S4.exp", negate = T)]
#exp_riego_files <- exp_files %>% str_subset("AIHU|SDTO|MRCO")
#exp_secano_files <- exp_files %>% str_subset("VVME|YOCS")
