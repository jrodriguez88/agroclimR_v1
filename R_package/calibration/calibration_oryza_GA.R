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
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
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
cal_yield_oryza <- function(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, phen_params, growth_params, cal_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  
  ##Crea set de parametros calibrados
  
  test_params <-  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8)) %>%
    bind_rows(phen_params, growth_params)
  
  
  
  
  
  
  
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
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(c("yield"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}



#x1 <- 0.0008554438  #"DVRJ"
#x2 <- 0.0007576     #"DVRI"
#x3 <- 0.0005704062  #"DVRP"
#x4 <- 0.002219568   #"DVRR"
#x5 = 20          #"SLATB"
#x6 = 10          #"FSHTB"
#x7 = 15          #"DRLVT"
#x8 = 12          #"BFTB" 
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

#Funcion de optimizacion para parametros 22 parametros de ORYZA v3 - requiere combinacion de tablas 
cal_oryza_global <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20,  params_to_cal, cal_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield")){
  
  
  ##Crea set de parametros  a calibrar 
  
  
  test_params <-  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)) %>% slice(-c(1:4)) %>%
    
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
        pull(data) %>% pluck(1) %>% mutate(Parameter = "DRLVT") %>% nest(Set_cal = -Parameter)
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
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(res_var, 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
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
                                 cal_stages = c("phen", "dry_matter_lai", "yield", "global"), 
                                 pop_iter = c(200, 50),  res_var = c("yield"), ncores = 4){
  
  
  message("This process can take some time. Go for coffee :V")
  
  
  # plan(multiprocess)
  registerDoFuture()
  cl <- makeCluster(ncores)
  plan(future::cluster, workers = cl)
  
  
  
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
  
  
  ### Parallel plan
  registerDoFuture()
  cl <- makeCluster(ncores)
  plan(future::cluster, workers = cl)
  
  if(all(cal_stages ==  c("global"))){
    
    ### all parameters of oryza
    params_to_cal <- tidy_to_write_crop(test_params_oryza)
    
    params_to_cal <- generate_combinations_paramsTb(params_to_cal, default_list, length_escenaries =  n_escenarios)
    
    
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
    Sys.sleep()
    
    
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
    
    return(list(params = oryza_params, GA_global = GA_oryza))
    
    
  
    
    } else if (all(cal_stages ==  c("phen"))){
    
    ## Filtrar los parametros a Calibrar --- Debe contener las columnas Base, Min y Max
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, "DVRJ|DVRI|DVRP|DVRR"))
    
    #1. Phenological development parameters 
    tic(paste0("Phenological Calibration"))
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
    
    
    closeAllConnections()
    
    return(list(params = phen_params, GA_phen = GA_phen))
    
    
    
    
    
  
    
    } else if(all(cal_stages ==  c("phen", "yield"))){
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenological Calibration"))
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
    
    load("calibracion_rendimiento_test.RData")
    
    
    phen_yield_params <- bind_rows(phen_params, yield_params) # %>% deframe()
    
    closeAllConnections()
    
    return(list(params = phen_yield_params, GA_phen = GA_phen, GA_yield = GA_yield))
    
    
    
    
    
    
    
    
  
    
    } else if(all(cal_stages ==  c("phen", "global"))){
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenological Calibration"))
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
    
    
    
  
    
    } else if(all(cal_stages ==  c("phen", "dry_matter_lai", "yield"))){
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenological Calibration"))
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
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% 
      dplyr::select(-Set_cal) 
    #   dplyr::select(Parameter, Set_cal = Base) %>% filter(test_params_model, str_detect(Parameter, growth_pattern))
    params_to_cal <- generate_combinations_paramsTb(filter(test_params_model, str_detect(Parameter, growth_pattern)), default_list, length_escenaries =  n_escenarios)
    
    
    #6. Growth parameters 
    
    tic("Growth and Leaf params Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_grow_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          params_to_cal, fenologia_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_model, basedata_path),
                    lower = params_to_cal$Min %>% unlist(), 
                    upper = params_to_cal$Max %>% unlist(), 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = params_to_cal$Parameter %>% unlist())
    
    GA_growth@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    grow_paramsGA <- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #  mutate(Set_cal =  map(Set_cal, ~.x))
    
    grow_params <-    bind_rows(
      
      params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(params_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
            dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
            pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(), 
      
      tibble(Parameter = params_to_cal$Parameter,
             Set_cal = grow_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
    
    test_params_model <- bind_rows(phen_params, grow_params) %>% right_join(test_params_oryza, by = "Parameter") %>%
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    
    
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
    
    closeAllConnections()
    
    return(list(params = bind_rows(phen_params, grow_params, yield_params), GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield))
    
    
  } else if(all(cal_stages ==  c("phen", "dry_matter_lai", "yield", "global"))){
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenological Calibration"))
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
    
    
    #6. Growth parameters 
    
    tic("Growth and Leaf params Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_grow_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          params_to_cal, fenologia_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_model, basedata_path),
                    lower = params_to_cal$Min %>% unlist(), 
                    upper = params_to_cal$Max %>% unlist(), 
                    maxiter = 50,
                    popSize = 200,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = params_to_cal$Parameter %>% unlist())
    
    GA_growth@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    grow_paramsGA <- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #  mutate(Set_cal =  map(Set_cal, ~.x))
    
    grow_params <-    bind_rows(
      
      params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(params_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
            dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
            pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(), 
      
      tibble(Parameter = params_to_cal$Parameter,
             Set_cal = grow_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
    
    test_params_model <- bind_rows(phen_params, grow_params) %>% right_join(test_params_oryza, by = "Parameter") %>%
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    
    
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
    
    test_params_model <- bind_rows(phen_params, grow_params, yield_params) %>% right_join(test_params_oryza, by = "Parameter") %>%
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
    
    oryza_params <-  bind_rows(
      
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
    
    return(list(params = oryza_params, GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield, GA_global = GA_oryza))
    
    
    
    
  }
  
  
  message("Quizas fue mas que un cafe...Terminó la cosa? :V.. miremos esos parametros...")
  
  
}




#tt <- calibration_oryza_GA(calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path, cal_stages = cal_stages, pop_iter = c(20, 3), ncores = 8)


#save(calibration_path, cultivar, grow_params, input_data, exp_files, yield_params, test_params_oryza, basedata_path, file = "test_data_GA_oryza.RData")

#closeAllConnections()


