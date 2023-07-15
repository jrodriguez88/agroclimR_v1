# Script to Calibrate DSSAT- RICE model with Genetic algorithms (GA)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023


## Load packages
#library(GA)
#library(tictoc)
#library(parallel)
#library(doFuture)

##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
#default_list <- tidy_to_write_crop(NULL)
#phen_pattern <- "P1|P2O|P2R|P5"
#growth_pattern <- "PHINT|G1|G2|G3"
#yield_pattern <- "THOT|TCLDP"

#test_params_model <- tidy_to_write_crop(NULL)
#calibration_path <- path_proj



#params_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))


## Funcion de optimizacion de parametros fenologicas del modelo DSSAT-CERES-RICE
cal_phen_dssat <- function(x1, x2, x3, x4, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  ## DSSAT Phenological parameters
  #x1 <- 500  #"P1"
  #x2 <- 12     #"P2O"
  #x3 <- 100  #"P2R"
  #x4 <- 450   #"P5"
  
  params_to_cal <- tibble(Parameter = params_to_cal$Parameter,
                          Set_cal = list(x1, x2, x3, x4)) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_dssat <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  
  

  
  
  copy_inputs_dssat(dir_run, basedata_path, crop = "rice")
  
  
  ### Write crop file 
  write_crop_dssat(dir_run, cultivar, crop_params_dssat, ecotype = "IB0001")

  ### Run model dssat
  
  run_model_dssat(dir_run, "rice", exp_files)
  
  
  
  sim_data_cal <- read_plantgro(paste0(dir_run, "/PlantGro.OUT"))
  
  
  
  metrics_cal <- map(c("phen"), 
                     ~eval_sim_dssat(input_data, sim_data_cal, .x, T)) %>% bind_rows()
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
#  file.remove(paste0(calibration_path, cultivar2, ".CRO"))
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
#cal_phen_dssat(x1, x2, x3, x4, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path)




#params_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, growth_pattern))
## Funcion de optimizacion de parametros de crecimiento del modelo DSSAT-CERES-RICE
cal_growth_dssat <- function(x1, x2, x3, x4, params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
    
  # Parameters to  growth calibration 
  
  
  #  x1 <- 83  #"PHINT"
  #  x2 <- 55   #"G1"
  #  x3 <- 0.025     #"G2"
  #  x4 <- 1  #"G3"

  
  test_params_growth  <-  tibble(Parameter = params_to_cal$Parameter,
                                 Set_cal = list(x1, x2, x3, x4)) 
  
  
  
  
  params_to_cal <- test_params_growth  %>% 
    bind_rows(phen_params) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  
  crop_params_dssat <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)


##Setting folder

id_run <- as.integer(runif(1) * 10000000)
dir_run <- make_dir_run(calibration_path, id_run)





copy_inputs_dssat(dir_run, basedata_path, crop = "rice")


### Write crop file 
write_crop_dssat(dir_run, cultivar, crop_params_dssat, ecotype = "IB0001")

### Run model dssat

run_model_dssat(dir_run, "rice", exp_files)



sim_data_cal <- read_plantgro(paste0(dir_run, "/PlantGro.OUT"))



metrics_cal <- map(c("dry_matter", "lai"), 
                   ~eval_sim_dssat(input_data, sim_data_cal, .x, T)) %>%  bind_rows() %>% 
  filter(var %in% c("WAGT", "LAI"))


#files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)


#map(files_remove, ~unlink(.x, recursive = T))

unlink(dir_run, recursive = T)
#  file.remove(paste0(calibration_path, cultivar2, ".CRO"))


return(mean(metrics_cal$NRMSE))

}
#cal_growth_dssat(x1, x2, x3, x4, params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path)
    



#params_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, yield_pattern))
## Funcion de optimizacion de parametros stress y rendimiento del modelo DSSAT-CERES-RICE -- Yield/Biomass response
cal_yield_dssat <- function(x1, x2, params_to_cal, phen_params, growth_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  # Parameters to  growth calibration 
  
  
  #  x1 <- 28  #"THOT"
  #  x2 <- 15   #"TCLDP"

  
  
  test_params_yield  <-  tibble(Parameter = params_to_cal$Parameter,
                                 Set_cal = list(x1, x2)) 
  
  
  
  
  params_to_cal <- test_params_yield  %>% 
    bind_rows(phen_params, growth_params) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  
  crop_params_dssat <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  
  
  
  
  
  copy_inputs_dssat(dir_run, basedata_path, crop = "rice")
  
  
  ### Write crop file 
  write_crop_dssat(dir_run, cultivar, crop_params_dssat, ecotype = "IB0001")
  
  ### Run model dssat
  
  run_model_dssat(dir_run, "rice", exp_files)
  
  
  
  sim_data_cal <- read_plantgro(paste0(dir_run, "/PlantGro.OUT"))
  
  
  
  metrics_cal <- map(c("dry_matter", "yield"), 
                     ~eval_sim_dssat(input_data, sim_data_cal, .x, T)) %>%  bind_rows() %>% 
    filter(var %in% c("WAGT", "YIELD"))
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  #  file.remove(paste0(calibration_path, cultivar2, ".CRO"))
  
  
  return(mean(metrics_cal$NRMSE))
  
}
#cal_yield_dssat(x1, x2, params_to_cal, phen_params, growth_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path)
  


#params_to_cal <- test_params_model
#phenol  <- phen_params
#Funcion de optimizacion para parametros 10 parametros de DSSAT
cal_dssat_global <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield"), phenol = NULL){
  
  ## DSSAT global parameters
  #x1 <- 500  #"P1"
  #x2 <- 12     #"P2O"
  #x3 <- 100  #"P2R"
  #x4 <- 450   #"P5"
  #x5 <- 83  #"PHINT"
  #x6 <- 55   #"G1"
  #x7 <- 0.025     #"G2"
  #x8 <- 1     #"G3"
  #x9 <- 28  #"THOT"
  #x10 <- 15   #"TCLDP"
  
  params_to_cal <- tibble(Parameter = params_to_cal$Parameter,
                          to_test = list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)) 
  
  crop_params_dssat <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  
  
  
  # COpy neccesary files
  
  copy_inputs_dssat(dir_run, basedata_path, crop = "rice")
  
  
  ### Write crop file 
  write_crop_dssat(dir_run, cultivar, crop_params_dssat, ecotype = "IB0001")
  
  ### Run model dssat
  
  run_model_dssat(dir_run, "rice", exp_files)
  
  
  
  sim_data_cal <- read_plantgro(paste0(dir_run, "/PlantGro.OUT"))
  
  
  
  metrics_cal <- map(res_var, 
                     ~eval_sim_dssat(input_data, sim_data_cal, .x, T)) %>% bind_rows() %>% 
    dplyr::filter(var %in% c("FDAT", "MDAT", "LAI", "WAGT", "YIELD"))
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  #  file.remove(paste0(calibration_path, cultivar2, ".CRO"))
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
#cal_global_dssat(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield"), phenol = NULL)




#cal_stages <- c("phen", "dry_matter_lai", "yield", "global")
#cal_stages <- c("phen", "dry_matter_lai", "yield")
##cal_stages <- c("phen", "lai", "dry_matter", "yield", "global")
##cal_stages <- c("phen", "dry_matter", "lai", "yield", "global")
#cal_stages <- c("phen", "yield")    # same that : cal_stages <- c("phen", "global") *res_var = c("yield")
#cal_stages <- c("global")
#cal_stages <- c("phen")


### Funcion que realiza optimizacion de parametros de dssat basado en fases 
calibration_dssat_GA <- function(calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, 
                                    cal_stages = c("phen", "dry_matter_lai", "yield", "global"), 
                                    pop_iter = c(20, 5),  res_var = c("yield"), ncores = 4){
  
  
  message("This process can take some time. Go for coffee :V")
  
  
  # plan(multiprocess)
  registerDoFuture()
  cl <- makeCluster(ncores)
  plan(future::cluster, workers = cl)
  
  
  safe_bind <- purrr::possibly(bind_rows, otherwise = NULL) 
  
  
  ##default list of parameters -- max an min == +/- 30%
  default_list <- tidy_to_write_crop(NULL)
  
  phen_pattern <- "P1|P2O|P2R|P5"
  growth_pattern <- "PHINT|G1|G2|G3"
  yield_pattern <- "THOT|TCLDP"
  
  
  ### Separa tamaño de la poblacion, maximo de iteraciones y numero de escenarios a simular en tablas de particion
  pop_size <- pop_iter[1]
  max_iter <- pop_iter[2]
  
  ### Posibles configuraciones de calibracion 
  if(all(cal_stages %in% c("global"))) {
    
    ### all parameters of dssat
    global_to_cal <- tidy_to_write_crop(test_params_model)
    
    
    # plan(multiprocess)
    #    registerDoFuture()
    #    cl <- makeCluster(ncores)
    #    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_dssat <- ga(type = "real-valued", 
                      fitness = function(x) -cal_dssat_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10],
                                                                 global_to_cal, calibration_path, cultivar, 
                                                                 input_data, exp_files, default_list, basedata_path, res_var = res_var),
                      lower = low_min1, 
                      upper = upp_max1, 
                      maxiter = max_iter,
                      popSize = pop_size,
                      pmutation = 0.2,
                      parallel = cl, 
                      names = names_par1)
    
    GA_dssat@solution
    toc()
    
    #   closeAllConnections()
    gc()
    
    
    global_params <<- as.data.frame(GA_dssat@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
    #  global_params <<-  safe_bind(gparams)
    
    
    #closeAllConnections()
    message("GA - Global calibration done!")
    
    #    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    return(list(params = global_params, GA_global = GA_dssat))
    
    message("Parameter Optimization Done!")
    
    
  }
  else if(all(cal_stages %in% c("phen"))) {
    
    ## Filtrar los parametros a Calibrar --- Debe contener las columnas Base, Min y Max
    phen_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    # plan(multiprocess)
    #registerDoFuture()
    #cl <- makeCluster(ncores)
    #plan(future::cluster, workers = cl)
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_dssat(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = 4,
                  popSize = 50,
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
    
    
    #   closeAllConnections()
    # file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    return(list(params = phen_params, GA_phen = GA_phen))
    
    
    
  } 
  else if(all(cal_stages %in% c("phen", "yield"))) {
    
    ## Filtrar los parametros a Calibrar --- Debe contener las columnas Base, Min y Max
    phen_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    # plan(multiprocess)
    #  registerDoFuture()
    #  cl <- makeCluster(4)
    #  plan(future::cluster, workers = cl)
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_dssat(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = 4,
                  popSize = 50,
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
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    
    
    test_params_model <- phen_params %>% right_join(test_params_model, by = "Parameter") %>%
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    
    growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
      dplyr::select(Parameter, Set_cal = Base)
    
    params_to_cal <- anti_join(test_params_model, bind_rows(phen_params, growth_params), by = join_by(Parameter))
    
    
    low_min <- params_to_cal$Min %>% unlist()
    upp_max <- params_to_cal$Max %>% unlist()
    names_par <- params_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_dssat(x[1], x[2],
                                                             params_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                             input_data, exp_files, test_params_model, basedata_path),
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
    
    #    load("calibracion_rendimiento_test.RData")
    
    
    phen_yield_params <- bind_rows(phen_params, yield_params) # %>% deframe()
    
    
    
    #  file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    
    return(list(params = phen_yield_params, GA_phen = GA_phen, GA_yield = GA_yield))
    
    
    
    
    
  } 
  else if(all(cal_stages %in% c("phen", "global"))) {
    
    
    
    ## Filtrar los parametros a Calibrar --- Debe contener las columnas Base, Min y Max
    phen_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    # plan(multiprocess)
    #  registerDoFuture()
    #  cl <- makeCluster(4)
    #  plan(future::cluster, workers = cl)
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_dssat(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
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
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    
    ### all parameters of dssat
    global_to_cal <- tidy_to_write_crop(phen_params)
    
    
    # plan(multiprocess)
    #    registerDoFuture()
    #    cl <- makeCluster(ncores)
    #    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_dssat <- ga(type = "real-valued", 
                      fitness = function(x) -cal_dssat_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10],
                                                                 global_to_cal, calibration_path, cultivar, 
                                                                 input_data, exp_files, default_list, basedata_path, res_var = res_var),
                      lower = low_min1, 
                      upper = upp_max1, 
                      maxiter = max_iter,
                      popSize = pop_size,
                      pmutation = 0.2,
                      parallel = cl, 
                      names = names_par1)
    
    GA_dssat@solution
    toc()
    
    closeAllConnections()
    gc()
    
    
    global_params <<- as.data.frame(GA_dssat@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    #   file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    
    return(list(params = global_params, GA_phen = GA_phen, GA_global = GA_dssat))
    
    
    
    
    
    
  } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield"))) {
    
    
    message("DSSAT_CERES-RICE - V4.8 - Genetic Algorithm
            
            - Parameter Optimization - 3 stage:
            ")
    
    
    
    phen_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_dssat(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
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
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    growth_to_cal <- test_params_model %>% 
      dplyr::filter(str_detect(Parameter, growth_pattern), str_detect(Parameter, "GDD", negate=T))
    
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_dssat(x[1], x[2], x[3], x[4],
                                                               growth_to_cal, phen_params, calibration_path, cultivar, 
                                                               input_data, exp_files, test_params_model, basedata_path),
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
    
    growth_params <<- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    
    
    
    yield_to_cal <- anti_join(test_params_model, bind_rows(phen_params, growth_params), by = join_by(Parameter))
    
    
    low_miny <- yield_to_cal$Min %>% unlist()
    upp_maxy <- yield_to_cal$Max %>% unlist()
    names_pary <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_dssat(x[1], x[2], 
                                                             yield_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                             input_data, exp_files, test_params_model, basedata_path),
                   lower = low_miny, 
                   upper = upp_maxy, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_pary)
    
    GA_yield@solution
    toc()
    
    closeAllConnections()
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    #    load("calibracion_rendimiento_test.RData")
    
    
    #closeAllConnections()
    
    params_f <- bind_rows(phen_params, growth_params, yield_params)
    message("GA - Yield done!")
    
    
    
    message("Parameter Optimization Done!")
    
    #  file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    
    return(list(params = params_f, 
                GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield))
    
    
    
    
    
  } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield", "global"))) {
    
    
    message("DSSAT_CERES-RICE - V4.8 - Genetic Algorithm
            
            - Parameter Optimization - 4 stage:
            ")
    
    
    #tictoc::tic()
    phen_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_dssat(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
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
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    
    growth_to_cal <- test_params_model %>% 
      dplyr::filter(str_detect(Parameter, growth_pattern), str_detect(Parameter, "GDD", negate=T))
    
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_dssat(x[1], x[2], x[3], x[4],
                                                               growth_to_cal, phen_params, calibration_path, cultivar, 
                                                               input_data, exp_files, test_params_model, basedata_path),
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
    
    growth_params <<- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    
    
    
    yield_to_cal <- anti_join(test_params_model, bind_rows(phen_params, growth_params), by = join_by(Parameter))
    
    
    low_miny <- yield_to_cal$Min %>% unlist()
    upp_maxy <- yield_to_cal$Max %>% unlist()
    names_pary <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_dssat(x[1], x[2],
                                                             yield_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                             input_data, exp_files, test_params_model, basedata_path),
                   lower = low_miny, 
                   upper = upp_maxy, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_pary)
    
    GA_yield@solution
    toc()
    
    closeAllConnections()
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    #    load("calibracion_rendimiento_test.RData")
    
    
    params_f <- safe_bind(phen_params, growth_params, yield_params)
    
    message("GA - Yield done!")
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    
    
    global_to_cal <-  params_f %>% 
      right_join(test_params_model, by = "Parameter") 
    # growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
    #   dplyr::select(Parameter, Set_cal = Base)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    message(paste0("4th Stage: GA Global - Parameters: ", phen_pattern, growth_pattern, yield_pattern))
    tic("Global calibration")
    GA_dssat <- ga(type = "real-valued", 
                      fitness = function(x) -cal_dssat_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10],
                                                                 global_to_cal, calibration_path, cultivar, 
                                                                 input_data, exp_files, default_list, basedata_path, res_var = res_var),
                      lower = low_min1, 
                      upper = upp_max1, 
                      maxiter = max_iter,
                      popSize = pop_size,
                      pmutation = 0.2,
                      parallel = cl, 
                      names = names_par1)
    
    GA_dssat@solution
    toc()
    
    closeAllConnections()
    gc()
    
    global_params <<- as.data.frame(GA_dssat@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    #closeAllConnections()
    message("GA - Global calibration done!")
    
#    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    return(list(parameters_final = global_params, parameters_3stage = params_f, GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield, GA_global = GA_dssat))
    
    message("Parameter Optimization Done!")
    
    
    
    
  }
  else{message("Unknown Calibration Stages")}
  
  
  
  
  message("Parameter Optimization Done!")
  tictoc::toc()
  
  
}



#calibration_dssat_GA(calibration_path, cultivar, input_data, exp_set, test_params_model, basedata_path, 
#                                 cal_stages = c("phen", "dry_matter_lai", "yield", "global"), 
#                                 pop_iter = c(20, 5),  res_var = c("yield"), ncores = 4)






