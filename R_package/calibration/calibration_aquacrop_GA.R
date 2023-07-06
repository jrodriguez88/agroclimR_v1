# Script to Calibrate AQUACROP model with Genetic algorithms (GA)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023


## Load packages
#library(GA)
#library(tictoc)
#library(parallel)
#library(doFuture)

#basedata_path <- "_AQUACROP/plugin/"
#calibration_path <- path_proj

# Funcion copia inputs base en directorio de simulacion de cada setups
copy_inputs_aquacrop <- function(path_proj, basedata_path){
  
  # ruta con los archivos necesarios para 
  files_default <- list.files(basedata_path, recursive = T, full.names = T)
  
  file.copy(files_default, path_proj, recursive = T)
  
  
  dir.create(paste0(path_proj, "/OUTP"))
  dir.create(paste0(path_proj, "/SIMUL"))
  dir.create(paste0(path_proj, "/LIST"))
  
  
  file.copy(list.files(path_proj, pattern = "CO2", full.names = T), paste0(path_proj, "/SIMUL/"))
  file.copy(list.files(path_proj, pattern = "DailyResults", full.names = T), paste0(path_proj, "/SIMUL/"))
  file.copy(list.files(path_proj, pattern = "DailyResults", full.names = T), paste0(path_proj, "/SIMUL/"))
  
  
}


##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
#default_list <- tidy_to_write_crop(NULL)
#phen_pattern <- "GDD_emergence|GDD_FL|GDD_FLL|GDD_M|GDD_CCx|GDD_senecence|GDD_HI"
#growth_pattern <- "CGC|CCx|CDC|Zr|WP|Kc"
#yield_pattern <- "HIo|Ks_exp|Ks_polc|Ks_polh"


## Aquacrop Phenological parameters
#x1 <- 150  #"GDD_emergence"
#x2 <- 800   #GDD_CCx
#x3 <- 1150     #"GDD_FL"
#x4 <- 350  #"GDD_FLL"
#x5 <- 1900   #"GDD_M"
#x6 <- 1700  #GDD_senecence
#x7 <- 500  #GDD_HI
#
#params_to_cal <- test_params_model %>% filter(str_detect(Parameter, phen_pattern))
#
#test_params_model <- tidy_to_write_crop(test_params_model)

## Funcion de optimizacion de parametros fenologicas del modelo aquacrop
cal_phen_aquacrop <- function(x1, x2, x3, x4, x5, x6, x7, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  ## Aquacrop Phenological parameters
  #x1 <- 150  #"GDD_emergence"
  #x2 <- 1150     #"GDD_FL"
  #x3 <- 350  #"GDD_FLL"
  #x4 <- 1900   #"GDD_M"
  
  params_to_cal <- tibble(Parameter = params_to_cal$Parameter,
                          Set_cal = list(x1, x2, x3, x4, x5, x6, x7)) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_aquacrop <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar2 <- paste0(cultivar, id_run)
  copy_inputs_aquacrop(dir_run, basedata_path)
  
  
  filesX <- list.files(paste0(calibration_path, "/LIST"), pattern = "PRM", full.names = T) %>%
    str_subset(pattern = paste(exp_files, collapse = "|"))
  
  list_run <- paste0(dir_run, "/LIST/")
  
  dir.create(list_run)
  
 # file.copy(filesX, paste0(dir_run, "/LIST/"))
  
 # new_proj <- list.files(paste0(dir_run, "/LIST/"), full.names = T)
  
  projects <-  filesX  %>% map(read_lines) %>% 
    map(function(x) {
      x[44] <- paste0(cultivar2, ".CRO")
      x})
  
  
  map2(.x = paste0(list_run, list.files(paste0(calibration_path, "/LIST"), pattern = "PRM")), .y = projects, .f = function(a,b){
    sink(file = a,  append = F)
    writeLines(b)
    sink()}
    )
  

  ### Write crop file 
  
  write_crop_aquacrop(calibration_path, cultivar2, crop_params_aquacrop)
  

  ### Run model aquacrop
  run_model_aquacrop(dir_run, cultivar2, exp_files)
  
  list_op <- paste0(dir_run, "OUTP/")
  
  sim_data_cal <- map(.x = list.files(list_op, pattern = "day"), 
                  ~read_aquacrop_day(file = .x, path = list_op)) %>% 
    set_names(list.files(list_op, pattern = "day") %>% str_remove_all("PRMday.OUT"))
  
  
  
  metrics_cal <- map(c("phen"), 
                     ~eval_sim_aquacrop(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}



## Aquacrop "growth|Growth AGB-RZ"

#x1 <- 0.007  #"CGC"
#x2 <- 0.88   #CCx
#x3 <- 19     #"WP"
#x4 <- 0.0052  #"CDC"
#x5 <- 0.45   #"Zr"
#x6 <- 1.1  #"Kc" 
#
#list.files(path_proj, full.names = T, pattern = ".CRO") %>% file.remove()
##params_to_cal <- test_params_model %>% dplyr::filter(str_detect(Component, "Leaf and stem growth|Growth AGB-RZ")) %>%
##  dplyr::filter(str_detect(Parameter, "SPGF|WGRMX|ZRTMCD", negate = T))
#
#growth_to_cal <- tidy_to_write_crop(test_params_model) %>% 
#  dplyr::filter(str_detect(Parameter, growth_pattern), str_detect(Parameter, "GDD", negate=T))
#
#test_params_model <- tidy_to_write_crop(test_params_model)

## Funcion de optimizacion de parametros de crecimiento del modelo aquacrop
cal_growth_aquacrop <- function(x1, x2, x3, x4, x5, x6, params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){

  
  # Parameters to  growth calibration 

  
#  x1 <- 0.007  #"CGC"
#  x2 <- 0.88   #CCx
#  x3 <- 19     #"WP"
#  x4 <- 0.0052  #"CDC"
#  x5 <- 0.45   #"Zr"
#  x6 <- 1.1  #"Kc" 
  

  
  
  test_params_growth  <-  tibble(Parameter = params_to_cal$Parameter,
                                 Set_cal = list(x1, x2, x3, x4, x5, x6)) 
  
  

  
  params_to_cal <- test_params_growth  %>% 
    bind_rows(phen_params) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  
  crop_params_aquacrop <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar2 <- paste0(cultivar, id_run)
  copy_inputs_aquacrop(dir_run, basedata_path)
  
  
  filesX <- list.files(paste0(calibration_path, "/LIST"), pattern = "PRM", full.names = T) %>%
    str_subset(pattern = paste(exp_files, collapse = "|"))
  
  list_run <- paste0(dir_run, "/LIST/")
  
  dir.create(list_run)
  
  # file.copy(filesX, paste0(dir_run, "/LIST/"))
  
  # new_proj <- list.files(paste0(dir_run, "/LIST/"), full.names = T)
  
  projects <-  filesX  %>% map(read_lines) %>% 
    map(function(x) {
      x[44] <- paste0(cultivar2, ".CRO")
      x})
  
  
  map2(.x = paste0(list_run, list.files(paste0(calibration_path, "/LIST"), pattern = "PRM")), .y = projects, .f = function(a,b){
    sink(file = a,  append = F)
    writeLines(b)
    sink()}
  )
  
  
  ### Write crop file 
  
  write_crop_aquacrop(calibration_path, cultivar2, crop_params_aquacrop)
  
  
  ### Run model aquacrop
  run_model_aquacrop(dir_run, cultivar2, exp_files)
  
  
  ### Extract ouputs
  list_op <- paste0(dir_run, "OUTP/")
  
  sim_data_cal <- map(.x = list.files(list_op, pattern = "day"), 
                      ~read_aquacrop_day(file = .x, path = list_op)) %>% 
    set_names(list.files(list_op, pattern = "day") %>% str_remove_all("PRMday.OUT"))
  

  
  ### Get statistical metrics
  metrics_cal <- map(c("dry_matter", "lai"), 
                     ~eval_sim_aquacrop(input_data, sim_data_cal, exp_files, .x, T)) %>% 
    bind_rows() %>% filter(var %in% c("WAGT", "LAI"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}

#cal_growth_aquacrop(x1, x2, x3, x4, x5, x6,  growth_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path)




# Aquacrop Yield and stress parameters

#x1 = 37       #"HIo"
#x2 = 1500000  #"NPLDS"
#x3 = 0.4      #"Ks_exp"  
#x4 = 8        #"Ks_polc"  
#x5 = 35       #"Ks_polh"   

#params_to_cal <- anti_join(test_params_model, bind_rows(phen_params, growth_params))

#Funcion de optimizacion para parametros de rendimiento y estreses abioticos
cal_yield_aquacrop <- function(x1, x2, x3, x4, x5,  params_to_cal, phen_params, growth_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  
  ##Crea set de parametros calibrados
  
  test_params <-  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5)) %>%
    bind_rows(phen_params, growth_params)
  
  
  
  params_to <- test_params %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_aquacrop <- params_to$to_test %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar <- paste0(cultivar, id_run)
  copy_inputs_aquacrop(dir_run, basedata_path)
  
  
  filesX <- list.files(paste0(calibration_path, "/LIST"), pattern = "PRM", full.names = T) %>%
    str_subset(pattern = paste(exp_files, collapse = "|"))
  
  list_run <- paste0(dir_run, "/LIST/")
  
  dir.create(list_run)
  
  # file.copy(filesX, paste0(dir_run, "/LIST/"))
  
  # new_proj <- list.files(paste0(dir_run, "/LIST/"), full.names = T)
  
  projects <-  filesX  %>% map(read_lines) %>% 
    map(function(x) {
      x[44] <- paste0(cultivar2, ".CRO")
      x})
  
  
  map2(.x = paste0(list_run, list.files(paste0(calibration_path, "/LIST"), pattern = "PRM")), .y = projects, .f = function(a,b){
    sink(file = a,  append = F)
    writeLines(b)
    sink()}
  )
  
  
  ### Write crop file 
  
  write_crop_aquacrop(calibration_path, cultivar2, crop_params_aquacrop)
  
  
  ### Run model aquacrop
  run_model_aquacrop(dir_run, cultivar2, exp_files)
  
  
  ### Extract ouputs
  list_op <- paste0(dir_run, "OUTP/")
  
  sim_data_cal <- map(.x = list.files(list_op, pattern = "day"), 
                      ~read_aquacrop_day(file = .x, path = list_op)) %>% 
    set_names(list.files(list_op, pattern = "day") %>% str_remove_all("PRMday.OUT"))
  
  
  
  ### Get statistical metrics
  metrics_cal <- map(c("yield", "dry_matter"), 
                     ~eval_sim_aquacrop(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows() %>% 
    dplyr::filter(var %in% c("WAGT", "YIELD"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}

#cal_yield_aquacrop(x1, x2, x3, x4, x5,  params_to_cal, phen_params, growth_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path)



# Aquacrop Global parameters


#x1 <- 150  #"GDD_emergence"
#x2 <- 800   #GDD_CCx
#x3 <- 1150     #"GDD_FL"
#x4 <- 350  #"GDD_FLL"
#x5 <- 1900   #"GDD_M"
#x6 <- 0.007  #"CGC"
#x7 <- 1700  #GDD_senecence
#x8 <- 0.88   #CCx
#x9 <- 19     #"WP"
#x10 <- 37       #"HIo"
#x11 <- 500  #GDD_HI
#x12 <- 0.0052  #"CDC"
#x13 <- 0.45   #"Zr"
#x14 <- 1500000  #"NPLDS"
#x15 <- 1.1  #"Kc"
#x16 <- 0.4      #"Ks_exp"  
#x17 <- 8        #"Ks_polc"  
#x18 <- 35       #"Ks_polh"   



#params_to_cal <- test_params_model


#Funcion de optimizacion para parametros 18 parametros de aquacrop v3 - 
cal_aquacrop_global <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18,  params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield"), phenol = NULL){
  
  
  ##Crea set de parametros  a calibrar 
  
  params_to <-  tibble(Parameter = params_to_cal$Parameter,
                          Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))
  
  crop_params_aquacrop <- params_to$Set_cal %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar <- paste0(cultivar, id_run)
  copy_inputs_aquacrop(dir_run, basedata_path)
  
  
  filesX <- list.files(paste0(calibration_path, "/LIST"), pattern = "PRM", full.names = T) %>%
    str_subset(pattern = paste(exp_files, collapse = "|"))
  
  list_run <- paste0(dir_run, "/LIST/")
  
  dir.create(list_run)
  
  # file.copy(filesX, paste0(dir_run, "/LIST/"))
  
  # new_proj <- list.files(paste0(dir_run, "/LIST/"), full.names = T)
  
  projects <-  filesX  %>% map(read_lines) %>% 
    map(function(x) {
      x[44] <- paste0(cultivar2, ".CRO")
      x})
  
  
  map2(.x = paste0(list_run, list.files(paste0(calibration_path, "/LIST"), pattern = "PRM")), .y = projects, .f = function(a,b){
    sink(file = a,  append = F)
    writeLines(b)
    sink()}
  )
  
  
  ### Write crop file 
  
  write_crop_aquacrop(calibration_path, cultivar2, crop_params_aquacrop)
  
  
  ### Run model aquacrop
  run_model_aquacrop(dir_run, cultivar2, exp_files)
  
  
  ### Extract ouputs
  list_op <- paste0(dir_run, "OUTP/")
  
  sim_data_cal <- map(.x = list.files(list_op, pattern = "day"), 
                      ~read_aquacrop_day(file = .x, path = list_op)) %>% 
    set_names(list.files(list_op, pattern = "day") %>% str_remove_all("PRMday.OUT"))
  
  
  
  metrics_cal <- map(res_var, 
                     ~eval_sim_aquacrop(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows() %>% 
    dplyr::filter(var %in% c("FDAT", "MDAT", "LAI", "WAGT", "YIELD"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}


#cal_aquacrop_global(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18,  params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield"), phenol = NULL)





#cal_stages <- c("phen", "dry_matter_lai", "yield", "global")
#cal_stages <- c("phen", "dry_matter_lai", "yield")
##cal_stages <- c("phen", "lai", "dry_matter", "yield", "global")
##cal_stages <- c("phen", "dry_matter", "lai", "yield", "global")
#cal_stages <- c("phen", "yield")    # same that : cal_stages <- c("phen", "global") *res_var = c("yield")
#cal_stages <- c("global")
#cal_stages <- c("phen")


### Funcion que realiza optimizacion de parametros de aquacrop basado en fases 
calibration_aquacrop_GA <- function(calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, 
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
  
  phen_pattern <- "GDD_emergence|GDD_FL|GDD_FLL|GDD_M|GDD_CCx|GDD_senecence|GDD_HI"
  growth_pattern <- "CGC|CCx|CDC|Zr|WP|Kc"
  yield_pattern <- "HIo|Ks_exp|Ks_polc|Ks_polh"
  
  
  ### Separa tamaño de la poblacion, maximo de iteraciones y numero de escenarios a simular en tablas de particion
  pop_size <- pop_iter[1]
  max_iter <- pop_iter[2]
  
  ### Posibles configuraciones de calibracion 
  if(all(cal_stages %in% c("global"))) {
    
    ### all parameters of aquacrop
    global_to_cal <- tidy_to_write_crop(test_params_model)
    
    
    # plan(multiprocess)
    #    registerDoFuture()
    #    cl <- makeCluster(ncores)
    #    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_aquacrop <- ga(type = "real-valued", 
                    fitness = function(x) -cal_aquacrop_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18],
                                                            global_to_cal, calibration_path, cultivar, 
                                                            input_data, exp_files, default_list, basedata_path, res_var = res_var),
                    lower = low_min1, 
                    upper = upp_max1, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_par1)
    
    GA_aquacrop@solution
    toc()
    
 #   closeAllConnections()
    gc()
    
    
    global_params <<- as.data.frame(GA_aquacrop@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
  #  global_params <<-  safe_bind(gparams)
    
    
    #closeAllConnections()
    message("GA - Global calibration done!")
    
    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    return(list(params = global_params, GA_global = GA_aquacrop))
    
    message("Parameter Optimization Done!")
    
    
  }
  else if(all(cal_stages %in% c("phen"))) {
    
    ## Filtrar los parametros a Calibrar --- Debe contener las columnas Base, Min y Max
    phen_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(4)
    plan(future::cluster, workers = cl)
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_aquacrop(x[1], x[2], x[3], x[4], x[5], x[6], x[7], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
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
    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
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
                  fitness = function(x) -cal_phen_aquacrop(x[1], x[2], x[3], x[4], x[5], x[6], x[7], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = 4,
                  popSize = 50,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
 #   closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    
    
    
    
    
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
                   fitness = function(x) -cal_yield_aquacrop(x[1], x[2], x[3], x[4], x[5],
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
    
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    #    load("calibracion_rendimiento_test.RData")
    
    
    phen_yield_params <- bind_rows(phen_params, yield_params) # %>% deframe()
    
    closeAllConnections()
    
    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    
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
                  fitness = function(x) -cal_phen_aquacrop(x[1], x[2], x[3], x[4], x[5], x[6], x[7], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = 4,
                  popSize = 50,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
    #   closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    
    
    
    
    ### all parameters of aquacrop
    global_to_cal <- tidy_to_write_crop(phen_params)
    
    
    # plan(multiprocess)
    #    registerDoFuture()
    #    cl <- makeCluster(ncores)
    #    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_aquacrop <- ga(type = "real-valued", 
                      fitness = function(x) -cal_aquacrop_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18],
                                                                 global_to_cal, calibration_path, cultivar, 
                                                                 input_data, exp_files, default_list, basedata_path, res_var = res_var),
                      lower = low_min1, 
                      upper = upp_max1, 
                      maxiter = max_iter,
                      popSize = pop_size,
                      pmutation = 0.2,
                      parallel = cl, 
                      names = names_par1)
    
    GA_aquacrop@solution
    toc()
    
    #   closeAllConnections()
    gc()
    
    
    global_params <<- as.data.frame(GA_aquacrop@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    
    return(list(params = global_params, GA_phen = GA_phen, GA_global = GA_aquacrop))
    
    
    
    
    
    
  } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield"))) {
    
    
    message("Aquacrop v6.1 - Genetic Algorithm
            
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
                  fitness = function(x) -cal_phen_aquacrop(x[1], x[2], x[3], x[4], x[5], x[6], x[7], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = 4,
                  popSize = 50,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
 #   closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    
    
    growth_to_cal <- test_params_model %>% 
      dplyr::filter(str_detect(Parameter, growth_pattern), str_detect(Parameter, "GDD", negate=T))
    
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_aquacrop(x[1], x[2], x[3], x[4], x[5], x[6], 
                                                               growth_to_cal, phen_params, calibration_path, cultivar, 
                                                               input_data, exp_files, test_params_model, basedata_path),
                    lower =  low_ming, 
                    upper = upp_maxg, 
                    maxiter = 4,
                    popSize = 50,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_parg)
    
    GA_growth@solution
    toc()
    
    
    gc()
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    growth_params <<- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    
    
    
    yield_to_cal <- anti_join(test_params_model, bind_rows(phen_params, growth_params), by = join_by(Parameter))
    
    
    low_miny <- yield_to_cal$Min %>% unlist()
    upp_maxy <- yield_to_cal$Max %>% unlist()
    names_pary <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_aquacrop(x[1], x[2], x[3], x[4], x[5],
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
    
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    #    load("calibracion_rendimiento_test.RData")
    
    
    closeAllConnections()
    
    params_f <- bind_rows(phen_params, growth_params, yield_params)
    message("GA - Yield done!")
    
    
    
    message("Parameter Optimization Done!")
    
    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    
    return(list(params = params_f, 
                GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield))
    
    
    
    
  
  } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield", "global"))) {
    

    message("Aquacrop v6.1 - Genetic Algorithm
            
            - Parameter Optimization - 4 stage:
            ")
    
    
    tictoc::tic()
    phen_to_cal <- test_params_model %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_aquacrop(x[1], x[2], x[3], x[4], x[5], x[6], x[7], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
 #   closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    
    
    growth_to_cal <- test_params_model %>% 
      dplyr::filter(str_detect(Parameter, growth_pattern), str_detect(Parameter, "GDD", negate=T))
    
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_aquacrop(x[1], x[2], x[3], x[4], x[5], x[6], 
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
    
    
    gc()
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    growth_params <<- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    
    
    
    yield_to_cal <- anti_join(test_params_model, bind_rows(phen_params, growth_params), by = join_by(Parameter))
    
    
    low_miny <- yield_to_cal$Min %>% unlist()
    upp_maxy <- yield_to_cal$Max %>% unlist()
    names_pary <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_aquacrop(x[1], x[2], x[3], x[4], x[5],
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
    
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    #    load("calibracion_rendimiento_test.RData")
    
    
    params_f <- safe_bind(phen_params, growth_params, yield_params)
    
    message("GA - Yield done!")
    
    
    
    global_to_cal <-  tidy_to_write_crop(params_f)
    # growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
    #   dplyr::select(Parameter, Set_cal = Base)

    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    message(paste0("4th Stage: GA Global - Parameters: ", phen_pattern, growth_pattern, yield_pattern))
    tic("Global calibration")
    GA_aquacrop <- ga(type = "real-valued", 
                      fitness = function(x) -cal_aquacrop_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18],
                                                                 global_to_cal, calibration_path, cultivar, 
                                                                 input_data, exp_files, default_list, basedata_path, res_var = res_var),
                    lower = low_min1, 
                    upper = upp_max1, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_par1)
    
    GA_aquacrop@solution
    toc()
    
    closeAllConnections()
    gc()
    
    global_params <<- as.data.frame(GA_aquacrop@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))

    
    #closeAllConnections()
    message("GA - Global calibration done!")
    
    file.remove(list.files(calibration_path, pattern = ".CRO", full.names = T))
    return(list(parameters_final = global_params, parameters_3stage = params_f, GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield, GA_global = GA_aquacrop))
    
    message("Parameter Optimization Done!")
    
   
    
    
  }
  else{message("Unknown Calibration Stages")}
  
  
  
  
  message("Parameter Optimization Done!")
  tictoc::toc()
  
  
}



#calibration_aquacrop_GA(calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, 
#                        cal_stages = c("phen", "dry_matter_lai", "yield", "global"), 
#                        pop_iter = c(10, 3),  res_var = c("yield"), ncores = 4)
#















