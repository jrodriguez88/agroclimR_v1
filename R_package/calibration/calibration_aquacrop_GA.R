# Script to Calibrate AQUACROP model with Genetic algorithms (GA)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023


## Load packages
#library(GA)
#library(tictoc)
#library(parallel)
#library(doFuture)

basedata_path <- "_AQUACROP/plugin/"
calibration_path <- path_proj

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
default_list <- tidy_to_write_crop(NULL)
phen_pattern <- "GDD_emergence|GDD_FL|GDD_FLL|GDD_M|GDD_CCx|GDD_senecence|GDD_HI"
growth_pattern <- "CGC|CCx|CDC|Zr|WP|Kc"
yield_pattern <- "HIo|Ks_exp|Ks_polc|Ks_polh"


## Aquacrop Phenological parameters
#x1 <- 0.0008554438  #"GDD_emergence"
#x2 <- 0.0007576     #"GDD_FL"
#x3 <- 0.0005704062  #"GDD_FLL"
#x4 <- 0.002219568   #"GDD_M"


x1 <- 150  #"GDD_emergence"
x2 <- 800   #GDD_CCx
x3 <- 1150     #"GDD_FL"
x4 <- 350  #"GDD_FLL"
x5 <- 1900   #"GDD_M"
x6 <- 1700  #GDD_senecence
x7 <- 500  #GDD_HI

params_to_cal <- test_params_model %>% filter(str_detect(Parameter, phen_pattern))


test_params_model <- tidy_to_write_crop(test_params_model)

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






message("Aquacrop v6.1 - Genetic Algorithm
            
            - Parameter Optimization - 3 stage:
            ")

##default list of parameters -- max an min == +/- 30%
default_list <- tidy_to_write_crop(NULL)

phen_pattern <- "GDD_emergence|GDD_FL|GDD_FLL|GDD_M|GDD_CCx|GDD_senecence|GDD_HI"
growth_pattern <- "CGC|CCx|CDC|Zr|WP|Kc"
yield_pattern <- "HIo|Ks_exp|Ks_polc|Ks_polh"

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






######################################
#####################################
#####################################

## Aquacrop "Leaf and stem growth|Growth AGB-RZ"

#x1 = 20          #"SLATB"
#x2 = 10          #"FSHTB"
#x3 = 15          #"DRLVT"
#x4 = 12          #"BFTB" 
#x5 = 0.0085      #"RGRLMX
#x6 = 0.004       #"RGRLMN
#x7 = 0.004337274 #"SLAMAX
#x8 = 0.2453043   #"FSTR"

CGC 
CCx
CDC
Zr
WP
Kc

HIo
Ks_exp
Ks_polc
Ks_polh


x1 <- 0.007  #"GDD_emergence"
x2 <- 0.88   #GDD_CCx
x3 <- 19     #"GDD_FL"
x4 <- 0.0052  #"GDD_FLL"
x5 <- 0.45   #"GDD_M"
x6 <- 1.1  #GDD_senecence

list.files(path_proj, full.names = T, pattern = ".CRO") %>% file.remove()
#params_to_cal <- test_params_model %>% dplyr::filter(str_detect(Component, "Leaf and stem growth|Growth AGB-RZ")) %>%
#  dplyr::filter(str_detect(Parameter, "SPGF|WGRMX|ZRTMCD", negate = T))

growth_to_cal <- tidy_to_write_crop(test_params_model) %>% dplyr::filter(str_detect(Parameter, growth_pattern), str_detect(Parameter, "GDD", negate=T)) -> params_to_cal


test_params_model <- tidy_to_write_crop(test_params_model)

## Funcion de optimizacion de parametros de crecimiento del modelo oryza
cal_growth_aquacrop <- function(x1, x2, x3, x4, x5, x6, params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  safe_bind <- purrr::possibly(bind_rows, otherwise = NULL) 
  

  #x1 = 20          "SLATB"
  #x2 = 10          "FSHTB"
  #x3 = 15          "DRLVT"
  #x4 = 12          "BFTB" 
  #x5 = 0.0085      "RGRLMX
  #x6 = 0.004       "RGRLMN
  #x7 = 0.004337274 "SLAMAX
  #x8 = 0.2453043   "FSTR"
  

  
  
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
  
  list_op <- paste0(dir_run, "OUTP/")
  
  sim_data_cal <- map(.x = list.files(list_op, pattern = "day"), 
                      ~read_aquacrop_day(file = .x, path = list_op)) %>% 
    set_names(list.files(list_op, pattern = "day") %>% str_remove_all("PRMday.OUT"))
  

  
  
  metrics_cal <- map(c("dry_matter", "lai"), 
                     ~eval_sim_aquacrop(input_data, sim_data_cal, exp_files, .x, T)) %>% 
    bind_rows() %>% filter(var %in% c("WAGT", "LAI"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}

#cal_growth_oryza(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path)








