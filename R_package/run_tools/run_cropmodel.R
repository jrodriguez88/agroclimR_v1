# Function to run crop models (ORYZA, DSSAT, AQUACROP)
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023


### Libraries
#library(tidyverse)



### Path folders
#path <- getwd()
#path_data <- path_data <- paste0(path, "/DATA/"
##set.seed(1234)
##cal_set <- sample(exp_files, length(exp_files)*0.7)    
##eval_set <- setdiff(exp_files, cal_set)




#Execute ORYZA  "ORYZA3.exe"
run_model_oryza <- function(path, cultivar, exp_set, tag = NULL) { 
  
  
  wd <- getwd()
  setwd(path)
  
  tag <- ifelse(is.null(tag), "", paste0("_", tag))  
  ## Make control run 
  
  sink(file = "control.dat", append = F)
  
  cat(paste0("CONTROLFILE = ", "'", "control.dat'"), sep = '\n')
  cat('strun = 1', sep = '\n')
  cat('*endrun = 72', sep = '\n')
  cat(
    "*------------------------------------------------------------------------------*
* control file for ORYZA_v3 model 'Create by https://github.com/jrodriguez88'  *
*------------------------------------------------------------------------------*", sep = '\n')
  cat(paste0("FILEON = ","'", cultivar, tag, "_res.DAT'"), sep = '\n') 
  cat(paste0("FILEOL = ", "'model.log'"), sep = '\n')  
  cat(paste0("FILEIT = ", "'EXP", "\\", exp_set[1], "'", sep=""), sep = '\n')
  cat(paste0("FILEI1 = ", "'", cultivar, ".crp'"), sep = '\n')
  cat(paste0("FILEIR = ", "'", cultivar, tag, "_reruns.rer'"), sep = '\n') 
  cat(paste0("FILEI2 = ", "'SOIL", "\\", str_sub(exp_set[1], 1, 4), ".sol'"), sep = '\n') 
  
  cat(           
    "*----------------------------------------------------------------------*
* Set output/print options                                             *
*----------------------------------------------------------------------*
    PRDEL  = 1.    ! Output time step (day)
IPFORM = 5     ! Code for output table format:", sep='\n')
  cat("    ! 4 = spaces between columns
! 5 = TAB's between columns (spreadsheet output)
! 6 = two column output
COPINF = 'N'   ! Switch variable whether to copy the input files
! to the output file ('N' = do not copy,
! 'Y' = copy)
DELTMP = 'N'   ! Switch variable what should be done with the
! temporary output file ('N' = do not delete,
! 'Y' = delete)
IFLAG  = 1100  ! Indicates where weather error and warnings
! go (1101 means errors and warnings to log
! file, errors to screen, see FSE manual)
*PRSEL = 
*'WSO_OBS','WSO',
*'WAGT_OBS','WAGT',
*'WST_OBS','WST',
*'WLVG_OBS','WLVG',
*'WLVD_OBS','WLVD',
*'LAI_OBS','LAI'
! The string array PRSEL contains the output variables for which
! formatted tables have to be made. One or more times there is a
! series of variable names terminated by the word <TABLE>.
! The translator writes the variables in each PRINT statement to
! a separate table
*IOBSD = 2008,161 
! List of observation data for which output is
! required. The list should consist of pairs
! <year>,<day> combination")
  
  sink()
  
  
  ## Make reruns
  sink(file = paste0(cultivar, tag, "_reruns.rer"), append = F)
  paste_reruns <- function(x) {
    cat('********************', sep = '\n')
    cat('\n')
    cat(paste("* Rerun set"," - ", which(exp_set==x[1]), " - ", x[1]), sep = '\n')
    cat(paste("FILEIT = ", "'", "EXP", "\\", x[1], "'", sep=""), sep = '\n')
    cat(paste0("FILEI1 = ", "'", cultivar, ".crp'"), sep = '\n')
    cat(paste0("FILEI2 = ", "'SOIL", "\\", str_sub(x[1], 1, 4), ".sol'"), sep = '\n')
    cat('\n')
  }
  
  walk(.x = exp_set, 
       .f = invisible(paste_reruns))
  
  sink()
  
  system("ORYZA3.exe")
  
  file.rename(paste0("op.dat"), 
              paste0(paste0(cultivar, tag,  "_op.dat")))
  
  
  setwd(wd)
  
}
#run_ORYZA(path, cultivar, cal_set)


#Executes the model depending on OS
# dir_run <- Carpeta con todos los inputs necesarios para simulacion con DSSAT 
# Para windows, el ejecutable V4.8 "DSCSM048.EXE" debe estar instalado o dispnible en "C:/DSSAT48/DSCSM048.EXE 
# Para windows tener imagen de dssat 

#Test DSSAT
# Crea archivo batch en el directorio especifico de la simulacion
write_batch_dssat <- function(crop, exp_set, filename = "DSSBatch.v48"){
  
  #treatments_number <- length(exp_set)
  
  
  batchfile <- rbind(
    rbind(
      # Batchfile headers            
      paste0("$BATCH(", toupper(crop), ")"),            
      "!",            
      cbind(sprintf("%6s %89s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", 
                    "CO"))),            
    cbind(sprintf("%6s %83s %6i %6i %6i %6i",            
                  paste0(exp_set),
                  1:1,  # Variable for treatment number            
                  1,  # Default value for RP element            
                  0,  # Default value for SQ element            
                  1,  # Default value for OP element            
                  0)))  # Default value for CO element 
  
  # Write the batch file to the selected folder  
  write(batchfile, file = filename, append = F)
  
  
  
}

# Execute DSSAT DSCSM048.EXE
run_model_dssat <- function(path, crop, exp_set, dssat_exe = "C:/DSSAT48/", tag = NULL){
  
  # set dir of simulation
  wd <- getwd()
  setwd(path)
  
  #tag <- ifelse(is.null(tag), "", paste0("_", tag))  
  
  # write DSSAT Batch file 
  batch_filename <- paste0(path, "/", "DSSBatch.v48")
  
  xfile <- map(exp_set, ~crop_name_setup(.x, crop)) %>% map_chr("ext")
  
  #treatments_number <- length(exp_set)    # number of escenaries
  
  write_batch_dssat(crop, xfile, batch_filename)
  
  
  
  
  # run DSSAT - OS
#  print(paste('Simulation: ', path, " - ", crop))
  
#  if (Sys.info()['sysname'] == 'Windows'){ 
    
    model <- paste0(crop_name_setup("CIAT0001", crop)[["model"]], 048) 
    system(paste0(dssat_exe, "DSCSM048.EXE " , model," B ", "DSSBatch.v48"), ignore.stdout = F, show.output.on.console = T)
#  }
#  else{
#    system(paste0('dssat ',  path,' B DSSBatch.v48'))
    
#  }
  
  
  setwd(wd)
  
  
}



# Execute AQUACROP "ACsaV60.exe"
run_model_aquacrop <- function(path, cultivar, exp_set, tag = NULL, timeout = 60){
  
  # set dir of simulation
  wd <- getwd()
  setwd(path)
  
#  tag <- ifelse(is.null(tag), "", paste0("_", tag))  
  
  ## Write project
  
  
  exp_patt <- paste(exp_set, collapse = "|")
  
  
  remove_exp <- list.files(paste0(path, "/LIST"), full.names = T) %>%
    str_subset(pattern = exp_patt, negate = T)
  
  file.remove(remove_exp)
  
  
#  message(paste("Simulation Projects ", cultivar, " :", path))
  
  system("ACsaV60.exe", timeout = timeout)
  
  
  

  
  
  
  ## Run Aquacrop plugin
  
  
  
  
  
  setwd(wd)
  
}

