# Script to create Aquacrop Crop File 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2023





tidy_to_write_crop <- function(param_data, model = "dssat", values = "Base", export_default = F){
  
  ###Crea tabla control de parametros y rangos, basados en el CRO standard de aquacrop
  ## Max and min  + Parameter (+/- 30%)
  
  default_list <- tibble(
    
    #strsplit(clipboard(), "\n") %>% unlist() %>% paste(collapse = "', '")
    Model = c(rep("DSSAT_CERES", 10)),
    
    Component = c('Phenology', 'Phenology', 'Phenology', 'Phenology', 
                  'Leaf and stem growth', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Temperature and drought stress','Temperature and drought stress' ),
    
    
    Parameter = c('P1', 'P2O', 'P2R', 'P5', 'PHINT', 'G1', 'G2', 'G3', 'THOT', 'TCLDP'),
    Unit = c('GDD', 'h', 'GDD', 'GDD', 'GDD', 'No/g', 'g', 'scaler value', '°C', '°C'),
    Base = map(list('500', '12', '100', '450', '83', '55', '0.025', '1', '28', '15'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
    Min = map(list('150', '11', '5', '150', '55', '50', '0.015', '0.7', '25', '12'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
    Max = map(list('800', '13', '300', '850', '90', '75', '0.032', '1.3', '42', '20'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
    
    Description = c('Basic vegetative phase', 'Critical photoperiod or the longest day length',
                    'Extent to which phasic development leading to panicle initiation is delayed',
                    'Time period from beginning of grain filling', 'Phylochron interval', 
                    'Potential spikelet number coefficient', 'Single grain weight', 'Tillering coefficient', 
                    'Temperature above which spikelet sterility is affected by high temperature',
                    'Temperature below which spikelet sterility is affected by low temperature')
    
    
    
  )
  
  
  if(is.null(param_data)){return(default_list)} 
  
  
  
  if(isTRUE(export_default)){default_list <<- default_list}
  
  
  if(any(class(param_data) == "data.frame")) {
    
    stopifnot(any(names(param_data) == "Parameter"))
    
    message(paste0("Parameters to CRP file: ", paste(param_data$Parameter, collapse = ", ")))
    
    param_data <- rename_with(param_data, .fn = function(x) str_replace(x, "Base|Set_cal", values))
    if(any(names(param_data) == "Min")){message("Minimum range are available")
    } else {param_data <- mutate(param_data, Min = list(NULL))}
    if(any(names(param_data) == "Max")){message("Maximum range are available")
    } else {param_data <- mutate(param_data, Max = list(NULL))}
    
    
    test_params_oryza <- default_list %>% 
      left_join(param_data, by = "Parameter") %>%
      mutate(Base = map2(Base.x, Base.y, function(x, y) if(is.null(y)){x} else {y}), 
             Min = map2(Min.x, Min.y, function(x, y) if(is.null(y)){x} else {y}),
             Max = map2(Max.x, Max.y, function(x, y) if(is.null(y)){x} else {y})) %>% 
      dplyr::select(-contains(".x"), -contains(".y"))
    
    
  } else if(any(class(param_data) == "list")) {
    
    stopifnot(any(default_list$Parameter %in% names(param_data)))
    
    message(paste0("Parameters to CRP file: ", paste(names(param_data), collapse = ", ")))
    
    param_data <- enframe(param_data, name = "Parameter", value = "Base") %>% 
      mutate(Min = list(NULL), Max = list(NULL))
    
    
    test_params_oryza <- default_list %>% 
      left_join(param_data, by = "Parameter") %>%
      mutate(Base = map2(Base.x, Base.y, function(x, y) if(is.null(y)){x} else {y}), 
             Min = map2(Min.x, Min.y, function(x, y) if(is.null(y)){x} else {y}),
             Max = map2(Max.x, Max.y, function(x, y) if(is.null(y)){x} else {y})) %>% 
      dplyr::select(-contains(".x"), -contains(".y"))
    
    
  }
  
  
  return(test_params_oryza)    
  
}


#crop_params_dssat <- tidy_to_write_crop(NULL) %>% dplyr::select(Parameter, Base) %>% deframe()



write_crop_dssat <- function(path, cultivar, crop_params_dssat, ecotype = "IB0001", id_var = "CROP00"){
  
  
  
  
  #write ´*.SPE´ file
  
  sink(paste0(path, '/RICER048.SPE'), append = F)
  
  
  cat("*RICE SPECIES COEFFICIENTS: RICER048 MODEL - by https://github.com/jrodriguez88", sep = "\n")
  cat("\n")
  cat("*CHARACTERISTICS", sep = "\n")
  cat("@C X(EN) Y(CH)  YSTD", sep = "\n")
  cat("RI OPT   SHME      1     ! Shock calculation method (1-standard, 2-Salaam)", sep = "\n")
  cat("RI OPT   SHFC    1.0     ! Shock factor", sep = "\n")
  cat("!RI OPT   PHIN   83.0     ! Phyllochron interval", sep = "\n")
  cat("RI OPT   CO2X      0  220  330  440  550  660  770  880  990 9999  ! CO2 X axis", sep = "\n")
  cat("RI OPT   CO2Y   0.00 0.71 1.00 1.08 1.17 1.25 1.32 1.38 1.43 1.50  ! CO2 Y axis", sep = "\n")
  cat("RI OPT   RWEP   1.50", sep = "\n")
  cat("RI OPT   PORM   0.00                   ! Minimum pore space", sep = "\n")
  cat("RI OPT   RWMX   0.03                   ! Max root water uptake", sep = "\n")
  cat("RI OPT   RLWR   1.05                   ! Root length weight ratio", sep = "\n")
  
    
    
 sink()
  
  

  
  
#write_cul <- function(matrix_cul, out_dir){
  
  # matrix_cul <- x
  
 sink(paste0(path, '/RICER048.CUL'), append = F)
  
  
  
  cat("*RICE CULTIVAR COEFFICIENTS: RICER048 MODEL - by https://github.com/jrodriguez88", sep = "\n")
  cat("\n")
  
  cat("@VAR#  VAR-NAME........ EXPNO   ECO#    P1   P2R    P5   P2O    G1    G2    G3 PHINT  THOT TCLDP TCLDF")
  # 
  cat("\n")

  cat(paste(sprintf("%6s", id_var),
            sprintf("%-16s", cultivar), 
            sprintf("%5s", '.'),
            sprintf("%6s", ecotype),
            sprintf("%5.1f", crop_params_dssat$P1),
            sprintf("%5.1f", crop_params_dssat$P2R),
            sprintf("%5.1f", crop_params_dssat$P5),
            sprintf("%5.1f", crop_params_dssat$P2O),
            sprintf("%5.1f", crop_params_dssat$G1), 
            sprintf(".0%3.0f", crop_params_dssat$G2*10000),
            sprintf("%5.2f", crop_params_dssat$G3),
            sprintf("%5.1f", crop_params_dssat$PHINT),
            sprintf("%5.1f", crop_params_dssat$THOT),
            sprintf("%5.1f", crop_params_dssat$TCLDP),
            sprintf("%5.1f", 15)))
  sink()
}

#write_crop_dssat(path, cultivar, crop_params_dssat, ecotype = "IB0001")
