# Calulate AQUACROP model params to write CRP file
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2023

#

#library(ggplot2)
#library(dplyr)
#library(broom)
#library(purrr)



## Exploratory Data Analysis - Experimental data - Seleccionar o descartar exp 

# Crop Phenology
#phen <- test_data$phen
#plot_phen_obs(phen) #%>% ggplotly()

#Leaf Area Index
#lai <- test_data$lai
#plot_lai_obs(lai) #%>% ggplotly() 

#Shoot Dry Matter
#dry_matter <- test_data$dry_matter
#plot_drymatter_obs(dry_matter) #%>% ggplotly()

#Yield dry matter
#yield <- test_data$yield
#plot_yield_obs(yield) + coord_flip() #%>% ggplotly()









# Temperatura base de desarrollo, tbase = 8, 
#stat <- c("mean", "min", "max")

get_params_aquacrop <- function(test_data, exp_set, tbase = 8, stat = "mean"){
  
  
  ## Exploratory Data Analysis - Experimental data - Seleccionar o descartar exp 
  
  exp_filter <- str_remove(exp_set, ".exp|.EXP")
  
  
  # Crop Phenology
  phen <- test_data$phen
  #plot_phen_obs(phen) #%>% ggplotly()
  
  #Leaf Area Index
  lai <- test_data$lai
  #plot_lai_obs(lai) #%>% ggplotly() 
  
  #Shoot Dry Matter
  dry_matter <- test_data$dry_matter %>% filter(var == "WAGT") %>% 
    nest(data  = - exp_file) %>% 
    mutate(data  =  map(data, ~.x %>% dplyr::filter(date == max(date)))) %>% 
    unnest(data) %>% dplyr::select(exp_file, WAGT = value, se)
  #plot_drymatter_obs(dry_matter) #%>% ggplotly()
  
  #Yield dry matter
  yield <- test_data$yield
  

  
  # Harvest Index calculation
  
  HI <- yield %>% left_join(dry_matter, by = join_by(exp_file)) %>%
    mutate(HI = value/WAGT) %>% dplyr::select(exp_file, HI) %>% 
    dplyr::filter(exp_file %in% exp_filter) 
  

  
  #funcion para remover separadores "_" de las variables a analizar
  remove_unders <- function(var){str_replace_all(var, "_", "")}
  
  
  
  #Agronomic data - Plant populations
  agro_data <- test_data$data$input_data %>% map(~.x[["AGRO_man"]]) %>% bind_rows() %>%
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    mutate(PDAT = as.Date(PDAT), exp_file  = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>%
    dplyr::select(exp_file, PDAT:NPLDS) #%>% set_names(~tolower(.x))
  
  
  # Daily Weather data
  wth_list <- test_data$data %>% 
    mutate(id_name = site) %>%
    dplyr::select(id_name, wth, lat, lon, elev) %>%
    mutate(wth = map(wth, ~.x %>% impute_mean_wth))
  
  

  # Join data to parameter estimation
  data_param_aquacrop <- phen %>% dplyr::select(exp_file, data) %>% 
    dplyr::distinct() %>% rename(phen = data) %>% 
    dplyr::filter(exp_file %in% exp_filter) %>%
    left_join(
      lai %>% nest(data = date:se) %>% rename(lai=data), by = join_by(exp_file)) %>%
    mutate(id_name = word(exp_file, 1, sep = "_")) %>% 
    left_join(wth_list, by = join_by(id_name)) %>% left_join(agro_data, by = join_by(exp_file)) %>% 
    dplyr::select(id_name, exp_file, phen, lai, everything())
  
  
  #safe function ( TryCash)
  safe_canopy_params <- possibly(cal_canopycover_params, NULL)
  
  
  ## Test data 
  test_data2 <- data_param_aquacrop %>% 
    mutate(phen_params = map2(phen, wth, cal_phenol_aquacrop),
           canopy_params = pmap(list(wth_data = wth, phen_data = phen, lai_data = lai),  safe_canopy_params))
  
  ## Phen Params
  phen_params <- dplyr::select(test_data2, exp_file, phen_params) %>% 
    deframe() %>% compact() %>% enframe(name = "exp_file") %>% unnest(value) 
  
  
  ## Canopy Params
  canopy_params <- dplyr::select(test_data2, exp_file, canopy_params) %>% 
    deframe() %>% compact() %>% enframe(name = "exp_file") %>% unnest(value) 
  
  

  ##Genera un indice para calcular y filtrar resultados
  metric <- switch (stat,
                    "mean" = 1,
                    "min" = 2,
                    "max" = 3
  )
  
  

  
  ## MAke param List fron TEST_DATA
  param_list <- list(  
    

  #from sowing to emergence
  GDD_emergence = bootstrap_param(phen_params$GDD_emergence, stat = stat),
    
    
  #from emergence to maximum canopy cover
  GDD_CCx = bootstrap_param(phen_params$GDD_CCx, stat = stat),  
    
  #from emergence to flowering
  GDD_FL = bootstrap_param(phen_params$GDD_FL, stat = stat),
  
  #Length of the flowering stage
  GDD_FLL = bootstrap_param(phen_params$GDD_FLL, stat = stat),
  
  #from emergence to maturity
  GDD_M = bootstrap_param(phen_params$GDD_M, stat = stat),
  
  #GDD from emergence to start senescence
  GDD_senecence = bootstrap_param(canopy_params$GDD_senescence, stat = stat),
  
  #Canopy Growth Coefficient
  CGC = bootstrap_param(canopy_params$CGC, stat = stat),
  
  #Maximun canopy cover
  CCx = bootstrap_param(canopy_params$CCx, stat = stat),
  
  #Plant population per hectare 
  NPLDS = bootstrap_param(data_param_aquacrop$NPLDS, stat = stat)*10000,
  
  #Crop Water Productivity
#  WP
  
  #Reference Harvest Index
  HIo = bootstrap_param(HI$HI, stat = stat)*100,
  
  #Building-up of Harvest Index during yield formation
  GDD_HI = bootstrap_param(phen_params$GDD_HI, stat = stat),
  
  #Canopy Decline Coefficient
  CDC = bootstrap_param(canopy_params$CDC, stat = stat))
  
  #Maximum effective rooting depth
#  Zr
  
  #Crop coefficient when canopy is complete
#  Kc
  
  #Soil water depletion factor for canopy expansion- Lower threshold
#  Ks_exp
  
  #Minimum air temperature below which pollination starts to fail
#  Ks_polc
  
  #Maximum air temperature above which pollination starts to fail
#  Ks_polh
  
  
  
  
 
  ##Retorna lista de parametros para ingresar a archivo de cultivo 
  
  return(param_list)  
  
  
  
  
}
  




## Function apply bootstrap method over crop parameters -- get mean, max or min value 
# param_data = parameter crop data ,
# reps  = Number of repetitions - boots
#ci = cofidence interval 
#stat =statistic value - mena, max o  min 
bootstrap_param <- function(param_data, reps = 2000, ci = 0.95, stat = "mean"){
  
  require(Hmisc)
  
  stat <- switch (stat,
                  "mean" = 1,
                  "min" = 2,
                  "max" = 3
  )
  
  smean.cl.boot(param_data, conf.int=ci, B=reps, na.rm=TRUE, reps=T)[stat][[1]]
  
  
}




# Function for caculate phenology parameters by GDD and days
# Temperatura base de desarrollo, tbase = 8, 
# Duracion de la floracion , FLL = 10

#wth_data <-  data_param_aquacrop$wth[[10]] 
#phen_data <- data_param_aquacrop$phen[[10]]

cal_phenol_aquacrop <- function(phen_data, wth_data, tbase = 8, Fleng = 10){
  
  
  
  #extract phenological dates
  
  PDAT <- phen_data %>% filter(var == "PDAT") %>% pull(value)
  EDAT <- phen_data %>% filter(var == "EDAT") %>% pull(value)
  IDAT <- phen_data %>% filter(var == "IDAT") %>% pull(value)
  FDAT <- phen_data %>% filter(var == "FDAT") %>% pull(value)
  MDAT <- phen_data %>% filter(var == "MDAT") %>% pull(value)
  
  clim_data <- wth_data %>%
    dplyr::filter(date >= PDAT,
                  date <= MDAT ) %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>% 
    dplyr::select(-c(tmax:rhum))
  
  #functions to filter
  
  filter_data_gdd <- function(clim_data, date1, date2, adj_d = 0){
    clim_data%>%
      dplyr::filter(date >= date1,
                    date < date2 + days(adj_d)) %>%
      summarise(sum_gdd = sum(HUH)) %>% pull(sum_gdd)
    
  }
  
  filter_data_days <- function(clim_data, date1, date2, adj_d = 0){
    clim_data%>%
      dplyr::filter(date >= date1,
                    date < date2 + days(adj_d)) %>%
      summarise(sum_days = n()) %>% pull(sum_days)
    
  }
  
  
  #make a param data.frame   
  data.frame(
    GDD_emergence   =  clim_data %>% filter_data_gdd(., PDAT, EDAT),
    GDD_CCx         =  clim_data %>% filter_data_gdd(., EDAT, IDAT),
    GDD_M           =  clim_data %>% filter_data_gdd(., EDAT, MDAT),
    GDD_FL          =  clim_data %>% filter_data_gdd(., EDAT, FDAT-5),
    GDD_FLL         =  clim_data %>% filter_data_gdd(., FDAT, FDAT, Fleng), # se estima como una duracion aproximada, necesita ser evaluado
    GDD_HI          =  clim_data %>% filter_data_gdd(., FDAT, MDAT -5),
    days_sow_eme    =  clim_data %>% filter_data_days(., PDAT, EDAT),
    days_sow_maxroot=  clim_data %>% filter_data_days(., PDAT, IDAT),
    days_sow_senesc =  clim_data %>% filter_data_days(., PDAT, FDAT, 14),
    days_sow_mat    =  clim_data %>% filter_data_days(., PDAT, MDAT),
    days_sow_flow   =  clim_data %>% filter_data_days(., PDAT, FDAT),
    days_flowlen    =  clim_data %>% filter_data_days(., FDAT-5, FDAT+5),
    days_buildHI    =  clim_data %>% filter_data_days(., FDAT, MDAT-7)
  )  
  
}




  
### Calculate Canopy Cover (CC) from lai
### Funcion para calcular parametros relacionados con Canopy cover 
  
#wth_data <-  data_param_aquacrop$wth[[10]] 
#phen_data <- data_param_aquacrop$phen[[10]]
#lai_data <- data_param_aquacrop$lai[[10]]
# Temperatura base de desarrollo, tbase = 8,
# CCo = area foliar inicial por planta (cm) / Canopy cover inicial 


  
cal_canopycover_params <- function(wth_data, phen_data, lai_data, tbase = 8, CCo = 3) {
    # Ajustar curva polinómica de segundo grado
  
  
  PDAT <- phen_data %>% filter(var == "PDAT") %>% pull(value)
  EDAT <- phen_data %>% filter(var == "EDAT") %>% pull(value)
  IDAT <- phen_data %>% filter(var == "IDAT") %>% pull(value)
  FDAT <- phen_data %>% filter(var == "FDAT") %>% pull(value)
  MDAT <- phen_data %>% filter(var == "MDAT") %>% pull(value)
  

  test <- wth_data %>%
    dplyr::filter(date >= PDAT,
                  date <= MDAT ) %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>% 
    dplyr::select(-c(tmax:rhum)) %>% 
    mutate(gdd = cumsum(HUH)) %>% left_join(lai_data, by = join_by(date)) %>% 
    dplyr::select(date, HUH, gdd, lai = value, se) %>% na.omit()
    #mutate(lai = case_when(date == PDAT ~ 0,
    #                       TRUE ~ lai ),
    #       se = case_when(date == PDAT ~ 0,
    #                      TRUE ~ se ))
  
  # canopy cover data.frame
  # Convert CC = 1 - exp(-k*LAI))
  
  if(mean(test$lai)>20){
    
    df <- test %>% mutate(canopy = lai/100)
    
  } else {
  
  df <- test %>%
    mutate(
      k = case_when(
        date <= IDAT ~ 0.4,
        date >= FDAT ~ 0.6,
        TRUE ~ 0.5),
      canopy = (1 - exp(-k*lai)))}
  
  
    
    max_obs <- df$gdd[which.max(df$canopy)]
    
    modelo <- lm(canopy ~ poly(gdd, 2), data = df)
    
    # Extraer valor máximo y punto de inflexión del modelo ajustado
    x_vals <- seq(min(df$gdd), max(df$gdd), length.out = 1000)
    y_vals <- predict(modelo, newdata = data.frame(gdd = x_vals))
    maximo <- max(y_vals)
    inflexion <- x_vals[which.max(y_vals)]
    
    # Calcular pendiente de la fase de crecimiento
    p1 <- coef(lm(canopy ~ gdd, data = subset(df, gdd <= inflexion)))
    growing_slope <- p1[2]
    
    # Calcular pendiente de la fase de senescencia
    p2 <- coef(lm(canopy ~ gdd, data = subset(df, gdd >= max_obs)))
    senescence_slope <- p2[2]
    
    # Crear gráfico
    plot(df$gdd, df$canopy, pch = 16, xlab = "Grados días de crecimiento", ylab = "% Cobertura de canopy")
    lines(x_vals, y_vals)
    points(inflexion, maximo, col = "green")
    abline(p1, col = "blue", lty = 2)
    abline(p2, col = "red", lty = 2)
    
    # Retornar resultados
    return(data.frame(CCx = maximo, GDD_senescence = inflexion, CGC = growing_slope*10,
                CDC = -senescence_slope*10))
  }  



#cal_canopycover_params(wth_data, phen_data, lai_data )





























