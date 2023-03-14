# Calulate ORYZA model params to write CRP file
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022

#
#list_params =  output of "extract_drates_param" function

#library(ggplot2)
#library(dplyr)
#library(broom)
#library(purrr)


#source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/run_tools/extract_drates_param.R", encoding = "UTF-8")
#source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/run_tools/run_drates_param.R", encoding = "UTF-8")

#basedata_path <- paste0(path_proj) -> path
#input_data <- test_data$data$input_data
#exp_files <- list.files(paste0(basedata_path, "/EXP/"), pattern = "\\.exp$")
#model_curves <- c("lm", "loess")
#stat <- c("mean", "min", "max")






test_data$phen$data[[1]]

test_data$data$input_data[[1]]



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


to_write_exp$PHEN_obs[[1]]$MDAT
to_write_exp$PHEN_obs[[1]]$IDAT


test_data$phen


PLANT$PLANT_gro[[1]]




#LAI to CC


* Table of light extinction coefficient for leaves (-; Y-value) as a function
* of development stage (-; X value):
  KDFTB = 0.00, 0.4,
0.65, 0.4,
1.00, 0.6,
2.50, 0.6





#Extract data by component
# Crop Phenology
phen <- extract_obs_var(test_data$data$, exp_files, "phen")

#Leaf Area Index
lai <- extract_obs_var(data$data, exp_files, "lai")

#Shoot Dry Matter
dry_matter <- extract_obs_var(data$data, exp_files, "dry_matter")

#Yield dry matter
yield <- extract_obs_var(data$data, exp_files, "yield")


### Calculate Canopy Cover (CC) from lai
## 
####

data_agroman <- data_final %>% #filter(str_detect(file, "YOCS|VVME")) %>% #slice(1)  %>% 
  mutate(data = map(data, ~.x[["AGRO_man"]] %>% 
                      left_join(.x[["YIELD_obs"]] %>% select(ID:YIELD_MAX)))) %>% unnest(data) %>% 
  mutate(PDAT = as.Date(PDAT),
         exp_file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>% 
  filter(cultivar == cult) 

data_param_aquacrop <- phen %>% select(exp_file, data) %>% dplyr::distinct() %>% rename(phen = data) %>% 
  left_join(
    lai %>% nest(date:se) %>% rename(lai=data)) %>% mutate(localidad = str_sub(exp_file, 1, 4)) %>% 
  left_join(
    wth_data_exp  %>% mutate(HUH = ((tmax + tmin)/2) - 8) %>% nest(-localidad) %>% rename(wth = data))



test_data$phen %>% 
  
  



test_data$data$wth[[1]] %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>%
  dplyr::filter(date >= sowing_date,
                date <= sowing_date + max_crop_duration - 1) %>%
  mutate(sum_gdd = cumsum(HUH)) %>%
  dplyr::filter(sum_gdd<= gdd_mt) %>% 
  count() %>% pull(n)




phen_data <- test_data$phen %>% dplyr::select(-c(var, value)) %>% distinct()



to_extract_params <- phen_data %>% 
  mutate(site = word(exp_file, 1, sep = "_")) %>% 
  left_join(test_data$data %>% dplyr::select(site, wth), by = "site")


# Function for caculate phenology
cal_phen_aquacrop <- function(phen_data, wth_data, tbase = 8){
  
  
  
  
  clim_data <- wth_data %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>% 
    dplyr::select(-c(tmax:rhum))
  
  #extract phenological dates
  
  PDAT <- phen_data %>% filter(var == "PDAT") %>% pull(value)
  EDAT <- phen_data %>% filter(var == "EDAT") %>% pull(value)
  IDAT <- phen_data %>% filter(var == "IDAT") %>% pull(value)
  FDAT <- phen_data %>% filter(var == "FDAT") %>% pull(value)
  MDAT <- phen_data %>% filter(var == "MDAT") %>% pull(value)
  
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
    GDD_emergence     =  clim_data %>% filter_data_gdd(., PDAT, EDAT),
    gdd_sow_maxroot =  clim_data %>% filter_data_gdd(., PDAT, FDAT),
    gdd_sow_senesc  =  clim_data %>% filter_data_gdd(., PDAT, FDAT, 14),
    gdd_sow_mat     =  clim_data %>% filter_data_gdd(., PDAT, MDAT, -7),
    gdd_sow_flow    =  clim_data %>% filter_data_gdd(., PDAT, FDAT),
    gdd_flowlen     =  clim_data %>% filter_data_gdd(., FDAT, FDAT, 10),
    gdd_buildHI     =  clim_data %>% filter_data_gdd(., FDAT, MDAT),
    days_sow_eme    =  clim_data %>% filter_data_days(., PDAT, EDAT),
    days_sow_maxroot=  clim_data %>% filter_data_days(., PDAT, FDAT),
    days_sow_senesc =  clim_data %>% filter_data_days(., PDAT, FDAT, 14),
    days_sow_mat    =  clim_data %>% filter_data_days(., PDAT, MDAT, -7),
    days_sow_flow   =  clim_data %>% filter_data_days(., PDAT, FDAT),
    days_flowlen    =  clim_data %>% filter_data_days(., FDAT, FDAT, 10),
    days_buildHI    =  clim_data %>% filter_data_days(., FDAT, MDAT)
  )  
  
}

a <- to_extract_params %>% 
  mutate(cal = map2(data, wth, cal_phen_aquacrop)%>%)
a$cal %>% bind_rows()


phen_params <- data_param_aquacrop %>% 
  mutate(phen_params = map2(phen, wth, cal_phen_aquacrop)) %>% select(exp_file, phen_params)

phen_params %>% unnest(phen_params) %>% 
  pivot_longer(-exp_file) %>% nest(-exp_file)





test_data$


GDD_emergence
GDD_CCmax
GDD_Flow
GDD_FLL
GDD_M
GDD_senecence
CGC
CC_max
NPLDS
WP
HIo
GDD_HI
CDC
Zr
Kc
Ks_exp
Ks_polc
Ks_polh














#from sowing to emergence
GDD_emergence <-


#from emergence to maximum canopy cover
GDD_CCmax <- 

#from emergence to flowering
GDD_FL

#Length of the flowering stage
GDD_FLL

#from emergence to maturity
GDD_M

#GDD from emergence to start senescence
CC_senecence

#Canopy Growth Coefficient
CGC

#Maximun canopy cover
CC_max

#Plant population per hectare
NPLDS

#Crop Water Productivity
WP

#Reference Harvest Index
HIo

#Building-up of Harvest Index during yield formation
GDD_HI

#Canopy Decline Coefficient
CDC

#Maximum effective rooting depth
Zr

#Crop coefficient when canopy is complete
Kc

#Soil water depletion factor for canopy expansion- Lower threshold
Ks_exp

#Minimum air temperature below which pollination starts to fail
Ks_polc

#Maximum air temperature above which pollination starts to fail
Ks_polh



get_params_aquacrop <- function(path, input_data, exp_files, tbase = 8, stat = "mean")
  
  
  
### Funcion para calcular parametros relacionados con Canopy cover 
  
  # datos de ejemplo
  df <- data.frame(gdd = c(200, 400, 800, 1000, 1200, 2000),
                   canopy = c(5, 20, 50, 80, 95, 80))  
  
cal_canopycover_params <- function(df) {
    # Ajustar curva polinómica de segundo grado
    
    max_obs<- df$gdd[which.max(df$canopy)]
    
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
    abline(p1, col = "blue", lty = 2)
    abline(p2, col = "red", lty = 2)
    
    # Retornar resultados
    return(list(maximo = maximo, inflexion = inflexion, pendiente_crecimiento = growing_slope,
                pendiente_senescencia = senescence_slope))
  }  



#cal_canopycover_params(df)





























