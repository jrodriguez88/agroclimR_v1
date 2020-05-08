# Script app - Pronósticos agro-climáticos estacionales.
# Author: Rodriguez-Espinoza J. / Esquivel A.
# Repository: https://github.com/jrodriguez88/
# 2020

### Objetivo: 
### Generar pronosticos de rendimiento a partir de una prediccion climatica probabilistica.


### 1. Cargar requerimientos

source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/agroclim_forecaster.R", encoding = "UTF-8")
load_agroclim_requeriments()
inpack(c("tidyverse", "data.table", "lubridate", "sirad", "naniar", "jsonlite" ,"soiltexture"))
crear_directorios_COF()


### 2. Definir zona de estudio
localidad <- "SitioA"
latitud <- 13.9
altitud <- 677

### 3. Leer datos de entrada
datos_historicos <- read_csv(paste0(directorio_datos, "/datos_clima.csv"))

pronostico <- read_csv(paste0(directorio_datos, "/pronostico_probabilistico.csv"))


### 4. Grafique los datos observados y las probabilidades

plot_prob_forecast(pronostico)
plot_weather_series(datos_historicos, localidad)

### 5. Realice el remuestreo estadistico sobre la serie historica
data_resampling <- resampling(datos_historicos, pronostico, 2020)


### Opcional : Guarde los escenarios
#dir.create(directorio_resultados)
#function_to_save(localidad, data_resampling, directorio_resultados)


### 6. Grafique los escenarios de remuestreo
plot_resampling(data_resampling, datos_historicos, localidad, stat = "median")


### 7. Configurar datos para formatos AquaCrop

cultivar <- list.files(aquacrop_files, pattern = ".CRO") %>% str_remove(".CRO")
suelos <- list.files(aquacrop_files, pattern = ".SOL")
start_sow <- min(data_resampling$data[[1]]$data[[1]]$month)   #c(month, day)


to_aquacrop1 <- map(cultivar, 
                    ~from_resampling_to_aquacrop(data_resampling, localidad, .x, "FrancoArcilloso", start_sow, get_sample = NA)) %>% bind_rows()
to_aquacrop2 <- map(cultivar, 
                    ~from_resampling_to_aquacrop(data_resampling, localidad, .x, "FrancoArenoso", start_sow, get_sample = NA)) %>% bind_rows()
to_aquacrop <- bind_rows(to_aquacrop1, to_aquacrop2)



### 8. Exportar datos a formato AquaCrop\

#Borra contenido de carpetas
file.remove(list.files(aquacrop_files, full.names = T, pattern = ".PLU|.ETo|CLI|Tnx"))
unlink(paste0(plugin_path, "/OUTP/*"))
unlink(paste0(plugin_path, "/LIST/*"))

#Exportar datos clmaticos
walk2(paste0(localidad, to_aquacrop$id), to_aquacrop$data,
      ~make_weather_aquacrop(aquacrop_files, .x, .y, latitud, altitud))

#Exportar proyectos
to_aquacrop %>% pull(to_project) %>% 
  walk(~make_project_by_date(id_name = .x$id_name, 
                             sowing_dates = .x$sowing_dates, 
                             cultivar = .x$cultivar, 
                             soil = .x$soil, clim_data =  .x$clim_data, 
                             max_crop_duration =  140,
                             aquacrop_files = aquacrop_files, plugin_path = .x$plugin_path))


### 9. Ejecutar las simulaciones de AquaCrop
system("agroclim_COF/plugin/ACsaV60.exe")


### 10. Lectura de resultados
path_op <- paste0(plugin_path, "/OUTP/")
season_files <- list.files(path_op, pattern = "season") 

file_str <- c("clima", "cultivar", "soil", "crop_sys")
season_data <- map(.x = season_files, ~read_aquacrop_season(.x, path_op)) %>%
  bind_rows() 


### 11. Graficar resultados finales
plot_agroclim_forecast(season_data, localidad, file_str = file_str, yield_units = "qq/mz")
plot_agroclim_hidric(season_data, localidad, file_str)


## Felicitaciones, ha terminado su primer pronostico agroclimatico.
