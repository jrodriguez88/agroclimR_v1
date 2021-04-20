# Script app - Pronosticos agro-climaticos estacionales.
# Author: Rodriguez-Espinoza J. / Esquivel A.
# Repository: https://github.com/jrodriguez88/
# 2020

### Objetivo: 
### Generar pronosticos de rendimiento a partir de una prediccion climatica probabilistica.


### 1. Cargar requerimientos

source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/agroclim_forecaster.R", encoding = "UTF-8")
load_agroclimr_requeriments()
inpack(c("tidyverse", "data.table", "lubridate", "sirad", "naniar", "jsonlite" ,"soiltexture", "Hmisc", "parallel"))
ubicar_directorios("")


### 2. Definir zona de estudio
localidad <- "Centroamerica"
latitud <- 13.9
longitud <- -86.0
altitud <- 677

### 3. Leer datos de entrada
datos_historicos <- read_csv(paste0(directorio_datos, "/datos_clima_test.csv"))

pronostico <- read_csv(paste0(directorio_datos, "/pronostico_probabilistico.csv"))


### 4. Grafique los datos observados y las probabilidades

plot_prob_forecast(pronostico)
plot_weather_series(datos_historicos, localidad)

### 5. Realice el remuestreo estadistico sobre la serie historica
data_resampling <- resampling(datos_historicos, pronostico, 2021)


### Opcional : Guarde los escenarios
#dir.create(directorio_resultados)
#function_to_save(localidad, data_resampling, directorio_resultados)


### 6. Grafique los escenarios de remuestreo
plot_resampling(data_resampling, datos_historicos, localidad, stat = "median")


### 7. Configurar datos para formatos AquaCrop

cultivar <- list.files(aquacrop_files, pattern = ".CRO") %>% str_remove(".CRO")
suelos <- list.files(aquacrop_files, pattern = ".SOL") %>% str_remove(".SOL")
start_sow <- data_resampling$data[[1]]$data[[1]]$month[[1]]   #c(month, day)


to_aquacrop <- map(cross2(cultivar, suelos),
                   ~from_resampling_to_aquacrop(
                     data_resampling, localidad, .x[[1]], .x[[2]], 
                     start_sow, get_sample = 10, date_breaks = 5)) %>% bind_rows()

### 8. Exportar datos a formato AquaCrop\

#Borra contenido de carpetas
file.remove(list.files(aquacrop_files, full.names = T, pattern = ".PLU|.ETo|CLI|Tnx"))
unlink(paste0(plugin_path, "/OUTP/*"))
unlink(paste0(plugin_path, "/LIST/*"))

#Exportar datos clmaticos
to_aquacrop %>% select(id_name, data) %>% distinct() %>% 
  walk2(.x = .$id_name, .y = .$data,
        .f = ~make_weather_aquacrop(aquacrop_files, .x, .y, latitud, altitud))

#Exportar proyectos

####################################################### Setting parallel 
ncores <- detectCores() - 1
cl <- makeCluster(ncores)
clusterExport(cl, c(as.vector(lsf.str()),
                    "to_aquacrop",
                    "aquacrop_files"))
clusterEvalQ(cl, {library(tidyverse);library(lubridate); library(sirad); library(data.table)})

#tictoc::tic()
parLapply(cl, to_aquacrop %>% pull(to_project), function(x){ 
  make_project_by_date(id_name = x$id_name,
                       sowing_dates = x$sowing_dates, 
                       cultivar = x$cultivar,
                       soil = x$soil, clim_data = x$clim_data, 
                       max_crop_duration = 150, 
                       aquacrop_files = aquacrop_files, plugin_path = x$plugin_path)})
#tictoc::toc()
stopCluster(cl)
#########################################################################

### 9. Ejecutar las simulaciones de AquaCrop
system("plugin/ACsaV60.exe")


### 10. Lectura de resultados
path_op <- paste0(plugin_path, "/OUTP/")
season_files <- list.files(path_op, pattern = "season") #%>% str_subset("Frijol")

file_str <- c("clima", "cultivar", "soil", "crop_sys")
season_data <- map(.x = season_files, ~read_aquacrop_season(.x, path_op)) %>%
  bind_rows() 


### 11. Graficar resultados finales
plot_agroclim_forecast(season_data, localidad, file_str = file_str, yield_units = "kg/ha")
plot_agroclim_hidric(season_data, localidad, file_str)


### 12. Guardar datos de simulacion (Opcional)
#write_csv(season_data, "Resultados_de_simulacion.csv")


if(exists("season_data")==T){
   message(paste0("Proceso Exitoso, ha generado la simulacion agroclimatica para ",
                 localidad, "\n", "Se evaluaron los cultivos de ", paste(cultivar, collapse = ", "),
                 "\nEn suelos ", paste(suelos, collapse = ", ")))}


