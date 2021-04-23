# Script app - Descarga de datos clima y suelo
# Author: Rodriguez-Espinoza J. / Esquivel A.
# Repository: https://github.com/jrodriguez88/csmt
# 2021

# Source of weather data: NASA Prediction Of Worldwide Energy Resources https://power.larc.nasa.gov/
# Source of soil data: https://www.soilgrids.org/  


### Argumentos de entrada
#localidad <- "Centroamerica"
#latitud <- 13.9
#longitud <- -86.0
#altitud <- 677
#ini_date <- make_date(1990, 1, 1)
#end_date <- make_date(2020, 12, 31)



## Descarga de NASAPOWER
datos_clima_nasa <- get_data_nasapower(latitud, longitud, make_date(1990, 1, 1), make_date(2020, 12, 31)) %>% 
  from_nasa_to_model() 
#datos_clima_nasa %>% rename(prec = rain) %>% select(day, month, year, prec, tmax, tmin) %>% 
#                plot_weather_series(id_label = "Jalapa_Nicaragua")  #%>%write_csv("datos_nasa.csv")


## Descarga de SOILGRIDS
datos_suelo_soilgrids <- get_data_soilgrids(latitud, longitud) %>% 
  from_soilgrids_to_aquacrop(id_name = localidad, soilgrids_data = .)


## Convertir a formato AquaCrop
make_weather_aquacrop("aquacrop_files/", localidad, datos_clima_nasa, latitud, altitud)

make_soil_aquacrop("aquacrop_files/", datos_suelo_soilgrids$data, datos_suelo_soilgrids$CN,datos_suelo_soilgrids$REW)
