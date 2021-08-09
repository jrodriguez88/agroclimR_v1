#### DSSAT make_weather
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2019

#### Load packages
#library(tidyverse)
#library(data.table)
#library(sirad)
#library(lubridate)


#path <- paste0(getwd(), "/outputs/weather/")
#id_name <- "test_name"
#lat <- 13.9
#alt <- 657
#wth_data <- read.csv("data/weather_to_aquacrop.csv") %>% 
#    mutate(date = ymd(date))

## write_wth_dssat function compute weather information to ORYZA weather file.
## 'wth_data':  csv file name or data.frame. 
#           str-> 
#                ..$ date: Date ->(mdy)
#                ..$ tmax: num  ->(oC)
#                ..$ tmin: num  ->(oC) 
#                ..$ rain: num  ->(mm) 
#                ..$ srad: num  ->(MJ) 
#                ..$ rhum: num  ->(%)
#                ..$ wspd: num  ->(m/s)
## 'path':  path folder or working directory
## 'id_name'  :  4 letters string of locality name. "AIHU"--> Aipe, Huila 
## 'lat':   latitud (decimal degrees)
## 'lon':   longitud (decimal degrees)
## 'elev':  elevation (meters above sea level)

tidy_wth_dssat <- function(wth_data){
  
  var_names <- colnames(wth_data)
  
  stopifnot(require(sirad))
  stopifnot(class(wth_data$date)=="Date" & all(c("tmax", "tmin", "rain", "srad") %in%  var_names))
  
  if("VP" %in% var_names){
    message("Early morning vapor pressure (VP; kPa) in data")
  } else if(isTRUE(cal_VP) & "rhum" %in% var_names)
  {
    wth_data <- mutate(wth_data, 
                       es = sirad::es(tmax, tmin),         #Determination of mean saturation vapour pressure http://www.fao.org/3/x0490e/x0490e07.htm  - eq.12
                       VP = es*rhum/100) %>% select(-es)   #Determination of actual vapour pressure vpd http://www.fao.org/3/x0490e/x0490e07.htm  - eq.19
    message("Early morning vapor pressure (VP; kPa) derived from relative humidity data")
    
  } else {
    wth_data <- mutate(wth_data, VP = NA_real_)
    message("Vapor Pressure is not Available - VP Set as NA: -99")
    
  }
  
  
  if (!"wspd" %in% var_names) {
    wth_data <- mutate(wth_data, wspd = " ")
#    message("Wind Speed is not Available - Set as NA: -99")
  }
  
  return(wth_data)
  
}


write_wth_dssat <-function(path, id_name, wth_data, lat, lon, elev, tav = -99, amp = -99, ref_ht = 2) 
  
  make_wth <- function(data, out_dir, lat, long, name_xfile_climate){
    
    # Function to write date into DSSAT format
    # 'var' must be object date class
    date_for_dssat <- function(var) {
      stopifnot(class(var)=="Date")
      stopifnot(require(lubridate))
      
      yr <- str_sub(year(var), -2)
      doy <- yday(var)
      
      paste0(yr, sprintf("%.3d", doy))
      
    }
    
    
    srad <- wth_data$srad
    tmax <- wth_data$tmax
    tmin <- wth_data$tmin
    rain <- wth_data$rain
    date <- date_for_dssat(data$date) 
    
    
    
    sink(paste0(path, id_name, '.WTH'), append = F)

    

    cat(paste("*WEATHER DATA :"), paste(id_name), "DSSAT Weather file - by https://github.com/jrodriguez88")
    cat("\n")
    cat("\n")
    cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
    cat("\n")
    cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", "CIAT", lat, lon, elev, tav, amp, ref_ht, ref_ht))
    cat("\n")
    cat(c('@DATE  SRAD  TMAX  TMIN  RAIN  DEWP  WIND   PAR  EVAP  RHUM'))
    cat("\n")
    cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f",
                      date, srad, tmax, tmin, rain, "", )), sep = "\n")
    sink()
    
    
  }



#ideal data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date))
# minimum data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date)) %>% select(date, tmax, tmin, rain)

data %>% mutate(srad = srad_cal(., 3.5,250, kRs = 0.16)) %>%
  write_wth_dssat("R_package/write_files/", "TEST", .,  3.5, -75, 250)