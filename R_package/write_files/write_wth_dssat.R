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
  
  if (!"rhum" %in% var_names) {
    wth_data <- mutate(wth_data, rhum = " ")
    #    message("Wind Speed is not Available - Set as NA: -99")
  }  
  
  if (!"wspd" %in% var_names) {
    wth_data <- mutate(wth_data, wspd = " ")
#    message("Wind Speed is not Available - Set as NA: -99")
  } else {wth_data <- mutate(wth_data, wspd = wspd*3.6*24)}  #from wind speed mean (m/s) to (km/day)
  
  tav <- wth_data %>% 
    mutate(tmean = (tmax + tmin)/2 ) %>% 
    summarise(tav = mean(tmean)) %>% pull(tav)
  
  
  amp <- wth_data %>% 
    group_by(month(date)) %>%
    summarise(tmax = mean(tmax), tmin = mean(tmin)) %>%
    mutate(amp = tmax - tmin) %>% pull(amp) %>% mean
  
  return(list(wth_data = wth_data, tav = tav, amp = amp))
  
}


write_wth_dssat <-function(path, id_name, wth_data, lat, lon, elev, ref_ht = 2){
  
    data <- tidy_wth_dssat(wth_data)
    data$datadata$wth_data
    
    # Function to write date into DSSAT format
    # 'var' must be object date class
    date_for_dssat <- function(var) {
      stopifnot(class(var)=="Date")
      stopifnot(require(lubridate))
      
      yr <- str_sub(year(var), -2)
      doy <- yday(var)
      
      paste0(yr, sprintf("%.3d", doy))
      
    }
    
    date <- date_for_dssat(data$wth_data$date)
    srad <- data$wth_data$srad
    tmax <- data$wth_data$tmax
    tmin <- data$wth_data$tmin
    rain <- data$wth_data$rain
    wspd <- if(is.numeric(data$wth_data$wspd)) as.character(sprintf("%3.1f", data$wth_data$wspd)) else data$wth_data$wspd 
    rhum <- if(is.numeric(data$wth_data$rhum)) as.character(sprintf("%2.1f", data$wth_data$rhum)) else data$wth_data$rhum
     
    
    
    
    sink(paste0(path, id_name, '.WTH'), append = F)

    

    cat(paste("*WEATHER DATA :"), paste(id_name), "DSSAT Weather file - by https://github.com/jrodriguez88")
    cat("\n")
    cat("\n")
    cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
    cat("\n")
    cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.1f %5.1f", "CIAT", lat, lon, elev, data$tav, data$amp, ref_ht, ref_ht))
    cat("\n")
    cat(c('@DATE  SRAD  TMAX  TMIN  RAIN  DEWP  WIND   PAR  EVAP  RHUM'))
    cat("\n")
    cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f %5s %5s %5s %5s %5s",
                      date, srad, tmax, tmin, rain, " ", wspd, " ", " ", rhum)), sep = "\n")
    sink()
    
    
  }



## Usage

# ideal data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date))
#data %>% 
#  write_wth_dssat("R_package/write_files/", "TEST", .,  3.5, -75, 250)


# minimum data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date)) %>% select(date, tmax, tmin, rain)
#data %>% mutate(
#  srad = srad_cal(., 3.5,250, kRs = 0.16)) %>%
#  write_wth_dssat("R_package/write_files/", "TEST", .,  3.5, -75, 250)
