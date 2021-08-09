#### write_wth_oryza - Function to create ORYZA WTH file 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/csmt
# 2020

### ORYZA weather file, include:

#  Column    Daily Value                                                                         
#     1      Station number                                                                       
#     2      Year                                                                           
#     3      Day                                                                    
#     4      irradiance         KJ m-2 d-1                                            
#     5      min temperature            oC                                                        
#     6      max temperature            oC                                                        
#     7      vapor pressure            kPa   early morning vapor pressure (VP; kPa)                                                     
#     8      mean wind speed         m s-1                                                        
#     9      precipitation          mm d-1


## write_wth_oryza function compute weather information to ORYZA weather file.
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
## 'stn:    station number (numeric)
## 'multiyear' : logical, TRUE = "*.cli" multiyear format or FALSE = yearly format (ie. 1998 = *. 998)

tidy_wth_oryza <- function(wth_data, cal_VP = T){
    
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
        wth_data <- mutate(wth_data, wspd = NA_real_)
        message("Wind Speed is not Available - Set as NA: -99")
    }
    
    return(wth_data)
    
}

write_wth_oryza <- function(path, id_name, wth_data, lat, lon, elev, stn=1, multiyear = T, tag = F) {
    

# Evalua colnames
stopifnot(require(tidyverse))
    
print_tag <- function(){
      
cat("*-----------------------------------------------------------", sep = '\n')        
cat(paste0("*  Station Name: ", id_name), sep = '\n')
cat(paste0("*  DSSAT Weather file - by https://github.com/jrodriguez88"), sep = '\n') 
cat(paste0("*  Longitude: ", lon, " -- Latitude: ", lat, " -- Elevation: ", elev, "m"), sep = '\n')
cat("*-----------------------------------------------------------", sep = '\n') 
cat(paste0("*  Date: ", min(wth_data$date), " : ", max(wth_data$date)), sep = '\n')
cat("*", sep = '\n') 
cat("*  Column    Daily Value
*     1      Station number
*     2      Year
*     3      Day
*     4      irradiance         KJ m-2 d-1
*     5      min temperature            oC
*     6      max temperature            oC
*     7      vapor pressure            kPa
*     8      mean wind speed         m s-1
*     9      precipitation          mm d-1
*-----------------------------------------------------------", sep = '\n')
        
}


# Data base
    data_to <- tidy_wth_oryza(wth_data) %>%
            mutate(stn = stn,
                   year = year(date),
                   day = yday(date),
                   srad = if_else(is.na(srad), median(wth_data$srad, na.rm = T), round(srad*1000, 2)),
                   tmax = if_else(is.na(tmax), -99, round(tmax, 2)),
                   tmin = if_else(is.na(tmin), -99, round(tmin, 2)),
                   rain = if_else(is.na(rain), -99, round(rain, 2)),
                   VP  = if_else(is.na(VP), -99, round(VP, 2)), 
                   wspd = if_else(is.na(wspd), -99, round(wspd, 2))) %>%
            select(stn, year, day, srad, tmin, tmax, VP, wspd, rain)
    
    
    #    dir.create(paste0(path,"/WTH"), showWarnings = FALSE)
    set_head <- paste(lon, lat, elev, 0, 0, sep = ",")
    
    if(isTRUE(multiyear)){
    #DATA=read.table(file, head=T)
        fname <- paste0(path, "/" , id, stn, ".cli")
        sink(file = fname, append = F)
        if(isTRUE(tag)) print_tag()
        cat(set_head)
        cat("\n")
        write.table(data_to , sep= ",", row.names = F, col.names = F)
        sink()
    } else { 
        data_list <- split(data_to, data_to$year)
        walk(data_list, function(x){
            fname <- paste(path,"/", id, stn,".", str_sub(unique(x$year), 2), sep = "")
            sink(file=fname)
            if(isTRUE(tag)) print_tag()
            cat(set_head)
            cat("\n")
            write.table(x ,sep=",",row.names=F,col.names=F)
            sink()})
        
        
        }

}

#ideal data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date))
#data %>% 
#    write_wth_oryza("R_package/write_files/", "TEST", .,  3.5, -75, 250, multiyear = F, tag = T)

# minimum data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date)) %>% select(date, tmax, tmin, rain)
#data %>% 
#    mutate(
#        srad = srad_cal(., lat =  3.5, kRs = 0.16)) %>%
#    write_wth_oryza("R_package/write_files/", "TEST", .,  3.5, -75, 250)

