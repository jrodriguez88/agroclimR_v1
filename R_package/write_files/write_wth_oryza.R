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
#     7      vapor pressure            kPa                                                        
#     8      mean wind speed         m s-1                                                        
#     9      precipitation          mm d-1


## Make_WTH_ORYZA function compute weather information to ORYZA weather file.
## 'data':  csv file name or data.frame. 
#           str-> 
#                ..$ DATE: Date ->(mdy)
#                ..$ TMAX: num  ->(oC)
#                ..$ TMIN: num  ->(oC) 
#                ..$ RAIN: num  ->(mm) 
#                ..$ SRAD: num  ->(MJ) 
#                ..$ RHUM: num  ->(%) 
## 'path':  path folder or working directory
## 'local': 4 letters string of locality name. "AIHU"--> Aipe, Huila 
## 'lat':   latitud (decimal degrees)
## 'lon':   longitud (decimal degrees)
## 'alt':   altitude (meters above sea level)


#map(data_list, copy_wth)
#data <- "weather_input.csv"
#path <- getwd() 
#local<- "AIHU"
#lat <- 3.5
#lon <- -75.5
#alt <- 250

#library(tidyverse)
#list.files(pattern=".RData") %>% lapply(load, .GlobalEnv)
#locs_id <- ls()

write_wth_oryza <- function(data, path, id, lat, lon, alt, stn=1) {
    
    stopifnot(require(sirad))
    
    if("rhum" %in% colnames(data))
    {
        data <- mutate(data, 
                       es = sirad::es(tmax, tmin), 
                       vpd = es*rhum/100) %>% select(-es)  #cal vpd
    } else {data <- mutate(data, vpd = NA_real_)}
    
    if (!"wspd" %in% colnames(data)) {
        data <- mutate(data, wspd = NA_real_)
        message("Wind Speed is not Available - Set as -99")
    }
    
    
    
        data_to <- data %>%
            mutate(stn = stn,
                   year = year(date),
                   day = yday(date),
                   srad = if_else(is.na(srad), median(data$srad, na.rm = T), round(srad*1000, 2)),
                   tmax = if_else(is.na(tmax), -99, round(tmax, 2)),
                   tmin = if_else(is.na(tmin), -99, round(tmin, 2)),
                   rain = if_else(is.na(rain), -99, round(rain, 2)),
                   vpd  = if_else(is.na(vpd), -99, round(vpd, 2)), 
                   wspd = if_else(is.na(wspd), -99, round(wspd, 2))) %>%
            select(stn, year, day, srad, tmin, tmax, vpd, wspd, rain)
        

    
    dir.create(paste0(path,"/WTH"), showWarnings = FALSE)
    set_head <- paste(lon, lat, alt, 0, 0, sep = ",")    
    #DATA=read.table(file, head=T)  
    data_list <- split(data_to, data_to$year)
    lapply(data_list, function(x){
        fname <- paste(path,"/WTH/" , id, stn,".", str_sub(unique(x$year), 2), sep = "")
        sink(file=fname)
        cat(set_head)
        cat("\n")
        write.table(x ,sep=",",row.names=F,col.names=F)
        sink()})
    
}

#write_wth_oryza(data, "write_files/", "TEST", 3.5, -75, 250)


