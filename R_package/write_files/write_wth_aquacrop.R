#### Aquacrop-R make_weather
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
#elev <- 657
#co2_file <- "MaunaLoa.CO2" # *.CO2 files are available in Aquacrop default DB
#wth_data <- read.csv("data/weather_to_aquacrop.csv") %>% 
#    mutate(date = ymd(date))

## write_wth_aquacrop function compute weather information to ORYZA weather file.
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

ETo_cal <- function(wth_data, lat, elev, ref_ht = 2, kRs = 0.175, ws_mean = 2){
    
    stopifnot(require(sirad))
    
    varnames <- colnames(wth_data)
    
    if (all(c("rhum", "tmax", "tmin", "srad", "wspd") %in% varnames)) {
        ### Cal ETo
        
        message("Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation")

            ## Estimate clear sky transmissivity
            extraT <- extrat(lubridate::yday(wth_data$date), radians(lat))$ExtraTerrestrialSolarRadiationDaily
            
            ## cal trasmisivity 3% days
            #    tal <- cst(RefRad = wth_data$srad, days = wth_data$date, extraT = extraT, lat = radians(lat), perce = 5)
            
            ETo <- wth_data %>%
                mutate(
                    es = sirad::es(tmax, tmin), 
                    ea = es*rhum/100,
                    #          ea = if_else(is.na(ea), 0.611*exp(17.27*tmin/(tmin+237.3)), ea),
                    #            vp = es-ea,
                    extraT = extraT,
                    tmean = (tmax+tmin)/2, 
                    ETo = sirad::et0(tmax, tmin, ea, srad, 0.85, elev, wspd, ref_ht, extraT),
                    ETo = case_when(is.na(ETo) ~ 0.0023*(tmean + 17.8)*((tmax - tmin)^0.5)*extraT/3,
                                    TRUE ~ ETo)
                ) %>%
                pull(ETo)
            
        } else if (all(c("rhum", "tmax", "tmin", "srad") %in% varnames)) {
        ### Cal ETo
        
        if (!"wspd" %in% varnames) {
            wth_data <- mutate(wth_data, wspd = ws_mean)
            message(paste0("Wind Speed = ", ws_mean, "m/s was used"))
        }
        
        message(paste0("Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation  +  Assumption: Wind Speed mean = ", ws_mean, "m/s"))

            ## Estimate clear sky transmissivity
            extraT <- extrat(lubridate::yday(wth_data$date), radians(lat))$ExtraTerrestrialSolarRadiationDaily
            
            ## cal trasmisivity 3% days
            #    tal <- cst(RefRad = wth_data$srad, days = wth_data$date, extraT = extraT, lat = radians(lat), perce = 5)
            
            ETo <- wth_data %>%
                mutate(
                    es = sirad::es(tmax, tmin), 
                    ea = es*rhum/100,
                    #          ea = if_else(is.na(ea), 0.611*exp(17.27*tmin/(tmin+237.3)), ea),
                    #            vp = es-ea,
                    extraT = extraT,
                    tmean = (tmax+tmin)/2, 
                    ETo = sirad::et0(tmax, tmin, ea, srad, 0.85, elev, wspd, ref_ht, extraT),
                    ETo = case_when(is.na(ETo) ~ 0.0023*(tmean + 17.8)*((tmax - tmin)^0.5)*extraT/3,
                                    TRUE ~ ETo)
                ) %>%
                pull(ETo)
            
        } else if (all(c("tmax", "tmin") %in% varnames)) {
        
        message("Reference evapotranspiration (ETo) Method: Hargreaves equation")
            
            if (!"wspd" %in% varnames) {
                wth_data <- mutate(wth_data, wspd = ws_mean)
                #                message("Wind Speed = 2 m/s was used")
            } 
            
            ## Estimate clear sky transmissivity
            extraT <- extrat(lubridate::yday(wth_data$date), radians(lat))$ExtraTerrestrialSolarRadiationDaily
            
            ETo <- wth_data %>% 
                mutate(extraT = extraT,
                       ea = 0.611*exp(17.27*tmin/(tmin+237.3)),
                       tmean = (tmax+tmin)/2, 
                       srad = kRs*sqrt(tmax - tmin)*extraT, # Coeficient, coastal
                       ETo = sirad::et0(tmax, tmin, ea, srad, 0.85, elev, wspd, ref_ht, extraT),
                       ETo = case_when(is.na(ETo) ~ 0.0023*(tmean + 17.8)*((tmax - tmin)^0.5)*extraT/2.5,
                                       TRUE ~ ETo)) %>% 
                pull(ETo)
            
        } else {
        
        message("No data to calculate ETo!.")
        
    }
    
    
return(ETo)
    
    
    
}
    
    
tidy_wth_aquacrop <- function(wth_data){
    
    var_names <- colnames(wth_data)
    stopifnot(class(wth_data$date)=="Date" & all(c("tmax", "tmin", "rain") %in%  var_names))
    
    impute_mean_wth(wth_data)

}


write_wth_aquacrop <-function(path, id_name, wth_data, lat, lon, elev, co2_file = "MaunaLoa.CO2") {
    

    data <- tidy_wth_aquacrop(wth_data) 
    
    ## Split data and write .ETo / .PLU / Tnx / .CLI files.
    
    # Climate file .CLI
    write_CLI <- function(id_name){
        sink(file = paste0(path, id_name, ".CLI"), append = F)   
        cat(paste(id_name, "Station, lat:", lat, "long:", lon, "- by https://github.com/jrodriguez88"), sep = "\n")
        cat("6.0   : AquaCrop Version (March 2017)", sep = "\n")
        cat(paste0(id_name, ".Tnx"), sep = "\n")
        cat(paste0(id_name, ".ETo"), sep = "\n")
        cat(paste0(id_name, ".PLU"), sep = "\n")
        cat(paste(co2_file), sep = "\n")
        
        sink()    
        
    }
    write_CLI(id_name)
    
    # Temperature file .Tnx
    write_Tnx <- function(id_name){
        sink(file = paste0(path , id_name, ".Tnx"), append = F)   
        cat(paste0(id_name, " : daily temperature data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Tmin (C)   TMax (C)", sep = "\n"))
        cat("=======================", sep = "\n")
        write.table(data.frame(tmin = sprintf("%10.1f", data$tmin),
                               tmax = sprintf("%10.1f", data$tmax)), 
                    row.names = F, quote = F, col.names = F)
        
        sink()
        
    }
    write_Tnx(id_name)
    
    write_PLU <- function(id_name){
        sink(file = paste0(path, id_name, ".PLU"), append = F)
        cat(paste0(id_name, " : daily rainfall data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Total Rain (mm)", sep = "\n"))
        cat("=======================", sep = "\n")
        writeLines(sprintf("%10.1f", data$rain))
        sink()
        
    }
    write_PLU(id_name)
    
    write_ETo <- function(id_name){
        sink(file = paste0(path, id_name, ".ETo"), append = F)
        cat(paste0(id_name, " : daily ETo data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Average ETo (mm/day)", sep = "\n"))
        cat("=======================", sep = "\n")
        writeLines(sprintf("%10.1f", data$ETo))
        sink()
        
    }
    write_ETo(id_name)
    
    
}



#ideal data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date))

#data %>% mutate(ETo = ETo_cal(., 3.5, 250)) %>%
#    write_wth_aquacrop("R_package/write_files/", "TEST", ., 3.5, -75, 250)
    


# minimum data
#data <- read_csv("data/wth_data.csv") %>% mutate(date  = lubridate::mdy(date)) %>% select(date, tmax, tmin, rain)
#data %>% mutate(ETo = ETo_cal(., 3.5, 250)) %>%
#    write_wth_aquacrop("R_package/write_files/", "TEST", ., 3.5, -75, 250)

 

