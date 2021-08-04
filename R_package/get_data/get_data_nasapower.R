# Script to download weather data from NASA-POWER
# Source of data: NASA Prediction Of Worldwide Energy Resources https://power.larc.nasa.gov/
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/ciat_tools
# 2019

## Load Packages
#library(tidyverse)
#library(curl)
#library(lubridate)
#library(jsonlite)
#library(naniar)
#library(tictoc)

##Set arguments. Information about wth_vars and data access : https://power.larc.nasa.gov/docs/v1/
#wth_vars <- c("PRECTOT" , 
#            "ALLSKY_SFC_SW_DWN", 
#            "RH2M",
#            "T2M_MAX",
#            "T2M_MIN",
#            "WS2M")
#ini_date <- ymd("1983-01-01")
#end_date <- ymd("2019-12-31")
#lat <- 6.8
#lon <- -58.1



## `get_data_nasapower()`function: Download weather data from https://power.larc.nasa.gov/
get_data_nasapower <- function(lat, lon, ini_date, end_date,
                               wth_vars = c("PRECTOT", "ALLSKY_SFC_SW_DWN", "RH2M", "T2M_MAX", "T2M_MIN", "WS2M")){
    
    ini_date <- format(ini_date, "%Y%m%d")
    end_date <- format(end_date, "%Y%m%d")
    
    link <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?request=execute&identifier=SinglePoint&parameters=",
           paste0(wth_vars, collapse = ","),"&startDate=", ini_date, "&endDate=", end_date,
           "&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",
           lat,"&lon=", lon, "&user=anonymous")

    json_data <- fromJSON(link)
    
    data <- json_data$features$properties$parameter 

map(data, ~gather(., "date", "value")) %>% 
    bind_rows(.id = "var") %>%
    spread(var, value) %>%
    mutate(date = ymd(date))

}



from_nasa_to_model <- function(df){
    
#    stopifnot(require(naniar))
    
    df %>% 
        replace_with_na_all(condition = ~.x == -99) %>%
        set_names(c("date", "srad", "rain", "rhum", "tmax", "tmin", "wspd")) %>%
        mutate(year = year(date), month = month(date), day = day(date)) %>% nest(data = -c(year, month)) %>%
        mutate(data = map(data, ~.x %>% impute_mean_at(.vars = vars(srad:wspd)))) %>% 
    unnest(data) %>% impute_mean_at(.vars = vars(srad:wspd))
    
}

#basic_qc_nasa <- function(df){
#    #t$ideam[[1]]
#    #df <-t$nasa_power[[1]]
#    #  df <- nasa_ideam_chirps$ideam[[1]]
#    
#    # med <- df %>% group_by(month = lubridate::month(date)) %>%
#    #    summarise_at(vars(tmax:rhum), .funs = median, na.rm=T)
#    
#    df %>% 
#        mutate(tmax = case_when(tmax>48|tmax<5 ~ median(df$tmax, na.rm = T),
#                                tmax<tmin ~ median(df$tmax, na.rm = T),
#                                tmax == tmin ~ median(df$tmax, na.rm = T), 
#                                TRUE ~ tmax),
#               tmin = case_when(tmin>40|tmin<=5 ~ median(df$tmin, na.rm = T),
#                                tmin > tmax ~ median(df$tmin, na.rm = T),
#                                tmin == tmax ~ median(df$tmin, na.rm = T), 
#                                TRUE ~ tmin),
#               rain = if_else(rain>200|rain<0, 0, rain),
#               srad = if_else(srad>32|srad<0,  median(df$srad, na.rm = T), srad),
#               rhum = if_else(rhum>100|rhum<15,median(df$rhum, na.rm = T), rhum),
#               wspd = if_else(wspd>10|wspd<0,median(df$wspd, na.rm = T), wspd)) 
#    
#}

# Usage and time-test
#tic()
#data <- get_data_nasapower(lat, lon, ini_date, end_date)
#toc()
# data %>% from_nasa_to_model() %>%  basic_qc_nasa()
#
### Plot data example
#data %>% replace_with_na_all(condition = ~.x == -999) %>% ggplot(aes(date, PRECTOT)) + geom_line()
#    
### Replace "NA" id and export to .csv
#data %>% replace_with_na_all(condition = ~.x == -999) %>% write_csv("data.csv")
#

