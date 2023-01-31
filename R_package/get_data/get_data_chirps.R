# Script to download rainfall data from CHIRPS
# Source of data: CHIRPS -
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2021



### Req
#library(tidyverse)
#library(lubridate)
#library(raster)
#library(parallel)
#library(doParallel)
#library(foreach)
#
#
#ini_date <- ymd("2019-12-28")
#end_date <- ymd("2019-12-31")
#lat <- 6.8
#lon <- -58.1
#path_chirps <-"D:/2021/test_chirps/"
#path <-"D:/2021/test_chirps/"
#source <- "CHIRP"  ## "CHIRPS-2.0"
#res <- "p25" ## "p05"


## function to download data by dates 
#https://data.chc.ucsb.edu/products/CHIRP/daily/1981/chirp.1981.01.01.tif.gz


### CHIRP
### monthly
#https://data.chc.ucsb.edu/products/CHIRP/monthly/CHIRP.1981.01.tif
### daily
#https://data.chc.ucsb.edu/products/CHIRP/daily/2019/chirp.2019.01.01.tif.gz
#
#
### CHIRPS
### monthly
#https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/chirps-v2.0.1988.10.tif.gz
###daily
##p25
#https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p25/1988/chirps-v2.0.1988.10.20.tif.gz
##p05
#https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/1988/chirps-v2.0.1988.10.20.tif.gz



### require a wide range of broadband   // a high speed internet connection.

download_data_chirps <- function(ini_date, end_date, path, source = "CHIRPS-2.0", freq = "daily", res = "p05",  n_cores = 4){
  

  if(all(source == "CHIRP", freq == "daily")){
    
    dates <- seq(as.Date(ini_date), as.Date(end_date), "days") %>% str_replace_all("-", ".")
    year_to <- year(ymd(dates))
    #urls <- paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="")
    urls <- map2_chr(dates, year_to, 
                     ~paste("https://data.chc.ucsb.edu/products/CHIRP/daily/",.y,"/chirp.",.x,".tif.gz",sep=""))
    
  } else if(all(source == "CHIRP", freq == "monthly")){
    
    dates <- seq(as.Date(ini_date), as.Date(end_date), "months") %>% str_replace_all("-", ".") %>% str_sub(1, -4)
    #urls <- paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="")
    urls <- map_chr(dates,
                    ~paste("https://data.chc.ucsb.edu/products/CHIRP/monthly/CHIRP.",.x,".tif.gz",sep=""))
    
  } else if(all(source == "CHIRPS-2.0", freq == "daily")){
    
    dates <- seq(as.Date(ini_date), as.Date(end_date), "days") %>% str_replace_all("-", ".") 
    year_to <- str_sub(dates, 1, 4)
    urls <- map2_chr(dates, year_to, 
                     ~paste("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/",res, "/", .y,"/chirps-v2.0.",.x,".tif.gz", sep=""))
    
  } else if(all(source == "CHIRPS-2.0", freq == "monthly")){
    
    dates <- seq(as.Date(ini_date), as.Date(end_date), "months") %>% str_replace_all("-", ".") %>% str_sub(1, -4)
    urls <- map_chr(dates,
                    ~paste("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/chirps-v2.0.",.x,".tif.gz",sep=""))
    
  } 
  
  
  
  
  file <- basename(urls)
  path_Chirp_all <- paste0(path,"/",file)
  
  if(.Platform$OS.type == "unix") {cl <- makeCluster(n_cores, type = "FORK")
  } else {cl <- makeCluster(n_cores) }
  
  clusterMap(cl, download.file, url = urls, destfile = path_Chirp_all, mode = "wb", 
             .scheduling = 'dynamic')
  
  
  clusterMap(cl, R.utils::gunzip, path_Chirp_all, remove = T)
  
  stopCluster(cl)




return(str_remove(path_Chirp_all, ".gz$"))

}

#download_data_chirps(ini_date, end_date, path, source = "CHIRPS-2.0", freq = "monthly", n_cores = 4)


### If data is downloaded ----->
#map(list.files(path, full.names = T), R.utils::gunzip, remove = F)
#files <- list.files(path, full.names = T, pattern = ".tif$")


  
extract_chirps <- function(files, lat, lon, freq = "daily") {
    
#    chirp_data  <- files[1:2] %>%
      chirp_data  <- files %>%
        raster::stack() %>% 
        raster::extract(., data.frame(x= lon, y= lat)) %>% 
        t() %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column(var = "file") %>%
        tibble() 
      
      
      if(freq == "daily"){
        
        chirp_data <- chirp_data %>%
          mutate(date = lubridate::ymd(str_sub(file, -10,-1))) %>%
          set_names("file", "rain", "date") %>% 
          dplyr::select(date, rain)
        
        
        
      } else if (freq == "monthly"){
        
        
        chirp_data <- chirp_data %>%
          mutate(date = lubridate::ym(str_sub(file, -7,-1))) %>%
          set_names("file", "rain", "date") %>% 
          dplyr::select(date, rain)}
      
      
      
      return(chirp_data)
      
  }
  
  
  #safe_extract_chirps <- possibly(extract_chirps, NULL)
  
  
  #extract_chirps(files, lat, lon, "daily")
  
  
  


















