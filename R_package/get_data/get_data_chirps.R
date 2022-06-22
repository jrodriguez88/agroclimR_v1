# Script to download rainfall data from CHIRPS
# Source of data: CHIRPS -
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2021



### Req
library(tidyverse)
library(lubridate)
library(raster)
library(parallel)
library(doParallel)
library(foreach)


ini_date <- ymd("1983-01-01")
end_date <- ymd("2019-12-31")
lat <- 6.8
lon <- -58.1
path_chirps <-"D:/2021/test_chirps/"


## function to download data by dates 
https://data.chc.ucsb.edu/products/CHIRP/daily/1981/chirp.1981.01.01.tif.gz


download_data_chirps <- function(ini_date, end_date, year_to, path_chirps, n_cores){
  
  fechas <- seq(as.Date(ini.date), as.Date(end.date), "days") %>% str_replace_all("-", ".")
  #urls <- paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="")
  urls <- paste("https://data.chc.ucsb.edu/products/CHIRPS/daily/",year_to,"/chirps.",fechas,".tif",sep="")
  file <- basename(urls)
  path_Chirp_all <- paste0(path_Chirp,"/",file)
  
  if(.Platform$OS.type == "unix") {cl <- makeCluster(no_cores, type = "FORK")}
  cl <- makeCluster(no_cores)
  clusterMap(cl, download.file, url = urls, destfile = path_Chirp_all, mode = "wb", 
             .scheduling = 'dynamic')
  
  stopCluster(cl)
  return(paste("CHIRPS files in", path))}


### If data is downloaded ----->
files <- list.files(path, full.names = T, pattern = ".tif$")


extract_chirps <- function(files, lat, lon) {
  
  chirp_data  <- files[1:2] %>%
    raster::stack() %>% 
    raster::extract(., data.frame(x= lon, y= lat)) %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "file") %>%
    tibble() %>%
    mutate(date = lubridate::ymd(str_sub(file, -10,-1))) %>%
    set_names("file", "rain", "date") %>% 
    dplyr::select(date, rain)
  
  return(chirp_data)
  
}
















