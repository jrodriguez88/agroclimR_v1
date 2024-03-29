#### Group of recursive functions used in ORYZA_AUTO_PARAM and ORYZA_Model_RTOOLS
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/ciat_tools
# 2020



### Download *.EXE  ----> drates.exe, param.exe, oryzav3.exe, standard.crp 
download_ORYZA_Tools <- function(path){
  
  # set dir to download
  wd <- getwd()
  setwd(path)
  folder <- "."
  
  ip <- function() {
    if (.Platform$OS.type == "windows") {
      ipmessage <- system("ipconfig", intern = TRUE)
    } else {
      ipmessage <- system("ifconfig", intern = TRUE)
    }
    validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    any(grep(validIP, ipmessage))
  }  
  
  if (all(c("ORYZA3.exe", "DRATE(v2).exe", "PARAM(v2).exe", "standard.crp", "AutoCalibration3.exe") %in% list.files())){
    
    print("All files in destination folder")
    
    
  } else if(ip()==T){
    
    # Download DRATES and PARAM app  
    download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/AllTools.zip',
                  destfile='AllTools.zip', method='auto')
    ls_tools<- unzip('AllTools.zip', list = T)
    unzip('AllTools.zip', exdir = folder, files = ls_tools$Name[c(1,2,4)])
    
    # Download ORYZA.exe
    download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/ORYZA3.zip',
                  destfile='ORYZA3.zip', method='auto')
    unzip('ORYZA3.zip', exdir = folder, files="ORYZA3.exe")
    
    #Download standard.crp
    download.file("https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/standard.crp",
                  destfile = paste(folder, "standard.crp", sep = "/"), method='auto')
    
    file.remove('AllTools.zip')
    file.remove('ORYZA3.zip')
  } else {
    mens <- cat(
      "#####################################################
####       WARNING! NO INTERNET CONECTION        ####
####      It is need copy ORYZA model Tools:     ####
####  ORYZA3.exe & drate(v2).exe & PARAM(v2).exe ####
####        AND CROP FILE standard.crp           ####
#####################################################")
    
    print(mens)
  }
  
  
  setwd(wd)  
  
}

### Install R Packages and dependeces. Add mesage
inpack <- function(pack){
    new_pack <- pack[!(pack %in% installed.packages()[, "Package"])]
    if (length(new_pack)) 
        install.packages(new_pack, dependencies = TRUE)
    sapply(pack, require, character.only = TRUE)
}


### 'read_INPUT_data' function to read xlsx files ---->  c(LOC_ID, cultivar), base_raw_data
read_INPUT_data <- function(file) {
    stopifnot(require(readxl))
    sheets <- readxl::excel_sheets(file)
    x <-    lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
    names(x) <- sheets
    x
}




### 'get_STC' function to get Soil Texture Class from soil sand, clay content.. based USDA system class
get_STC <- function(S, C, sysclass="USDA") {
    stopifnot(require(soiltexture))
    
    Si <- 100-(S+C)
    dat <- data.frame(SAND=S, CLAY=C, SILT=Si)
    
    STC <- TT.points.in.classes(
        tri.data = dat,
        class.sys = paste0(sysclass, ".TT"),
        PiC.type = "t"
    )
    
    return(STC)
    
}


### function to calculate HUH (growing thermal units) _ tbase,    topt,and thigh depends of crop
HUH_cal <- function(tmax, tmin, tbase = 8, topt = 30, thigh = 42.5) {
    
    tav <- (tmin + tmax)/2
    
    h <- 1:24
    
    Td <- tav + (tmax - tmin)*cos(0.2618*(h - 14))/2 
    
    huh <- Td %>% enframe(name = NULL, "td") %>%
        mutate(HUH = case_when(td <= tbase | td >= thigh ~ 0,
                               td > tbase | td <= topt ~ (td - tbase)/24,
                               td > topt | td < thigh ~ (topt-(td - topt)*(topt - tbase)/(thigh - topt))/24))
    
    sum(huh$HUH)   
    
} 


# function for search and replace outliers data
replace_outlier <- function(data, fill = "na"){
    
    fill <- switch(fill,
                   na = NA_real_,
                   median = median(data, na.rm = T),
                   mean = mean(data, na.rm = T))
    
    data[data %in% boxplot.stats(data)$out] = fill
    
    return(data)
    
}

###Bootstraping function 
mean_boot <- function(x){
  
    smean.cl.boot(x, conf.int=.95, B=1000, na.rm=TRUE, reps=T)[1]}

# Function to calculate evaluation metrics || 
# Must have observated and simulated data in columns"obs" and "sim"
#get_metrics <- function(data) {
#    
#    data %>% filter(complete.cases(.)) %>%
#        summarise(n = n(),
#                  r = cor(obs, sim, method = c("pearson")),
#                  tau = cor(obs, sim, method = c("kendall")),
#                  RMSE = sqrt(mean((sim - obs)^2, na.rm = T)),
#                  NRMSE = RMSE/mean(obs, na.rm = T),
#                  MAE = sum(abs(sim - obs)/n),
#                  MBE = sum((sim - obs))/n,
#                  d = 1 - ((sum((sim - obs)^2, na.rm = T))/
#                               sum((abs(sim - mean(obs, na.rm = T)) +
#                                        abs(obs - mean(obs, na.rm = T)))^2, na.rm = T)),
#                  NSE = 1 - ((sum((sim - obs)^2, na.rm = T))/
#                                 sum((obs - mean(obs, na.rm = T))^2, na.rm = T)),
#                  rsq = summary(lm(sim ~ obs))$r.squared)
#    
#}

##ggplot fav theme
set_theme_jre <- function() {
  theme_jre <<- theme_bw() + theme(
    legend.position="bottom",
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))
}


#function to get elevation from # https://www.opentopodata.org/#public-api
# lan & lon in decimal degrees
# dataset c("aster30m", "strm30", "mapzen", ...)

get_elevation <- function(lat, lon, dataset = "aster30m"){
  
  elev_raw <- fromJSON(
    paste0("https://api.opentopodata.org/v1/", dataset, "?locations=", lat, ",", lon)
  )
  
  return(elev_raw$results$elevation)
  
  
}
#get_elevation(3.82, -76.5)




##### DSSAT  --- from DSSAT API jr


# Crea formatos de fechas de simulacion 
make_sim_dates <- function(initial_date, planting_before, number_days, freq_sim){
  
  #  require(tidyverse)
  #  require(lubridate)
  #  require(magrittr)
  
  
  start_date <- seq.Date(initial_date, initial_date + days(number_days), by = freq_sim)
  
  plantig_date <- start_date + days(planting_before)
  
  dates <- list(start_date = start_date, planting_date = plantig_date)
  
  return(dates)
  
  
}

# Funcion para crear carpeta de simulacion en directorio raiz
make_dir_run <- function(dir_run_main, id_run){
  
  
  #  require(stringr)
  dir <- paste0(dir_run_main, id_run, '/')
  dir <- stringr::str_replace(dir, "ñ", "n")
  
  if (!dir.exists(dir)) { 
    
    dir.create(dir, showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir))
}























