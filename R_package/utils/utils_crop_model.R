#### Group of recursive functions used in ORYZA_AUTO_PARAM and ORYZA_Model_RTOOLS
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/ciat_tools
# 2020



### Download *.EXE  ----> drates.exe, param.exe, oryzav3.exe, standard.crp 
download_ORYZA_Tools <- function(folder = "."){
    ip <- function() {
        if (.Platform$OS.type == "windows") {
            ipmessage <- system("ipconfig", intern = TRUE)
        } else {
            ipmessage <- system("ifconfig", intern = TRUE)
        }
        validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
        any(grep(validIP, ipmessage))
    }  
    
    if (all(c("ORYZA3.exe", "DRATE(v2).exe", "PARAM(v2).exe", "standard.crp") %in% list.files(folder))){
            
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
    
}

### Install R Packages and dependeces.
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


### function to calculate solar radiation from sunshine hours or temperature data
# data must have "date" and 'sbri' (or 'tmax' and 'tmin') variable,
## A & B parameters from FAO, 
#Frère M, Popov GF. 1979. Agrometeorological crop monitoring and forecasting. Plant
#Production Protection Paper 17. Rome: Food and Agricultural Organization.
#64 p.
# kRs adjustment coefficient (0.16.. 0.19) -- for interior (kRs = 0.16) and coastal (kRs = 0.19) regions
srad_cal <- function(data, lat, sh_name =  "sbri", A = 0.29, B = 0.45, kRs = 0.175, fill = F){
    
    stopifnot(require(sirad))
  

  if (sh_name %in% colnames(data)){
    
    data <- data %>% 
      mutate(
        extraT = extrat(lubridate::yday(date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
        srad = ap(date, lat = lat, lon = lon,    # aqui aplica Angstrom-Prescott
                  extraT, A, B, sbri))
    
    
    
    message("Method for estimate Solar Radiation: Angstrom-Prescott (A-P) Model")
    
  } else if (all(c("tmax", "tmin") %in%  colnames(data))){
    
    data <- mutate(data, sbri = NA_real_)   #Aqui crea la variable brillo por si no existe
    message("Method for estimate Solar Radiation: Hargreaves Model")
    
    data <- data %>% 
      mutate(
        extraT = extrat(lubridate::yday(date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
        srad = kRs*sqrt(tmax - tmin)*extraT) ## kRs adjustment coefficient (0.16.. 0.19) -- for interior (kRs = 0.16) and coastal (kRs = 0.19) regions
    
  } else {
      
      message("No data to calculate Solar Radiation!.")
      
  }
  
  
  
  
  
  if (isTRUE(fill)) {
    
    max_srad <- mean(data$extraT)*0.80     # calcula el maximo teorico de radiacion
    
    data <- data %>% 
      mutate(
        srad = if_else(is.na(srad), kRs*sqrt(tmax - tmin)*extraT, srad),
        srad = if_else(srad>max_srad|srad<0|is.na(srad),  median(step1$srad, na.rm = T), srad))
        
    
  }
  

    return(pull(data, srad))   # retorna la radiacion en MJ/m2*dia
    
    
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
get_metrics <- function(data) {
    
    data %>% filter(complete.cases(.)) %>%
        summarise(n = n(),
                  r = cor(obs, sim, method = c("pearson")),
                  k = cor(obs, sim, method = c("kendall")),
                  RMSE = sqrt(mean((sim - obs)^2, na.rm = T)),
                  NRMSE = RMSE/mean(obs, na.rm = T),
                  MAE = sum(abs(sim - obs)/n),
                  MBE = sum((sim - obs))/n,
                  d = 1 - ((sum((sim - obs)^2, na.rm = T))/
                               sum((abs(sim - mean(obs, na.rm = T)) +
                                        abs(obs - mean(obs, na.rm = T)))^2, na.rm = T)),
                  NSE = 1 - ((sum((sim - obs)^2, na.rm = T))/
                                 sum((obs - mean(obs, na.rm = T))^2, na.rm = T)),
                  rsq = summary(lm(sim ~ obs))$r.squared)
    
}

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



#Function to impute missing values by monthly mean
impute_mean_wth <- function(wth_data){
  
  stopifnot(require(naniar))
  
  wth_data %>% 
    mutate(year = year(date), month = month(date)) %>% 
    nest(data = -c(year, month)) %>%
    mutate(data = map(data, ~.x %>% 
                        impute_mean_at(.vars = vars(-date)))) %>% 
    unnest(data) %>% dplyr::select(-c(year, month))
  
  
}

daily_to_monthly <- function(wth_data, ...){

  
  var_names <- colnames(wth_data)
  
#  stopifnot(require(lubridate))
  stopifnot(class(wth_data$date)=="Date")
  
#  wth_vars <- var_names[var_names != "date"]

  
  wth_data %>% 
    group_by(year = year(date), month = month(date)) %>%
    summarise(
      across(matches("rain|prec"), sum, ...), 
      across(!matches("rain|prec"), mean, ...), .groups = 'drop') %>%
    dplyr::select(-c(date))
      
      
}


#daily_to_monthly(a, na.rm = T)


## Bias correction for tmax, tmin  and Ratio correction for rain
#https://core.ac.uk/download/pdf/42720291.pdf
#var = c("tmax", "tmin", "rain")

remote_data_correction <- function(obs_data, target_data, var = c("tmax", "tmin")) {
  
  # varsW  = colnames(obs_data)
  
  obs <-  daily_to_monthly(obs_data, na.rm = T) %>% group_by(month) %>% 
    summarise(across(!matches("year|month"), mean, na.rm = T))
  
  sim <- daily_to_monthly(target_data, na.rm = T) %>% 
    dplyr::select(year, month, everything()) %>% group_by(month) %>% 
    summarise(across(!matches("year|month"), mean, na.rm = T))
  
  
  monthly_diff <- left_join(obs, sim, by = "month") %>% 
    mutate(tmax = tmax.x - tmax.y,
           tmin = tmin.x - tmin.y,
           rain = case_when("rain" %in% var ~ rain.x/rain.y,
                            TRUE ~ 1)) %>%
    dplyr::select(month, tmax, tmin, rain) %>%
    pivot_longer(cols = -c(month), names_to = "var", values_to = "corr")
  
  
  target_data %>% 
    mutate(month = month(date)) %>% 
    pivot_longer(cols = -c(date, month), names_to = "var") %>%
    left_join(monthly_diff, by = c("month", "var")) %>%
    mutate(value = case_when(var == "tmax" | var == "tmin" ~ value + corr,
                             var == "rain" ~ value*corr, 
                             TRUE ~ value)) %>%
    dplyr::select(-c(month, corr)) %>%
    pivot_wider(names_from = var)
  
  
}
#remote_data_correction(d, a)



cal_metrics_wth <- function(obs_data, sim_data, time = "monthly"){
  
  if(time == "monthly"){
    
    wth_data_test <- left_join(
      daily_to_monthly(obs_data), 
      daily_to_monthly(sim_data), 
      by = c("year", "month")) %>%
      pivot_longer(-c(year, month), names_to = "var")
    
  } else if(time == "daily") {
    
    wth_data_test <- left_join(
      obs_data, 
      sim_data, 
      by = c("date")) %>%
      pivot_longer(-c(date), names_to = "var")
    
  }
  
  
  wth_data_test %>%
    mutate(source = case_when(str_detect(var, "[.x]$") ~ "obs",
                              str_detect(var, "[.y]$") ~ "sim")) %>%
    drop_na() %>%
    mutate(
      var = str_sub(var, 1, -3)) %>%
    pivot_wider(names_from = source) %>% nest(-var) %>%
    mutate(metrics = map(data, ~get_metrics(.x))) %>%
    unnest(metrics)
  
  
  
}


plot_na  <- function(id, data) {
  
  
  
  vis_miss(dplyr::select(data, -date), warn_large_data = F) +
    #  facet_wrap(id ~.)
    labs(title = id) +
    #       subtitle = paste0(city, " (", dpto, ") "),
    #       caption = "Source: IDEAM") +
    scale_y_continuous(breaks = seq(0, length(data$date), by = 365*5), 
                       labels = cut.Date(data$date, breaks = "5 years") %>% 
                         unique() %>% year()) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 0))
  
  
}


###Join Data base and impute with NASA - CHIRPS corrected
join_data <- function(obs_data, fill_data, iyear = 1990, fyear = 2019){
  
  date_control <- seq.Date(make_date(iyear), make_date(fyear, 12, 31), by = "days") %>%
    enframe(value = "date", name = NULL) 
  
  list_data <- list(date_control, obs_data, fill_data) %>%  
    reduce(left_join, by="date") 
  
  
  
  list_data %>%
    mutate(tmin = if_else(is.na(tmin.x), tmin.y, tmin.x),
           tmax = if_else(is.na(tmax.x), tmax.y, tmax.x),
           rain = if_else(is.na(rain.x), rain.y, rain.x)) %>%
    dplyr::select(date, rain, tmax, tmin, srad, wspd, rhum) %>%
    impute_mean_wth()
  
  
  
}




