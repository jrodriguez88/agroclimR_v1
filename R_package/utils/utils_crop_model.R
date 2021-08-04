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
srad_cal <- function(data, lat, lon, alt, A = 0.29, B = 0.45, kRs = 0.175){
    
    stopifnot(require(sirad))
    
    if(!"sbri" %in% colnames(data))
    {
        data <- mutate(data, sbri = NA_real_)   #Aqui crea la variable brillo por si no existe
    }
    
    step1 <- data %>% 
        mutate(
            extraT = extrat(lubridate::yday(date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
            srad = ap(date, lat = lat, lon = lon,    # aqui aplica Angstrom-Prescott
                      extraT, A, B, sbri),
            srad = if_else(is.na(srad), kRs*sqrt(tmax - tmin)*extraT, srad))  # Aqui aplica Hargreaves
    
    max_srad <- mean(step1$extraT)*0.80     # calcula el maximo teorico de radiacion
    
    step2 <- step1 %>%  
        mutate(
            srad = if_else(srad>max_srad|srad<0|is.na(srad),  median(step1$srad, na.rm = T), srad)) %>%
        dplyr::pull(srad)
    
    
    return(step2)   # retorna la radiacion en MJ/m2*dia
    
    
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
# Must had observated and simulated data in columns"obs" and "sim"
get_metrics <- function(data) {
    
    data %>% filter(complete.cases(.)) %>%
        summarise(n = n(),
                  r = cor(obs, sim, method = c("pearson")),
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





