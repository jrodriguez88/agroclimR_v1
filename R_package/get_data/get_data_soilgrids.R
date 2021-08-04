# Script to download soil data from SOILGRIDS
# Source of data: https://www.soilgrids.org/  
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/ciat_tools
# 2019

## Load Packages
#library(tidyverse)
#library(jsonlite)
#library(soiltexture)
#library(tictoc)


#Set arguments. REST SoilGrids API : https://rest.soilgrids.org/    https://www.isric.org/explore/soilgrids/faq-soilgrids 
#path <- getwd()
#soil_vars <- c("BLDFIE","CLYPPT","SNDPPT","CRFVOL","ORCDRC","WWP","AWCh1","AWCtS")
#depths <- c("sl1", "sl2", "sl3", "sl4", "sl5")
#lat <- 6.8
#lon <- -58.1



#### 'get_data_soilgrids' function for download soilgriddata
##get_data_soilgrids_v1 <- function(soil_vars, lat, lon, depths){
#    
#    link <- paste0("https://rest.soilgrids.org/query?lon=", lon, "&lat=", lat, "&attributes=",
#                   paste0(soil_vars, collapse = ","),"&depths=", paste0(depths, collapse = ","))
#    
#    json_data <- fromJSON(link)
#    
#    data <- map(.x = soil_vars, ~json_data$properties[[.x]]$M) %>% set_names(soil_vars)
#    
#    dept_value <- json_data$properties$depthCodesMeters %>% 
#        bind_cols %>% dplyr::select(contains("sl")) %>% 
#        gather(sl, depth)
#    
#    data %>% bind_rows(.id = "soil_vars") 
#        
#}
#

#Default value : List [ "bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc" ]
#soil_vars <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc")
#Default value : List [ "Q0.05", "Q0.5", "Q0.95", "mean", "uncertainty" ]
#value = c("Q0.5", "mean") 
#Default value : List [ "0-5cm", "0-30cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm" ]
#depths = c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm") 

### 'get_data_soilgrids' function for download soilgriddata V2 https://www.isric.org/explore/soilgrids/faq-soilgrids 
get_data_soilgrids <- function(lat, lon, soil_vars = c("bdod", "cfvo", "clay", "nitrogen", "sand", "silt", "soc"),
                               value = c("mean"), depths = c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")){
    
    var_to_dl <- paste0("&property=", soil_vars, collapse = "")
    depts_to_dl <- paste0("&depth=", depths, collapse = "")
    values_to_ext <- paste0("&value=", value, collapse = "")
    
    
    link <- paste0("https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lon=", 
                   lon, "&lat=", lat, 
                   var_to_dl, 
                   depts_to_dl, 
                   values_to_ext)
    
    json_data <- fromJSON(link)
    
    
 vars <- json_data$properties$layers$name
 
 units <- json_data$properties$layers$unit_measure
 
 mdata <- enframe(vars, value = "var", name = NULL) %>% 
     bind_cols(units) %>% select(-uncertainty_unit)
 
 data <- json_data$properties$layers$depths %>% 
     set_names(vars) %>% 
     bind_rows(.id = "var") %>% nest(data = c(range, label, values)) %>% 
     right_join(mdata, by = "var")
 
 
 return(data)
}

#soilgrids_data <- get_data_soilgrids(lat, lon)

#soilgrids_data %>% unnest(data) %>% select(var, mapped_units) %>% distinct()


from_soilgrids_to_aquacrop <- function(id_name, soilgrids_data, Penetrability = 100) {
    
    source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/utils/soil_PTF.R", encoding = "UTF-8")

    
    ## transform data to aquacrop format
    data_inp <- soilgrids_data %>% unnest(data) %>% 
        select(var, range, label, values) %>% flatten() %>%
        #    set_names(c("var", "tdepth","bdepth", "unit", "label", "value")) %>%
        pivot_wider(names_from = var, values_from = values.mean) %>% 
        mutate_at(.vars = vars(bdod, cfvo, clay, sand, silt), ~.x/10) %>%
        mutate(Penetrability = Penetrability,
               TKL = c(0.05, diff(abs(range.bottom_depth/100))),
               bdod = bdod/10,
               OM = (100/58)*soc/100,
               WCFC = WCFC_Saxton(sand, clay, OM),
               WCST = WCST_Saxton(bdod),
               WCWP = WCWP_Saxton(sand, clay, OM),
               SSKS = SSKS_Suleiman_Ritchie(sand, clay, OM, bdod)*24,   #Method developed by Suleiman and Ritchie (2001)
               STC = get_STC(sand, clay),
               CRa = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-0.3112 - SSKS*10^(-5)),
                               str_detect(STC, "Lo|SiLo|Si") ~ (-0.4986 + SSKS*9*10^(-5)),
                               str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-0.5677 - SSKS*4*10^(-5)),
                               str_detect(STC, "SiClLo|SiCl|Cl") ~ (-0.6366 + SSKS*8*10^(-4))),
               CRb = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-1.4936 + 0.2416*log(SSKS)),
                               str_detect(STC, "Lo|SiLo|Si") ~ (-2.1320 + 0.4778*log(SSKS)),
                               str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-3.7189 + 0.5922*log(SSKS)),
                               str_detect(STC, "SiClLo|SiCl|Cl") ~ (-1.9165 + 0.7063*log(SSKS)))) %>%
        rename(Gravel = cfvo) %>% 
        dplyr::select(TKL, WCST, WCFC, WCWP, SSKS, Penetrability, Gravel, CRa, CRb, STC) %>%
        setNames(c("Thickness", "Sat", "FC", "WP", "Ksat", "Penetrability", "Gravel", "CRa", "CRb", "description"))
    
    #CN: Curve number (dimensionless)
    CN <- data_inp[1,] %>% 
        mutate(CN = case_when(Ksat <= 10 ~ 85,
                              Ksat > 10 & Ksat <=50 ~ 80,
                              Ksat > 50 & Ksat <=250 ~ 75,
                              Ksat > 250 ~ 65)) %>% pull(CN)
    
    
    # REW: Readily Evaporable Water (mm)
    REW <- data_inp[1,] %>%
        mutate(REW_cal = (10*(FC - WP/2)*0.04),
               REW = case_when(REW_cal >=15 ~ 15, 
                               REW_cal < 0 ~ 0,
                               TRUE ~ REW_cal)) %>% pull(REW) %>% sprintf("%1.f", .)
    
    
    return(list(id_name = id_name, data = data_inp[-1,], CN = CN, REW = REW))
    
}
