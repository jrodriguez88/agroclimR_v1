#### Write_soil_dssat
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2021

# Info about soil parameters
#https://dssat.net/wp-content/uploads/2012/05/Romero-2012-Reanalysis-of-a-global-soil-database-for-crop-and-environmental-modeling.pdf


###Standard table in soil file structure

#*IB00000001  IBSNAT      SIC     210 DEFAULT - DEEP SILTY CLAY
#@SITE        COUNTRY          LAT     LONG SCS FAMILY
# Generic     Generic           -99    -99  Generic
#@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
#   -99  0.11   6.0  0.30  85.0  1.00  1.00 IB001 IB001 IB001
#@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
#     5   -99 0.228 0.385 0.481 1.000   -99  1.30  1.75  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
#    15   -99 0.228 0.385 0.481 1.000   -99  1.30  1.75  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
#    30   -99 0.249 0.406 0.482 0.638   -99  1.30  1.60  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
#    45   -99 0.249 0.406 0.465 0.472   -99  1.35  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 
#    60   -99 0.249 0.406 0.465 0.350   -99  1.35  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 
#    90   -99 0.308 0.456 0.468 0.223   -99  1.35  1.10  50.0  45.0   0.0 0.110   6.5   -99   -99   -99 
#   120   -99 0.207 0.341 0.452 0.122   -99  1.40  0.65  50.0  45.0   0.0 0.060   6.5   -99   -99   -99 
#   150   -99 0.243 0.365 0.455 0.067   -99  1.40  0.30  50.0  45.0   0.0 0.030   6.5   -99   -99   -99 
#   180   -99 0.259 0.361 0.457 0.037   -99  1.40  0.10  50.0  45.0   0.0 0.010   6.5   -99   -99   -99 
#   210   -99 0.259 0.361 0.457 0.020   -99  1.40  0.01  50.0  45.0   0.0 0.000   6.5   -99   -99   -99 


#*UFBG760002  SCS         S        71 Lauderhill Muck
#@SITE        COUNTRY          LAT     LONG SCS FAMILY
# EREC        USA            26.400  80.400 euic, hyperthermic Lithic Haplosaprist
#@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
#    BL  0.09   6.0  0.25  61.0  1.00  1.00 IB001 SA001 IB001
#@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
#    20   Oap 0.150 0.250 0.500 1.000 21.00  0.35 45.00   5.0   5.0   0.0 3.500   6.2   -99 211.0   -99 
#    45   Oa2 0.150 0.250 0.500 0.522 21.00  0.35 46.00   5.0   5.0   0.0 3.500   6.3   -99 210.0   -99 
#    60   Oa3 0.150 0.250 0.500 0.330 21.00  0.35 40.00   5.0   5.0   0.0 3.500   6.6   -99 141.0   -99 
#    71   -99 0.026 0.058 0.402 0.010  0.00  1.53  0.00   0.0   0.0  80.0   -99   8.4   -99   -99   -99 
#@  SLB  SLPX  SLPT  SLPO CACO3  SLAL  SLFE  SLMN  SLBS  SLPA  SLPB  SLKE  SLMG  SLNA  SLSU  SLEC  SLCA
#    20  96.0 210.0 187.0  0.27  2.30  2.30   -99   0.1   -99   -99  0.08  0.13  0.00 13.30   -99  0.27 
#    45  56.0 183.0 155.0  0.12  2.01  2.30   -99   -99   -99   -99  0.03  0.05  0.00 15.30   -99  0.12 
#    60  40.0 150.3 100.0  0.11  1.25  2.30   -99   -99   -99   -99  0.03  0.04  0.00 14.10   -99  0.11 
#    71   0.1   0.1   0.1  0.09  0.09  0.90  0.09   0.1  0.09  0.09  0.09  0.09  0.00 90.09  0.09  0.10 




soil_data_list <- soilgrids_data %>% soilgrids_to_dssat("test", .)



tidy_soil_dssat <- function(soil_data){
  
  #CN: Curve number (dimensionless)
CN <- soil_data[1,] %>% 
    mutate(SSKS = SSKS*10, 
           CN = case_when(SSKS <= 10 ~ 85,
                          SSKS > 10 & SSKS <=50 ~ 80,
                          SSKS > 50 & SSKS <=250 ~ 75,
                          SSKS > 250 ~ 65)) %>% pull(CN)
  
SCOM <- "-99"    # SCOM     Color, moist, Munsell hue  
SALB <- 0.13     # SALB     Albedo, fraction 
SLU1 <- 6.0        # SLU1     Evaporation limit, mm   
SLDR <- 0.6      # SLDR     Drainage rate, fraction day-1
SLRO <- CN       # SLRO     Runoff curve no. (Soil Conservation Service)
SLNF <- 1        # SLNF     Mineralization factor, 0 to 1 scale
SLPF <- 0.95     # SLPF     Photosynthesis factor, 0 to 1 scale 
SMHB <- "-99"    # SMHB     pH in buffer determination method, code
SMPX <- "-99"    # SMPX     Phosphorus determination code 
SMKE <- "-99"    # SMKE     Potassium determination method, code

#SLB  <- 5          #   Depth, base of layer, cm
#SBDM <- 1.37       #   Bulk density, moist, g cm-3                                          
#SCEC <- 15.4       #   Cation exchange capacity, cmol kg-1                                  
#SDUL <- 0.26       #   Upper limit, drained, cm3 cm-3                                       
#SLBS <- 0.1        #   Base saturation, cmol kg-1                                           
#SLCF <- 2.2        #   Coarse fraction (>2 mm), %                                           
#SLCL <- 26.0       #   Clay (<0.002 mm), %                                                  
#SLHB <- 5.3        #   pH in buffer                                                         
#SLHW <- 6.5        #   pH in water                                                          
#SLLL <- 0.125       #   Lower limit, cm3 cm-3                                                
#SLMH <- "A1"       #   Master horizon                                                       
#SLNI <- 4.444       #   Total nitrogen, %                                                    
#SLOC <- 2.83       #   Organic carbon, %                                                    
#SLSI <- 26.0       #   Silt (0.05 to 0.002 mm), %                                           
#SRGF <- 0.988       #   Root growth factor, soil only, 0.0 to 1.0                            
#SSAT <- 0.412       #   Upper limit, saturated, cm3 cm-3                                     
#SSKS <- 7.40       #   Sat. hydraulic conductivity, macropore, cm h-1   
  
  
  var_names <- colnames(soil_data)
  stopifnot(class(wth_data$date)=="Date" & all(c("tmax", "tmin", "rain") %in%  var_names))
  
  format_var <- function(soil_data, par, pat = "%3.1f"){
    
    par <- soil_data[[par]]
    
    if(is.numeric(par)){
      as.character(sprintf(pat, par))
    } else if (is.null(par)){
      "-99"
    } else {
      par
    }
    
  }
  
  
soil_data_col <- c('SLB', 'SLMH', 'SLLL', 'SDUL', 'SSAT', 'SRGF', 'SSKS', 'SBDM', 'SLOC', 'SLCL', 'SLSI', 'SLCF', 'SLNI', 'SLHW', 'SLHB', 'SCEC', 'SADC')
format_data_col <- c("%6.0f", "%5s", "%5.3f", "%5.3f", "%5.3f", "%5.3f", "%5.2f", "%5.2f", "%5.2f", "%5.1f", "%5.1f", "%5.1f", " %5.1f", "%5.1f", "%5.1f", "%5.1f", "%5.1f") 

soil_tb <- map2(soil_data_col, format_data_col, 
                ~format_var(soil_data = soil_data, par = .x, pat = .y)) %>% 
  set_names(soil_data_col) %>% bind_cols()  
  
}

id_name <- "testname"
lat <- 13.9
lon <- -86.0


write_soil_dssat <- function(path, id_name, soil_data, salb = 0.13){
  
  
sink(paste0(path, id_name, '.SOL'), append = F)



cat(paste0("*SOIL DATA : ",  id_name, "DSSAT Soil file - by https://github.com/jrodriguez88/agroclimR"), sep = "\n")
cat("\n")
cat(paste0("*CIAT000001   - ", id_name, " -  USDA Texture Class: ", stc[1]), sep = "\n")
cat(c("@SITE        COUNTRY          LAT     LONG SCS FAMILY"), sep = "\n")
cat(sprintf("%13s %14s %8.3f %8.3f %16s", id_name, "AgroclimR", lat, lon , paste0("USDA Texture: ", paste(soil_data[1:nrow(soil_data),"STC"], collapse = "-"))))
cat("\n")
cat(c('@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE'))
cat("\n")
cat(sprintf("%6s %5.2f %5.1f %5.2f %5.1f %5.2f %5.2f %5s %5s %5s", 
            SCOM, SALB, SLU1, SLDR, SLRO, SLNF, SLPF, SMHB, SMPX, SMKE))
cat(c('@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC'))
cat("\n")
cat(cbind(sprintf("%6s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s",
                  soil_tb$SLB, soil_tb$SLMH, soil_tb$SLLL, soil_tb$SDUL, soil_tb$SSAT, soil_tb$SRGF, soil_tb$SSKS, 
                  soil_tb$SBDM, soil_tb$SLOC, soil_tb$SLCL, soil_tb$SLSI, soil_tb$SLCF, soil_tb$SLNI, soil_tb$SLHW, 
          soil_tb$SLHB, soil_tb$SCEC, soil_tb$SADC)), sep = "\n")
sink()
  
}



soil_data <- soil_data_list$data







                    

  



  



  