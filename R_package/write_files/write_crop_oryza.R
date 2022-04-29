#########################################################
####          Make CRP Crop File (*.crp)             ####
####     By https://github.com/jrodriguez88          ####
####      ORYZA Rice Crop Model described in:        #### 
####   http://books.irri.org/9712201716_content.pdf  ####
#########################################################


#stand_crp <- readLines("standard.crp")
#writeLines(stand_crp)

#cat(stand_crp)

### Function to join oryza params into list

# 'data' is a tibble 

write_crop_oryza <- function(cultivar, data, SWISLA='TABLE') {
  
  crp_tb <- function(tb, sig=5) {
    tb1 <-  cbind(
      paste0(sprintf("%.2f", tb[[1]]), ","),
      paste0(sprintf(paste0("%.",sig,"f"), tb[[2]]), 
             c(rep(",", nrow(tb)-1), "")))
    
    write.table(tb1 ,row.names = F, col.names = F, quote = F)
  }
  
  
  sink(file=paste0(path,'/', cultivar, ".crp"), append = F)
  
  # crp_tb function to write CRP-tables in ORYZA model format    
  
  
  cat("**********************************************************************", sep = '\n')         
  cat("* Crop data file for ORYZA rice growth model *", sep = '\n')         
  cat(paste0("* Variety   : ", cultivar), sep = '\n')      
  cat(paste0("* File name : ", cultivar, ".CRP"), sep = '\n')       
  cat(paste0("* 'Create with https://github.com/jrodriguez88/ORYZA_Model_RTOOLS' *"), sep = '\n') 
  cat("**********************************************************************")
  cat('\n')
  cat('\n')
  cat("*---------------------------------------------------------------------        
* 1. Phenological development parameters                                      
*---------------------------------------------------------------------")
  cat('\n')
  cat("TBD    = 8. ", sep = '\n')          
  cat("TBLV   = 8. ", sep = '\n')        
  cat("TMD    = 42.", sep = '\n')          
  cat("TOD    = 30.", sep = '\n')          
  cat(paste0("DVRJ = ", data$DVRJ), sep = '\n')
  cat(paste0("DVRI = ", data$DVRI), sep = '\n')
  cat(paste0("DVRP = ", data$DVRP), sep = '\n')
  cat(paste0("DVRR = ", data$DVRR), sep = '\n')
  cat("MOPP   = 11.50", sep = '\n')  
  cat("PPSE   = 0.0  ", sep = '\n')   
  cat("SHCKD  = 0.4  ", sep = '\n')  
  cat('\n')
  
  cat("*---------------------------------------------------------------------        
* 2. Leaf and stem growth parameters                                          
*---------------------------------------------------------------------")
  cat('\n')
  cat("RGRLMX = 0.0085", sep = '\n')
  cat("RGRLMN = 0.0040", sep = '\n')
  cat("SHCKL  = 0.25  ", sep = '\n')
  
  cat("SHADET = 0.90", sep = '\n') 
  cat('\n')             
  #"*SWISLA = 'FUNCTION' ! Give function parameters ASLA, BSLA, CSLA, DSLA, SLAMAX"                                                 
  #"SWISLA = 'TABLE'    ! Give SLA as a function of DVS in the table SLATB"
  cat("SWISLA = 'TABLE'", sep = '\n')
  
  #* SLA = ASLA + BSLA*EXP(CSLA*(DVS-DSLA)), and SLAMAX"                          
  cat("ASLA = 0.0024                                 
BSLA = 0.0025                                 
CSLA = -4.5                                   
DSLA = 0.14                                   
SLAMAX = 0.0045")
  
  cat('\n')
  cat('\n')
  cat("* If SWISLA='TABLE', supply table of specific leaf area (ha kg-1; Y value) *
SLATB = ", sep = '\n')
  crp_tb(crp_params$SLATB[[1]])
  cat('\n')
  #
  #        0.00, 0.0045,"                                                         
  #        0.16, 0.0045,"                                                         
  #        0.33, 0.0033,"                                                         
  #        0.65, 0.0028,"                                                         
  #        0.79, 0.0024,"                                                         
  #        2.10, 0.0023,"                                                         
  #        2.50, 0.0023"                                                          
  
  cat("* Table of specific green stem area (ha kg-1; Y value) as a function of       
* development stage (-; X value):                                             
SSGATB = 0.0, 0.0003,                                                      
         0.9, 0.0003,                                                         
         2.1, 0.0000,                                                         
         2.5, 0.0000")                                                          
  cat('\n')                                                                            
  cat("
*---------------------------------------------------------------------
* 3. Photosynthesis parameters
*---------------------------------------------------------------------
FRPAR  = 0.5  ! Fraction of sunlight energy that is 
! photosynthetically active (-)
SCP    = 0.2  ! Scattering coefficient of leaves for PAR (-)
CO2REF = 340. ! Reference level of atmospheric CO2 (ppm)
CO2    = 385. ! Ambient CO2 concentration (ppm)

* Table of light extinction coefficient for leaves (-; Y-value) as a function
* of development stage (-; X value):
KDFTB = 0.00, 0.4,
0.65, 0.4,
1.00, 0.6,
2.50, 0.6

* Table of extinction coefficient of N profile in the canopy (-; Y-value) 
* as a function of development stage (-; X value):
KNFTB = 0.0,  0.4,
0.65, 0.4,
1.00, 0.4,
2.5,  0.4 


* Table of light use efficiency (-; Y-value) as a function of 
* temperature (oC; X value):
EFFTB  =   0., 0.54,
10., 0.54,
25., 0.54,
40., 0.36,
60., 0.24

* Table of effect of temperature on AMAX (-; Y-value) as a function of 
* temperature (oC; X value):
REDFTT = -10., 0.,
10., 0.,
20., 1.,
37., 1.,
43., 0.,
60., 0.

* Table of N fraction in leaves on leaf area basis (g N m-2 leaf; Y-value) 
* as a function of development stage (-; X value):
NFLVTB = 0.00, 0.54,
0.16, 0.54,
0.33, 1.53,
0.65, 1.22,
0.79, 1.56,
1.00, 1.29,
1.46, 1.37,
2.02, 0.83,
2.50, 0.83

AMAXSLN0 = 22.0 !* Leaf nitrogen content corresponding 
!* coefficient to AMax (g N/m-2 leaf))

MINSLN = 0.2    !* * Minimum leaf nitrogen content for that 
!* Am (maximum rate CO2 assimilation) is 0.0 (g N/m2 leaf)
")
  
  cat('\n') 
  cat("*---------------------------------------------------------------------        
* 4. Maintenance parameters                                                   
*---------------------------------------------------------------------        
* Maintenance respiration coefficient (kg CH2O kg-1 DM d-1) of:               
MAINLV = 0.02  
MAINST = 0.015
MAINSO = 0.003 
MAINRT = 0.01 
TREF   = 25.   
Q10    = 2.")
  cat('\n')
  
  cat("
*---------------------------------------------------------------------
* 5. Growth respiration parameters 
*---------------------------------------------------------------------
* Carbohydrate requirement for dry matter production (kg CH2O kg-1 DM leaf)of:
CRGLV  = 1.326  ! Leaves 
CRGST  = 1.326  ! Stems
CRGSO  = 1.462  ! Storage organs (panicles)
CRGRT  = 1.326  ! Roots
CRGSTR = 1.11   ! Stem reserves

LRSTR  = 0.947  ! Fraction of allocated stem reserves that is
                ! available for growth (-)")
  cat('\n')
  cat("*---------------------------------------------------------------------        
* 6. Growth parameters                                                        
*---------------------------------------------------------------------")
  cat('\n')
  cat(paste0("FSTR  = ", round(data$FSTR, 3)))
  cat('\n')
  
  cat("TCLSTR = 10.       ! Time coefficient for loss of stem reserves (1 d-1)", sep='\n')
  cat(paste0("SPGF   = ", sprintf("%.1f", data$SPGF )))
  cat('\n')
  cat(paste0("WGRMX  = ", sprintf("%.7f", data$WGRMX)), sep = '\n')                                                                              
  cat('\n')
  cat("**************** Partitioning tables ****************", sep = '\n')                                                         
  cat("* Table of fraction total dry matter partitioned to the shoot (-; Y-value)    
* as a function of development stage (-; X value):                            
FSHTB  = ")  
  cat('\n')
  crp_tb(crp_params$FSHTB[[1]])
  cat('\n')                                                                           
  cat("* Table of fraction shoot dry matter partitioned to the leaves *                            
FLVTB  = ")
  cat('\n')
  crp_tb(crp_params$FLVTB[[1]])
  cat('\n')                                                                           
  cat("* Table of fraction shoot dry matter partitioned to the stems *
FSTTB  = ", sep = '\n')
  crp_tb(crp_params$FSTTB[[1]])
  cat('\n')                                                                            
  cat("* Table of fraction shoot dry matter partitioned to the panicles *
FSOTB  = ")
  cat('\n')
  crp_tb(crp_params$FSOTB[[1]])
  cat('\n') 
  cat('\n')
  cat("* Table of leaf death coefficient *
DRLVT  = ")
  cat('\n')
  crp_tb(crp_params$DRLVT[[1]])
  
  
  cat('\n')                                                        
  cat("*---------------------------------------------------------------------        
* 7. Carbon balance parameters                                                
*---------------------------------------------------------------------        
* Mass fraction carbon (kg C kg-1 DM) in the:                                 
FCLV   = 0.419 
FCST   = 0.431 
FCSO   = 0.487 
FCRT   = 0.431 
FCSTR  = 0.444") 
  cat('\n')
  cat('\n')
  cat("*---------------------------------------------------------------------        
* 8. Root parameters                                                          
*---------------------------------------------------------------------")
  cat('\n')  
  cat(paste0("GZRT   = ", data$GZRT), sep='\n')         
  cat(paste0("ZRTMCW = ", data$GZRT), sep='\n')        
  cat(paste0("ZRTMCD = ", data$GZRT), sep='\n')         
  cat('\n')                                                                             
  cat("*ADDITIONAL INFORMATION since JUNE 2009                                       
*SROOTL = 90.0 
RMINT  = 5.0   
ROPTT  = 25.0  
RTBS   = 2.0   
RCNL   = 0.012 
               
SODT   = 0.9")   
  cat('\n')
  cat('\n')
  cat("*---------------------------------------------------------------------        
* 9. Temperature and drought stress parameters                                
*---------------------------------------------------------------------")
  cat('\n')  
  cat("COLDMIN = 12.    ! Lower air temperature threshold for growth (oC)            
COLDEAD = 3.     ! Consecutive number of days below COLDMIN that crop dies (-)")
  cat('\n') 
  cat(paste0("COLDREP = ",  sprintf("%.1f", data$COLDREP)))
  cat('\n') 
  cat(paste0("CTSTER = ", sprintf("%.1f", data$CTSTER)))
  cat('\n')
  cat('\n') 
  cat("* Upper and lower limits for drought stress effects                           
ULLS =   74.13    ! Upper limit leaf rolling (kPa)                            
LLLS =  794.33    ! Lower limit leaf rolling (kPa)                            
ULDL =  630.95    ! Upper limit death of leaves (kPa)                         
LLDL = 1584.89    ! Lower limit death of leaves (kPa)")
  cat('\n') 
  cat(paste0("ULLE = ", sprintf("%.2f",  data$ULLE)))    
  cat('\n') 
  cat(paste0("LLLE = ", sprintf("%.2f",  data$LLLE)))                         
  cat('\n') 
  cat('\n')
  cat(
    "* Switch to use ULTR and LLTR as given above or function built in ORYZA 
* for the reduction in relative transpiration:
* SWIRTR = 1, Use ULRT AND LLRT for transpiration; SWIRTR = 2, Use SWIRTRF 
* for transpiration; SWIRTR = 3, Use FSWTD for transpiration 
SWIRTR = 3 ! Use function 

* Give value of ULRT and LLRT for SWIRTR = 1
ULRT =   74.13    ! Upper limit relative transpiration reduction (kPa)
LLRT = 1584.89    ! Lower limit relative transpiration reduction (kPa)

* Give value for SWIRTRF if SWIRTR= 2, default value SWIRTRF=0.003297
SWIRTRF = 0.020597

* Give value for FSWTD if SWIRTR= 3, default value SWIRTRF=0.003297
* The upper limit factor while transpiration declines which is the ratio of 
* remaining available water to total water supply capability")
  cat('\n') 
  cat(paste0("FSWTD = ", data$FSWTD))
  cat('\n') 
  cat("
*---------------------------------------------------------------------
* 10. Nitrogen parameters
*---------------------------------------------------------------------
NMAXUP  = 8.      ! Maximum daily N uptake (kg N ha-1 d-1)
RFNLV   = 0.004   ! Residual N fraction of leaves (kg N kg-1 leaves)
FNTRT   = 0.15    ! Fraction N translocation from roots, as (additonal) 
! Fraction of total N translocation from stems and leaves 
! (-)
RFNST   = 0.0015  ! Residual N fraction of stems (kg N kg-1 stems)
TCNTRF  = 10.     ! Time coefficient for N translocation to grains (d)
NFLVI   = 0.5     ! Initial leaf N fraction (on area basis: g N m-2 leaf)
FNLVI   = 0.025   ! Initial leaf N fraction (on weight basis: kg N kg-1 leaf)
NDSENS = 0.95     !* Nitrogen deficiency sensitivity, 0.5=fair as default, 
!* >0.5 tolerance, <0.5 sensitive, Value range 0.0 to 1.0
NMAXSO  = 0.0175  ! Maximum N concentration in storage organs (kg N kg-1)
    
* Table of minimum N concentration in storage organs (kg N kg-1 DM; Y value) 
* as a function of the amount of N in the crop till flowering (kg N ha-1; X 
* value):
NMINSOT =  0., .006,
50., .0008,
150., .0125,
250., .015,
400., .017,
1000., .017
    
* Table of maximum leaf N fraction on weight basis (kg N kg-1 leaves; Y value)
* as a function of development stage (-; X value):
NMAXLT = 0.0, .053,
0.4, .053,
0.75, .040,
1.0, .028,
2.0, .022,
2.5, .015
    
* Table of minimum leaf N fraction on weight basis (kg N kg-1 leaves; Y value)
* as a function of development stage (-; X value):
NMINLT = 0.0, 0.025,
1.0, 0.012,
2.1, 0.007,
2.5, 0.007
    
*--- Table of effect of N stress on leaf death rate (-; Y value)
* as a function of N stress level (-; X value):
NSLLVT = 0., 1.0,
1.1, 1.0,
1.5, 1.4,
2.0, 1.5,
2.5, 1.5")
  
  sink()
  
}