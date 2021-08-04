#### soil_PTF - R Functions of Soil Pedotransfer Funtions (PTF) 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/csmt
# 2020


## Inputs example
## 2020 - update - 
#C <- 15.7  # Clay (%)
#S <- 28.3  # Clay (%)
#OM <- 1.14 # Soil Organic Matter (%)
#SPOR <- 48 # Total Soil porosity (%)
#SBDM <- 1.53  # Soil Bulk Density (g/cm3)
#DEPTH <- 20   # (cms)
#GWCFC <- 25.5 #(%)

## Outputs example
# Water content PTF =  v/v (%)
# Soil Saturated Hydraulic Conductivity = SSKS = (mm/h)


#############################################################################################
##### SAXTON & RAWLS: SOIL WATER CHARACTERISTICS ESTIMATES
##### http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.452.9733&rep=rep1&type=pdf

#  Saxton, K. E., & Rawls, W. J. (2006). Soil Water Characteristic Estimates by Texture and 
#  Organic Matter for Hydrologic Solutions. Soil Science Society of America Journal,
#  70(5), 1569–1578. https://doi.org/10.2136/sssaj2005.0117

#############################################################################################

# 1500 kPa moisture, %v
WCWP_Saxton <- function(S, C, OM) {
  WCWP_i <- -0.024*S/100 + 0.487*C/100 + 0.006*OM +
    0.005*(S*OM/100) - 0.013*(C*OM/100) +
    0.068 *(S/100)*(C/100) + 0.031
  
  WCWP <- WCWP_i + (0.14*WCWP_i - 0.02)
  
  return(WCWP*100)             
} #Eq 1
#WCWP_Saxton(S, C,OM)

# 33 kPa moisture, normal density, %v
WCFC_Saxton <- function(S,C,OM) {
  WCFC_i <- -0.251*S/100 + 0.195*C/100 + 0.011*OM +
    0.006*(S*OM/100) - 0.027*(C*OM/100) +
    0.452*(S/100)*(C/100) + 0.299
  
  WCFC <- WCFC_i + (1.283*(WCFC_i^2) - (0.374*WCFC_i) - 0.015)
  
  return(WCFC*100)
  
}   #Eq 2
#WCFC_Saxton(S, C,OM)

# SAT-33 kPa moisture, normal density %v
WCSAT_Saxton <- function(S,C,OM) {
  
  WCST_i <- 0.278*(S/100) + 0.034*(C/100) + 0.022*OM -
    0.018*(S/100)*OM - 0.027*(C/100)*OM +
    0.584 *(S/100)*(C/100) + 0.078
  
  WCSAT <- WCST_i + (0.6360*WCST_i) - 0.107
  
  return(WCSAT*100)
  
}  #Eq 3
#WCSAT_Saxton(S,C,OM)

# Tension at air entry (bubbling pressure), kPa
TAE_BP_Saxton <- function(S,C,OM) {
  WCSAT <- WCSAT_Saxton(S,C,OM)
  TAE_BPi <- -2.27 -(27.93*C/100) -(81.97*WCSAT) + (71.12*S*WCSAT/100) + (8.29*C*WCSAT/100) + (14.05*(S/100)*(C/100)) + 27.16
  
  TAE_BP <- TAE_BPi + (0.02*(TAE_BPi^2) -0.113*TAE_BPi -0.70)
  
  return(TAE_BP)
  
} #Eq 4
#TAE_BP_Saxton(S,C,OM)

# Saturated moisture (0 kPa), normal density, %v
WCST_Saxton1 <- function(S,C,OM) {
  
  WCFC <- WCFC_Saxton(S,C,OM)    
  WCSAT <- WCSAT_Saxton(S,C,OM)
  WCST <- WCFC + WCSAT -0.097*(S/100) + 0.043
  
  return(WCST)
  
}   #Eq 5
#WCST_Saxton1(S,C,OM)

# pN Normal density, g/cm³
pN_Saxton <- function(S,C,OM) {
  WCST <- WCST_Saxton(S,C,OM)
  pN <- (1 - WCST)*2.65
  return(pN)
}
#pN_Saxton(S,C,OM)
#pDF <- pN_cal(S,C,OM) #DF Compactation soil factor (0.9-1.3)
#pB <- pN

# Saturated moisture (0 kPa), adjusted density, %v
WCST_Saxton <- function(SBDM){
  
  WCST_S <- 1-(SBDM/2.65)
  return(WCST_S*100)
}    #Eq 8
#WCST_Saxton(SBDM)

# Saturated conductivity (matric soil), mm/h
SSKS_Saxton <- function(S, C, OM, SBDM, WCFC=NULL, WCWP=NULL, WCST=NULL){
  if(is.null(WCFC)){
    #        message("Water Content PTF used")
    WCFC <- WCFC_Saxton(S,C,OM)}
  if(is.null(WCWP)){
    #        message("Water Content PTF used")
    WCWP <- WCWP_Saxton(S,C,OM)}
  if(is.null(WCST)){
    #        message("Water Content PTF used")
    WCST <- WCST_Saxton(SBDM)}
  
  B <- (log(1500) - log(33))/(log(WCFC/100) - log(WCWP/100))  #Eq 15
  alp <- 1/B                #Eq 18
  
  SSKS <- 1930*(WCST/100 - WCFC/100)^(3-alp)   # Eq 16
  
  return(SSKS)
  
}
#SSKS_Saxton(S,C,OM, SBDM)
#SSKS_Saxton(S,C,OM, SBDM, WCFC, WCWP)
#SSKS_Saxton(S,C,OM, SBDM, WCFC)
#SSKS_Saxton(WCFC = 22, WCWP = 10, WCST = 55)


########################################################
########### Predicting Soil properties in the tropics
########### Budiman Minasny, Alfred E. Hartemink. (2011)

#############################################################################################
##### Predicting Soil properties in the tropics
##### Budiman Minasny, Alfred E. Hartemink. (2011)

#  Minasny, B., & Hartemink, A. E. (2011). Predicting soil properties in the tropics. 
#  Earth-Science Reviews, 106(1-2), 52-62. https://doi.org/10.1016/j.earscirev.2011.01.005

#############################################################################################

#Soil bulk density with OM effect
SDB_Minasny <- function(DEPTH, S, OM, BD_OM=0.224){
  SBDmin <- 0.93 + (0.049*log(DEPTH)) + (0.005*S) + 0.000065*((S-38.96)^2)
  SBD_M <- 100/((OM/BD_OM)+((100-OM)/SBDmin))
  return(SBD_M)
} #Eq. 1 & 2

# Saturated water content at (-10kPa), may consider WCST
WCST_Minasny <- function(S, SBDM){
  WCST <- 59.9 - (8.78*SBDM) - (0.31*S)
  return(WCST)
}  #Eq. 3
#WCST_Minasny(S, SBDM)

#Saturated water content at (-33kPa)
WCFC_Minasny <- function(S, SBDM){
  WCFC <- 56.5 - (7.49*SBDM) -(0.34*S)
  return(WCFC)
}  #Eq. 4
#WCFC_Minasny(S, SBDM)

# Saturated water content at (-1500kPa)
WCWP_Minasny <- function(C, OM){
  WCWP <- 7.95 + (0.86*OM) + (0.4*C) - 0.004*((C-37.7)^2)
  return(WCWP)
}    #Eq. 5
#WCWP_Minasny(C, OM)



#############################################################################################
##### PEDOTRANSFER FUNCTIONS FOR TROPICAL SOILS Cap21
##### J. Tomasella, and M. Hodnett (2004)

#  Tomasella, J., & Hodnett, M. (2004). Pedotransfer functions for tropical soils. 
#  Developments in Soil Science, 30, 415-429. https://doi.org/10.1016/S0166-2481(04)30021-8

#############################################################################################

# Alpha van Genuchten parameter
Alpha_Tomasella <- function(S, C, SBDM, CS, FS, GWCFC) {
  Si <- 100-C-S
  x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
  x2 <- -1.17468 + 0.0808098*FS  #FS Fine sand
  x3 <- -1.05976 + 0.0650437*Si
  x4 <- -2.10641 + 0.0427715*C
  x5 <- -2.21391 + 8.9226800*GWCFC
  
  x6 <- -6.03516 + 4.81197*SMBD
  z1 <-  4.25417*x1 + 2.72322*x2 + 3.07242*x3 + 5.00093*x4 - 0.195062*x5 - 0.377081*x6
  z2 <- 0.110144 + 0.640373*z1 - 1.16884*(z1)^2 - 0.155394*x4 - 0.358591*z1*x4 - 1.00996*x4*(z1)^2 + 0.126617*(x4)^3
  
  alpha_T <-  10^(0.0736768 + 0.789068*z2)
  
  return(alpha_T)
}

# N van Genuchten parameter
N_Tomasella <-  function(S, C, SBDM, CS, FS, GWCFC) {
  Si <- 100-C-S
  x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
  x2 <- -1.17468 + 0.0808098*FS  #FS Fine sand
  x3 <- -1.05976 + 0.0650437*Si
  x4 <- -2.10641 + 0.0427715*C
  x5 <- -2.21391 + 8.9226800*GWCFC
  x6 <- -6.03516 + 4.81197*SMBD
  
  z3 <- 0.37398*x1 - 0.0940338*(x1)^3 + 0.838535*x1*x5 - 0.590525*x5*(x1)^2 + 0.76113*(x5)^2 -
    0.789465*x1*(x5)^2 - 0.273647*(x5)^3 - 0.512764*x6 + 0.455363*x1*x6 - 0.38428*x6*(x1)^2 + 
    0.731809*x5*x6 - 1.00484*x1*x5*x6 - 0.172341*x6*(x5)^2 + 0.219746*(x6)^2 - 0.367679*x1*(x6)^2 + 0.131251*(x6)^3
  
  z4 <- -0.360294 + 0.76878*z3 + 0.0770122*(z3)^3 - 0.193142*x2 - 0.121583*z3*x2 + 0.0889415*x2*(z3)^2 +
    0.284168*(x2)^2 - 0.0674767*(x2)^3 - 0.202897*x3 - 0.341951*z3*x3 - 0.270616*x2*x3 + 0.0880845*x3*(x2)^2 +
    0.24982*(x3)^2 + 0.102658*x2*(x3)^2 - 0.0801841*(x3)^3
  
  n_vg <- 10^(0.140543 + 0.0797516*z4)
  
  return(n_vg) 
}

# van Genuchten parameter
WC0_Tomasella <- function(S, C, CS, SBDM) {
  Si <- 100-C-S
  x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
  x3 <- -1.05976 + 0.0650437*Si
  x6 <- -6.03516 + 4.81197*SMBD
  
  z5 <- 0.164417 + 0.126139*(x1)^2 + 0.281797*x3 + 0.484823*x1*x3 - 0.293866*(x3)^2 - 0.354924*x1*(x3)^2 -
    0.705803*x6 - 0.189153*x3*x6 - 0.267997*x1*x3*x6 - 0.023954*x6*(x3)^2 - 0.0918816*x1*(x6)^2 + 0.0323997*(x6)^3
  
  WC_0 <- 0.515224 + 0.100899*z5
  
  return(WC_0)
}

# Residual water content 
WCR_Tomasella <- function(S, C, GWCFC){ 
  Si <- 100-C-S
  x3 <- -1.05976 + 0.0650437*Si
  x5 <- -2.21391 + 8.9226800*GWCFC/100
  
  z6 <- 0.12867 - 0.492412*x3 + 0.787425*x5 - 0.235254*x3*x5
  
  WC_r <- 0.161487 + 0.101111*z6
  
  return(WC_r*100)
}
#WCR_Tomasella(S,C, GWCFC)

# Saturated water content at (0kPa), may consider WCST
WCST0_Tomasella <- function(C, S, SBDM){
  x7 <- -1.0553 + 0.0533922*C*S
  x8 <- -1.07131 + 0.0649731*S
  x9 <- -6.18145 + 4.95385*SBDM
  z7 <- 0.159379 + 0.137397*(x7^2) + 0.265398*x8 + 0.519965*x7*x8 - 0.276027*(x8^2) - 0.362393*x7*(x8^2) - 0.702969*x9 - 0.222252*x8*x9 - 0.244634*x7*x8*x9 - 0.092267*x7*(x9^2) + 0.0332669*(x9^3)
  
  WCST_T1 <- 0.517589 + 0.0994301*z7
  return(WCST_T1*100)
}  
#WCST_Tomasella(C, S, SBDM)

# Saturated water content at (-10kPa), may consider WCST
WCST_Tomasella <- function(S, C, SBDM, GWCFC) {
  Si <- 100-C-S
  x14 <- -1.05501 + 0.0650857*Si
  x15 <- -2.07588 + 0.0423954*C
  x16 <- -6.03402 + 4.80572*SBDM
  x17 <- -2.18409 + 8.84963*GWCFC/100
  z9 <- 0.175202 + 1.818513*x17 - 0.0996042*(x17^2) + 0.327915*x16 - 0.0758657*(x16^2)
  z10 <- 0.929344*z9 + 0.132519*x14
  
  WC10_T1 <- 0.339255 + 0.112526*z10
  
  return(WC10_T1*100)
}
#WC10_Tomasella(S,C,SBDM, GWCFC)

#Saturated water content at (-33kPa)    
WCFC_Tomasella <- function(SBDM, GWCFC) {
  x16 <- -6.03402 + 4.80572*SBDM
  x17 <- -2.18409 + 8.84963*GWCFC/100
  z11 <- 0.191452 + 1.25652*x17 - 0.079098*(x17^2) + 0.393814*x16 + 0.152095*x17*x16
  
  WCFC_T1 <- 0.28951 + 0.103815*z11
  
  return(WCFC_T1*100)
}
#WCFC_Tomasella(SBDM, GWCFC)

# Saturated water content at (-1500kPa)    
WCWP_Tomasella <- function(C, SBDM, GWCFC){
  
  x15 <- -2.07588 + 0.0423954*C
  x16 <- -6.03402 + 4.80572*SBDM
  x17 <- -2.18409 + 8.84963*GWCFC/100
  z13 <- 0.235084 + 0.33033*x15 - 0.191838*(x15^2) + 0.0543679*(x15^3) + 0.977685*x17 + 0.304174*x15*x17 - 0.218857*(x17^2) - 0.164373*x15*(x17^2) + 0.0415057*(x17^3) + 0.373361*x16 + 0.0811861*x17*x16 - 0.0768087*x15*x17*x16
  
  WCWP_T1 <- 0.214008 + 0.0862945*z13
  return(WCWP_T1*100)
}
#WCWP_Tomasella(C, SBDM, GWCFC)


#############################################################################################
##### Saturated hydraulic conductivity Functions (SSKS)
##### Main authors: Pachepsky(2004), Ghanbarian(2017), Ferrer(2004) .. Many others

#  Pachepsky, Y., & Rawls, W. J. (Eds.). (2004). Development of pedotransfer functions in soil hydrology 
#  (Vol. 30). Elsevier.https://www.elsevier.com/books/development-of-pedotransfer-functions-in-soil-hydrology/pachepsky/978-0-444-51705-0

#  Ghanbarian, B., Taslimitehrani, V., & Pachepsky, Y. A. (2017). Accuracy of sample dimension-dependent pedotransfer functions in 
#  estimation of soil saturated hydraulic conductivity. Catena, 149, 374-380. https://doi.org/10.1016/j.catena.2016.10.015 

#  Ferrer Julià, M., T.E. Monreal, A.S. del Corral Jiménez, and E. García Meléndez. 2004. Constructing a saturated hydraulic conductivity
#  map of Spain using pedotransfer functions and spatial prediction. Geoderma 123:257–277. doi:10.1016/j.geoderma.2004.02.011


#############################################################################################


# Wösten et al. (1999)
#Topsoil is a parameter that is set to 1 for topsoils and to 0 for subsoils,
SSKS_Wosten99 <- function(S,C,OM,SBDM, tops=0) {
  Si=100-C-S
  x <- 7.755 + 0.0352*Si + 0.93*tops - 0.967*(SBDM^2)- 0.000484*(C^2) - 0.000322*(Si^2) + 
    0.001/Si - 0.0748/OM - 0.643*log(Si) - 0.01398*SBDM*OM - 0.1673*SBDM*OM + 
    0.02986*tops*C - 0.03305*tops*Si
  
  SSKS_W <- 1.15741*(10^-7)*exp(x)*3600000  #conversion to mm/h
  return(SSKS_W)
}
#SSKS_Wosten99(S,C,OM,SBDM, tops=0)

# Vereecken et al. (1990)
SSKS_Vereecken <- function(S, C, OM, SBDM) {
  x <- 20.62- 0.96*log(C)- 0.66*log(S) - 0.46*log(OM) - 8.43*SBDM
  SSKS_Ve <- exp(x)*10/24
  
  return(SSKS_Ve)
  
}
#SSKS_Vereecken(S, C, OM, SBDM)  ##verificar


#Ferrer-Julia et al. (2004)
SSKS_Ferrer <- function(S) {
  SSKS_Fer <-  3600000* 2.556*(10^-7)*exp(0.0491*S)
  
  return(SSKS_Fer)
}
#SSKS_Ferrer(S)


# Brakensiek et al. (1984) SPOR = %
SSKS_Brakensiek <- function(S,C, SBDM, SPOR=NULL) {
  #SPOR = as fraction    
  if(is.null(SPOR)){
    #        message("Porosity was estimated using 2.65g/cm3 as particle density")
    SPOR <- (1-(SBDM/2.65))*100
  }
  SSKS_Br <- 24*exp(19.52348*(SPOR/100) - 8.96847 - 0.028212*C + 0.00018107*(S^2) - 0.0094125*(C^2) - 8.395215*((SPOR/100)^2) + 0.077718*((SPOR/100)*S) - 0.00298*((SPOR/100)^2)*(S^2) -0.019492*((SPOR/100)^2)*(C^2) + 0.0000173*(C*S^2) + 0.02733*((SPOR/100)*C^2) + 0.001434*((SPOR/100)*S^2)- 0.0000035*(S*C^2))
  return(SSKS_Br*10/24)
}
#SSKS_Brakensiek(S, C, SBDM)
#SSKS_Brakensiek(S, C, SPOR = 42)

# Campbell and Shiozawa (1994)  #Error Si by S
SSKS_Campbell <- function(S,C){
  SSKS_Ca <- 129.6*exp(-0.07*S - 0.167*C)
  return(SSKS_Ca*10/24)
}
#SSKS_Campbell(S, C)

# Cosby et al. (1984) 
SSKS_Cosby <- function(S,C){
  SSKS_Co <- 60.96*10^((-0.6+0.01268*S) - (0.0064*C))
  return(SSKS_Co*10/24)
}
#SSKS_Cosby(S,C)

#Jabro (1992)
SSKS_Jabro <- function(S, C, SBDM){
  Si <- 100-C-S
  SSKS_Ja <- exp(9.56 - 0.81*log(Si) - 1.09*log(C) - 4.64*SBDM)
  return(SSKS_Ja*10)
}
#SSKS_Jabro(S,C,SBDM)


# Puckett et al. (1985)
SSKS_Puckett <- function(C){
  SSKS_Pu <- 376.7*exp(-0.1975*C)
  return(SSKS_Pu*10/24)
}
#SSKS_Puckett(C)

# Dane and Puckett (1994)
SSKS_Dane_Puck <- function(C){
  SSKS_DP <- 729.22*exp(-0.144*C)
  return(SSKS_DP*10/24)
}
#SSKS_Dane_Puck(C)

# Saxton et al. (1986)
#24*exp(12.012 - (0.0755*S2))  ## No useful 

# RAWLS (1986)
SSKS_Rawls <- function(S,C){
  SST_Rawls <- 0.332 - (7.251*(10^-4)*S) + 0.1276*log10(C)
  SSKS_R <- 24*exp(12.012 - (7.55*(10^-2)*S) + (-3.8950 + (0.03671*S) - (0.1103*C) + 8.7546*(10^-4)*(C^2))*(1/(SST_Rawls)))
  
  return(SSKS_R*10/24)
}
#SSKS_Rawls(S,C)



#############################################################################################
##### Relative Effective Porosity Model (REPM)
##### Suleiman, A. A., & Ritchie, J. T. (2001).

#  Suleiman, A. A., & Ritchie, J. T. (2001). Estimating saturated hydraulic conductivity from soil porosity. 
#  Transactions of the ASAE, 44(2), 235. https://doi.org/10.13031/2013.4683

#cited by: 
#  Gijsman, A. J., Thornton, P. K., & Hoogenboom, G. (2007). Using the WISE database to parameterize soil inputs for crop simulation models.
#  Computers and Electronics in Agriculture, 56(2), 85-100. https://doi.org/10.1016/j.compag.2007.01.001 

#############################################################################################

SSKS_Suleiman_Ritchie <- function(S, C, OM, SBDM, SPOR=NULL, WCFC=NULL){
  #    stopifnot(SPOR)
  if(is.null(WCFC)){
    message("WCFC was estimated using Saxton-PTF")
    WCFC <- WCFC_Saxton(S,C,OM)}
  
  if(is.null(SPOR)){
    message("Porosity was estimated using 2.65g/cm3 as particle density")
    SPOR <- (1-(SBDM/2.65))*100
  }
  
  SSKS <- 75*((SPOR-WCFC)^2/(WCFC)^2)
  
  return(SSKS*10/24)
  
}
#SSKS_Suleiman_Ritchie(S,C,OM, SBDM)
#SSKS_Suleiman_Ritchie(SPOR = 42, WCFC = 24)


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



# Function to estimate Soil Saturated Hydraulic Conductivity (SSKS) from different Soil Pedotransfer Function (PTF)
#SBDM by texture <-  "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/office/ssr10/tr/?cid=nrcs144p2_074844"
# For rice-soils BD values between 1.3 - 1.7 (g/cm³)
# OM values between 1 - 3 (%)
# min and max are SSKS-threshold

SSKS_cal <- function(S, C, SOM=1.5, SBDM=1.5, kmin=1, kmax=NA, output='bootmean') {
  
  #    source("https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/PT_Functions.R")
  
  
  ssks_data <- tibble(S,C,SOM,SBDM) %>%
    mutate(SSKS_Brakensiek = SSKS_Brakensiek(S, C, SBDM),
           SSKS_Campbell = SSKS_Campbell(S, C),
           SSKS_Cosby = SSKS_Cosby(S, C),
           SSKS_Dane_Puckett = SSKS_Dane_Puck(C),
           SSKS_Jabro = SSKS_Jabro(S, C, SBDM),
           SSKS_Puckett = SSKS_Puckett(C),
           SSKS_Rawls = SSKS_Rawls(S, C),
           SSKS_Saxton = SSKS_Saxton(S, C, SOM, SBDM), 
           SSKS_Suleiman_Ritchie = SSKS_Suleiman_Ritchie(S, C, SOM, SBDM), 
           SSKS_Wosten = SSKS_Wosten99(S,C, SOM, SBDM),
           SSKS_Vereecken = SSKS_Vereecken(S, C, SOM, SBDM),
           SSKS_Ferrer = SSKS_Ferrer(S)) %>%
    select(contains("SSKS")) %>%
    gather(key="KS_PTF", value = "SSKS") %>%
    extract(KS_PTF, "KS_PTF", "_([a-zA-Z0-9_]+)") %>%
    mutate(KS_PTF=as.factor(KS_PTF), SSKS=replace(SSKS, SSKS>kmax, NA), SSKS=replace(SSKS, SSKS<kmin, NA))%>%
    drop_na() 
  
  summary <- ssks_data %>%
    summarize(ssks_bootmean = mean(sample(SSKS, 1000, replace = T)),
              ssks_mean = mean(SSKS),
              ssks_bootmedian = median(sample(SSKS, 1000, replace = T)),
              ssks_median = median(SSKS),
              ssks_min = quantile(sample(SSKS, 1000, replace = T),0.025),
              ssks_max = quantile(sample(SSKS, 1000, replace = T),0.975),
              ssks_sd = sd(SSKS))
  
  switch (output,
          data = ssks_data,
          summary = summary,
          bootmean = summary$ssks_bootmean,
          mean = summary$ssks_mean,
          bootmedian = summary$bootmedian,
          median = summary$ssks_median,
          min = summary$ssks_min,
          max = summary$ssks_max,
          sd = summary$ssks_sd)
  
  
}


#SSKS_cal(39.2, 15.15, 1.14, 1.53)

#SSKS_cal(39.2, 15.15, 1.14, 1.53, output = 'data')
#SSKS_cal(39.2, 15.15, 1.14, 1.53, output = 'summary')
#SSKS_cal(39.2, 15.15, 1.14, 1.53, output = 'min')











