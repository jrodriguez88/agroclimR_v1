# Script to create Aquacrop Crop File 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2023



## Load packages

#library(tidyverse)
#library(lubridate)
#library(readxl)

## read data
#crops?? and params -- define by GDD 

## tidy data and quality control

AQUACROP	Phenology	GDD_CCmax	GDD	370	259	481	from emergence to maximum rooting depth
AQUACROP	Phenology	GDD_FL	GDD	1150	805	1495	from emergence to flowering
AQUACROP	Phenology	GDD_FLL	GDD	350	245	455	Length of the flowering stage
AQUACROP	Phenology	GDD_M	GDD	1900	1330	2470	from emergence to maturity
AQUACROP	Leaf and stem growth	CGC	%/d	0.12257	0.085799	0.159341	Canopy Growth Coefficient
AQUACROP	Leaf and stem growth	CC_senecence	GDD	1300	910	1690	GDD from emergence to start senescence
AQUACROP	Leaf and stem growth	CC_max	%	0.95	0.665	1.235	Maximun canopy cover
AQUACROP	Growth AGB-RZ	WP	g/m²	19	13.3	24.7	Crop Water Productivity
AQUACROP	Growth AGB-RZ	HIo	%	43	30.1	55.9	Reference Harvest Index
AQUACROP	Growth AGB-RZ	GDD_HI	GDD	680	476	884	Building-up of Harvest Index during yield formation
AQUACROP	Growth AGB-RZ	GDC	%/GDD	0.0933	0.06531	0.12129	Canopy Decline Coefficient
AQUACROP	Growth AGB-RZ	Zr	m	0.5	0.35	0.65	Maximum effective rooting depth 
AQUACROP	Temperature and drought stress	Kc	KcTr	1.1	0.77	1.43	Crop coefficient when canopy is complete 
AQUACROP	Temperature and drought stress	Ks_exp	p-exp	0.4	0.28	0.52	Soil water depletion factor for canopy expansion- Lower threshold
AQUACROP	Temperature and drought stress	Ks_polc	°C	8	5.6	10.4	Minimum air temperature below which pollination starts to fail
AQUACROP	Temperature and drought stress	Ks_polh	°C	35	24.5	45.5	Maximum air temperature above which pollination starts to fail


## Evaluate params with observed data

## write files


crop_def <- read_lines("_AQUACROP/base_data/agroclimR.CRO")


crop_params_aquacrop <- 

GDD_CCmax
GDD_FL
GDD_FLL
GDD_M
CGC
CC_senecence
CC_max
NPLDS
WP
HIo
GDD_HI
GDC
Zr
Kc
Ks_exp
Ks_polc
Ks_polh

tidy_to_write_crop <- function(param_data, model = "aquacrop", values = "Base", export_default = T){
  
  ###Crea tabla control de parametros y rangos, basados en el CRO standard de aquacrop
  ## Max and min  + Parameter (+/- 30%)

default_list <- tibble(
  
  #strsplit(clipboard(), "\n") %>% unlist() %>% paste(collapse = "', '")
  Model = c(rep("AQUACROP", 18)),
  
  Component = c('Phenology', 'Phenology', 'Phenology', 'Phenology', 'Leaf and stem growth', 'Leaf and stem growth', 'Leaf and stem growth',
                'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 
                'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress'),
  
  
  Parameter = c('GDD_emergence', 'GDD_CCmax', 'GDD_Flow', 'GDD_FL', 'GDD_M', 'CGC', 'GDD_senecence', 'CC_max', 'NPLDS', 'WP', 'HIo', 'GDD_HI', 'CDC', 'Zr', 'Kc', 'Ks_exp', 'Ks_polc', 'Ks_polh'),
  Unit = c('GDD', 'GDD', 'GDD', 'GDD', 'GDD', '%/d', 'GDD', '%','plants/ha', 'g/m²', '%', 'GDD', '%/GDD', 'm', 'KcTr', 'p-exp', '°C', '°C'),
  Base = map(list('150', '370', '1150', '350', '1900', '0.12257', '1300', '0.95', '1500000', '19', '43', '680', '0.0933', '0.5', '1.1', '0.4', '8', '35'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
  Min = map(list('70', '259', '805', '245', '1330', '0.085799', '910', '0.665', '1000000', '13.3', '30.1', '476', '0.06531', '0.35', '0.77', '0.28', '5.6', '24.5'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
  Max = map(list('200', '481', '1495', '455', '2470', '0.159341', '1690', '1.235', '2000000', '24.7', '55.9', '884', '0.12129', '0.65', '1.43', '0.52', '10.4', '45.5'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
  
  Description = c('from sowing to emergence', 'from emergence to maximum rooting depth', 'from emergence to flowering', 'Length of the flowering stage', 'from emergence to maturity', 'Canopy Growth Coefficient', 'GDD from emergence to start senescence', 'Maximun canopy cover', 'Plant population per hectare', 'Crop Water Productivity', 'Reference Harvest Index', 'Building-up of Harvest Index during yield formation', 'Canopy Decline Coefficient', 'Maximum effective rooting depth', 
                  'Crop coefficient when canopy is complete', 'Soil water depletion factor for canopy expansion- Lower threshold', 'Minimum air temperature below which pollination starts to fail', 'Maximum air temperature above which pollination starts to fail')
  
)

GDD_sow_emer

if(is.null(param_data)){return(default_list)} 



if(isTRUE(export_default)){default_list <<- default_list}


}



#crop_params_aquacrop <- dplyr::select(tidy_to_write_crop(NULL), Parameter, Base) %>% deframe
































write_crop_aquacrop <- function(path, cultivar, crop_params_aquacrop){
  

### Inicia la creacion del archivo CRO 

sink(paste0(path, "/", cultivar, ".CRO"), append = F) 


### Info data
cat(paste(cultivar, "AquaCrop CRO  by https://github.com/jrodriguez88"), sep = '\n')
cat("     6.1       : AquaCrop Version (May 2018)", sep = '\n')
cat("     1         : File not protected", sep = '\n')
cat("     2         : fruit/grain producing crop", sep = '\n')
cat("     1         : Crop is sown", sep = '\n')
cat("     0         : Determination of crop cycle : by growing degree-days", sep = '\n')
cat("     1         : Soil water depletion factors (p) are adjusted by ETo", sep = '\n')
cat("     8.0       : Base temperature (°C) below which crop development does not progress", sep = '\n')
cat("    30.0       : Upper temperature (°C) above which crop development no longer increases with an increase in temper", sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_M),"         : Total length of crop cycle in growing degree-days"), sep = '\n')
cat("     0.00      : Soil water depletion factor for canopy expansion (p-exp) - Upper threshold", sep = '\n')
cat(paste0(sprintf("     %1.2f", crop_params_aquacrop$Ks_exp),"      : Soil water depletion factor for canopy expansion (p-exp) - Lower threshold"), sep = "\n")
cat("     3.0       : Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)", sep = '\n')
cat("     0.50      : Soil water depletion fraction for stomatal control (p - sto) - Upper threshold", sep = '\n')
cat("     3.0       : Shape factor for water stress coefficient for stomatal control (0.0 = straight line)", sep = '\n')
cat("     0.55      : Soil water depletion factor for canopy senescence (p - sen) - Upper threshold", sep = '\n')
cat("     3.0       : Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)", sep = '\n')
cat("     0         : Sum(ETo) during stress period to be exceeded before senescence is triggered", sep = '\n')
cat("     0.75      : Soil water depletion factor for pollination (p - pol) - Upper threshold", sep = '\n')
cat("     0         : Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)", sep = '\n')
cat("    50         : Considered soil fertility stress for calibration of stress response (%)", sep = '\n')
cat("    25.00      : Response of canopy expansion is not considered", sep = '\n')
cat("    25.00      : Response of maximum canopy cover is not considered", sep = '\n')
cat("    25.00      : Response of crop Water Productivity is not considered", sep = '\n')
cat("    25.00      : Response of decline of canopy cover is not considered", sep = '\n')
cat("    -9         : dummy - Parameter no Longer required", sep = '\n')
cat(paste0(sprintf("     %1.0f", crop_params_aquacrop$Ks_polc), "         : Minimum air temperature below which pollination starts to fail (cold stress) (°C)"), sep = '\n')
cat(paste0(sprintf("    %1.0f", crop_params_aquacrop$Ks_polh), "         : Maximum air temperature above which pollination starts to fail (heat stress) (°C)"), sep = '\n')
cat("    10.0       : Minimum growing degrees required for full crop transpiration (°C - day)", sep = '\n')
cat("     3         : Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)", sep = '\n')
cat("    11         : Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)", sep = '\n')
cat("    -9         : Dummy - no longer applicable", sep = '\n')
cat("    25         : Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))", sep = '\n')
cat("   100         : Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))", sep = '\n')
cat(paste0(sprintf("     %1.2f", crop_params_aquacrop$Kc), "      : Crop coefficient when canopy is complete but prior to senescence (KcTr,x)"), sep = '\n')
cat("     0.150     : Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.", sep = '\n')
cat("     0.15      : Minimum effective rooting depth (m)", sep = '\n')
cat(paste0(sprintf("     %1.2f", crop_params_aquacrop$Zr),"      : Maximum effective rooting depth (m)"), sep = '\n')
cat("    25         : Shape factor describing root zone expansion", sep = '\n')
cat("     0.048     : Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone", sep = '\n')
cat("     0.012     : Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone", sep = '\n')
cat("    50         : Effect of canopy cover in reducing soil evaporation in late season stage", sep = '\n')
cat("     3.00      : Soil surface covered by an individual seedling at 90 % emergence (cm2)", sep = '\n')
cat("     3.00      : Canopy size of individual plant (re-growth) at 1st day (cm2)", sep = '\n')
cat(paste0(sprintf("  %1.0f", crop_params_aquacrop$NPLDS), "      : Number of plants per hectare"), sep = '\n')
cat(paste0(sprintf("     %1.5f", crop_params_aquacrop$CGC),"   : Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)"), sep = '\n')
cat("    -9         : Maximum decrease of Canopy Growth Coefficient in and between seasons - Not Applicable", sep = '\n')
cat("    -9         : Number of seasons at which maximum decrease of Canopy Growth Coefficient is reached - Not Applicable", sep = '\n')
cat("    -9.0       : Shape factor for decrease Canopy Growth Coefficient - Not Applicable", sep = '\n')
cat(paste0(sprintf("     %1.2f", crop_params_aquacrop$CC_max), "      : Maximum canopy cover (CCx) in fraction soil cover"), sep = '\n')
cat(paste0(sprintf("     %1.5f", crop_params_aquacrop$CDC), "   : Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)"), sep = '\n')
cat("     4         : Calendar Days: from sowing to emergence", sep = '\n')
cat("    65         : Calendar Days: from sowing to maximum rooting depth", sep = '\n')
cat("   108         : Calendar Days: from sowing to start senescence", sep = '\n')
cat("   158         : Calendar Days: from sowing to maturity (length of crop cycle)", sep = '\n')
cat("    96         : Calendar Days: from sowing to flowering", sep = '\n')
cat("    29         : Length of the flowering stage (days)", sep = '\n')
cat("     1         : Crop determinancy linked with flowering", sep = '\n')
cat("   100         : Excess of potential fruits (%)", sep = '\n')
cat("    57         : Building up of Harvest Index starting at flowering (days)", sep = '\n')
cat(paste0(sprintf("    %1.1f", crop_params_aquacrop$WP), "       : Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)"), sep = '\n')
cat("   100         : Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)", sep = '\n')
cat("    50         : Crop performance under elevated atmospheric CO2 concentration (%)", sep = '\n')
cat(paste0(sprintf("    %1.0f", crop_params_aquacrop$HIo), "         : Reference Harvest Index (HIo) (%)"), sep = '\n')
cat("     0         : Possible increase (%) of HI due to water stress before flowering", sep = '\n')
cat("    10.0       : Coefficient describing positive impact on HI of restricted vegetative growth during yield formation", sep = '\n')
cat("     7.0       : Coefficient describing negative impact on HI of stomatal closure during yield formation", sep = '\n')
cat("    15         : Allowable maximum increase (%) of specified HI", sep = '\n')
cat(paste0(sprintf("%6.0f", 50), "         : GDDays: from sowing to emergence"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_CCmax), "         : GDDays: from sowing to maximum rooting depth"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$CC_senecence),"         : GDDays: from sowing to start senescence"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_M),"         : GDDays: from sowing to maturity (length of crop cycle)"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_FL),"         : GDDays: from sowing to flowering"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_FLL),"         : Length of the flowering stage (growing degree days)"), sep = '\n')
cat("     0.006467  : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)", sep = '\n')
cat("     0.005003  : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)", sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_HI),"         : GDDays: building-up of Harvest Index during yield formation"), sep = '\n')

sink()

}


write_crop_aquacrop(path_proj, cultivar, crop_params_aquacrop)






#Default Paddy Rice, GDD agroclimR
#     6.1       : AquaCrop Version (May 2018)
#     1         : File not protected
#     2         : fruit/grain producing crop
#     1         : Crop is sown
#     0         : Determination of crop cycle : by growing degree-days
#     1         : Soil water depletion factors (p) are adjusted by ETo
#     8.0       : Base temperature (°C) below which crop development does not progress
#    30.0       : Upper temperature (°C) above which crop development no longer increases with an increase in temperature
#  1900         : Total length of crop cycle in growing degree-days
#     0.00      : Soil water depletion factor for canopy expansion (p-exp) - Upper threshold
#     0.40      : Soil water depletion factor for canopy expansion (p-exp) - Lower threshold
#     3.0       : Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)
#     0.50      : Soil water depletion fraction for stomatal control (p - sto) - Upper threshold
#     3.0       : Shape factor for water stress coefficient for stomatal control (0.0 = straight line)
#     0.55      : Soil water depletion factor for canopy senescence (p - sen) - Upper threshold
#     3.0       : Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)
#     0         : Sum(ETo) during stress period to be exceeded before senescence is triggered
#     0.75      : Soil water depletion factor for pollination (p - pol) - Upper threshold
#     0         : Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)
#    50         : Considered soil fertility stress for calibration of stress response (%)
#    25.00      : Response of canopy expansion is not considered
#    25.00      : Response of maximum canopy cover is not considered
#    25.00      : Response of crop Water Productivity is not considered
#    25.00      : Response of decline of canopy cover is not considered
#    -9         : dummy - Parameter no Longer required
#     8         : Minimum air temperature below which pollination starts to fail (cold stress) (°C)
#    35         : Maximum air temperature above which pollination starts to fail (heat stress) (°C)
#    10.0       : Minimum growing degrees required for full crop transpiration (°C - day)
#     3         : Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)
#    11         : Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)
#    -9         : Dummy - no longer applicable
#    25         : Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))
#   100         : Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))
#     1.10      : Crop coefficient when canopy is complete but prior to senescence (KcTr,x)
#     0.150     : Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.
#     0.15      : Minimum effective rooting depth (m) 
#     0.45      : Maximum effective rooting depth (m)
#    25         : Shape factor describing root zone expansion
#     0.048     : Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone
#     0.012     : Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone
#    50         : Effect of canopy cover in reducing soil evaporation in late season stage
#     3.00      : Soil surface covered by an individual seedling at 90 % emergence (cm2)
#     3.00      : Canopy size of individual plant (re-growth) at 1st day (cm2)
#  3000000      : Number of plants per hectare
#     0.07750   : Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)
#    -9         : Maximum decrease of Canopy Growth Coefficient in and between seasons - Not Applicable
#    -9         : Number of seasons at which maximum decrease of Canopy Growth Coefficient is reached - Not Applicable
#    -9.0       : Shape factor for decrease Canopy Growth Coefficient - Not Applicable
#     0.95      : Maximum canopy cover (CCx) in fraction soil cover
#     0.06045   : Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)
#     4         : Calendar Days: from sowing to emergence
#    65         : Calendar Days: from sowing to maximum rooting depth
#   108         : Calendar Days: from sowing to start senescence
#   158         : Calendar Days: from sowing to maturity (length of crop cycle)
#    96         : Calendar Days: from sowing to flowering
#    29         : Length of the flowering stage (days)
#     1         : Crop determinancy linked with flowering
#   100         : Excess of potential fruits (%)
#    57         : Building up of Harvest Index starting at flowering (days)
#    19.0       : Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)
#   100         : Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)
#    50         : Crop performance under elevated atmospheric CO2 concentration (%)
#    43         : Reference Harvest Index (HIo) (%)
#     0         : Possible increase (%) of HI due to water stress before flowering
#    10.0       : Coefficient describing positive impact on HI of restricted vegetative growth during yield formation
#     7.0       : Coefficient describing negative impact on HI of stomatal closure during yield formation
#    15         : Allowable maximum increase (%) of specified HI
#    50         : GDDays: from sowing to emergence
#   780         : GDDays: from sowing to maximum rooting depth
#  1300         : GDDays: from sowing to start senescence
#  1900         : GDDays: from sowing to maturity (length of crop cycle)
#  1150         : GDDays: from sowing to flowering
#   350         : Length of the flowering stage (growing degree days)
#     0.006467  : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)
#     0.005003  : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)
#   680         : GDDays: building-up of Harvest Index during yield formation







































