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

#AQUACROP	Phenology	GDD_CCmax	GDD	370	259	481	from emergence to maximum rooting depth
#AQUACROP	Phenology	GDD_FL	GDD	1150	805	1495	from emergence to flowering
#AQUACROP	Phenology	GDD_FLL	GDD	350	245	455	Length of the flowering stage
#AQUACROP	Phenology	GDD_M	GDD	1900	1330	2470	from emergence to maturity
#AQUACROP	Leaf and stem growth	CGC	%/d	0.12257	0.085799	0.159341	Canopy Growth Coefficient
#AQUACROP	Leaf and stem growth	CC_senecence	GDD	1300	910	1690	GDD from emergence to start senescence
#AQUACROP	Leaf and stem growth	CC_max	%	0.95	0.665	1.235	Maximun canopy cover
#AQUACROP	Growth AGB-RZ	WP	g/m²	19	13.3	24.7	Crop Water Productivity
#AQUACROP	Growth AGB-RZ	HIo	%	43	30.1	55.9	Reference Harvest Index
#AQUACROP	Growth AGB-RZ	GDD_HI	GDD	680	476	884	Building-up of Harvest Index during yield formation
#AQUACROP	Growth AGB-RZ	GDC	%/GDD	0.0933	0.06531	0.12129	Canopy Decline Coefficient
#AQUACROP	Growth AGB-RZ	Zr	m	0.5	0.35	0.65	Maximum effective rooting depth 
#AQUACROP	Temperature and drought stress	Kc	KcTr	1.1	0.77	1.43	Crop coefficient when canopy is complete 
#AQUACROP	Temperature and drought stress	Ks_exp	p-exp	0.4	0.28	0.52	Soil water depletion factor for canopy expansion- Lower threshold
#AQUACROP	Temperature and drought stress	Ks_polc	°C	8	5.6	10.4	Minimum air temperature below which pollination starts to fail
#AQUACROP	Temperature and drought stress	Ks_polh	°C	35	24.5	45.5	Maximum air temperature above which pollination starts to fail


## Evaluate params with observed data

## write files

tidy_to_write_crop <- function(param_data, model = "aquacrop", values = "Base", export_default = T, NPLDS = 150000){
  
  ###Crea tabla control de parametros y rangos, basados en el CRO standard de aquacrop
  ## Max and min  + Parameter (+/- 30%)

default_list <- tibble(
  
  #strsplit(clipboard(), "\n") %>% unlist() %>% paste(collapse = "', '")
  Model = c(rep("AQUACROP", 18)),
  
  Component = c('Phenology','Phenology', 'Phenology', 'Phenology', 'Phenology', 'Leaf and stem growth', 'Leaf and stem growth', 'Leaf and stem growth',
                'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ',
                'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress'),
  
  
  Parameter = c('GDD_emergence', 'GDD_CCx', 'GDD_FL', 'GDD_FLL', 'GDD_M', 'CGC', 'GDD_senecence', 'CCx', 'WP', 'HIo', 'GDD_HI', 'CDC', 'Zr', "NPLDS",  'Kc', 'Ks_exp', 'Ks_polc', 'Ks_polh'),
  Unit = c('GDD', 'GDD', 'GDD', 'GDD', 'GDD', '%/d', 'GDD', '%', 'g/m²', '%', 'GDD', '%/GDD', 'm', 'plants/ha', 'KcTr', 'p-exp', '°C', '°C'),
  Base = map(list('150', '370', '1150', '350', '1900', '0.12257', '1300', '0.95', '19', '43', '680', '0.0933', '0.45', paste(NPLDS), '1.1', '0.4', '8', '35'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
  Min = map(list('70', '259', '805', '245', '1330', '0.085799', '910', '0.665', '13.3', '30.1', '476', '0.06531', '0.25', paste(NPLDS*0.95),  '0.77', '0.28', '5.6', '24.5'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
  Max = map(list('200', '481', '1495', '455', '2470', '0.159341', '1690', '1.235', '24.7', '55.9', '884', '0.12129', '0.65', paste(NPLDS*1.05), '1.43', '0.52', '10.4', '45.5'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
  
  Description = c('from sowing to emergence', 'from emergence to maximum rooting depth', 'from emergence to flowering', 'Length of the flowering stage', 'from emergence to maturity', 'Canopy Growth Coefficient', 'GDD from emergence to start senescence', 'Maximun canopy cover', 'Crop Water Productivity', 'Reference Harvest Index', 'Building-up of Harvest Index during yield formation', 'Canopy Decline Coefficient', 'Maximum effective rooting depth', 'Number of plants per hectare', 
                  'Crop coefficient when canopy is complete', 'Soil water depletion factor for canopy expansion- Lower threshold', 'Minimum air temperature below which pollination starts to fail', 'Maximum air temperature above which pollination starts to fail')
  
)


if(is.null(param_data)){return(default_list)} 



if(isTRUE(export_default)){default_list <<- default_list}


if(any(class(param_data) == "data.frame")) {
  
  stopifnot(any(names(param_data) == "Parameter"))
  
  message(paste0("Parameters to CRP file: ", paste(param_data$Parameter, collapse = ", ")))
  
  param_data <- rename_with(param_data, .fn = function(x) str_replace(x, "Base|Set_cal", values))
  if(any(names(param_data) == "Min")){message("Minimum range are available")
  } else {param_data <- mutate(param_data, Min = list(NULL))}
  if(any(names(param_data) == "Max")){message("Maximum range are available")
  } else {param_data <- mutate(param_data, Max = list(NULL))}
  
  
  test_params_oryza <- default_list %>% 
    left_join(param_data, by = "Parameter") %>%
    mutate(Base = map2(Base.x, Base.y, function(x, y) if(is.null(y)){x} else {y}), 
           Min = map2(Min.x, Min.y, function(x, y) if(is.null(y)){x} else {y}),
           Max = map2(Max.x, Max.y, function(x, y) if(is.null(y)){x} else {y})) %>% 
    dplyr::select(-contains(".x"), -contains(".y"))
  
  
} else if(any(class(param_data) == "list")) {
  
  stopifnot(any(default_list$Parameter %in% names(param_data)))
  
  message(paste0("Parameters to CRP file: ", paste(names(param_data), collapse = ", ")))
  
  param_data <- enframe(param_data, name = "Parameter", value = "Base") %>% 
    mutate(Min = list(NULL), Max = list(NULL))
  
  
  test_params_oryza <- default_list %>% 
    left_join(param_data, by = "Parameter") %>%
    mutate(Base = map2(Base.x, Base.y, function(x, y) if(is.null(y)){x} else {y}), 
           Min = map2(Min.x, Min.y, function(x, y) if(is.null(y)){x} else {y}),
           Max = map2(Max.x, Max.y, function(x, y) if(is.null(y)){x} else {y})) %>% 
    dplyr::select(-contains(".x"), -contains(".y"))
  
  
}


return(test_params_oryza)    

}



#crop_params_aquacrop <- dplyr::select(tidy_to_write_crop(NULL), Parameter, Base) %>% deframe



write_crop_aquacrop <- function(path, cultivar, crop_params_aquacrop, NPLDS = 150000){
  

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
cat(paste0(sprintf("     %1.2f", crop_params_aquacrop$CCx), "      : Maximum canopy cover (CCx) in fraction soil cover"), sep = '\n')
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
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_emergence), "         : GDDays: from sowing to emergence"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_CCx), "         : GDDays: from sowing to maximum rooting depth"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_senecence),"         : GDDays: from sowing to start senescence"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_M),"         : GDDays: from sowing to maturity (length of crop cycle)"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_FL),"         : GDDays: from sowing to flowering"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_FLL),"         : Length of the flowering stage (growing degree days)"), sep = '\n')
cat(paste0(sprintf("     %1.5f", crop_params_aquacrop$CGC),"   : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)"), sep = '\n')
cat(paste0(sprintf("     %1.5f", crop_params_aquacrop$CDC), "   : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)"), sep = '\n')
cat(paste0(sprintf("%6.0f", crop_params_aquacrop$GDD_HI),"         : GDDays: building-up of Harvest Index during yield formation"), sep = '\n')

sink()

}


#write_crop_aquacrop(path_proj, cultivar, crop_params_aquacrop)




#tidy_to_write_crop(CRO_ac) %>% write_crop_aquacrop(path = path_proj, cultivar = cultivar, crop_params_aquacrop = .) 



















