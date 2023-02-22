# Calulate ORYZA model params to write CRP file
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022

#
#list_params =  output of "extract_drates_param" function

#library(ggplot2)
#library(dplyr)
#library(broom)
#library(purrr)


#source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/run_tools/extract_drates_param.R", encoding = "UTF-8")
#source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/run_tools/run_drates_param.R", encoding = "UTF-8")

basedata_path <- paste0(path_proj) -> path
input_data <- test_data$data$input_data
exp_files <- list.files(paste0(basedata_path, "/EXP/"), pattern = "\\.exp$")
model_curves <- c("lm", "loess")
stat <- c("mean", "min", "max")


## Funcion que calcula y extrae los parametros para escribir el archivo de cultivo del modelo ORYZA 
get_params_oryza <- function(path, input_data, exp_files, model_curves = "lm", stat){
  
  
  ### evalua si se encuantran las gherramientas necesarias en la carpeta de trabajo 
  ##Descarga y ejecuta aplicativos DRATES.exe y PARAM.exe para obetener parametros de cultico para ORYZA model
  
  download_ORYZA_Tools(path)
  run_drates_param(exp_files, path)
  
  ### Extract and import params from PARAM.out
  raw_params <- extract_drates_param(exp_files, path)
  tidy_params <- tidy_params_oryza(input_data, raw_params, method =  1)
  

  
  #Development rates data 
  DVR_data <- tidy_params$DVR_tb %>% pivot_wider(names_from = DVR_Parameter, values_from = Value)
  
  ## Selected tables -- Particion de biomasa --- area foliar especifica y tasa de hojas muertas
  BPF_tb <- make_grow_curves(tidy_params$BPF_tb, dvs = c(0, 0.5, 0.75, 1, 1.5, 2.5),  model = model_curves)
  SLA_tb <- make_grow_curves(tidy_params$SLA_tb, dvs = c(0, 0.16, 0.33, 0.65, 0.79, 2.1, 2.5), model = model_curves)
  DRLV_tb <- make_grow_curves(tidy_params$DRLV_tb, dvs = c(0, 0.6, 1, 1.6, 2.1, 2.5), model = model_curves)

  
  

  ##Genera un indice para calcular y filtrar resultados
  metric <- switch (stat,
                    "mean" = 1,
                    "min" = 2,
                    "max" = 3
  )
  
  #Function to copy partitioning tables in correct format  ---inputs from tidy_params  
  paste_crp <- function(tidy_tb, param, metric){
    
    
    tidy_tb[[metric]] %>%
      spread(Partition_Parameter, Value) %>%
      select(DVS, all_of(param))
    
  }
  

 ### Genera una lista d eparametros con base en la informacion ingresada en input data y extraida en la parametrizacion  
  
 param_list <- list(  
  
#Development rate in juvenile phase                
  
  DVRJ = bootstrap_param(DVR_data$DVRJ, stat = stat),
  
  
#Development rate in photoperiod-sensitive phase    
  
  DVRI = bootstrap_param(DVR_data$DVRI, stat = stat),
  
  
#Development rate in panicle development               
  
  DVRP = bootstrap_param(DVR_data$DVRP, stat = stat),
  
  
#Development rate in reproductive phase   
  
  DVRR = bootstrap_param(DVR_data$DVRR, stat = stat),
  
  
#Maximum relative growth rate of leaf area   
  
#  RGRLMX 
  
  
#Minimum relative growth rate of leaf area  
  
#  RGRLMN
  
  
#Maximum value of SLA   
  
  SLAMAX = SLA_max(tidy_params$SLA_tb),
  
#Give SLA as a function of DVS in the table SLATB
  
  SLATB = SLA_tb[[metric]],
  
  
#Fraction of carbohydrates allocated to stems that is stored as reserves  
  
  FSTR = FSTR_cal(tidy_params$FSTR_tb, stat = stat),
  
  
  
  
#Spikelet growth factor  
  
  SPGF = SPGF_cal(tidy_params$SPGF_tb),
  
  
#Maximum individual grain weight   
  
  WGRMX  = WGRMX_cal(tidy_params$YIELD_tb$GW1000),
  
  
#Table of fraction total dry matter partitioned to the shoot  
  
#  FSHTB = 
  
  
#Table of fraction shoot dry matter partitioned to the leaves  
  
  FLVTB = paste_crp(BPF_tb, "FLV", metric),
  
  
#Table of fraction shoot dry matter partitioned to the stems  
  
  FSTTB = paste_crp(BPF_tb, "FST", metric),
  
  
#Table of fraction shoot dry matter partitioned to the panicles  
  
  FSOTB = paste_crp(BPF_tb, "FSO", metric),
  
  
#Table of leaf death coefficient 
  
  DRLVT = DRLV_tb[[metric]]
  
  
#Maximum depth of roots if drought   
  
#  ZRTMCD
  
  
#Upper limit leaf expansion  
  
#  ULLE
  
  
#Lower limit leaf expansion  
  
#  LLLE
  
  
#Ratio of remaining available water to total water supply capability - transpiration eq  
  
#  FSWTD
  
  
#The threshold temperature for cold caused sterility   
  
#  COLDREP
  
  
#The threshold temperature for heat caused sterility   
  
#  CTSTER
  
)
  
  
  
  
 ##Retorna lista de parametros para ingresar a archivo de cultivo 
  
 return(param_list) 
  
  
  
  
}



## Funcion que organiza los datos observados y parametros en un formato estandar - Agroclimr

tidy_params_oryza <- function(input_data, raw_params, method = 1){
  
  
  
  if(method == 1){
    
    
    ### Method 1. According to ORYZA_V3_User_Manual_2014
    ### Apply filters by DVSM 
    BPF_tb <- raw_params$BPART_df %>% 
      pivot_longer(cols = FLV:FSO, names_to = "Partition_Parameter", values_to = "Value") %>%
      mutate(
        #        Growth_phase = case_when(
        #            DVSM<=0.65 ~ "Vegetative",
        #            DVSM>0.65&DVSM<=1 ~"Reproductive",
        #            DVSM>1 ~"Ripening"),
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
          DVSM>1.1 &  Partition_Parameter == "FLV" ~ 0,
          DVSM>1.1 &  Partition_Parameter == "FST" ~ 0,
          DVSM>1.1 &  Partition_Parameter == "FSO" ~ 1,
          Value<0 ~ 0,
          Value>1 ~ 1,
          TRUE ~ Value))
    
    
  } else {
    
    ### Method 2. Grafical analysis, logical-physiological filters
    BPF_tb <- raw_params$BPART_df %>%
      filter(FLV>=-0.2, FLV<=1.2,
             FST>=-0.2, FST<=1.2,
             FST>=-0.2, FSO<=1.2) %>%
      pivot_longer(cols = FLV:FSO, names_to = "Partition_Parameter", values_to = "Value") %>%
      mutate(
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
          Partition_Parameter == "FSO" & DVSM>1 & Value<0.30   ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM>1 & Value>0.25   ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<1 & Value>0.75   ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<0.75 & Value<0.4 ~ NA_real_,
          Partition_Parameter == "FST" & DVSM>1 & Value>0.5    ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<1 & Value>0.80   ~ NA_real_,
          Value<0 ~ 0,
          Value>1 ~ 1,
          TRUE ~ Value))
    
    
  }
  
  
  ##### tidy Specific LEaf Area
  SLA_df <- raw_params$LAI_df %>%
    select(exp_file, DVS, LAI) %>% na.omit() %>%
    left_join(raw_params$BMASS_df) %>%
    mutate(SLA = LAI/WLVG, Value = SLA, Parameter = "SLA") %>%
    filter(SLA < 0.0055) %>%
    dplyr::rename(DVSM=DVS)
  
  
  # DVR_plots
  DVR_tb <- raw_params$DVR_df %>%
    pivot_longer(cols = -exp_file, names_to = "DVR_Parameter", values_to = "Value") %>%
    filter(Value < 0.0046) %>% #DVR!= "DVRI", 
    mutate(DVR_Parameter = factor(DVR_Parameter, c("DVRJ", "DVRI", "DVRP", "DVRR"))) 
  
  
  ## Table of leaf death coefficient 
  DRLV_tb <- tibble(raw_params$DRLV_df %>% dplyr::filter(DRLV>0)) %>% 
    mutate(Value = DRLV, Parameter = "DRLV") 
  
  # TAble of Fraction of carbohydrates allocated to stems that is stored as reserves
  FSTR_tb <- tibble(raw_params$FSTR_df) %>% dplyr::filter(FSTR > 0.03)
  
  
  
  # Yield params and components
  filt_exp <- DVR_tb$exp_file %>% unique()
  
  id_input_exp <- input_data %>% 
    map(., ~.[["AGRO_man"]]) %>% bind_rows() %>% 
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = function(x) str_replace_all(x, "_", "")) %>%
    mutate(exp_file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>% 
    dplyr::select(c(ID,	exp_file, LOC_ID,	CULTIVAR))  %>% 
    filter(exp_file %in% filt_exp)
  
  
  YIELD_tb <- id_input_exp %>% 
    left_join(map(input_data, ~.x$YIELD_obs) %>% bind_rows()) %>%
    dplyr::select(exp_file, YIELD_AVG, GW1000, ST_M2, PAN_M2, GT_PAN, GF_PAN)
  
  ## Spikelet growth factor - yield parameters
  SPGF_tb <- id_input_exp %>% 
    left_join(map(input_data, SPGF_extract) %>% bind_rows()) %>% 
    drop_na() %>%
    dplyr::select(exp_file, everything())
  
  
  
  
  
 ###Retorna una lista con las tablas de parametros del modelo ORYZA 
  
  return(list(DVR_tb = DVR_tb, 
              BPF_tb = BPF_tb, 
              SLA_tb = SLA_df, 
              DRLV_tb = DRLV_tb, 
              FSTR_tb = FSTR_tb,
              SPGF_tb = SPGF_tb,
              YIELD_tb = YIELD_tb))
  
  
}



## Function apply bootstrap method over crop parameters -- get mean, max or min value 
# param_data = parameter crop data ,
# reps  = Number of repetitions - boots
#ci = cofidence interval 
#stat =statistic value - mena, max o  min 
bootstrap_param <- function(param_data, reps = 2000, ci = 0.95, stat = "mean"){
  
  require(Hmisc)
  
  stat <- switch (stat,
                  "mean" = 1,
                  "min" = 2,
                  "max" = 3
  )
  
  smean.cl.boot(param_data, conf.int=ci, B=reps, na.rm=TRUE, reps=T)[stat][[1]]
  
  
}


### Specifif Leaf Area - Maximun 
### SLA_df
SLA_max <- function(SLA_df, min = 0.0039, max = 0.0051, fr=0.5) {
  
  sla_max <- SLA_df %>%
    filter(DVSM<fr) %>%
    lm(SLA~DVSM, data = .) %>%
    summary() %>%
    .$coefficients %>% as_tibble() %>%
    mutate(par=Estimate+1.96*`Std. Error`) %>%
    .$par %>%
    .[1]
  
  
  if (sla_max < min) {
    
    message(paste("SLA_max calculated:", sla_max, "- is a low value.", "- Use default"))
    return(min)
    
  } else if (sla_max > max){
    
    message(paste("SLA_max
                  calculated:", sla_max, "- is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("SLA_max calculated:", sla_max, "- is a normal value.", "- Use data"))
    return(sla_max)
    
  }
}


## Function to Calculate Fraction of carbohydrates allocated to stems that is stored as reserves
FSTR_cal <- function(FSTR_df, stat = "mean", min = 0.14, max = 0.24){
  
  FSTR <- bootstrap_param(FSTR_df$FSTR, stat = stat)
  
  
  if (FSTR < min) {
    
    message(paste("FSTR calculated:", FSTR, "- is a low value.", "- Use default"))
    return(min)
    
  } else if (FSTR > max){
    
    message(paste("FSTR calculated:", FSTR, "- is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("FSTR calculated:", FSTR, "- is a normal value.", "- Use data"))
    return(FSTR)
    
  }
  
}

## Function to plot Fraction of carbohydrates allocated to stems that is stored as reserves
FSTR_plot <- function(FSTR_df, save_plot = "N") {
  
  plot <- FSTR_df %>% filter(FSTR>0) %>% mutate(LOC_ID =  str_sub(exp_file, 1,4)) %>%
    ggplot(aes(LOC_ID, FSTR, label=exp_file)) +
    geom_jitter(width = 0.1) +
    stat_summary(fun.data = mean_se, color="red") +
    geom_hline(yintercept = mean(FSTR_df$FSTR), color="blue", linetype="twodash") +
    annotate("text", x = 0.65, y = mean(FSTR_df$FSTR), 
             label =  paste0("mean =\n", round(mean(FSTR_df$FSTR),3))) + 
    labs(title = paste0("Fraction of carbohydrates allocated to stems (stored as reserves) - ", cultivar),
         x="Locality") +
    theme_bw()
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(plot, filename = paste0("FSTR for ", cultivar, ".png"), width=7, height=3))
  
  plot
  
}


#WGRMX  = 0.0000249 ! Maximum individual grain weight (kg grain-1)
WGRMX_cal <- function(GW1000, min =  0.000019 , max = 0.000040){
  
  wgrmx <- bootstrap_param(GW1000)
  
  #(kg grain-1)
  WGRMX <-  wgrmx[[1]]/(1000000)
  
  if (WGRMX < min) {
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a low value.", "- Use default"))
    return(min)
    
  } else if (WGRMX > max){
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a normal value.", "- Use data"))
    return(WGRMX)
    
  }
  

}


# SPGF: The spikelet growth formation factor (SPGF; number kg−1) was derived as
#       the slope of the relationship between spikelet number m−2 and growth of the
#       crop between panicle initiation and flowering (g m-2)

## SPGF_cal function compute SPGF from experimental data, It take yield traits (spikelet number), 
##          growth data (biomass) and phenology observation to find the relationship between spikelet
##          number and and crop growth between panicle initiation and flowering.

## Function to extract variables (from INPUT_data.xls template) requiere to SPGF calculation
## Function to extract variables (from INPUT_data.xls template) requiere to SPGF calculation
SPGF_extract <- function(INPUT_data, max_diff = 4) {
  
  
  ##Load data for each workbook (XLSX)   
  
  ##Extract Spikelets number from YIELD_obs and join with Phenology observations (PHEN_bs)   
  SPIK_by_EXP <- INPUT_data$YIELD_obs %>%
    mutate(SPIK_M2_avg=PAN_M2*GT_PAN,
           SPIK_M2_min=(PAN_M2-PAN_M2_SE)*(GT_PAN-GT_PAN_SE),
           SPIK_M2_max=(PAN_M2+PAN_M2_SE)*(GT_PAN+GT_PAN_SE))%>%
    left_join(INPUT_data$PHEN_obs, by="ID")%>%
    select(ID, contains("SPIK_M2"), IDAT, FDAT)
  
  ##Extract  Total dry weight at panicle initiation or closest sampling date
  WAGT_PI <- INPUT_data$PLANT_gro %>%
    inner_join(SPIK_by_EXP, by="ID") %>%
    mutate(diff_pi=abs(as.integer(difftime(SAMPLING_DATE, IDAT, units = "days"))),
           WAGT_PI=WAGT_OBS, 
           WAGT_PI_SE=WAGT_SE)%>%
    group_by(ID) %>%
    filter(diff_pi==min(diff_pi))%>%
    select(ID, diff_pi, contains("WAGT_PI"))
  
  ##Extract  Total dry weight at flowering initiation or closest sampling date    
  WAGT_FL <- INPUT_data$PLANT_gro %>%
    inner_join(SPIK_by_EXP, by="ID") %>%
    mutate(diff_fl=abs(as.integer(difftime(SAMPLING_DATE, FDAT, units = "days"))),
           WAGT_FL=WAGT_OBS, 
           WAGT_FL_SE=WAGT_SE)%>%
    group_by(ID) %>%
    filter(diff_fl==min(diff_fl))%>%
    select(ID, diff_fl, contains("WAGT_FL"))
  
  ##Join data and compute variables to SPGF calculation  
  SPIK_by_EXP %>%
    left_join(WAGT_PI, by = "ID")%>%left_join(WAGT_FL, by = "ID") %>%
    mutate(diff_pi_fl=(WAGT_FL-WAGT_PI)/10) %>%#(g/m²)
    filter(diff_fl<max_diff, diff_pi<max_diff) %>%
    mutate(LOC_ID=substr(ID, 1,4))
  
}


## Function to calculate SPGF by lm 

SPGF_cal <- function(SPGF_df, out="", min = 35000, max = 67000) {
  
  
  SPGF_df <- SPGF_df %>% filter(ID != out)
  
  
  ## Linear model between Spikelet number (number/ m²) and crop growth between panicle initiation and flowering (g/m²)
  lm_spgf <- lm(formula = SPIK_M2_max ~ diff_pi_fl, data = SPGF_df)
  
  ## Print SPGF compute from lm-slope
  spgf <-  sprintf("%.1f", coef(lm_spgf)[[2]]*1000) # Spikelet growth factor (no kg-1)"))
  
  
  
  if (as.numeric(spgf) < min) {
    
    message(paste("SPGF calculated:", spgf, "- is a low value.", "- Use default"))
    return(min)
    
  } else if (as.numeric(spgf) > max){
    
    message(paste("SPGF calculated:", spgf, "- is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("SPGF calculated:", spgf, "- is a normal value.", "- Use data"))
    return(as.numeric(spgf))
    
  }
  
}



# Function to get mean, minimun and maximun partition factor tables
## Function to create growth curves (Biomass Partitioning tables,  Specific leaf area, Leaf death coeff )

#data = tidy_params --. c(BPF. SLA, DRLV)  
#model = c("loess, "lm")
#dvs = c(0, 0.5, 0.75, 1, 1.5, 2.5)
#span = 0.5     #Param smoth loess

make_grow_curves <- function(data, dvs = c(0, 0.5, 0.75, 1, 1.5, 2.5), model = "loess", span = 0.5){
  
  
  
  if(model == "loess"){
    
    modeled_data <- data %>% dplyr::select(contains("Parameter"), contains("DVS"), Value) %>% 
      nest(data = -contains("Parameter")) %>%
      mutate(data =  map(data , ~.x %>% set_names(c("X", "Y"))), 
             model =  map(data, ~loess(Y~X, data = .x, se = T, span = span)),
             predict_tb = map(model, ~predict(.x, newdata = data.frame(X = dvs), interval = "confidence", se =T) %>% 
                                as.data.frame() %>% 
                                mutate(
                                  Min = fit - 1.96*se.fit,
                                  Max = fit + 1.96*se.fit) %>%
                                dplyr::select(Base = fit, Min, Max) %>% 
                                round(5) %>% mutate(DVS = dvs))) %>% 
      dplyr::select(contains("Parameter"), predict_tb) %>% 
      unnest(predict_tb) %>% dplyr::select(contains("Parameter"), DVS, everything())
    
    
    
    
  } else {
    
    # Function to find the best linear model to describe crop grow curves
    find_best_model <- function(data) {
      
      #data$Y
      
      # Crear una lista de modelos que se quieren comparar
      models <- list(lm(Y ~ X, data),
                     lm(Y ~ X + I(X^2), data),
                     lm(Y ~ X + I(X^2) + I(X^3), data),
                     #loess(Y ~X , data), 
                     glm(Y ~ X, family = quasibinomial(link = "logit"), data)
                     #nls(Y ~ A * exp(-B * exp(-C * X)), start = list(A = 1, B = 1, C = 1), data),
                     #nls(Y ~ A * (1 - exp(-B * (X - C))), start = list(A = 1, B = 1, C = 1), data),
                     #nls(Y ~ A / (1 + exp(-B * (X - C))^D), start = list(A = 1, B = 1, C = 1, D = 1), data),
                     #nls(Y ~ K * X / (X + a * (1 - X/K)), start = list(K = 1, a = 1), data)
      )
      
      # Función para calcular el AIC (Akaike Information Criterion)
      calculate_aic <- function(model) {
        AIC(model, k = log(nrow(data)))
      }
      
      # Calcular el AIC para cada modelo
      aics <- map_dbl(models, calculate_aic)
      
      # Encontrar el modelo con el AIC más bajo
      best_model <- models[[which.min(aics)]]
      
      # Devolver el modelo con el AIC más bajo y su AIC
      return(list(best_model, aics[which.min(aics)]))
    }
    
    #result <- find_best_model(data)
    #best_model <- result[[1]]
    #best_aic <- result[[2]]
    
    
    
    modeled_data <- data %>% dplyr::select(contains("Parameter"), contains("DVS"), Value) %>% 
      nest(data = -contains("Parameter")) %>%
      mutate(data =  map(data , ~.x %>% set_names(c("X", "Y"))), 
             model =  map(data, ~find_best_model(.x)[[1]]),
             predict_tb = map(model, ~predict(.x, newdata = data.frame(X = dvs), interval = "confidence") %>% 
                                as.data.frame() %>% set_names(c("Base", "Min", "Max")) %>% 
                                round(5) %>% mutate(DVS = dvs))) %>% 
      dplyr::select(contains("Parameter"), predict_tb) %>% 
      unnest(predict_tb) %>% dplyr::select(contains("Parameter"), DVS, everything())
  }
  
  
  
  
  # Function to create crp tb
  crp_pf_tb <- function(data, stat = "Base") {
    
    data <- data %>% dplyr::select(Partition_Parameter, DVS, Value = contains(stat)) %>%#bind_rows(.id = "Partition_Parameter") %>%
      mutate(
        Value=case_when(
          Partition_Parameter == "FLV" & DVS > 1.5 ~ 0, 
          Partition_Parameter == "FST" & DVS > 1.5 ~ 0,
          Partition_Parameter != "FSO" & DVS  == 0 ~ 0.5,
          Partition_Parameter == "FSO" & DVS > 1.5 ~ 1,
          Partition_Parameter == "FSO" & DVS < 0.75 ~ 0,
          Value<0 | is.na(Value) ~ 0,
          TRUE ~ Value))
    
    
    if(stat == "Base"){
      
      data <-  data %>%
        pivot_wider(names_from =Partition_Parameter, values_from =  Value) %>%
        mutate(PF_sum=FLV+FSO+FST,
               PF_diff=1-PF_sum,
               FLV = case_when(
                 DVS<1 ~ FLV+(PF_diff/2),
                 TRUE ~ FLV),
               FST = case_when(
                 DVS<1 ~ FST+(PF_diff/2),
                 TRUE ~ FST),
               FSO = case_when(
                 DVS>=1 ~ FSO+PF_diff,
                 TRUE ~ FSO),
               PF_sum2= FLV+FSO+FST,
               Test_log = (FLV+FSO+FST)==1) %>%
        #            rename(DVSM=DVS) %>%
        select(DVS, FLV, FST, FSO) %>%
        pivot_longer(cols = -DVS, names_to = "Partition_Parameter", values_to = "Value") %>%
        mutate(Partition_Parameter = factor(Partition_Parameter,
                                            c("FLV", "FST", "FSO"))) %>%
        dplyr::select(Partition_Parameter, DVS, Value)
      
    }
    
    return(data)
    
  }
  
  crp_sla_tb <- function(data, stat = "Max", SLA_max) {
    
    data %>%  dplyr::select(DVS, Value = contains(stat)) %>%
      mutate(
        Value = case_when(
          DVS == 0 ~ SLA_max,
          Value < 0 | is.na(Value) ~ min(Value, na.rm = T),
          TRUE ~ Value))
    
  }
  
  crp_drlv_tb <- function(data, stat = "Base") {
    
    data %>%  dplyr::select(DVS, Value = contains(stat)) %>%
      mutate(
        Value= case_when(
          DVS > 2   ~ 0.05,
          DVS == 0 ~ 0,
          Value < 0 | is.na(Value) ~ 0,
          TRUE ~ Value))
    
  }
  
  #library(caret)
  #
  ##define k-fold cross validation method
  #ctrl <- trainControl(method = "cv", number = 5)
  #grid <- expand.grid(span = seq(0.3, 0.9, by = 0.05), degree = 1)
  #
  ##perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
  #model <- train(Y ~ X,  data = a$data[[1]], method = "gamLoess", tuneGrid=grid, trControl = ctrl)
  
  
  # join data in list
  
  if(any(str_detect(names(data), "SLA"))){
    
    SLA_max = SLA_max(data)
    
    crp_list <- list(
      Base = crp_sla_tb(modeled_data, stat = "Base", SLA_max),
      Min =  crp_sla_tb(modeled_data, stat = "Min" , SLA_max),
      Max =  crp_sla_tb(modeled_data, stat = "Max" , SLA_max))
    
  } else if (any(str_detect(names(data), "DRLV"))){
    
    crp_list <- list(
      Base = crp_drlv_tb(modeled_data, stat = "Base"),
      Min =  crp_drlv_tb(modeled_data, stat = "Min"),
      Max =  crp_drlv_tb(modeled_data, stat = "Max"))
    
  } else {
    
    crp_list <- list(
      Base = crp_pf_tb(modeled_data, "Base"),
      Min = crp_pf_tb(modeled_data, "Min"),
      Max = crp_pf_tb(modeled_data, "Max"))}
  
  
  return(crp_list)
  
  
}


#make_grow_curves(tidy_params$DRLV_tb, model = "loess", span = 0.5)



#params_oryza_lm <- map2(.x = c("mean", "min", "max"), .y = c("Base", "Min", "Max"), 
#      ~get_params_oryza(path, input_data, exp_files, model_curves = "lm", stat = .x) %>%
#        enframe(name = "Parameter", value = .y)) %>% reduce(left_join)
#
#params_oryza_loess <- map2(.x = c("mean", "min", "max"), .y = c("Base", "Min", "Max"), 
#                        ~get_params_oryza(path, input_data, exp_files, model_curves = "loess", stat = .x) %>%
#                          enframe(name = "Parameter", value = .y)) %>% reduce(left_join)






#test_params_oryza <- default_list %>% dplyr::filter(Model == "ORYZA_v3") %>%
#  left_join(params_oryza_lm, by = "Parameter") %>%
#  mutate(Base = map2(Base.x, Base.y, function(x, y) if(is.null(y)){x} else {y}), 
#         Min = map2(Min.x, Min.y, function(x, y) if(is.null(y)){x} else {y}),
#         Max = map2(Max.x, Max.y, function(x, y) if(is.null(y)){x} else {y})) %>% 
#  dplyr::select(-contains(".x"), -contains(".y"))


#Map(`%in%`, tt$Base, tt$Base.y)




#save(test_params_oryza, file = "calibracion/test_params_oryza.RData")






































