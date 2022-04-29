# Calulate ORYZA CRP params to write CRP file
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022

#
#list_params =  output of "extract_drates_param" function
tidy_params_oryza <- function(list_params, method = 1){
  
  if(method == 1){
    
    
    ### Method 1. According to ORYZA_V3_User_Manual_2014
    ### Apply filters by DVSM 
    PF_tb <- list_params$BPART_df %>% 
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
    
    
  } else if (method == 2){
    
    ### Method 2. Grafical analysis, logical-physiological filters
    PF_tb <- list_params$BPART_df %>%
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
         # Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<1 & Value>0.65   ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<0.75 & Value<0.4 ~ NA_real_,
          Partition_Parameter == "FST" & DVSM>1 & Value>0.5    ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<1 & Value>0.80   ~ NA_real_,
          Value< 0 ~ 0,
          Value>1 ~ 1,
          TRUE ~ Value))
    } else {} 
  
  
##### tidy Specific LEaf Area
SLA_df <- list_params$LAI_df %>%
    select(exp_file, DVS, LAI) %>% na.omit() %>%
    left_join(list_params$BMASS_df) %>%
    mutate(SLA = LAI/WLVG, Value = SLA) %>%
    filter(SLA < 0.0045) %>%
    dplyr::rename(DVSM=DVS)


# DVR_plots
DVR_tb <- list_params$DVR_df %>%
  pivot_longer(cols = -exp_file, names_to = "DVR", values_to = "Value") %>%
  filter(Value<0.0046) %>% #DVR!= "DVRI", 
  mutate(DVR=factor(DVR, c("DVRJ", "DVRI", "DVRP", "DVRR"))) 


 return(list(DVR_tb = DVR_tb, PF_tb = PF_tb, SLA_tb = SLA_df))

}

#tidy_params_oryza(params)




bootstrap_params <- function(param_data, param, reps = 1000){
  
  # Generate bootstrap distribution of medians
  boots <- data %>%
    # Specify the variable of interest
    specify(response = get(param)) %>%  
    # Generate 15000 bootstrap samples
    generate(reps = reps, type = "bootstrap") %>% 
    # Calculate the median of each bootstrap sample
    calculate(stat = "mean") 
  
  
  
}


### Specifif Leaf Area - Maximun 
# SLA_df 
SLA_max <- function(SLA_df, fr=0.50) {
  SLA_df %>%
    filter(DVSM<fr) %>%
    lm(SLA~DVSM, data = .) %>%
    summary() %>%
    .$coefficients %>% as_tibble() %>%
    mutate(par=Estimate+1.96*`Std. Error`) %>%
    .$par %>%
    .[1]
}


## Function to Calculate Fraction of carbohydrates allocated to stems that is stored as reserves
FSTR_cal <- function(FSTR_df, default = 0.2){
  
  FSTR <- mean_boot(FSTR_df$FSTR)[[1]]
  
  
  if (FSTR < 0.14) {
    
    message(paste("FSTR calculated:", FSTR, "- is a low value.", "- Use default"))
    return(default)
    
  } else if (FSTR > 0.26){
    
    message(paste("FSTR calculated:", FSTR, "- is a high value.", "- Use default"))
    return(default)
    
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
WGRMX_cal <- function(INPUT_data, default = 0.000025){
  
  data <- INPUT_data %>% map("YIELD_obs") %>% bind_rows()
  
  wgrmx <- mean_boot(data$GW1000)[[1]] %>% round(3)
  
  #(kg grain-1)
  WGRMX <-  wgrmx/(1000000)
  
  if (WGRMX < 0.000019) {
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a low value.", "- Use default"))
    return(default)
    
  } else if (WGRMX > 0.000040){
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a high value.", "- Use default"))
    return(default)
    
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
SPGF_extract <- function(INPUT_data, max_diff = 5) {
  
  
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
SPGF_cal <- function(SPGF_df, out="", default = 45000) {
  
  
  SPGF_df <- SPGF_df %>% filter(ID != out)
  
  
  ## Linear model between Spikelet number (number/ m²) and crop growth between panicle initiation and flowering (g/m²)
  lm_spgf <- lm(formula = SPIK_M2_max ~ diff_pi_fl, data = SPGF_df)
  
  ## Print SPGF compute from lm-slope
  spgf <-  sprintf("%.1f", coef(lm_spgf)[[2]]*1000) # Spikelet growth factor (no kg-1)"))
  

  
  if (as.numeric(spgf)<35000) {
    
    message(paste("SPGF calculated:", spgf, "- is a low value.", "- Use default"))
    return(default)
    
  } else if (as.numeric(spgf)> 65000){
    
    message(paste("SPGF calculated:", spgf, "- is a high value.", "- Use default"))
    return(default)
    
  } else {
    
    message(paste("SPGF calculated:", spgf, "- is a normal value.", "- Use data"))
    return(as.numeric(spgf))
    
  }
  
}


# Function to get mean, minimun and maximun partition factor tables
loess_crp_oryza <- function(data, DVS, span=0.5, nse=4) {
  
  #data convert to list
  if(any(str_detect(names(data), "SLA"))){
    SLA_max = SLA_max(data)
    data = list(SLA=data)
  } else {data=split(data, data$Partition_Parameter)}
  
  
  
  # Function to create crp tb
  crp_pf_tb <- function(data) {
    
    data %>% bind_rows(.id = "Partition_Parameter") %>%
      mutate(
        Value=case_when(
          Partition_Parameter != "FSO" & DVS == 0 ~ 0.5,
          Partition_Parameter == "FSO" & DVS > 1.5 ~ 1,
          Partition_Parameter == "FSO" & DVS < 0.75 ~ 0,
          Value<0 | is.na(Value) ~ 0,
          TRUE ~ Value)) %>%
      spread(Partition_Parameter, Value) %>%
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
      gather(Partition_Parameter, Value, -1) %>%
      mutate(Partition_Parameter = factor(Partition_Parameter,
                                          c("FLV", "FST", "FSO"))) %>%
      select(Partition_Parameter, DVS, Value)
    
  }
  
  crp_sla_tb <- function(data, SLA_max) {
    
    data %>% bind_rows() %>%
      mutate(
        Value=case_when(
          DVS == 0 ~ SLA_max,
          Value < 0 | is.na(Value) ~ min(Value, na.rm = T),
          TRUE ~ Value))
    
  }
  
  
  
  
  #Loess model by Partition factor    
  mod1 <- lapply(data, function(x){loess(Value~DVSM, data = x, span = span)}) %>%
    #predicted_list
    lapply(., function(x){predict(x, newdata = DVS, se=T)})
  
  #Cal mean
  PTB_crp_mean <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                      Value=x$fit)})
  #Cal min
  PTB_crp_min <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                     Value=x$fit+nse*x$se.fit)})
  #Cal_max
  PTB_crp_max <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                     Value=x$fit-nse*x$se.fit)})
  
  
  if(any(str_detect(names(data), "SLA"))){
    crp_list <- list(
      mean = crp_sla_tb(PTB_crp_mean, SLA_max),
      min =  crp_sla_tb(PTB_crp_max, SLA_max),
      max =  crp_sla_tb(PTB_crp_min, SLA_max))
  } else {
    crp_list <- list(
      mean = crp_pf_tb(PTB_crp_mean),
      min = crp_pf_tb(PTB_crp_min),
      max = crp_pf_tb(PTB_crp_max))}
  
  
  return(crp_list)
  
}