### Script to evaluale paramietrization


# Arguments
#path_data <- "data/DATA_FINAL/"
#cultivar = "F2000"
#files <- list.files(path_data, pattern = fixed(cultivar))
#require(eval.R script)
## Funcion para importar datos experimentales/observados organizados en el arquivo-template INPUT_data.xlsx
import_exp_data <- function(path_data, INPUT_data_files, cultivar, model = "oryza"){
  
  # listar los archivos o listas de experimentos disponibles en la carpeta de datos
  
  # files_cultivar <- list.files(path, pattern = fixed(cultivar))
  
  ## Importa datos de trabajo
  data <- INPUT_data_files %>% 
    enframe(name = NULL, value = "file") %>%
    mutate(loc_cul = str_sub(file, 1,-6)) %>% 
    separate(col = loc_cul, into = c("site", "cultivar"), sep = "_") %>%
    mutate(input_data =  map(file, ~read_INPUT_data(paste0(path_data, .))))
  
  ## Extrae datos de suelo
  soil <- data %>% mutate(soil_data = map(input_data, ~.x$SOIL_obs))  %>% dplyr::select(-input_data)  %>% 
    unnest(soil_data) %>%
    mutate(LOC_ID = str_sub(ID, 1, 4)) %>% group_by(LOC_ID, NL) %>%
    summarize_if(is.numeric, .funs = mean_boot) %>%
    mutate(ID=LOC_ID, STC=get_STC(SAND, CLAY)) %>% ungroup() %>%  rename_with(.fn = function(x) str_replace(x, "SC", "SOC")) %>% split(.$ID) %>% 
    enframe(name = "site", value = "soil")
  
  
  ## Extrae datos de clima
  
  wth <- data %>% mutate(ws_id = 1, wth = map(input_data, ~.x$WTH_obs )) %>% 
    select(site, ws_id, wth) %>% 
    #  group_by(localidad) %>% slice(1) %>% 
    unnest(wth) %>% 
    mutate(DATE = as.Date(DATE)) %>%
    #  mutate(wspd = suppressWarnings(as.numeric(WVEL))) %>% ###### si existen ?
    set_names(tolower(names(.))) %>% 
    #  dplyr::select(-wvel) %>%
    dplyr::distinct() %>% 
    nest(wth = -c(site, ws_id)) %>% 
    left_join(
      
      data$input_data %>% map("AGRO_man") %>% bind_rows() %>% 
        dplyr::select(LOC_ID, contains("LAT"), contains("LONG"), starts_with("A")) %>% 
        distinct() %>% set_names(c("site", "lat", "lon", "elev")) %>%
        group_by(site) %>% slice(1) %>% 
        ungroup(),
      by = "site") %>% rename(stn = ws_id) #%>% 
  #  mutate(path = "data/OUTPUTS/WTH/") %>%
  #  select(path, id_name, wth_data, lat, lon, elev, stn)
  
  # Extract and plot - Growth and Development observed data by component
  
  # Crop Phenology
  phen <- extract_obs_var(data$input_data, "phen")
  #  plot_phen_obs(phen) %>% ggplotly()
  
  #Leaf Area Index
  lai <- extract_obs_var(data$input_data, "lai")
  #  plot_lai_obs(lai) %>% ggplotly() 
  
  #Shoot Dry Matter
  dry_matter <- extract_obs_var(data$input_data, "dry_matter")
  #  plot_drymatter_obs(dry_matter) %>% ggplotly()
  
  #Yield dry matter
  yield <- extract_obs_var(data$input_data, "yield")
  #  plot_yield_obs(yield) %>% ggplotly()
  
  
  
  # crea lista de datos extraidos
  data_list <- list(
    
    data =  data %>% left_join(soil, by = "site") %>% left_join(wth, by = "site"),
    
    phen =  phen, lai = lai, dry_matter = dry_matter, yield = yield
    
  )
  
  
  
  
  return(data_list)
  
  
}
# Test function 
#test_data <- import_exp_data(path_data, files, cultivar)

#Arguments
#path_proj <- "D:/00_DEVELOPER/oryza_2022/test_oryza1"
#path_data <- "data/DATA_FINAL/"
#cultivar = "F2000"
#files <- list.files(path_data, pattern = fixed(cultivar))
#test_data <- import_exp_data(path_data, files, cultivar)
#Funcion para escribir los archivos climaticos, suelo, y experimental a partir de INPUT_data en un directorio especifico 
write_files_oryza <- function(path_proj, test_data){
  
  
  ### Directorio de salidas (OUTPUTS)
  dir.create(path_proj)
  
  ##Crear Archivos climaticos
  dir_wth <- paste0(path_proj, "/WTH/")
  dir.create(dir_wth)
  
  test_data$data %>% 
    mutate(path = dir_wth , id_name = site) %>%
    dplyr::select(path, id_name, wth_data = wth, lat, lon, elev, stn) %>%
    mutate(wth_data = map(wth_data, ~.x %>% impute_mean_wth),
           pwalk(., write_wth_oryza, multiyear = F, tag = F))
  
  
  #crear archivos experimentales
  dir_exp <- paste0(path_proj, "/EXP/")
  dir.create(dir_exp, showWarnings = T)
  map(test_data$data$input_data, ~write_exp_oryza(.x, dir_exp, ET_mod = "PRIESTLY TAYLOR"))
  
  #crear archivos suelo
  dir_soil <- paste0(path_proj, "/SOIL/")
  dir.create(dir_soil, showWarnings = T)
  
  soil_data <- test_data$data$soil %>% 
    map(~.x %>%
          mutate(SOC = case_when(LOC_ID == "YOCS" ~ SOC/5,
                                 TRUE ~ SOC),
                 OM = (100/58)*SOC/10) %>% 
          mutate(SSKS = pmap_dbl(.l = list(SAND, CLAY, OM, SBDM), ~SSKS_cal(SAND, CLAY, OM, SBDM))*240))
  
  
  map2(soil_data, test_data$data$site,  ~write_soil_oryza(dir_soil, .y, .x, SATAV = 25, RIWCLI = 'NO'))
  
  
  
  
}

#Arguments
#path_proj <- getwd() %>% paste0("/_AQUACROP")
#test_data <- import_exp_data(path_data, files, cultivar)
#unlink(path_proj, recursive = T)

write_files_aquacrop <- function(path_proj, test_data, cultivar){
  
  
  ### Directorio de salidas (OUTPUTS)
  dir.create(path_proj)
  
  ##Crear Archivos climaticos
  dir_wth <- paste0(path_proj, "/WTH/")
  dir.create(dir_wth)
  
  test_data$data %>% 
    mutate(path = dir_wth , id_name = site) %>%
    dplyr::select(path, id_name, wth = wth, lat, lon, elev) %>%
    mutate(wth = map(wth, ~.x %>% impute_mean_wth)) %>%
    pmap(., write_wth_aquacrop)
  
  
  
  #crear archivos suelo
  dir_soil <- paste0(path_proj, "/SOIL/")
  dir.create(dir_soil, showWarnings = T)
  #test_data$data$input_data[[1]]$Metadata %>% View %>%filter(VAR_NAME == "SC")
  
  ## transform data to aquacrop format
  test_data$data %>% unnest(soil) %>% 
    mutate(SOC = case_when(LOC_ID == "YOCS" ~ SOC/5,
                           TRUE ~ SOC)) %>% 
    mutate(Penetrability = 100, 
           TKL = c(DEPTH/100),
           bdod = SBDM, Gravel = 0,
           OM = (100/58)*SOC/10, # Organic matter (%) = Total organic carbon (%) x 1.72
           SSKS = pmap_dbl(.l = list(SAND, CLAY, OM, SBDM), ~SSKS_cal(SAND, CLAY, OM, SBDM))*24,
           CRa = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-0.3112 - SSKS*10^(-5)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-0.4986 + SSKS*9*10^(-5)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-0.5677 - SSKS*4*10^(-5)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-0.6366 + SSKS*8*10^(-4))),
           CRb = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-1.4936 + 0.2416*log(SSKS)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-2.1320 + 0.4778*log(SSKS)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-3.7189 + 0.5922*log(SSKS)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-1.9165 + 0.7063*log(SSKS)))) %>%
    dplyr::select(id_name = site, TKL, WCST, WCFC, WCWP, SSKS, Penetrability, Gravel, CRa, CRb, STC) %>%
    setNames(c("id_name", "Thickness", "Sat", "FC", "WP", "Ksat", "Penetrability", "Gravel", "CRa", "CRb", "description")) %>%
    nest(data = -c(id_name)) %>% mutate(map2(id_name, data, ~write_soil_aquacrop(dir_soil, .x, .y)))
  
  
  
  #crear archivos experimentales
  #funcion para remover separadores "_" de las variables a analizar
  remove_unders <- function(var){str_replace_all(var, "_", "")}
  
  # Crop Phenology
  phen <- test_data$phen
  #plot_phen_obs(phen) #%>% ggplotly()
  
  
  
  #Agronomic data - Plant populations
  agro_data <- test_data$data$input_data %>% map(~.x[["AGRO_man"]]) %>% bind_rows() %>%
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    mutate(PDAT = as.Date(PDAT), exp_file  = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>%
    dplyr::select(exp_file, PDAT:NPLDS) #%>% set_names(~tolower(.x))
  
  
  
  # Join data to parameter estimation
  data_param_aquacrop <- phen %>% dplyr::select(exp_file, data) %>% 
    dplyr::distinct() %>% 
    mutate(MDAT = map(data, ~.x %>% dplyr::filter(var == "MDAT") %>% pull(value))) %>%
    unnest(MDAT) %>% dplyr::select(-data) %>%
    #  dplyr::filter(exp_file %in% exp_filter) %>%
    mutate(id_name = word(exp_file, 1, sep = "_")) %>% 
    left_join(agro_data, by = join_by(exp_file)) %>% 
    dplyr::select(id_name, exp_file, PDAT, everything()) 
  
  
  
  data_param_aquacrop %>% 
    mutate(exp = pmap(list(path_proj = path_proj,
                           id_name = exp_file,
                           clim_name = id_name,
                           soil_name = id_name,
                           cultivar  = cultivar,
                           sowing_date = PDAT,
                           harvest_date = MDAT,
                           co2_name = "MaunaLoa",
                           irri_name = tolower(CROP_SYS),
                           man_agro = "rice"), write_exp_aquacrop))
  
  
  
  
  # write irrigation file 
  write_irri_aquacrop(path_proj)
  
  
  
  # write agronomic management file
  write_man_aquacrop(path_proj)
  
  
  
  
  
}




# Function to calculate evaluation metrics || 
# Must have observated and simulated data in columns"obs" and "sim"
get_metrics <- function(data) {
    
    data %>% filter(complete.cases(.)) %>%
        summarise(n = n(),
                  r = cor(obs, sim, method = c("pearson")),
                  tau = cor(obs, sim, method = c("kendall")),
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

## extract_sim_var function to extract simulation data by variable (phen, dry_matter, yield, lai)
extract_sim_oryza <- function(sim_data, exp_set, variable) {
    
    vars <- switch(variable, 
                   dry_matter = c("date", "DVS", "WAGT", "WLVG", "WST", "WSO", "WLVD"), 
                   lai = c("date", "DVS", "LAI"),
                   yield = c("date", "DVS", "WRR14"), 
                   phen = c("date", "DVS"))
    
    date_to_dae <- function(data) {
        
        edate <- data %>% filter(var == "EDAT") %>% pull(value)
        
        data %>% mutate(value = as.numeric(value - edate)) %>%
            dplyr::filter(var != "PDAT", var != "EDAT")
        
    }
    
    sim_dat <- sim_data %>%
        map(., ~dplyr::select(., one_of(vars))) %>% 
        set_names(str_sub(exp_set, 1, -5)) %>%
        bind_rows(.id = "exp_file") %>% drop_na()
    
    sim_select <- switch(variable, 
                         dry_matter = sim_dat %>% 
                             gather(var, value, -(1:3)), 
                         lai = sim_dat %>%
                             rename(value = LAI) %>%
                             mutate(var = "LAI") %>%
                             dplyr::select(exp_file, date, DVS, var, value),
                         yield = sim_dat %>%
                             group_by(exp_file) %>%
                             summarise(date = max(date), value = max(WRR14)) %>%
                             mutate(var = "YIELD") %>%
                             dplyr::select(exp_file, var, date, value), 
                         phen = sim_dat %>%
                             mutate(var = case_when(
                                 DVS == 0 ~ "EDAT",
                                 DVS >= 0.649 & DVS <=0.67 ~ "IDAT",
                                 DVS >= 1 & DVS <= 1.2 ~ "FDAT",
                                 DVS >= 2 ~ "MDAT")) %>% 
                             drop_na() %>%
                             nest(data = -c(exp_file, var)) %>%
                             mutate(ref = case_when(var == "EDAT" ~ 0.00,
                                                    var == "IDAT" ~ 0.65,
                                                    var == "FDAT" ~ 1.00,
                                                    var == "MDAT" ~ 2.00),
                                    date = map2_dbl(data, ref, ~.x %>% 
                                                    mutate(diff = DVS - .y) %>%
                                                    dplyr::slice(which.min(diff)) %>%
                                                    pull(date)) %>% as.Date(., origin = "1970-01-01")) %>%
                             dplyr::select(exp_file, var, date) %>% 
                             rename(value = date) %>% 
                             nest(data = -exp_file) %>% 
                             mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                             unnest(phen_dae))
                                    
    
    return(dplyr::as_tibble(sim_select))
    
    
}

### extract from base data
extract_obs_var <- function(obs_data, variable, model = NULL) {
        
        # vars select shet names required
        vars <- switch(variable, 
                       dry_matter = "PLANT_gro", 
                       lai = "PLANT_gro",
                       yield = "YIELD_obs", 
                       phen = "PHEN_obs")
        
        date_to_dae <- function(data) {
            
            edate <- data %>% filter(var == "EDAT") %>% pull(value)
            
            data %>% mutate(value = as.numeric(value - edate)) %>%
                dplyr::filter(var != "PDAT", var != "EDAT")
            
        }
        
    #   set <- exp_set %>%
    #       str_sub(1,-5) %>% enframe(name = NULL, value = "exp_file") %>%
    #       separate(exp_file, c("LOC_ID", "CULTIVAR","PROJECT", "TR_N"), sep = "_", remove = F) %>%
    #       mutate(ID = paste0(LOC_ID, TR_N, PROJECT))
        
        remove_unders <- function(var){str_replace_all(var, "_", "")}
        
        set <- obs_data %>% 
            map(., ~.[["AGRO_man"]]) %>% bind_rows() %>% 
            mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
            mutate(exp_file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>% 
            dplyr::select(c(ID,	exp_file, LOC_ID,	PROJECT,	CULTIVAR,	TR_N))
    
        
        
        
        
        obs_data2 <- obs_data %>%
            map(., ~.[[vars]]) %>%
            bind_rows() %>% 
            dplyr::select(-LOC_ID, -CULTIVAR) %>%
            nest(data = -c(ID)) %>% right_join(set, by= "ID") %>% unnest(data) %>%
            select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N)) 
        
        
        

        
        
        op <- switch(variable, 
                     dry_matter = obs_data2 %>%
                         mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>%
                         rename(date = SAMPLING_DATE) %>%
                         select(ID:WAGT_SE, exp_file, -contains("LAI")) %>% 
                         gather(var, value, -c(ID, exp_file, date)) %>%
                         separate(var, c("var", "metric"), sep = "_") %>%
                         spread(metric, value) %>% 
                         rename(value = OBS, se = SE) %>% 
                         dplyr::select(exp_file, date, var, value, se), 
                     lai = obs_data2 %>%
                         mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>% 
                         rename(date=SAMPLING_DATE) %>%
                         select(exp_file, date, contains("LAI")) %>% 
                         mutate(var = "LAI") %>%
                         rename(value=LAI_OBS, se=LAI_SE) %>%
                         dplyr::select(exp_file, date, var, value, se),
                     yield = obs_data2 %>%
                         dplyr::select(exp_file, YIELD_AVG, YIELD_MIN, YIELD_MAX) %>%
                         rename(value = YIELD_AVG, ymin = YIELD_MIN, ymax = YIELD_MAX) %>%
                         mutate(var = "YIELD", diff_min = value - ymin,
                                diff_max = ymax - value,
                                se = (diff_max+diff_min)/2) %>%
                         dplyr::select(exp_file, var, value, se),
                     phen = obs_data2 %>% 
                         dplyr::select(-ID) %>%
                         gather(var, value, -exp_file) %>%
                         mutate(value = as.Date(value)) %>%
                         nest(data = -exp_file) %>% 
                         mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                         unnest(phen_dae) %>%
                         mutate(value = if_else(var == "MDAT", value - 7 , value)))
        
        
        
        if(all(variable == "lai", model == "aquacrop")){
          
          phen_data <- obs_data %>%
            map(., ~.[["PHEN_obs"]]) %>%
            bind_rows() %>% left_join(set) %>% 
            dplyr::select(exp_file, IDAT, FDAT) %>% mutate(across(contains("DAT"), as.Date))
          
          
  
          # canopy cover data.frame
          # Convert CC = 1 - exp(-k*LAI))
          
        op <- op %>% na.omit() %>% nest(data = -exp_file) %>% left_join(phen_data) %>%
            mutate(
              data = pmap(list(data, IDAT, FDAT), function(a,b,c){
              a %>% 
                mutate(
                  k = case_when(
                    date <= b ~ 0.4,
                    date >= c ~ 0.6,
                    TRUE ~ 0.5),
                  canopy = (1 - exp(-k*value))*100,
                  se = (1 - exp(-k*se))*100)})) %>% unnest(data) %>%
            dplyr::select(exp_file, date, var, value = canopy, se)
          
       }
        
        
        
        
        return(op)
        
        
}


#sim_ <- extract_sim_var(sim_data, cal_set, variable = "dry_matter")
#obs_ <- extract_obs_var(data, cal_set, variable = "dry_matter")
#
#sim_ <- extract_sim_var(sim_data, cal_set, variable = "yield")
#obs_ <- extract_obs_var(obs_data, cal_set, variable = "yield")
#
#sim_ <- extract_sim_var(sim_data, cal_set, variable = "lai")
#obs_ <- extract_obs_var(obs_data, cal_set, variable = "lai")
#
#sim_ <- extract_sim_var(sim_data, cal_set, variable = "phen")
#obs_ <- extract_obs_var(obs_data, cal_set, variable = "phen")


## eval_sim_oryza fucntion to summarise and compute observed vs simulate values
eval_sim_oryza <- function(obs_data, sim_data, exp_set, variable = "phen", by_var = F) {
    
    sim_ <- extract_sim_var(sim_data, exp_set, variable = variable)
    
    obs_ <- extract_obs_var(obs_data, variable = variable) %>% 
      dplyr::filter(exp_file %in% str_remove(exp_set, ".exp"))
    
    
    id_join <- if(variable == "phen" | variable == "yield"){ 
        c("exp_file", "var")
    } else {
        c("exp_file", "var", "date")
    } 
    
    if(variable == "phen"){
        obs_ <- obs_ %>% dplyr::select(-contains("data"))
        sim_ <- sim_ %>% dplyr::select(-contains("data"))
        }
    
    
    test_select <- obs_ %>%
        left_join(sim_, by = id_join) %>% 
        rename_at(vars(ends_with(".x")), list(~paste("obs"))) %>%
        rename_at(vars(ends_with(".y")), list(~paste("sim"))) %>% 
        filter(complete.cases(.), obs > 0)
        
    
    if(by_var == TRUE){
        
        test_select %>%
        nest(data = -c(var)) %>% 
        mutate(eval = map(data, ~get_metrics(.x)),
               plot = map(data, ~.x %>% 
                              ggplot(aes(obs, sim)) +
                              geom_point(aes(color = exp_file)) +
                              expand_limits(x = 0, y = 0) + 
                              geom_abline(intercept = 0, slope = 1, linetype = "twodash", linewidth=1)+
                              geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", linewidth=0.5, color = "red") +
                              geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", linewidth=0.5, color = "red") + 
                              #geom_smooth(method = "lm", se=F)+
                              theme_bw())) %>% 
        unnest(eval)
    } else {
        
        test_select %>%
            get_metrics %>% 
            mutate(var  = variable,
                   plot = list(test_select%>% 
                                   ggplot(aes(obs, sim)) +
                                   geom_point(aes(color = exp_file)) +
                                   expand_limits(x = 0, y = 0) + 
                                   geom_abline(intercept = 0, slope = 1, linetype = "twodash", linewidth=1)+
                                   geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", linewidth=0.5, color = "red") +
                                   geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", linewidth=0.5, color = "red") + 
                                   #geom_smooth(method = "lm", se=F)+
                                   theme_bw())) %>% 
            dplyr::select(var, everything())
        
    }
    
    
 
}

#vars <- c("phen", "dry_matter", "lai",  "yield")
#metrics <- map(vars, ~eval_sim_aquacrop(input_data, sim_data, cal_set, .x, F)) %>% bind_rows()
#
#metrics %>% unnest(data) %>% 
#    mutate(locality = str_sub(exp_file, 1, 4)) %>%
#    ggplot(aes(obs, sim, color = locality)) + geom_point() +
#    expand_limits(x = 0, y = 0) + 
#    geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
#    geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
#    geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
#    facet_wrap(~var, scales = "free") + 
#    theme_bw()
##metrics %>% mutate(plot = map2(plot, var, ~.x + ggtitle(.y))) %>% pull(plot)








######

######   AQUACROP 



# Funcion copia inputs base en directorio de simulacion de cada setups
copy_inputs_aquacrop <- function(path_proj, basedata_path){
  
  # ruta con los archivos necesarios para 
  files_default <- list.files(basedata_path, recursive = T, full.names = T)
  
  file.copy(files_default, path_proj, recursive = T)
  
  
  dir.create(paste0(path_proj, "/OUTP"))
  dir.create(paste0(path_proj, "/SIMUL"))
  
  
  file.copy(list.files(path_proj, pattern = "CO2", full.names = T), paste0(path_proj, "/SIMUL/"))
  file.copy(list.files(path_proj, pattern = "DailyResults", full.names = T), paste0(path_proj, "/SIMUL/"))
  
  
}

#copy_inputs_aquacrop(path_proj, "_AQUACROP/")

## extract_sim_var function to extract simulation data by variable (phen, dry_matter, yield, lai)
extract_sim_aquacrop <- function(sim_data, exp_set, variable) {
  
  vars <- switch(variable, 
                 dry_matter = c("date", "Stage", "Biomass"), 
                 lai = c("date", "Stage", "CC" ),
                 yield = c("date", "Stage", "YieldPart"), 
                 phen = c("date", "Stage", "DAP"))
  

  
  sim_dat <- sim_data %>% map(., ~.x %>% mutate(date = make_date(Year, Month, Day))) %>%
    map(., ~dplyr::select(., one_of(vars))) %>% #set_names(exp_set) %>%
    bind_rows(.id = "exp_file") %>% filter(Stage>0) %>%
    dplyr::filter(exp_file %in% exp_set)
  
  
  date_to_dae <- function(data) {
    
    edate <- data %>% filter(Stage == 1) %>% pull(n)
    
    data %>% mutate(value = as.numeric(value - edate)) 
    
  }
  
  sim_select <- switch(variable, 
                       dry_matter = sim_dat %>% 
                         mutate(var = "WAGT", value = Biomass*1000) %>%
                         dplyr::select(exp_file, date, var, value), 
                       lai = sim_dat %>%
                         mutate(var = "LAI") %>%
                         dplyr::select(exp_file, date, var, value = CC),
                       yield = sim_dat %>%
                         group_by(exp_file) %>%
                         summarise(date = max(date), value = max(YieldPart)*1000) %>%
                         mutate(var = "YIELD") %>%
                         dplyr::select(exp_file, var, date, value), 
                       phen = sim_dat %>% group_by(exp_file, Stage) %>% 
                         dplyr::summarise(n = n(), 
                                          min_dap = min(DAP),
                                          max_dap = max(DAP)) %>% ungroup() %>%
                         mutate(var = case_when(
                                    Stage == 1 ~ "EDAT",
                                    Stage == 3 ~ "FDAT",
                                    Stage == 4  ~ "MDAT"),
                                value = case_when(
                                    var == "EDAT" ~ max_dap,
                                    var == "FDAT" ~ max_dap,
                                    var == "MDAT" ~ max_dap)
                                ) %>% 
                         drop_na() %>% 
                         nest(data = -exp_file) %>% 
                         mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                         unnest(phen_dae) %>% dplyr::filter(value>0) %>%
                         dplyr::select(exp_file, data, var, value))
  
  
  return(dplyr::as_tibble(sim_select))
  
  
}


## eval_sim_oryza fucntion to summarise and compute observed vs simulate values
eval_sim_aquacrop <- function(obs_data, sim_data, exp_set, variable = "phen", by_var = F){
  
  sim_ <- extract_sim_aquacrop(sim_data, exp_set, variable = variable)

  obs_ <- extract_obs_var(obs_data, variable = variable, model = "aquacrop") %>% 
    dplyr::filter(exp_file %in% str_remove(exp_set, ".exp"))


id_join <- if(variable == "phen" | variable == "yield"){ 
  c("exp_file", "var")
} else {
  c("exp_file", "var", "date")
} 

if(variable == "phen"){
  obs_ <- obs_ %>% dplyr::select(-contains("data"))
  sim_ <- sim_ %>% dplyr::select(-contains("data"))
}



test_select <- obs_ %>%
  left_join(sim_, by = id_join) %>% 
  rename_at(vars(ends_with(".x")), list(~paste("obs"))) %>%
  rename_at(vars(ends_with(".y")), list(~paste("sim"))) %>% 
  filter(complete.cases(.), obs > 0)


if(by_var == TRUE){
  
  test_select %>%
    nest(data = -c(var)) %>% 
    mutate(eval = map(data, ~get_metrics(.x)),
           plot = map(data, ~.x %>% 
                        ggplot(aes(obs, sim)) +
                        geom_point(aes(color = exp_file)) +
                        expand_limits(x = 0, y = 0) + 
                        geom_abline(intercept = 0, slope = 1, linetype = "twodash", linewidth=1)+
                        geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", linewidth=0.5, color = "red") +
                        geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", linewidth=0.5, color = "red") + 
                        #geom_smooth(method = "lm", se=F)+
                        theme_bw())) %>% 
    unnest(eval)
} else {
  
  test_select %>%
    get_metrics %>% 
    mutate(var  = variable,
           plot = list(test_select%>% 
                         ggplot(aes(obs, sim)) +
                         geom_point(aes(color = exp_file)) +
                         expand_limits(x = 0, y = 0) + 
                         geom_abline(intercept = 0, slope = 1, linetype = "twodash", linewidth=1)+
                         geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", linewidth=0.5, color = "red") +
                         geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", linewidth=0.5, color = "red") + 
                         #geom_smooth(method = "lm", se=F)+
                         theme_bw())) %>% 
    dplyr::select(var, everything())
  
}



}

