### Script to evaluale paramietrization


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
extract_sim_var <- function(sim_data, exp_set, variable) {
    
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
                             nest(-c(exp_file, var)) %>%
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
                             nest(-exp_file) %>% 
                             mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                             unnest(phen_dae))
                                    
    
    return(dplyr::as_tibble(sim_select))
    
    
}

### extract from base data
extract_obs_var <- function(obs_data, variable) {
        
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
    
        
        
        
        
        obs_data <- obs_data %>%
            map(., ~.[[vars]]) %>%
            bind_rows() %>% 
            dplyr::select(-LOC_ID, -CULTIVAR) %>%
            nest(data = -c(ID)) %>% right_join(set, by= "ID") %>% unnest(data) %>%
            select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N)) 
        
        
        op <- switch(variable, 
                     dry_matter = obs_data %>%
                         mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>%
                         rename(date = SAMPLING_DATE) %>%
                         select(ID:WAGT_SE, exp_file, -contains("LAI")) %>% 
                         gather(var, value, -c(ID, exp_file, date)) %>%
                         separate(var, c("var", "metric"), sep = "_") %>%
                         spread(metric, value) %>% 
                         rename(value = OBS, se = SE) %>% 
                         dplyr::select(exp_file, date, var, value, se), 
                     lai = obs_data %>%
                         mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>% 
                         rename(date=SAMPLING_DATE) %>%
                         select(exp_file, date, contains("LAI")) %>% 
                         mutate(var = "LAI") %>%
                         rename(value=LAI_OBS, se=LAI_SE) %>%
                         dplyr::select(exp_file, date, var, value, se),
                     yield = obs_data %>%
                         dplyr::select(exp_file, YIELD_AVG, YIELD_MIN, YIELD_MAX) %>%
                         rename(value = YIELD_AVG, ymin = YIELD_MIN, ymax = YIELD_MAX) %>%
                         mutate(var = "YIELD", diff_min = value - ymin,
                                diff_max = ymax - value,
                                se = (diff_max+diff_min)/2) %>%
                         dplyr::select(exp_file, var, value, se),
                     phen = obs_data %>% 
                         dplyr::select(-ID) %>%
                         gather(var, value, -exp_file) %>%
                         mutate(value = as.Date(value)) %>%
                         nest(data = -exp_file) %>% 
                         mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                         unnest(phen_dae) %>%
                         mutate(value = if_else(var == "MDAT", value - 7 , value)))
        
        
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
    obs_ <- extract_obs_var(obs_data, exp_set, variable = variable)
    
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
        nest(-c(var)) %>% 
        mutate(eval = map(data, ~get_metrics(.x)),
               plot = map(data, ~.x %>% 
                              ggplot(aes(obs, sim)) +
                              geom_point(aes(color = exp_file)) +
                              expand_limits(x = 0, y = 0) + 
                              geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
                              geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
                              geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
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
                                   geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
                                   geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
                                   geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
                                   #geom_smooth(method = "lm", se=F)+
                                   theme_bw())) %>% 
            dplyr::select(var, everything())
        
    }
    
    
 
}

#vars <- c("phen", "dry_matter", "lai",  "yield")
#metrics <- map(vars, ~eval_sim_oryza(data, sim_data, cal_set, .x, F)) %>% bind_rows()
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

