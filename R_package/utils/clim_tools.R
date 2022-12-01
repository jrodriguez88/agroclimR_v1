#### Group of  functions to manipulate weather and climate data
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2021




### function to calculate solar radiation from sunshine hours or temperature data
# data must have "date" and 'sbri' (or 'tmax' and 'tmin') variable,
## A & B parameters from FAO, 
#Frère M, Popov GF. 1979. Agrometeorological crop monitoring and forecasting. Plant
#Production Protection Paper 17. Rome: Food and Agricultural Organization.
#64 p.
# kRs adjustment coefficient (0.16.. 0.19) -- for interior (kRs = 0.16) and coastal (kRs = 0.19) regions
srad_cal <- function(data, lat, sh_name =  "sbri", A = 0.29, B = 0.45, kRs = 0.175, fill = F){
  
  stopifnot(require(sirad))
  
  
  if (sh_name %in% colnames(data)){
    
    data <- data %>% 
      mutate(
        extraT = extrat(lubridate::yday(date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
        srad = ap(date, lat = lat, lon = lon,    # aqui aplica Angstrom-Prescott
                  extraT, A, B, sbri))
    
    
    
    message("Method for estimate Solar Radiation: Angstrom-Prescott (A-P) Model")
    
  } else if (all(c("tmax", "tmin") %in%  colnames(data))){
    
    data <- mutate(data, sbri = NA_real_)   #Aqui crea la variable brillo por si no existe
    message("Method for estimate Solar Radiation: Hargreaves Model")
    
    data <- data %>% 
      mutate(
        extraT = extrat(lubridate::yday(date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
        srad = kRs*sqrt(tmax - tmin)*extraT) ## kRs adjustment coefficient (0.16.. 0.19) -- for interior (kRs = 0.16) and coastal (kRs = 0.19) regions
    
  } else {
    
    message("No data to calculate Solar Radiation!.")
    
  }
  
  
  
  
  
  if (isTRUE(fill)) {
    
    max_srad <- mean(data$extraT)*0.80     # calcula el maximo teorico de radiacion
    
    data <- data %>% 
      mutate(
        srad = if_else(is.na(srad), kRs*sqrt(tmax - tmin)*extraT, srad),
        srad = if_else(srad>max_srad|srad<0|is.na(srad),  median(step1$srad, na.rm = T), srad))
    
    
  }
  
  
  return(pull(data, srad))   # retorna la radiacion en MJ/m2*dia
  
  
}



#Function to impute missing values by monthly mean
impute_mean_wth <- function(wth_data, temp = "daily"){
  
  stopifnot(require(naniar))
  
  if(temp == "daily"){
    
    imp_data <- wth_data %>% 
      mutate(year = year(date), month = month(date)) %>% 
      nest(data = -c(year, month)) %>%
      mutate(data = map(data, ~.x %>% 
                          impute_mean_at(.vars = vars(-date)))) %>% 
      unnest(data) %>% dplyr::select(-c(year, month))
    
    
  } else if(temp == "monthly"){
    
    imp_data <- wth_data %>% 
      nest(data = -c(month)) %>%
      mutate(data = map(data, ~.x %>% 
                          impute_mean_at(.vars = vars(-year)))) %>% 
      unnest(data) %>% dplyr::select(c(year, month), everything())
    
  }
  
  
  return(imp_data)
  
  
}


daily_to_monthly <- function(wth_data, ...){
  
  
  var_names <- colnames(wth_data)
  
  #  stopifnot(require(lubridate))
  stopifnot(class(wth_data$date)=="Date")
  
  #  wth_vars <- var_names[var_names != "date"]
  
  
  wth_data %>% 
    group_by(year = year(date), month = month(date)) %>%
    summarise(
      across(matches("rain|prec"), sum, ...), 
      across(!matches("rain|prec"), mean, ...), .groups = 'drop') %>%
    dplyr::select(-c(date))
  
  
}
#daily_to_monthly(a, na.rm = T)


## Bias correction for tmax, tmin  and Ratio correction for rain
#https://core.ac.uk/download/pdf/42720291.pdf
#var = c("tmax", "tmin", "rain")

remote_data_correction <- function(obs_data, target_data, wth_vars = c("tmax", "tmin")) {
  
  # varsW  = colnames(obs_data)
  
  obs <-  daily_to_monthly(obs_data, na.rm = T) %>% group_by(month) %>% 
    summarise(across(!matches("year|month"), mean, na.rm = T))
  
  sim <- daily_to_monthly(target_data, na.rm = T) %>% 
    dplyr::select(year, month, everything()) %>% group_by(month) %>% 
    summarise(across(!matches("year|month"), mean, na.rm = T))
  
  
  monthly_diff <- left_join(obs, sim, by = "month") %>% 
    mutate(tmax = tmax.x - tmax.y,
           tmin = tmin.x - tmin.y,
           rain = case_when("rain" %in% wth_vars ~ rain.x/rain.y,
                            TRUE ~ 1)) %>%
    dplyr::select(month, tmax, tmin, rain) %>%
    pivot_longer(cols = -c(month), names_to = "var", values_to = "corr")
  
  
  target_data %>% 
    mutate(month = month(date)) %>% 
    pivot_longer(cols = -c(date, month), names_to = "var") %>%
    left_join(monthly_diff, by = c("month", "var")) %>%
    mutate(value = case_when(var == "tmax" | var == "tmin" ~ value + corr,
                             var == "rain" ~ value*corr, 
                             TRUE ~ value)) %>%
    dplyr::select(-c(month, corr)) %>%
    pivot_wider(names_from = var)
  
  
}
#remote_data_correction(d, a)



cal_metrics_wth <- function(obs_data, sim_data, time = "monthly"){
  
  if(time == "monthly"){
    
    wth_data_test <- left_join(
      daily_to_monthly(obs_data), 
      daily_to_monthly(sim_data), 
      by = c("year", "month")) %>%
      pivot_longer(-c(year, month), names_to = "var")
    
  } else if(time == "daily") {
    
    wth_data_test <- left_join(
      obs_data, 
      sim_data, 
      by = c("date")) %>%
      pivot_longer(-c(date), names_to = "var")
    
  }
  
  
  wth_data_test %>%
    mutate(source = case_when(str_detect(var, "[.x]$") ~ "obs",
                              str_detect(var, "[.y]$") ~ "sim")) %>%
    drop_na() %>%
    mutate(
      var = str_sub(var, 1, -3)) %>%
    pivot_wider(names_from = source) %>% nest(-var) %>%
    mutate(metrics = map(data, ~get_metrics(.x))) %>%
    unnest(metrics)
  
  
  
}


plot_na_wth  <- function(id, wth_data) {
  
  
  vis_miss(dplyr::select(wth_data, -date), warn_large_data = F) +
    #  facet_wrap(id ~.)
    labs(title = id) +
    #       subtitle = paste0(city, " (", dpto, ") "),
    #       caption = "Source: IDEAM") +
    scale_y_continuous(breaks = seq(0, length(wth_data$date), by = 365*5), 
                       labels = cut.Date(wth_data$date, breaks = "5 years") %>% 
                         unique() %>% year()) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 0))
  
  
}


###Join Data base and impute with NASA - CHIRPS corrected
join_data <- function(obs_data, fill_data, iyear = 1990, fyear = 2019){
  
  date_control <- seq.Date(make_date(iyear), make_date(fyear, 12, 31), by = "days") %>%
    enframe(value = "date", name = NULL) 
  
  list_data <- list(date_control, obs_data, fill_data) %>%  
    reduce(left_join, by="date") 
  
  
  
  list_data %>%
    mutate(tmin = if_else(is.na(tmin.x), tmin.y, tmin.x),
           tmax = if_else(is.na(tmax.x), tmax.y, tmax.x),
           rain = if_else(is.na(rain.x), rain.y, rain.x)) %>%
    dplyr::select(date, rain, tmax, tmin, srad, wspd, rhum) %>%
    impute_mean_wth()
  
  
  
}


plot_wth_dat <- function(wth_data, station = NULL){
  
  if(!is.null(station)){
    wth_data <- mutate(wth_data, station = station)
  }
  
  wth_data %>%
    pivot_longer(cols = c(tmax, tmin, rain), names_to = "var") %>%
    ggplot(aes(date, value)) +
    geom_line(aes(color = var)) +
    facet_grid(var ~ station, scales = "free") +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_colour_manual(values = c("blue", "red", "orange"))
  
}

basic_qc <- function(df, fill=NA_real_, IQR_times = 5, max_temp_diff = 15, by_var = T){
  
  # change col data 
  if(any(str_detect(colnames(df), "Date"))){df <- rename(df, date = Date)}
  
  
  ## detect column names 
  var_names <- colnames(df)
  
  
  # detect consecutive values
  detect_consecutive <- function(data, n, exclude_zero = F){
    
    repet <- rle(data)
    
    if(exclude_zero == F){
      rep(repet$lengths >= n , times = repet$lengths)
    } else {
      rep(repet$lengths >= n & repet$values != 0, times = repet$lengths)
    }
    
    
  }

  # detect maximun temperature diference by day  
  detect_difference <- function(data, max_diff = max_temp_diff){
    
    c(0, abs(diff(data))) >= max_diff
    
  }
  
  
  
  if (class(df$date)=="Date" & all(c("tmax", "tmin", "rain") %in%  var_names)  & !isTRUE(by_var)){
    
    ## Calcula ranges by variable
    dat <- df
    
    range_tmin <- boxplot(dat$tmin~month(dat$date), plot = F, range = IQR_times)$stats
    range_tmax <- boxplot(dat$tmax~month(dat$date), plot = F, range = IQR_times)$stats
    #  range_rhum <- boxplot(dat$rhum~month(dat$date), plot = F, range = IQR_times)$stats
    #range_sbright <- boxplot(dat$sbright~month(dat$date), plot = F, range = IQR_times + 2)$stats
    
    
    ## base tb
    dat <- dat %>%
      mutate(tmin_min = range_tmin[1,month(date)],
             tmin_max = range_tmin[5,month(date)], 
             tmax_min = range_tmax[1,month(date)],
             tmax_max = range_tmax[5,month(date)]) %>% 
      #           rhum_min = range_rhum[1,month(date)],
      #           rhum_max = range_rhum[5,month(date)]) %>%
      #              sbright_min = range_sbright[1,month(date)],
      #              sbright_max = range_sbright[5,month(date)]) %>%
      mutate(tmax_consecutive = detect_consecutive(tmax, 5),
             tmax_difference = detect_difference(tmax, 15),
             tmin_consecutive = detect_consecutive(tmin, 5),
             tmin_difference = detect_difference(tmin, 15),
             rain_consecutive = detect_consecutive(rain, 5, T),
             tmax2 = case_when(tmax>48|tmax<15 ~ fill,
                               tmax > tmax_max|tmax < tmax_min ~ fill,
                               tmax < tmin ~ fill,
                               tmax == tmin ~ fill, 
                               tmax_consecutive == T ~ fill,
                               tmax_difference == T ~ fill,
                               TRUE ~ tmax),
             tmin2 = case_when(tmin>35|tmin<10 ~ fill,
                               tmin > tmin_max|tmin < tmin_min ~ fill,
                               tmin > tmax ~ fill,
                               tmin == tmax ~ fill,
                               tmin_consecutive == T ~ fill,
                               tmin_difference == T ~ fill,
                               TRUE ~ tmin),
             rain = case_when(rain>150|rain<0 ~ fill,
                              rain_consecutive == T ~ fill,
                              TRUE ~ rain),
             #         srad = if_else(srad>32|srad<4,  fill, srad),
             tmin = tmin2,
             tmax = tmax2) %>%
      dplyr::select(date, tmax, tmin, rain)
    
    
  
    
    } else if (class(df$date)=="Date" & any(str_detect(var_names, "rain|prec"))) {
      
      dat <- df
    
    # change col data 
      if(any(str_detect(colnames(dat), "prec"))){dat <- rename(dat, rain = prec)}
    
    ## base tb
    dat <- dat %>%
      mutate(rain_consecutive = detect_consecutive(rain, 5, T),
             rain = case_when(rain>150|rain<0 ~ fill,
                              rain_consecutive == T ~ fill,
                              TRUE ~ rain)) %>%
      dplyr::select(date, rain)
    
  
    } else if (class(df$date)=="Date" & any(str_detect(var_names, "tmax")) & isTRUE(by_var)) {
    
      dat <- df
      
     # range_tmin <- boxplot(dat$tmin~month(dat$date), plot = F, range = IQR_times)$stats
      range_tmax <- boxplot(dat$tmax~month(dat$date), plot = F, range = IQR_times)$stats
      #  range_rhum <- boxplot(dat$rhum~month(dat$date), plot = F, range = IQR_times)$stats
      #range_sbright <- boxplot(dat$sbright~month(dat$date), plot = F, range = IQR_times + 2)$stats
      
      
      ## base tb
      dat <- dat %>%
        mutate(tmax_min = range_tmax[1,month(date)],
               tmax_max = range_tmax[5,month(date)]) %>% 
        #           rhum_min = range_rhum[1,month(date)],
        #           rhum_max = range_rhum[5,month(date)]) %>%
        #              sbright_min = range_sbright[1,month(date)],
        #              sbright_max = range_sbright[5,month(date)]) %>%
        mutate(tmax_consecutive = detect_consecutive(tmax, 5),
               tmax_difference = detect_difference(tmax, 15),
               tmax2 = case_when(tmax>48|tmax<15 ~ fill,
                                 tmax > tmax_max|tmax < tmax_min ~ fill,
                                 #tmax < tmin ~ fill,
                                 #tmax == tmin ~ fill, 
                                 tmax_consecutive == T ~ fill,
                                 tmax_difference == T ~ fill,
                                 TRUE ~ tmax),
               tmax = tmax2) %>%
        dplyr::select(date, tmax)
    
    
    
    
    
    
    
  
    } else if (class(df$date)=="Date" & any(str_detect(var_names, "tmin")) &  isTRUE(by_var) ) {
      
      dat <- df
      
      range_tmin <- boxplot(dat$tmin~month(dat$date), plot = F, range = IQR_times)$stats
       #  range_rhum <- boxplot(dat$rhum~month(dat$date), plot = F, range = IQR_times)$stats
      #range_sbright <- boxplot(dat$sbright~month(dat$date), plot = F, range = IQR_times + 2)$stats
      
      
      ## base tb
      dat <- dat %>%
        mutate(tmin_min = range_tmin[1,month(date)],
               tmin_max = range_tmin[5,month(date)]) %>% 
        #           rhum_min = range_rhum[1,month(date)],
        #           rhum_max = range_rhum[5,month(date)]) %>%
        #              sbright_min = range_sbright[1,month(date)],
        #              sbright_max = range_sbright[5,month(date)]) %>%
        mutate(tmin_consecutive = detect_consecutive(tmin, 5),
               tmin_difference = detect_difference(tmin, 15),
               tmin2 = case_when(tmin>35|tmin<10 ~ fill,
                                 tmin > tmin_max|tmin < tmin_min ~ fill,
                                 #tmin > tmax ~ fill,
                                 #tmin == tmax ~ fill,
                                 tmin_consecutive == T ~ fill,
                                 tmin_difference == T ~ fill,
                                 TRUE ~ tmin),
               tmin = tmin2) %>%
        dplyr::select(date, tmin)
      
      
      
      
      
      
      
      
    } else {print("no data")}
  
  
  
  return(dat)
  
  
}
