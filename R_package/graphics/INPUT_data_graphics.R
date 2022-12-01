# Script to create Oryza plots
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2022


#library(tidyverse)
#library(plotly)
#library(Hmisc)

#Extract observed data by component

## Crop Phenology
#phen <- extract_obs_var(data$data, "phen")
#
##Leaf Area Index
#lai <- extract_obs_var(data$data, "lai")
#
##Shoot Dry Matter
#dry_matter <- extract_obs_var(data$data, "dry_matter")
#
##Yield dry matter
#yield <- extract_obs_var(data$data, "yield")


plot_phen_obs <- function(phen_data){
  
  
  phen_ <- phen %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))
  
  plot_phen <- phen_ %>% mutate(locality = str_sub(exp_file, 1, 4),
                                var = factor(var, levels = c("IDAT", "FDAT", "MDAT"))) %>%
    ggplot(aes(var, value, label = exp_file)) +
    geom_jitter(aes(color = locality), size = 2) +
    facet_grid(~var, scales = "free_x") +
    stat_summary(fun.data  = mean_cl_normal) +
    theme_bw() + 
    theme(
      axis.text.x = element_blank(),
      legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) +
    expand_limits(y = 0) +
    labs(title = paste0("Crop Phenology - Cultivar ", phen_$cultivar[[1]]), 
         x = "Development Stage",
         y = "Days After Emergence (days)",
         color = "Site: ")
  
  return(plot_phen)
  
}

#plot_phen_obs(phen)
#ggplotly()


plot_lai_obs <- function(lai_data){
  
  
  lai_ <- lai %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))
  
  plot_lai <- lai_  %>% 
    ggplot(aes(date, value, label = exp_file)) + 
    geom_smooth(aes(y = value), color = "darkgreen" , linetype = "twodash", fill = "lightgray") +
    geom_point(aes(color = locality)) +
    geom_errorbar(aes(ymin = value - se, ymax = value + se, color = locality)) +
    #  geom_point(aes(y = value), color = "black") + ylim(0, 10) +
    theme_bw() + facet_wrap(~exp_file, scales = "free_x") +
    theme(
      #        axis.text.x = element_blank(),
      legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) +
    labs(title = paste0("Leaf Area Index - Cultivar ", lai_$cultivar[[1]]), 
         x = "Date",
         y = "LAI (m²/m²)",
         color = "Site: ") +
    ylim(0, 10)
  
  return(plot_lai)
  
}

#plot_lai_obs(lai) 
#ggplotly()  


plot_drymatter_obs <- function(dry_matter_data){
  
  dry_matter_ <- dry_matter_data %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))
  
  plot_dry_matter <- dry_matter_ %>% 
    ggplot(aes(date, value, label = exp_file)) + 
    #    geom_smooth(aes(y = sim, color = locality), linetype = "twodash", fill = "lightgray") +
    geom_point(aes(color = var)) +
    geom_errorbar(aes(ymin = value - se, ymax = value + se, color = var)) +
    geom_smooth(aes(y = value, color = var), se = F) + 
    facet_wrap(~exp_file, scales = "free_x") + 
    theme_bw() +
    theme(
      #        axis.text.x = element_blank(),
      legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) + ylim(0, max(dry_matter_$value)+1000) +
    labs(title = paste0("Rice shoot dry matter - Cultivar ", dry_matter_$cultivar[[1]]), 
         x = "Date",
         y = "Dry matter (Kg/ha)",
         color = "Crop organs: ")
  
  return(plot_dry_matter)
  
}

#plot_drymatter_obs(dry_matter)


plot_yield_obs <- function(yield_data){
  
  
  yield_ <- yield_data %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))
  
  plot_yield <-  yield_ %>%
    ggplot(aes(exp_file, value, label = exp_file)) + 
    geom_point() +
    geom_errorbar(aes(ymin = value - se, ymax = value + se, color = locality), width = .2, position = position_dodge(0.2)) +
    geom_point(aes(y = value, color = locality)) + 
    #    geom_vline(xintercept = mean(obs)) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45),
      #legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) +
    expand_limits(y = 0) +
    labs(title = paste0("Rice Yield - Cultivar ", yield_$cultivar[[1]]) , 
         x = "Experimental file",
         y = "Yield (Kg/ha - 14% grain moisture )",
         color = "Site: ")
  
  return(plot_yield)
  
  
}