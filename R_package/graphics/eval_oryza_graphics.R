# Script to create Evaluation Oryza graphics
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2023



## Data para graficos finales 

yield_eval <- metrics_cultivar %>% dplyr::filter(var == "YIELD") %>% unnest(data) %>% mutate(locality = str_sub(exp_file, 1, 4))
biomass_eval <- metrics_cultivar %>% dplyr::filter(var == "WAGT") %>% unnest(data) %>% mutate(locality = str_sub(exp_file, 1, 4))
lai_eval <- metrics_cultivar %>% dplyr::filter(var == "LAI") %>% unnest(data) %>% mutate(locality = str_sub(exp_file, 1, 4))
phen_eval <- metrics_cultivar %>% dplyr::filter(str_detect(var, "DAT")) %>% unnest(data) %>% 
  mutate(locality = str_sub(exp_file, 1, 4)) %>% dplyr::select(param_set:sim) %>% nest(data = -param_set) %>% 
  mutate(metrics = map(data, get_metrics)) %>% unnest(metrics) %>% unnest(data) %>% mutate(locality = str_sub(exp_file, 1, 4))


update_geom_defaults("text", list(size = 3.1))


yield_plot <- yield_eval %>% #mutate(obs = obs/0.86) %>% 
  ggplot(aes(exp_file, obs, label = exp_file)) + 
  geom_point(aes(fill = "Observed Yield"), size = 1.5) +
  geom_errorbar(aes(ymin = obs - se, ymax = obs + se), width = .2, position = position_dodge(0.2), size= 0.8) +
  geom_jitter(aes(y = sim, color = param_set, shape = param_set), position = position_jitter(0.2), size = 3) + 
  #  geom_hline(yintercept = mean(yield_eval$obs)) + 
  #   geom_hline(yintercept = mean(yield_eval$sim), color = "red") + 
  theme_bw() +
  theme(
    #    axis.text.x = element_text(angle = 45),
    #    legend.position = "bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold")) +
  expand_limits(y = 0) + coord_flip() + 
  labs(title = paste0("Yield ", stage,  " - ", cultivar),
       subtitle = "Parameter set comparison",
       x = "Experimental trial",
       y = "Yield - (Kg/ha)", 
       fill = "Experimental data",
       color = "Parameter set", 
       shape = "Parameter set")


ggsave(filename = paste0(path_proj, cultivar, "_", stage,  "_Yield_1.png"), plot = yield_plot, width = 200, height = 130, units = "mm")



lmax <- round(max(max(yield_eval$obs), max(yield_eval$sim)) + 5*10^(-3-1), -3)

yield_xy <- yield_eval %>%
  ggplot(aes(obs, sim, color = locality, label = exp_file)) + geom_point() +
  geom_errorbar(aes(xmin = obs - se, xmax = obs + se), width = .2, position = position_dodge(0.2), size= 0.8) +
  #  expand_limits(x = 0, y = 0) + 
  geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
  geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
  geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
  geom_text(color = "black",aes(x=-Inf, y=Inf, label=paste0( "RMSE: ", round(RMSE, 2)), vjust=1.2), hjust=-0.1) + 
  geom_text(color = "black",aes(x=-Inf, y=Inf, label=paste0( "MAE: ", round(MAE, 2)), vjust=2.4), hjust=-0.1) +
  geom_text(color = "black",aes(x=-Inf, y=Inf, label=paste0( "NRMSE: ", round(NRMSE, 2)), vjust=3.6), hjust=-0.1) +
  geom_text(color = "black",aes(x=-Inf, y=Inf, label=paste0( "Willmott: ", round(d, 2)), vjust=4.8), hjust=-0.1) +
  geom_text(color = "black",aes(x=-Inf, y=Inf, label=paste0( "Nash: ", round(NSE, 2)), vjust=6), hjust=-0.1) +
  coord_equal(xlim = c(0,lmax), ylim = c(0,lmax)) + 
  facet_wrap(param_set ~.) + 
  theme_bw() +
  theme(
    #    axis.text.x = element_text(angle = 45),
    legend.position = "bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))  +
  labs(title = paste0("Yield " , stage,  " - ", cultivar),
       subtitle = "Parameter set comparison",
       x = "Observed Yield (kg/ha)",
       y = "Simulated Yield (kg/ha)", color = "Site")


ggsave(filename = paste0(path_proj, cultivar, "_", stage,  "_Yield_2.png"), plot = yield_xy, width = 190, height = 190, units = "mm")


lmax <- round(max(max(biomass_eval$obs), max(biomass_eval$sim)) + 5*10^(-3-1), -3)

biomass_plot <- biomass_eval %>%
  ggplot(aes(obs, sim, color = locality, label = exp_file)) + geom_point() +
  geom_errorbar(aes(xmin = obs - se, xmax = obs + se), width = .2, position = position_dodge(0.2), size= 0.8) +
  #  expand_limits(x = 0, y = 0) + 
  geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
  geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
  geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "RMSE: ", round(RMSE, 2)), vjust=1.2), hjust=-0.1) + 
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "MAE: ", round(MAE, 2)), vjust=2.4), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "NRMSE: ", round(NRMSE, 2)), vjust=3.6), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "Willmott: ", round(d, 2)), vjust=4.8), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "Nash: ", round(NSE, 2)), vjust=6), hjust=-0.1) +
  coord_equal(xlim = c(0,lmax), ylim = c(0,lmax)) +  
  facet_wrap(param_set ~.) + 
  theme_bw() +
  theme(
    #    axis.text.x = element_text(angle = 45),
    legend.position = "bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))  +
  labs(title = paste0("Biomass " , stage,  " - ", cultivar),
       subtitle = "Parameter set comparison",
       x = "Observed Total Biomass (kg/ha)",
       y = "Simulated Total Biomass (kg/ha)", color = "Site")

ggsave(filename = paste0(path_proj, cultivar, "_", stage,  "_Biomass.png"), plot = biomass_plot, width = 190, height = 190, units = "mm")




lmax <- round(max(max(lai_eval$obs), max(lai_eval$sim)) + 5*10^(-1-1), -1)

lai_plot <- lai_eval %>%
  ggplot(aes(obs, sim, color = locality, label = exp_file)) + geom_point() +
  geom_errorbar(aes(xmin = obs - se, xmax = obs + se), width = .2, position = position_dodge(0.2), size= 0.8) +
  #  expand_limits(x = 0, y = 0) + 
  geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
  geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
  geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "RMSE: ", round(RMSE, 2)), vjust=1.2), hjust=-0.1) + 
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "MAE: ", round(MAE, 2)), vjust=2.4), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "NRMSE: ", round(NRMSE, 2)), vjust=3.6), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "Willmott: ", round(d, 2)), vjust=4.8), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "Nash: ", round(NSE, 2)), vjust=6), hjust=-0.1) +
  coord_equal(xlim = c(0,lmax), ylim = c(0,lmax)) + 
  facet_wrap(param_set ~.) + 
  theme_bw() +
  theme(
    #    axis.text.x = element_text(angle = 45),
    legend.position = "bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))  +
  labs(title = paste0("Leaf Area Index (LAI) ", stage,  " - ", cultivar),
       subtitle = "Parameter set comparison",
       x = "Observed LAI",
       y = "Simulated LAI", color = "Site")

ggsave(filename = paste0(path_proj, cultivar, "_", stage,  "_LAI.png"), plot = lai_plot, width = 190, height = 190, units = "mm")



lmax <- round(max(max(phen_eval$obs), max(phen_eval$sim)) + 5*10^(-1-1), -1)

phen_plot <- phen_eval %>%
  ggplot(aes(obs, sim, color = locality, label = exp_file)) + geom_point() +
  #  geom_errorbar(aes(xmin = obs - se, xmax = obs + se), width = .2, position = position_dodge(0.2), size= 0.8) +
  #  expand_limits(x = 0, y = 0) + 
  geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
  geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
  geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "RMSE: ", round(RMSE, 2)), vjust=1.2), hjust=-0.1) + 
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "MAE: ", round(MAE, 2)), vjust=2.4), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "NRMSE: ", round(NRMSE, 2)), vjust=3.6), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "Willmott: ", round(d, 2)), vjust=4.8), hjust=-0.1) +
  geom_text(color = "black", aes(x=-Inf, y=Inf, label=paste0( "Nash: ", round(NSE, 2)), vjust=6), hjust=-0.1) +
  coord_equal(xlim = c(0,lmax), ylim = c(0,lmax)) + 
  facet_wrap(param_set ~.) + 
  theme_bw() +
  theme(
    #    axis.text.x = element_text(angle = 45),
    legend.position = "bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))  +
  labs(title = paste0("Phenology ", stage,  " - ", cultivar),
       subtitle = "Parameter set comparison",
       x = "Observed Days",
       y = "Simulated Days", color = "Site")