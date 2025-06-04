require(readxl)
require(lubridate)
require(posterior)
require(bayesplot)
require(ggdist)
library(tidyverse)

model_summary <- readRDS("model_summary.RDS")))

fitfull <- model_summary$fit    
outputfull <- model_summary$post

filename <- outputfull$filename
modelname <- outputfull$modelname

n_chains <- outputfull$n_chains
n_post <- outputfull$n_post

chain_samples <- 1:n_chains %>% map(~c(rep(.x, n_post))) %>% unlist

data_t <- fitfull$data_t

cop_raw <- readRDS(here::here("outputs", "fits", "fudan_e3_hpc", "base_hier_2", "figs", "post", "plt_data", "cop_data.RDS"))

cop_age <- cop_raw[[1]] %>% left_join(data_t$raw_sero %>% select(id, age_group) %>% unique) %>% filter(titre_val > 3)


require(cmdstanr)
require(posterior)
require(tidybayes)
cop_age_s <- cop_age %>% filter(age_group == "<=5")

data_list_A <- list(
    N = nrow(cop_age_s),
    x = cop_age_s$titre_val,
    y = cop_age_s$prop
)

log_curve_fit <- cmdstan_model( here::here("src", "logit_curve_fit.stan") )

log_curve_fit <- cmdstan_model("logit_curve_fit.stan") 

post_A <- log_curve_fit$sample(data = data_list_A, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)

y_hat_new_post_full <- post_A$draws() %>% spread_draws(y_hat_rel[i], y_prot[i], y_hat_new[i], x_new[i]) 


y_hat_new_post_full %>% 
    ggplot() +
    stat_lineribbon(aes(x = x_new, y = y_hat_new), .width = 0.95,alpha = 0.3) + 
    geom_point(data = cop_age_s, aes(x = titre_val, y = prop), alpha = 0.6, size = 2) +
    facet_wrap(vars(biomarker)) + theme_minimal() +
    guides(color = "none", fill = "none") +
#scale_x_continuous(breaks = seq(0, 1, length.out = 10), labels = labels_x_spike) + 
    labs(x = "Log titre", y = "Posterior probability of infection", color = "Biomarker", fill = "Biomarker") + 
    ggtitle("D. Fitted curves for infection risk")




require(cmdstanr)
require(posterior)
require(tidybayes)
require(patchwork)

cop_age_s <- cop_age 

data_list_A <- list(
    N = nrow(cop_age_s),
    x = cop_age_s$titre_val,
    y = cop_age_s$prop,
    x_cov = cop_age_s$age_group,
    N_cov = 5
)

log_curve_fit <- cmdstan_model( here::here("src", "logit_curve_fit_covar.stan") )
log_curve_fit <- cmdstan_model("logit_curve_fit_covar.stan") 

post_A <- log_curve_fit$sample(data = data_list_A, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)

y_hat_new_post_age <- post_A$draws() %>% spread_draws(y_hat_rel_age[i, j], y_prot_age[i, j], y_hat_new_age[i, j], x_new[i]) %>% 
    mutate(age_group = recode(j, `1` = "<=5", `2` = "5-18", `3` = "19-59", `4` = "60-74", `5` = "75+")) %>% 
    mutate(age_group = factor(age_group, levels = c("<=5", "5-18", "19-59", "60-74", "75+"))) 



post_params <- post_A$draws() %>% spread_draws(L_temp[j], k_temp[j], x0_temp[j]) 

p1 <- post_params %>% ggplot() + 
  stat_pointinterval(aes(x = as.character(j), y = L_temp)) 


p2 <- post_params %>% ggplot() + 
  stat_pointinterval(aes(x = as.character(j), y = k_temp)) 


p3 <- post_params %>% ggplot() + 
  stat_pointinterval(aes(x = as.character(j), y = x0_temp)) 

#p1 / p2 / p3


p1 <- y_hat_new_post_age %>% 
    ggplot() +
    stat_lineribbon(aes(x = x_new, y = y_hat_new_age, fill = age_group), .width = 0.95,alpha = 0.3) + 
    geom_point(data = cop_age_s, aes(x = titre_val, y = prop, color = age_group), alpha = 0.6, size = 2) +
    facet_wrap(vars(biomarker)) + theme_minimal() +
    guides(color = "none", fill = "none") +
    facet_wrap(vars(age_group)) +
#scale_x_continuous(breaks = seq(0, 1, length.out = 10), labels = labels_x_spike) + 
    labs(x = "PreF titre log(10)", y = "Posterior probability of infection", color = "Biomarker", fill = "Biomarker") + 
    ggtitle("A") + 
    theme_ft() + 
    theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))



p2 <- y_hat_new_post_age %>% 
    ggplot() +
    stat_summary(aes(x = x_new, y = y_prot_age, color = age_group), geom = "line", alpha = 1, size = 2) + 
   # geom_point(data = cop_age_s, aes(x = titre_val, y = prop), alpha = 0.6, size = 2) +
   # facet_wrap(vars(j)) + theme_minimal() +
   # facet_wrap(vars(j)) +
#scale_x_continuous(breaks = seq(0, 1, length.out = 10), labels = labels_x_spike) + 
    labs(x = "PreF titre log(10)", y = "Probability of protection given exposure", color = "Biomarker") + 
    ggtitle("B") + 
    theme_ft() + 
    theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10))) +
    theme(legend.position = "none")

p3 <- y_hat_new_post_age %>% 
    ggplot() +
    stat_summary(aes(x = x_new, y = y_hat_rel_age, color = age_group), geom = "line",alpha = 1, size = 2) + 
   # geom_point(data = cop_age_s, aes(x = titre_val, y = prop), alpha = 0.6, size = 2) +
   # facet_wrap(vars(j)) + theme_minimal() +

   # facet_wrap(vars(j)) +
#scale_x_continuous(breaks = seq(0, 1, length.out = 10), labels = labels_x_spike) + 
    labs(x = "PreF titre log(10)", y = "Relative probability of infection", color = "Biomarker") + 
    ggtitle("C") + theme_ft() +
    theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))

p1 / (p2 + p3) + plot_layout(guides = "collect")