library(patchwork)
model_summary <- readRDS(file = "model_summary.RDS")

fitfull <- model_summary$fit    
outputfull <- model_summary$post

filename <- outputfull$filename
modelname <- outputfull$modelname

n_chains <- outputfull$n_chains
n_post <- outputfull$n_post

chain_samples <- 1:n_chains %>% map(~c(rep(.x, n_post))) %>% unlist

data_t <- fitfull$data_t

abkin_df_ind <- readRDS(file = "ab_kinetics_recov_indiv_ High.RDS")

abkin_df_ind[[1]] <- abkin_df_ind[[1]] %>% mutate(date = days(t) + ymd("2023-01-01") )

pA <- abkin_df_ind[[1]] %>% 
    ggplot() +
        geom_line(aes(x = date, y = titre_trajectory, group = sample), color = "#960319", alpha = 0.05) + facet_wrap(vars(id)) + 
        geom_point(data = data_t$raw_sero %>% filter(id %in% unique(abkin_df_ind[[1]]$id)) %>% mutate(date = days(time) + ymd("2023-01-01") ), 
            aes(x = date, y = PreF), shape = 21, size = 3, fill = "gray") + theme_bw() + 
        labs(x = "Date", y = "PreF titre value log(10)", color = "Exposure type") + ggtitle(paste0("A"))  + 
        theme(text = element_text(size = 12))  + 
    theme_ft() +
    theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))

exptime_df <- readRDS(file = "exposure_time.RDS")

inferred_wave <- map(1:length(exptime_df[[2]]), 
    ~exptime_df[[2]][[.x]] %>% mutate(wave_number = case_when(bin < 350 ~ "Wave 1", TRUE ~ "Wave 2")) %>% mutate(date = days(round(bin, 0)) + ymd("2023-01-01"))
)

pB <- exptime_df[[1]] %>% ggplot() +
     # geom_histogram(aes(x = date, y = after_stat(count), fill = "gray40"), size = 2, alpha = 0.5, color = "black") + 
    #geom_line(data = bind_rows(hist_data), aes(x = bin, y = count), alpha = 0.1, color = "blue") +
    geom_line(data = data.frame(date = inferred_wave[[1]]$date, count = exptime_df[[3]], wave_number = inferred_wave[[1]]$wave_number), 
        aes(x = date, y = count, color = wave_number), size = 1.5) +
    geom_ribbon(data = data.frame(date = inferred_wave[[1]]$date, 
                                    lower = exptime_df[[3]] - exptime_df[[4]], 
                                    upper = exptime_df[[3]] + exptime_df[[4]], 
                                    wave_number = inferred_wave[[1]]$wave_number), 
                aes(x = date, ymin = lower, ymax = upper, fill = wave_number), 
                 alpha = 0.5) +
    scale_fill_manual(
         values =c('Wave 1'='#1f78b4','Wave 2'='#e6550d')) +
    scale_color_manual(
         values =c('Wave 1'='#1f78b4','Wave 2'='#e6550d')) +
    theme_minimal() +
            labs(x = "Date", y = "Number of people infected", fill = "") + 
            ggtitle("B") + 
    guides(color = "none") +
    theme_ft() +
    theme(legend.position = "none") + 
    theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))

known_delta <- nrow(exptime_df[[1]])

df_plot <- 
    map_df(1:length(inferred_wave), ~
    data.frame(
        wave_number = c("Wave 1", "Wave 2"),
        tot = (inferred_wave[[.x]] %>% summarise(sum = sum(count), .by = wave_number) %>% pull(sum)))
    ) %>%   group_by(wave_number) %>%
        summarise(
        q025 = quantile(tot, 0.025),
        q50  = quantile(tot, 0.5),
        q975 = quantile(tot, 0.975)
    )
    
  
colnames(df_plot) <- c("wave_number", "lb", "mean", "ub")
N <- model_summary$fit$data_t$N
pC <- df_plot %>% 
    ggplot() +
  geom_bar(aes(x = wave_number, y = mean / N, fill = wave_number), size = 2, alpha = 0.5, stat = "identity", position = position_dodge(width = 0.0), width = 0.5) +
  geom_errorbar( aes(x = wave_number, ymin = lb /N, ymax = ub/ N), width = 0.4, size = 0.5) + 
    scale_fill_manual(
         values =c('Wave 1'='#1f78b4','Wave 2'='#e6550d')) +
  labs(title = "C",
       x = "RSV wave",
       y = "Incidence rate", fill = "")  + theme_minimal()  + 
    theme_ft() + theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))

pBC <- pB + pC + plot_layout(widths = c(2, 1))

## age group
outputfull <- model_summary$post
fit_states <- outputfull$fit_states
n_chains <- outputfull$n_chains
n_post <- outputfull$n_post
n_length <- n_chains * n_post

df_sero_model_age <- data_t$raw_sero %>% select(id, age_group) %>% unique

length_i <- 4 * (fit_states %>% left_join(df_sero_model_age) %>% pull(sample_c) %>% max)

fit_states_age_1 <- fit_states %>% left_join(df_sero_model_age) %>%
    summarise(inf_tot = sum(inf_ind), n = n(), .by = c("age_group")) %>% 
    mutate(inf_rate = inf_tot / sum(inf_tot)) 

fit_states_age_alt <- fit_states %>% left_join(df_sero_model_age) %>%
    summarise(inf_rate = mean(inf_ind), n = n(), .by = c("age_group")) 
    
pDii <- fit_states_age_alt %>% 
    ggplot() + geom_col(aes(x = age_group, y = inf_rate))  + 
    labs(x = "Age group", y = "Incidence rate (both waves)") +
    ggtitle("E")   + 
    theme_ft()  +
    theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))

pA / (pBC + pDii) + plot_layout(heights = c(2, 1))


