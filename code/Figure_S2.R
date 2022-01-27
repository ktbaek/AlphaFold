# read data ---------------------------------------------------------------

all_data <- read_csv("data/all_data.csv")

# Plot individual scatter -------------------------------------------------

plot_scatter_grid <- function(df) {
  
  n_af <- length(unique(df$AF))
  color_values <- c(rep(c("#00AFBB", "#FC4E07"), floor(n_af / 2)), rep('#00AFBB', n_af %% 2))
  
  n_pairs <- length(unique(df$pair))
  
  max_value <- max(c(df$main_rel_af, df$main_rel_exp), na.rm = TRUE)
  
  for(i in c(1:(ceiling(n_pairs / 72)))) {
    df %>% 
      group_by(pair) %>% 
      mutate(
        MAE = mean(abs(main_rel_exp - main_rel_af), na.rm = TRUE),
        MSD = mean(main_rel_af - main_rel_exp, na.rm = TRUE)
      ) %>% 
      ggplot() +
      coord_obs_pred() +
      geom_point(aes(main_rel_af, main_rel_exp, color = AF), stroke = 0, alpha = 0.6, size = rel(0.9)) +
      geom_point(aes(main_rel_af, main_rel_exp, alpha = 1 - pLDDT / 100), color = "black", stroke = 0, size = rel(0.9)) +
      geom_abline(slope = 1, intercept = 0, color = "gray50", size = rel(0.3)) +
      geom_text(
        aes(
          x = 0.1, 
          y = max_value - 0.1, 
          label = Num_chains_exp
        ),
        size = rel(3),
        check_overlap = TRUE) +
      geom_text(
        aes(
          x = max_value - 0.3, 
          y = 0.1, 
          label = count
        ),
        size = rel(3),
        check_overlap = TRUE) +
      geom_text(
        aes(
          x = max_value / 2, 
          y = max_value - 0.1, 
          label = sprintf("%.3f",round(MAE,3))
        ),
        size = rel(3),
        check_overlap = TRUE) +
      labs(x = expression(RSA["AlphaFold"]), y = expression(RSA["Exp"])) +
      facet_wrap_paginate(~ pair, ncol = 8, nrow = 9, page = i) + 
      scale_color_manual(values = color_values) +
      theme(
        strip.text = element_text(size = rel(0.55)),
        strip.text.x = element_text(margin = margin(b = 3)),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none"
      )
    
    ggsave(paste0("figures/Figure_S2_page_", i, ".png"), width = 19, height = 26, unit = "cm", dpi = 600)
    
  }
}

all_data %>% plot_scatter_grid()