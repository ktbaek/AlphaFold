all_data <- read_csv("data/all_data.csv")

af_ids <- all_data %>% 
  filter(Mer == "Monomer") %>% 
  group_by(AF_id) %>% 
  summarize(n_exp = n_distinct(Experimental)) %>% 
  filter(n_exp >= 5) %>% 
  pull(AF_id)

p1 <- all_data %>% 
  filter(AF_id %in% af_ids, Mer == "Monomer") %>%
  group_by(AF_id, Number_af) %>% 
  summarize(
    sd_exp = sd(main_rel_exp, na.rm = TRUE),
    sd_abs = sd(abs(main_rel_af - main_rel_exp), na.rm = TRUE),
    mae = mean(abs(main_rel_af - main_rel_exp), na.rm = TRUE),
    mean_rsa = mean(main_rel_af, na.rm = TRUE)
  ) %>%
  mutate(R = sqrt(rsquared(mae, sd_abs))) %>% 
  ggplot() +
  geom_point(aes(mae, sd_abs), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(mae, sd_abs), method = "lm", color = "orange", fill = "orange", se = FALSE, size = rel(0.6)) +
  geom_text(
    aes(
      x = 0, 
      y = 0.33, 
      label = paste0("R = ", sprintf("%.2f",round(R,2)))
    ),
    hjust = 0,
    vjust = 1,
    size = rel(4),
    check_overlap = TRUE) +
  labs(
    x = "MAE",
    y = expression(SD["Abs"])
  )

p2 <- all_data %>% 
  filter(AF_id %in% af_ids, Mer == "Monomer") %>%
  group_by(AF_id, Number_af) %>% 
  summarize(
    sd_exp = sd(main_rel_exp, na.rm = TRUE),
    sd_abs = sd(abs(main_rel_af - main_rel_exp), na.rm = TRUE),
    mae = mean(abs(main_rel_af - main_rel_exp), na.rm = TRUE),
    mean_rsa_exp = mean(main_rel_exp, na.rm = TRUE),
    mean_rsa_af = mean(main_rel_af, na.rm = TRUE)
  ) %>%
  mutate(R = sqrt(rsquared(mean_rsa_exp, sd_abs))) %>% 
  ggplot() +
  geom_point(aes(mean_rsa_exp, sd_abs), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(mean_rsa_exp, sd_abs), method = "lm", color = "orange", fill = "orange", se = FALSE, size = rel(0.6)) +
  geom_text(
    aes(
      x = 0, 
      y = 0.33, 
      label = paste0("R = ", sprintf("%.2f",round(R,2)))
    ),
    hjust = 0,
    vjust = 1,
    size = rel(4),
    check_overlap = TRUE) +
  labs(
    x = expression(RSA["Exp"]),
    y = expression(SD["Abs"])
  )

plot_grid(p1, p2, ncol = 2, rel_heights = c(1, 1), labels = "AUTO", scale = 1)

ggsave("figures/Figure_6.png", width = 16, height = 7, unit = "cm", dpi = 600)