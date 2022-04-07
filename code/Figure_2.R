all_data <- read_csv("data/all_data.csv")

dataset_names <- c(
  "all_100" = "100%",
  "hi_99" = ">99%, <100%, <1.5Å",
  "lo_99" = ">99%, <100%, 1.5-2.0Å")

p1 <- all_data %>% 
  group_by(dataset, Mer) %>% 
  mutate(R = sqrt(rsquared(main_rel_exp, main_rel_af))) %>% 
  ggplot() +
  geom_point(aes(main_rel_af, main_rel_exp), size = rel(0.3),stroke = 0, alpha = 0.3) +
  scale_x_continuous(limits = c(0,2.2)) +
  scale_y_continuous(limits = c(0,2.2)) +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  geom_text(
    aes(
      x = 0, 
      y = 2.2, 
      label = paste0("R = ", sprintf("%.2f",round(R,2)))
    ),
    hjust = 0,
    vjust = 1,
    size = rel(4),
    check_overlap = TRUE) +
  labs(x = expression(RSA["AF"]), y = expression(RSA["Exp"])) +
  facet_grid(Mer ~ dataset, labeller = labeller(dataset = dataset_names)) 

p2 <- all_data %>% 
  group_by(AF, Number_af, dataset, Mer) %>% 
  summarize(
    main_rel_exp = mean(main_rel_exp, na.rm = TRUE),
    main_rel_af = mean(main_rel_af, na.rm = TRUE)
  ) %>% 
  group_by(dataset, Mer) %>% 
  mutate(R = sqrt(rsquared(main_rel_exp, main_rel_af))) %>% 
  ggplot() +
  geom_point(aes(main_rel_af, main_rel_exp), size = rel(0.3),stroke = 0, alpha = 0.3) +
  scale_x_continuous(limits = c(0,2)) +
  scale_y_continuous(limits = c(0,2)) +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  geom_text(
    aes(
      x = 0, 
      y = 2, 
      label = paste0("R = ", sprintf("%.2f",round(R,2)))
    ),
    hjust = 0,
    vjust = 1, 
    size = rel(4),
    check_overlap = TRUE) +
  labs(x = expression(RSA["AF"]), y = expression(Averaged~RSA["Exp"])) +
  facet_grid(Mer ~ dataset, labeller = labeller(dataset = dataset_names))

plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1), labels = "AUTO", scale = 1)

ggsave("figures/Figure_2.png", width = 15, height = 21, unit = "cm", dpi = 600)