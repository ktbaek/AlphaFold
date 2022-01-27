all_data <- read_csv("data/all_data.csv")

p <- all_data %>% 
  ggplot() +
  geom_point(aes(Wild, abs(main_rel_exp - main_rel_af), color = Wild), position = "jitter", alpha = 0.2, stroke = 0, size = rel(0.5)) +
  geom_hpline(aes(Wild, abs(main_rel_exp - main_rel_af)), stat="summary", fun="mean") +
  stat_summary(
    aes(Wild, abs(main_rel_exp - main_rel_af)),
    fun = mean,
    size = 0.2,
    geom = "linerange",
    fun.max = function(x) mean(x) + sd(x),
    fun.min = function(x) mean(x) - sd(x)) +
  labs(y = "Absolute error", x = "Amino acid") +
  scale_color_manual(values = rep(c("#00AFBB", "#FC4E07"), 10)) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

p1 <- p + facet_grid(~ Mer)

p <- all_data %>% 
  ggplot() +
  geom_point(aes(Wild, main_rel_af - main_rel_exp, color = Wild), position = "jitter", alpha = 0.2, stroke = 0, size = rel(0.5)) +
  geom_hpline(aes(Wild, main_rel_af - main_rel_exp), stat="summary", fun="mean") +
  stat_summary(
    aes(Wild, main_rel_af - main_rel_exp),
    fun = mean,
    size = 0.2,
    geom = "linerange",
    fun.max = function(x) mean(x) + sd(x),
    fun.min = function(x) mean(x) - sd(x)) +
  labs(x = "Amino acid", y = expression(RSA["AF"] - RSA["Exp"])) +
  scale_color_manual(values = rep(c("#00AFBB", "#FC4E07"), 10)) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

p2 <- p + facet_grid(~ Mer)

plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1), axis = "lr", labels = "AUTO", scale = 1)
ggsave("figures/Figure_S8.png", width = 12, height = 14, unit = "cm", dpi = 600)
