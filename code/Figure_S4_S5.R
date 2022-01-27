all_data <- read_csv("data/processed/all_data.csv")

dataset_names <- c(
  "all_100" = "100%",
  "hi_99" = ">99%, <100%, <1.5Å",
  "lo_99" = ">99%, <100%, 1.5-2.0Å")

plot_data <- all_data %>% 
  mutate(group = cut_width(pLDDT, 10, center = 5),
         group = str_remove_all(group, "[()\\]\\[]"),
         group = str_replace_all(group, ",", "-")) %>%
  group_by(dataset, Mer, group) %>%
  mutate(n = n())

p <- plot_data %>% 
  ggplot() +
  geom_point(aes(group, abs(main_rel_exp - main_rel_af)), stroke = 0, alpha = 0.1, position = "jitter", color = "gray70") +
  geom_hpline(data = subset(plot_data, n>4), aes(group, abs(main_rel_exp - main_rel_af)), stat="summary", fun.y="mean") +
  stat_summary(data = subset(plot_data, n>4),
               aes(group, abs(main_rel_exp - main_rel_af)),
               fun = mean,
               geom = "linerange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x)) +
  labs(x = "pLDDT", y = "Absolute error") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = rel(2))
  )

p1 <- p + facet_grid(Mer ~ dataset, labeller = labeller(dataset = dataset_names))
p2 <- p + facet_grid(~ Mer)

p <- plot_data %>% 
  ggplot() +
  geom_point(aes(group, main_rel_af - main_rel_exp), stroke = 0, alpha = 0.1, position = "jitter", color = "gray70") +
  geom_hpline(data = subset(plot_data, n>4), aes(group, main_rel_af - main_rel_exp), stat="summary", fun="mean") +
  stat_summary(data = subset(plot_data, n>4),
               aes(group, main_rel_af - main_rel_exp),
               fun = mean,
               geom = "linerange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x)) +
  labs(x = "pLDDT", y = expression(RSA["AF"] - RSA["Exp"])) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = rel(2))
  )

p3 <- p + facet_grid(Mer ~ dataset, labeller = labeller(dataset = dataset_names))
p4 <- p + facet_grid(~  Mer) 

plot_grid(p1, p3, ncol = 1, align = "v", rel_heights = c(1, 1), axis = "lr", labels = "AUTO", scale = 1)

ggsave("figures/Figure_S4.png", width = 19, height = 22, unit = "cm", dpi = 600)

plot_grid(p2, p4, ncol = 1, align = "v", rel_heights = c(1, 1), axis = "lr", labels = "AUTO", scale = 1)

ggsave("figures/Figure_S5.png", width = 12, height = 14, unit = "cm", dpi = 600)