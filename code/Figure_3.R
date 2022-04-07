all_data <- read_csv("data/all_data.csv")

plot_data <- all_data %>% 
  mutate(group = cut_width(main_rel_exp, 0.4, center = 0.2),
         group = str_remove_all(group, "[()\\]\\[]"),
         group = str_replace_all(group, ",", "-")) %>% 
  group_by(dataset, Mer, group) %>%
  mutate(n = n())

dataset_names <- c(
  "all_100" = "100%",
  "hi_99" = ">99%, <100%, <1.5Å",
  "lo_99" = ">99%, <100%, 1.5-2.0Å")

error_names <- c(
  "abs" = "Absolute error",
  "signed" = "Signed deviation")

af_average <- function(df) {
  
  df %>% 
    group_by(AF, Number_af, group, dataset, Mer) %>% 
    summarize(
      main_rel_exp = mean(main_rel_exp, na.rm = TRUE),
      main_rel_af = mean(main_rel_af, na.rm = TRUE)
    ) %>% 
    mutate(
      abs = abs(main_rel_af - main_rel_exp),
      signed = main_rel_af - main_rel_exp
    ) %>% 
    pivot_longer(c(abs, signed), names_to = "error_type", values_to = "error") %>% 
    group_by(group, error_type, Mer, dataset) %>%
    mutate(n = n())
  
}

plot_data_af_mono <- plot_data %>%
  filter(Mer == "Monomer") %>% 
  af_average()

p1 <- plot_data_af_mono %>% 
  ggplot() +
  geom_point(aes(group, error), alpha = 0.5, color = "gray70", stroke = 0, position = "jitter") +
  geom_hpline(data = subset(plot_data_af_mono, n>4), aes(group, error), stat="summary", fun="mean") +
  stat_summary(data = subset(plot_data_af_mono, n>4),
               aes(group, error),
               fun = mean,
               geom = "linerange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x)) +
  labs(x = expression(RSA["Exp"]), y = "Error (RSA)") +
  facet_grid(error_type ~ dataset, labeller = labeller(
    dataset = dataset_names, 
    error_type = error_names), scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = rel(2)),
    legend.position = "none"
  )

plot_data <- all_data %>% 
  mutate(group = cut_width(pLDDT, 10, center = 5),
         group = str_remove_all(group, "[()\\]\\[]"),
         group = str_replace_all(group, ",", "-")) %>%
  group_by(dataset, Mer, group) %>%
  mutate(n = n())

plot_data_af_mono <- plot_data %>%
  filter(Mer == "Monomer") %>% 
  af_average()

p2 <- plot_data_af_mono %>% 
  ggplot() +
  geom_point(aes(group, error), alpha = 0.5, color = "gray70", stroke = 0, position = "jitter") +
  geom_hpline(data = subset(plot_data_af_mono, n>4), aes(group, error), stat="summary", fun="mean") +
  stat_summary(data = subset(plot_data_af_mono, n>4),
               aes(group, error),
               fun = mean,
               geom = "linerange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x)) +
  labs(x = "pLDDT", y = "Error (RSA)") +
  facet_grid(error_type ~ dataset, labeller = labeller(
    dataset = dataset_names, 
    error_type = error_names), scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = rel(2)),
    legend.position = "none"
  )

plot_grid(p2, p1, ncol = 1, align = "v", rel_heights = c(1, 1), axis = "lr", labels = "AUTO", scale = 1)

ggsave("figures/Figure_3.png", width = 16, height = 18, unit = "cm", dpi = 600)